module Hoard.Effects.Cache.Singleflight
    ( Singleflight
    , withCache
    , updateCache
    , removeFromCache
    , runSingleflight
    ) where

import Effectful (Eff, Effect, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Dispatch.Dynamic (interpretWith, localSeqUnlift)
import Effectful.Exception (throwIO, try)
import Effectful.TH (makeEffect)
import Prelude hiding (Reader, ask)

import Data.Map.Strict qualified as Map
import Effectful.Concurrent.STM qualified as STM

import Hoard.Effects.Monitoring.Tracing (ToAttribute, Tracing, addAttribute, addEvent)


-- | Singleflight cache effect for deduplicating concurrent computations
--
-- When multiple concurrent operations request the same key:
-- - The first request executes the computation
-- - Subsequent requests wait for and share the result
-- - After completion, all requests receive the same result
data Singleflight key value :: Effect where
    -- | Execute a computation with singleflight semantics
    -- If another computation for the same key is in-flight, wait for its result
    WithCache :: (Ord key, ToAttribute key) => key -> m value -> Singleflight key value m value
    -- | Pre-populate the cache with known values
    -- Useful when values are already available to avoid redundant computation
    UpdateCache :: [(key, value)] -> Singleflight key value m ()
    -- | Remove keys from the cache so future requests recompute from source
    RemoveFromCache :: [key] -> Singleflight key value m ()


makeEffect ''Singleflight


type Cache key value = TVar (Map key (TMVar (Either SomeException value)))


-- | Run the Singleflight effect with an in-memory cache
runSingleflight
    :: forall key value es a
     . (Concurrent :> es, Ord key, Tracing :> es)
    => Eff (Singleflight key value : es) a
    -> Eff es a
runSingleflight action = do
    -- Initialize the cache
    cache :: Cache key value <- STM.newTVarIO Map.empty

    -- Run with the cache in Reader context, interpreting Singleflight operations
    interpretWith action $ \env -> \case
        WithCache key computation -> localSeqUnlift env $ \unlift -> do
            -- Singleflight pattern: check if computation is already in-flight
            (mvar, isFirst) <- STM.atomically $ do
                cache' <- STM.readTVar cache
                case Map.lookup key cache' of
                    Just tmvar -> pure (tmvar, False) -- Computation already in flight
                    Nothing -> do
                        -- Create new empty TMVar and insert into cache
                        tmvar <- STM.newEmptyTMVar
                        STM.writeTVar cache (Map.insert key tmvar cache')
                        pure (tmvar, True) -- We're the first request for this key

            -- Try to read the result (non-blocking check if already filled)
            mResult <- STM.atomically $ STM.tryReadTMVar mvar
            case mResult of
                Just (Right value) -> do
                    -- Cache hit with successful value
                    addAttribute @Text "cache.status" "hit"
                    addEvent "cache_hit" [("cache_key", key)]
                    pure value
                Just (Left exception) -> do
                    -- Cache hit but it's an exception - re-throw it
                    addAttribute @Text "cache.status" "hit_exception"
                    addEvent "cache_hit_exception" [("cache_key", key)]
                    throwIO exception
                Nothing
                    | isFirst -> do
                        -- We're the first request - execute computation
                        addAttribute @Text "cache.status" "miss"
                        addEvent "cache_miss" [("cache_key", key)]

                        -- Execute computation and catch any exceptions
                        result <- unlift $ try @SomeException computation

                        -- Try to fill the TMVar with result (success or failure) for waiters
                        -- Use tryPutTMVar in case updateCache already filled it
                        filled <- STM.atomically $ STM.tryPutTMVar mvar result

                        if filled then do
                            -- We successfully filled the TMVar with our result
                            -- If it was an exception, remove from cache so future requests can retry
                            case result of
                                Left _exception -> do
                                    STM.atomically $ do
                                        cache' <- STM.readTVar cache
                                        STM.writeTVar cache (Map.delete key cache')
                                Right _ -> pure ()

                            -- Return the result or re-throw the exception
                            case result of
                                Left exception -> throwIO exception
                                Right value -> pure value
                        else do
                            -- updateCache filled it before us - read and use that value
                            finalResult <- STM.atomically $ STM.readTMVar mvar
                            case finalResult of
                                Left exception -> throwIO exception
                                Right value -> pure value
                    | otherwise -> do
                        -- Another request is already executing, wait for it
                        addAttribute @Text "cache.status" "deduplicated"
                        addEvent "computation_deduplicated_waiting" [("cache_key", key)]
                        result <- STM.atomically $ STM.readTMVar mvar
                        addEvent "computation_deduplicated_received" [("cache_key", key)]

                        -- Re-throw if it was an exception, otherwise return the value
                        case result of
                            Left exception -> throwIO exception
                            Right value -> pure value
        UpdateCache entries -> do
            STM.atomically $ do
                cache' <- STM.readTVar cache
                forM_ entries $ \(key, value) -> do
                    case Map.lookup key cache' of
                        Just existingTMVar -> do
                            -- In-flight computation exists: update its result
                            -- Try to take whatever value is there (or Nothing if empty)
                            _ <- STM.tryTakeTMVar existingTMVar
                            -- Put the correct value (wrapped in Right for success)
                            STM.putTMVar existingTMVar (Right value)
                        Nothing -> do
                            -- No in-flight computation: create fresh TMVar with value
                            tmvar <- STM.newTMVar (Right value)
                            STM.modifyTVar cache (Map.insert key tmvar)
        RemoveFromCache keys ->
            STM.atomically
                $ STM.modifyTVar cache
                $ \cache' -> foldr Map.delete cache' keys
