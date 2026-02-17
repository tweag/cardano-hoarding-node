{-# LANGUAGE TemplateHaskell #-}

-- | Quota tracking effect for message limits
--
-- Tracks the total number of messages per key, enforcing a hard limit on
-- messages per key with time-based eviction.
--
-- Quota entries are evicted after a configurable TTL (time-to-live), allowing
-- the cache to naturally clean up old entries and prevent unbounded memory growth.
--
-- The effect is polymorphic over the key type, allowing it to be used for
-- various tracking scenarios (e.g., per-peer-per-slot for blocks).
--
-- == Basic Usage
--
-- @
-- handleBlock :: (Quota (ID Peer, Int64) :> es) => Block -> Eff es ()
-- handleBlock block = do
--     let key = (block.peerId, block.slotNumber)
--     withQuotaCheck key $ \\count status -> do
--         case status of
--             Accepted -> insertBlock block
--             FirstOverflow -> do
--                 warn $ "Peer " <> show block.peerId <> " exceeded quota at slot " <> show block.slotNumber
--                 markPeerAsSpam block.peerId
--             SubsequentOverflow ->
--                 debug $ "Additional spam from " <> show block.peerId <> " (count: " <> show count <> ")"
-- @
module Hoard.Effects.Quota
    ( -- * Effect
      Quota
    , MessageStatus (..)
    , withQuotaCheck

      -- * Re-exports
    , module Hoard.Effects.Quota.Config

      -- * Interpreters
    , runQuota
    ) where

import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Effectful (Eff, Effect, (:>))
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Dispatch.Dynamic (interpretWith, localSeqUnlift)
import Effectful.Reader.Static (Reader, ask)
import Effectful.TH (makeEffect)
import Prelude hiding (Reader, ask)

import Data.HashMap.Strict qualified as HashMap
import Effectful.Concurrent.STM qualified as STM

import Hoard.Effects.Clock (Clock, currentTime)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Monitoring.Tracing (Tracing, addAttribute, addEvent)
import Hoard.Effects.Quota.Config

import Hoard.Effects.Conc qualified as Conc


-- | Status of a message with respect to quota limits
--
-- 'FirstOverflow' and 'SubsequentOverflow' are distinct so that callers can
-- react differently to the first violation (e.g. log a warning, flag the peer)
-- without having to manually compare the count against the configured limit.
data MessageStatus
    = -- | Message is within quota limits
      Accepted
    | -- | First message exceeding the quota
      FirstOverflow
    | -- | Subsequent messages after quota was exceeded
      SubsequentOverflow
    deriving stock (Eq, Show)


-- | Quota tracking effect parameterized by key type
data Quota key :: Effect where
    -- | Execute an action with quota checking
    --
    -- The action receives:
    -- - The current message count for this key (after incrementing)
    -- - The status indicating whether the quota is exceeded
    WithQuotaCheck
        :: (Hashable key, Ord key, Show key)
        => key -> (Int -> MessageStatus -> m a) -> Quota key m a


makeEffect ''Quota


-- | Internal state for a quota key
data QuotaState = QuotaState
    { messageCount :: !Int
    , createdAt :: !UTCTime
    }
    deriving stock (Show)


-- | Global quota store: map from keys to per-key state
type QuotaStore key = HashMap.HashMap key (TVar QuotaState)


-- | Run the Quota effect with in-memory STM-based tracking
--
-- Reads quota configuration from the environment.
-- A background thread periodically evicts expired entries based on TTL.
runQuota
    :: forall key es a
     . (Clock :> es, Conc :> es, Concurrent :> es, Hashable key, Reader Config :> es, Show key, Tracing :> es)
    => Eff (Quota key : es) a
    -> Eff es a
runQuota action = do
    quotaConfig <- ask @Config
    let maxMessages = quotaConfig.maxBlocksPerPeerPerSlot
        ttl = quotaConfig.entryTtl
        intervalMicros = round (realToFrac quotaConfig.cleanupInterval * 1_000_000 :: Double)

    store :: TVar (QuotaStore key) <- STM.newTVarIO HashMap.empty

    Conc.fork_ $ forever $ do
        threadDelay intervalMicros
        now <- currentTime
        evicted <- STM.atomically $ evictExpiredEntries store ttl now
        when (evicted > 0)
            $ addEvent "quota_cleanup" [("evicted", show evicted)]

    interpretWith action $ \env -> \case
        WithQuotaCheck key continuation -> localSeqUnlift env $ \unlift -> do
            now <- currentTime
            (count, status) <- STM.atomically $ checkAndUpdate store maxMessages now key

            case status of
                Accepted -> do
                    addAttribute "quota.status" "accepted"
                    addEvent "quota_check" [("key", show key), ("count", show count), ("status", "accepted")]
                FirstOverflow -> do
                    addAttribute "quota.status" "first_overflow"
                    addEvent "quota_exceeded" [("key", show key), ("count", show count), ("limit", show maxMessages)]
                SubsequentOverflow -> do
                    addAttribute "quota.status" "subsequent_overflow"
                    addEvent "quota_overflow_continued" [("key", show key), ("count", show count)]

            unlift $ continuation count status


-- | Atomically check and update quota state for a key
checkAndUpdate
    :: (Hashable key)
    => TVar (QuotaStore key)
    -> Int
    -> UTCTime
    -> key
    -> STM (Int, MessageStatus)
checkAndUpdate storeVar maxMessages now key = do
    store <- STM.readTVar storeVar

    stateVar <- case HashMap.lookup key store of
        Just var -> pure var
        Nothing -> do
            var <- STM.newTVar QuotaState {messageCount = 0, createdAt = now}
            STM.writeTVar storeVar (HashMap.insert key var store)
            pure var

    currentState <- STM.readTVar stateVar
    let newCount = currentState.messageCount + 1
        status
            | newCount <= maxMessages = Accepted
            | newCount == maxMessages + 1 = FirstOverflow
            | otherwise = SubsequentOverflow

        newState =
            QuotaState
                { messageCount = newCount
                , createdAt = currentState.createdAt
                }

    STM.writeTVar stateVar newState
    pure (newCount, status)


-- | Remove all entries whose TTL has expired. Returns the number of evicted entries.
evictExpiredEntries
    :: (Hashable key)
    => TVar (QuotaStore key)
    -> NominalDiffTime
    -> UTCTime
    -> STM Int
evictExpiredEntries storeVar ttl now = do
    store <- STM.readTVar storeVar
    (live, evicted) <- partitionM (isAlive . snd) (HashMap.toList store)
    STM.writeTVar storeVar (HashMap.fromList live)
    pure (length evicted)
  where
    isAlive stateVar = do
        st <- STM.readTVar stateVar
        pure (now < addUTCTime ttl st.createdAt)

    -- Partition a list of monadic predicates into (passing, failing).
    partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
    partitionM p = foldr go (pure ([], []))
      where
        go x acc = do
            (yes, no) <- acc
            b <- p x
            pure if b then (x : yes, no) else (yes, x : no)
