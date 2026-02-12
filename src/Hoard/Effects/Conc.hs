module Hoard.Effects.Conc
    ( -- * Effect
      Conc
    , Thread
    , fork
    , fork_
    , await
    , awaitAll
    , forkTry

      -- * Interpreters
    , runConc
    )
where

import Effectful
    ( Dispatch (..)
    , DispatchOf
    , Eff
    , Effect
    , IOE
    , Limit (..)
    , Persistence (..)
    , (:>)
    )
import Effectful.Dispatch.Static
    ( SideEffects (..)
    , StaticRep
    , concUnliftIO
    , evalStaticRep
    , unsafeEff
    , unsafeSeqUnliftIO
    )
import Effectful.Dispatch.Static.Primitive (getEnv)
import Ki (Scope, Thread)

import Ki qualified


data Conc :: Effect
type instance DispatchOf Conc = Static WithSideEffects
newtype instance StaticRep Conc = Conc Scope


-- | Fork a computation into a distinct thread and immediately return a
-- reference to said thread.
fork :: (Conc :> es) => Eff es a -> Eff es (Thread a)
fork action = unsafeEff \env -> do
    Conc scope <- getEnv env
    concUnliftIO env Persistent (Limited 1) \unlift ->
        Ki.fork scope $ unlift action


-- | Fork a computation that blocks indefinitely and immediately return. Since
-- the computation is expected to block indefinitely, there is no use to
-- `await` the thread, and thus we don't need a thread reference, so this
-- function does not return one.
fork_ :: (Conc :> es) => Eff es Void -> Eff es ()
fork_ action = unsafeEff \env -> do
    Conc scope <- getEnv env
    concUnliftIO env Persistent (Limited 1) \unlift ->
        Ki.fork_ scope $ unlift action


-- | Await a forked thread, blocking the current thread until the awaited
-- thread terminates.
await :: (Conc :> es) => Thread a -> Eff es a
await thread = unsafeEff \env -> do
    -- We don't use the scope here, but we should use `getStaticRep` here to
    -- ensure we require `Conc` in the context. Otherwise, this function would
    -- hide the use of `IO` in `unsafeEff_` a bit too well, and its behavior
    -- might seem unintuitive.
    Conc _ <- getEnv env
    atomically $ Ki.await thread


-- | Await all forked threads in the current scope.
awaitAll :: (Conc :> es) => Eff es ()
awaitAll = unsafeEff \env -> do
    Conc scope <- getEnv env
    atomically $ Ki.awaitAll scope


-- | Fork a computation and catch all exceptions in an `Either e a`. This will
-- not catch:
-- - Synchronous exceptions that do not match `e`
-- - Asynchronous exceptions
forkTry :: forall e a es. (Conc :> es, Exception e) => Eff es a -> Eff es (Thread (Either e a))
forkTry action = unsafeEff \env -> do
    Conc scope <- getEnv env
    concUnliftIO env Persistent (Limited 1) \unlift ->
        Ki.forkTry scope $ unlift action


-- | Run a `Conc` effect with `Ki`.
runConc :: (IOE :> es) => Eff (Conc : es) a -> Eff es a
runConc eff = do
    unsafeSeqUnliftIO \unlift ->
        Ki.scoped \scope ->
            unlift $ evalStaticRep (Conc scope) eff
