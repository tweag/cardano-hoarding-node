{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module mostly exists to make Effectful's `Eff` compatible with
-- `io-classes` various `Monad` classes, since those are used extensively by
-- the `ouroboros` family of packages.
module Effectful.Instances () where

import Effectful (Eff, (:>))

import Control.Concurrent.STM.TSem qualified as STM
import Control.Concurrent.STM.TVar qualified as STM
import Control.Monad.Class.MonadAsync qualified as IOC
import Control.Monad.Class.MonadFork qualified as IOC
import Control.Monad.Class.MonadST qualified as IOC
import Control.Monad.Class.MonadSTM.Internal qualified as IOC
import Control.Monad.Class.MonadThrow qualified as IOC
import Control.Monad.Primitive (stToPrim)
import Effectful.Concurrent qualified as Eff
import Effectful.Concurrent.Async (Concurrent)
import Effectful.Concurrent.Async qualified as Eff
import Effectful.Concurrent.STM qualified as Eff
import Effectful.Dispatch.Static qualified as Eff
import Effectful.Exception (BlockedIndefinitelyOnSTM, Exception (..), handle, throwIO)
import Effectful.Exception qualified as Eff
import Effectful.Prim (Prim)
import GHC.Conc.Sync qualified as IO
import GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)


newtype EffThreadId es = EffThreadId (Eff.ThreadId)
    deriving (Show, Eq, Ord) via Eff.ThreadId


instance (Concurrent :> es) => IOC.MonadThread (Eff es) where
    type ThreadId (Eff es) = EffThreadId es
    myThreadId = EffThreadId <$> Eff.myThreadId
    labelThread (EffThreadId threadId) x = Eff.unsafeEff_ $ IO.labelThread threadId x
    threadLabel (EffThreadId threadId) = Eff.unsafeEff_ $ IO.threadLabel threadId


newtype EffSTM es a = EffSTM {getEffSTM :: Eff.STM a}
    deriving (Functor, Applicative, Monad) via Eff.STM
newtype EffTVar es a = EffTVar {getEffTVar :: Eff.TVar a}
newtype EffTMVar es a = EffTMVar {getEffTMVar :: Eff.TMVar a}
newtype EffTQueue es a = EffTQueue {getEffTQueue :: Eff.TQueue a}
newtype EffTBQueue es a = EffTBQueue {getEffTBQueue :: Eff.TBQueue a}
data EffTArray es a b
newtype EffTSem es = EffTSem {getEffTSem :: STM.TSem}
newtype EffTChan es a = EffTChan {getEffTChan :: Eff.TChan a}


instance (Concurrent :> es) => IOC.MonadSTM (Eff es) where
    type STM (Eff es) = EffSTM es
    atomically = wrapBlockedIndefinitely . Eff.atomically . getEffSTM


    type TVar (Eff es) = EffTVar es
    type TMVar (Eff es) = EffTMVar es
    type TQueue (Eff es) = EffTQueue es
    type TBQueue (Eff es) = EffTBQueue es
    type TArray (Eff es) = EffTArray es
    type TSem (Eff es) = EffTSem es
    type TChan (Eff es) = EffTChan es


    newTVar = EffSTM . fmap EffTVar . Eff.newTVar
    readTVar = EffSTM . Eff.readTVar . getEffTVar
    writeTVar (EffTVar tvar) x = EffSTM $ Eff.writeTVar tvar x
    retry = EffSTM Eff.retry
    orElse (EffSTM a) (EffSTM b) = EffSTM $ Eff.orElse a b
    check = EffSTM . Eff.check


    modifyTVar (EffTVar tvar) f = EffSTM $ Eff.modifyTVar tvar f
    modifyTVar' (EffTVar tvar) f = EffSTM $ Eff.modifyTVar' tvar f
    stateTVar (EffTVar tvar) f = EffSTM $ STM.stateTVar tvar f
    swapTVar (EffTVar tvar) x = EffSTM $ Eff.swapTVar tvar x


    newTMVar = EffSTM . fmap EffTMVar . Eff.newTMVar
    newEmptyTMVar = EffSTM $ EffTMVar <$> Eff.newEmptyTMVar
    takeTMVar = EffSTM . Eff.takeTMVar . getEffTMVar
    tryTakeTMVar = EffSTM . Eff.tryTakeTMVar . getEffTMVar
    putTMVar (EffTMVar tmvar) x = EffSTM $ Eff.putTMVar tmvar x
    tryPutTMVar (EffTMVar tmvar) x = EffSTM $ Eff.tryPutTMVar tmvar x
    readTMVar = EffSTM . Eff.readTMVar . getEffTMVar
    tryReadTMVar = EffSTM . Eff.tryReadTMVar . getEffTMVar
    swapTMVar (EffTMVar tmvar) x = EffSTM $ Eff.swapTMVar tmvar x
    writeTMVar (EffTMVar tmvar) x = EffSTM $ Eff.writeTMVar tmvar x
    isEmptyTMVar = EffSTM . Eff.isEmptyTMVar . getEffTMVar


    newTQueue = EffSTM $ EffTQueue <$> Eff.newTQueue
    readTQueue = EffSTM . Eff.readTQueue . getEffTQueue
    tryReadTQueue = EffSTM . Eff.tryReadTQueue . getEffTQueue
    peekTQueue = EffSTM . Eff.peekTQueue . getEffTQueue
    tryPeekTQueue = EffSTM . Eff.tryPeekTQueue . getEffTQueue
    flushTQueue = EffSTM . Eff.flushTQueue . getEffTQueue
    writeTQueue (EffTQueue tqueue) x = EffSTM $ Eff.writeTQueue tqueue x
    isEmptyTQueue = EffSTM . Eff.isEmptyTQueue . getEffTQueue
    unGetTQueue (EffTQueue tqueue) x = EffSTM $ Eff.unGetTQueue tqueue x


    newTBQueue = EffSTM . fmap EffTBQueue . Eff.newTBQueue
    readTBQueue = EffSTM . Eff.readTBQueue . getEffTBQueue
    tryReadTBQueue = EffSTM . Eff.tryReadTBQueue . getEffTBQueue
    peekTBQueue = EffSTM . Eff.peekTBQueue . getEffTBQueue
    tryPeekTBQueue = EffSTM . Eff.tryPeekTBQueue . getEffTBQueue
    writeTBQueue (EffTBQueue tbqueue) x = EffSTM $ Eff.writeTBQueue tbqueue x
    flushTBQueue = EffSTM . Eff.flushTBQueue . getEffTBQueue
    lengthTBQueue = EffSTM . Eff.lengthTBQueue . getEffTBQueue
    isEmptyTBQueue = EffSTM . Eff.isEmptyTBQueue . getEffTBQueue
    isFullTBQueue = EffSTM . Eff.isFullTBQueue . getEffTBQueue
    unGetTBQueue (EffTBQueue tbqueue) x = EffSTM $ Eff.unGetTBQueue tbqueue x


    newTSem = EffSTM . fmap EffTSem . STM.newTSem
    waitTSem = EffSTM . STM.waitTSem . getEffTSem
    signalTSem = EffSTM . STM.signalTSem . getEffTSem
    signalTSemN n (EffTSem tsem) = EffSTM $ STM.signalTSemN n tsem


    newTChan = EffSTM $ EffTChan <$> Eff.newTChan
    newBroadcastTChan = EffSTM $ EffTChan <$> Eff.newBroadcastTChan
    dupTChan = EffSTM . fmap EffTChan . Eff.dupTChan . getEffTChan
    cloneTChan = EffSTM . fmap EffTChan . Eff.cloneTChan . getEffTChan
    readTChan = EffSTM . Eff.readTChan . getEffTChan
    tryReadTChan = EffSTM . Eff.tryReadTChan . getEffTChan
    peekTChan = EffSTM . Eff.peekTChan . getEffTChan
    tryPeekTChan = EffSTM . Eff.tryPeekTChan . getEffTChan
    writeTChan (EffTChan tchan) x = EffSTM $ Eff.writeTChan tchan x
    unGetTChan (EffTChan tchan) x = EffSTM $ Eff.unGetTChan tchan x
    isEmptyTChan = EffSTM . Eff.isEmptyTChan . getEffTChan


    newTVarIO = fmap EffTVar . Eff.newTVarIO
    readTVarIO = Eff.readTVarIO . getEffTVar
    newTMVarIO = fmap EffTMVar . Eff.newTMVarIO
    newEmptyTMVarIO = EffTMVar <$> Eff.newEmptyTMVarIO
    newTQueueIO = EffTQueue <$> Eff.newTQueueIO
    newTBQueueIO = fmap EffTBQueue . Eff.newTBQueueIO
    newTChanIO = EffTChan <$> Eff.newTChanIO
    newBroadcastTChanIO = EffTChan <$> Eff.newBroadcastTChanIO


newtype EffAsync es a = EffAsync {getEffAsync :: Eff.Async a}


instance forall es. (Concurrent :> es) => IOC.MonadAsync (Eff es) where
    type Async (Eff es) = EffAsync es
    async = fmap EffAsync . Eff.async
    asyncBound = fmap EffAsync . Eff.asyncBound
    asyncOn cpu = fmap EffAsync . Eff.asyncOn cpu
    asyncThreadId (EffAsync a) = EffThreadId $ Eff.asyncThreadId a
    withAsync action inner = Eff.withAsync action (inner . EffAsync)
    withAsyncBound action inner = Eff.withAsyncBound action (inner . EffAsync)
    withAsyncOn cpu action inner = Eff.withAsyncOn cpu action (inner . EffAsync)


    waitSTM = EffSTM . Eff.waitSTM . getEffAsync
    pollSTM = EffSTM . Eff.pollSTM . getEffAsync
    waitCatchSTM = EffSTM . Eff.waitCatchSTM . getEffAsync


    waitAnySTM as = do
        (a, result) <- EffSTM $ Eff.waitAnySTM (map (\(EffAsync x) -> x) as)
        pure (EffAsync a, result)
    waitAnyCatchSTM as = do
        (a, result) <- EffSTM $ Eff.waitAnyCatchSTM (map (\(EffAsync x) -> x) as)
        pure (EffAsync a, result)
    waitEitherSTM (EffAsync a) (EffAsync b) = EffSTM $ Eff.waitEitherSTM a b
    waitEitherSTM_ (EffAsync a) (EffAsync b) = EffSTM $ Eff.waitEitherSTM_ a b
    waitEitherCatchSTM (EffAsync a) (EffAsync b) = EffSTM $ Eff.waitEitherCatchSTM a b
    waitBothSTM (EffAsync a) (EffAsync b) = EffSTM $ Eff.waitBothSTM a b


    wait (EffAsync a) = Eff.wait a
    poll (EffAsync a) = Eff.poll a
    waitCatch (EffAsync a) = Eff.waitCatch a
    cancel (EffAsync a) = Eff.cancel a
    cancelWith (EffAsync a) e = Eff.cancelWith a e
    uninterruptibleCancel (EffAsync a) = Eff.uninterruptibleCancel a


    waitAny as = do
        (a, result) <- Eff.waitAny (map (\(EffAsync x) -> x) as)
        pure (EffAsync a, result)
    waitAnyCatch as = do
        (a, result) <- Eff.waitAnyCatch (map (\(EffAsync x) -> x) as)
        pure (EffAsync a, result)
    waitAnyCancel as = do
        (a, result) <- Eff.waitAnyCancel (map (\(EffAsync x) -> x) as)
        pure (EffAsync a, result)
    waitAnyCatchCancel as = do
        (a, result) <- Eff.waitAnyCatchCancel (map (\(EffAsync x) -> x) as)
        pure (EffAsync a, result)
    waitEither (EffAsync a) (EffAsync b) = Eff.waitEither a b
    waitEitherCatch (EffAsync a) (EffAsync b) = Eff.waitEitherCatch a b
    waitEitherCancel (EffAsync a) (EffAsync b) = Eff.waitEitherCancel a b
    waitEitherCatchCancel (EffAsync a) (EffAsync b) = Eff.waitEitherCatchCancel a b
    waitEither_ (EffAsync a) (EffAsync b) = Eff.waitEither_ a b
    waitBoth (EffAsync a) (EffAsync b) = Eff.waitBoth a b


    race = Eff.race
    race_ = Eff.race_
    concurrently = Eff.concurrently
    concurrently_ = Eff.concurrently_


    asyncWithUnmask f = EffAsync <$> Eff.asyncWithUnmask f
    asyncOnWithUnmask cpu f = EffAsync <$> Eff.asyncOnWithUnmask cpu f
    withAsyncWithUnmask f inner = Eff.withAsyncWithUnmask f (inner . EffAsync)
    withAsyncOnWithUnmask cpu f inner = Eff.withAsyncOnWithUnmask cpu f (inner . EffAsync)


    compareAsyncs (EffAsync a) (EffAsync b) = Eff.compareAsyncs a b


wrapBlockedIndefinitely :: (HasCallStack) => Eff es a -> Eff es a
wrapBlockedIndefinitely = handle (throwIO . BlockedIndefinitely callStack)


--

-- | Wrapper around 'BlockedIndefinitelyOnSTM' that stores a call stack
data BlockedIndefinitely = BlockedIndefinitely
    { blockedIndefinitelyCallStack :: CallStack
    , blockedIndefinitelyException :: BlockedIndefinitelyOnSTM
    }
    deriving (Show)


instance Exception BlockedIndefinitely where
    displayException (BlockedIndefinitely cs e) =
        unlines
            [ displayException e
            , prettyCallStack cs
            ]


instance IOC.MonadThrow (Eff es) where
    throwIO = Eff.throwIO
    bracket = Eff.bracket
    bracket_ = Eff.bracket_
    finally = Eff.finally


instance (Prim :> es) => IOC.MonadST (Eff es) where
    stToIO = stToPrim
