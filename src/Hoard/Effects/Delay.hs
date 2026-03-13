module Hoard.Effects.Delay
    ( Delay
    , wait
    , every
    , runDelay
    , controlDelay
    , Controls (..)
    ) where

import Effectful (Effect, inject)
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, tryPutMVar)
import Effectful.Dispatch.Dynamic (interpret, interpretWith, localSeqUnlift)
import Effectful.State.Static.Shared (evalState, gets, modify)
import Effectful.TH (makeEffect)

import Hoard.Effects.Conc (Conc)

import Hoard.Effects.Conc qualified as Conc


data Delay :: Effect where
    -- | Halt the thread and wait @delay@ seconds.
    Wait :: Int -> Delay m ()
    -- | Runs an action repeatedly every @delay@ seconds, starting immediately.
    -- Returns Void since it runs forever. Caller is responsible for forking.
    Every :: Int -> m () -> Delay m Void


makeEffect ''Delay


asMicro :: Int -> Int
asMicro s = s * 1_000_000


runDelay :: (Concurrent :> es) => Eff (Delay : es) a -> Eff es a
runDelay = interpret \env -> \case
    Wait seconds -> threadDelay (seconds `asMicro`)
    Every seconds action ->
        let delay = (seconds `asMicro`)
        in  forever do
                localSeqUnlift env \unlift -> unlift action
                threadDelay delay


data Controls es = Controls
    { triggerEvery :: Eff es ()
    , triggerDelay :: Eff es ()
    , triggerAll :: Eff es ()
    }


controlDelay :: (Conc :> es, Concurrent :> es) => Eff (Delay : es) a -> Eff es (a, Controls es)
controlDelay eff = evalState @([MVar ()], [MVar ()]) mempty do
    triggerEveryVar <- newEmptyMVar
    _ <- Conc.fork_ $ forever do
        () <- takeMVar triggerEveryVar
        gets snd >>= traverse_ (`tryPutMVar` ())

    triggerDelayVar <- newEmptyMVar
    _ <- Conc.fork_ $ forever do
        () <- takeMVar triggerDelayVar
        gets fst >>= traverse_ (`tryPutMVar` ())

    res <- interpretWith (inject eff) \env -> \case
        Wait _ -> do
            var <- newEmptyMVar
            modify $ first (var :)
            takeMVar var
        Every _ action -> do
            var <- newEmptyMVar
            modify $ second (var :)
            forever do
                localSeqUnlift env \unlift -> unlift action
                takeMVar var

    let ctrl =
            Controls
                { triggerEvery =
                    void $ tryPutMVar triggerEveryVar ()
                , triggerDelay =
                    void $ tryPutMVar triggerDelayVar ()
                , triggerAll = do
                    ctrl.triggerEvery
                    ctrl.triggerDelay
                }

    pure (res, ctrl)
