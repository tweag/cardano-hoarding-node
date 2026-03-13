module Atelier.Effects.Delay
    ( Delay
    , wait
    , every
    , Microseconds
    , seconds
    , micros
    , nominalDiffTime
    , runDelay

      -- * Mock delays and give control to the callsite
    , runDelayWithControls
    , settle
    , Timers
    , Timer (..)
    , mkTimers
    , tick
    , tickNext
    ) where

import Data.Time (NominalDiffTime)
import Effectful (Effect, IOE)
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Concurrent.MVar (newEmptyMVar, takeMVar)
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.State.Static.Shared (State, modifyM)
import Effectful.TH (makeEffect)

import Control.Concurrent.MVar qualified as IO


data Delay :: Effect where
    -- | Halt the thread and wait for the passed duration before continuing.
    Wait :: Microseconds -> Delay m ()


newtype Microseconds = Microseconds Word
    deriving stock (Eq, Ord)


seconds :: (Integral i) => i -> Microseconds
seconds = Microseconds . (* 1_000_000) . fromIntegral


micros :: (Integral i) => i -> Microseconds
micros = Microseconds . fromIntegral


nominalDiffTime :: NominalDiffTime -> Microseconds
nominalDiffTime = micros @Word . round @Double . (* 1_000_000) . realToFrac


makeEffect ''Delay


-- | Runs an action repeatedly, waiting the given duration in between each
-- execution, starting immediately. Returns Void since it runs forever.
-- Caller is responsible for forking.
every :: (Delay :> es) => Microseconds -> Eff es () -> Eff es Void
every delay action = forever do
    action
    wait delay


runDelay :: (Concurrent :> es) => Eff (Delay : es) a -> Eff es a
runDelay = interpret_ \(Wait (Microseconds delay)) ->
    threadDelay $ fromIntegral delay


runDelayWithControls
    :: ( Concurrent :> es
       , State Timers :> es
       )
    => Eff (Delay : es) a -> Eff es a
runDelayWithControls = do
    interpret_ \(Wait timeRemaining) -> do
        var <- newEmptyMVar
        modifyM \ts -> do
            let timer =
                    Timer
                        { timeRemaining
                        , release = void $ IO.tryPutMVar var ()
                        }
            pure $ addTimer timer ts
        takeMVar var
        pure ()


tick :: (Concurrent :> es, IOE :> es, State Timers :> es) => Microseconds -> Eff es ()
tick delay = withSettledThreads do
    modifyM $ tickTimers delay


tickNext :: (Concurrent :> es, IOE :> es, State Timers :> es) => Eff es ()
tickNext = withSettledThreads do
    modifyM \case
        [] -> pure []
        t : ts -> do
            let delay = t.timeRemaining
            tickTimers delay (t : ts)


tickTimers :: (IOE :> es) => Microseconds -> Timers -> Eff es Timers
tickTimers delay = fmap catMaybes . traverse (tickTimer delay)


settle :: (Concurrent :> es) => Eff es ()
settle = withSettledThreads $ pure ()


withSettledThreads :: (Concurrent :> es) => Eff es a -> Eff es a
withSettledThreads eff = do
    -- Wait 10 ms, to ensure running threads get the chance to hit a `Wait` or
    -- `Every` operation.
    threadDelay 10_000
    res <- eff
    -- Wait 10 ms, to ensure other threads settle.
    threadDelay 10_000
    pure res


type Timers = [Timer]


mkTimers :: Timers
mkTimers = []


addTimer :: Timer -> Timers -> Timers
addTimer t ts = sortOn (.timeRemaining) $ t : ts


tickTimer
    :: (IOE :> es)
    => Microseconds
    -> Timer
    -> Eff es (Maybe Timer)
tickTimer delay timer
    | timer.timeRemaining > delay =
        pure
            $ Just
                timer
                    { timeRemaining = subMicros timer.timeRemaining delay
                    }
    | otherwise = do
        liftIO timer.release
        pure Nothing


data Timer = Timer
    { release :: IO ()
    , timeRemaining :: Microseconds
    }


subMicros :: Microseconds -> Microseconds -> Microseconds
subMicros (Microseconds a) (Microseconds b) = Microseconds $ a - b
