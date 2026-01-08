module Hoard.Triggers (runTriggers) where

import Effectful (Eff, (:>))
import Effectful.Concurrent (Concurrent, threadDelay)

import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Pub (Pub, publish)
import Hoard.Events.ImmutableTipRefreshTriggered (ImmutableTipRefreshTriggered (..))
import Hoard.Events.MonitoringRequest (MonitoringRequest (..))


-- | Starts all periodic triggers.
runTriggers :: (Concurrent :> es, Conc :> es, Pub :> es) => Eff es ()
runTriggers = do
    every 20 $ publish ImmutableTipRefreshTriggered
    every 10 $ publish MonitoringRequest


-- | Runs an action repeatedly every @delay@ seconds in a background thread, starting
-- immediately.
every :: (Concurrent :> es, Conc :> es) => Int -> Eff es () -> Eff es ()
every delay action = do
    Conc.fork_ $ forever $ do
        action
        threadDelay (delay * 1000000)
