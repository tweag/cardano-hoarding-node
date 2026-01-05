module Hoard.Triggers (runTriggers) where

import Effectful (Eff)
import Effectful.Concurrent (threadDelay)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Pub (publish)
import Hoard.Events.ImmutableTipRefreshTriggered (ImmutableTipRefreshTriggered (..))


-- | Starts all periodic triggers.
runTriggers :: (_) => Eff es ()
runTriggers = do
    every 20 $ publish ImmutableTipRefreshTriggered


-- | Runs an action repeatedly every @delay@ seconds in a background thread, starting
-- immediately.
every :: (_) => Int -> Eff es () -> Eff es ()
every delay action = do
    Conc.fork_ $ forever $ do
        action
        threadDelay (delay * 1000000)
