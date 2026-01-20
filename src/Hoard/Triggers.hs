module Hoard.Triggers (runTriggers, every) where

import Effectful (Eff, (:>))
import Effectful.Concurrent (Concurrent, threadDelay)

import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Publishing (Pub, publish)
import Hoard.Events.ImmutableTipRefreshTriggered (ImmutableTipRefreshTriggered (..))


-- | Starts all periodic triggers.
runTriggers :: (Concurrent :> es, Conc :> es, Pub :> es) => Eff es ()
runTriggers = do
    every 20 $ publish ImmutableTipRefreshTriggered


-- | Runs an action repeatedly every @delay@ seconds in a background thread, starting
-- immediately.
every :: (Concurrent :> es, Conc :> es) => Int -> Eff es () -> Eff es ()
every delay action = do
    Conc.fork_ $ forever $ do
        action
        threadDelay (delay * 1000000)
