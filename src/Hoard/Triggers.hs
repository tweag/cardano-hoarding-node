module Hoard.Triggers (runTriggers, every) where

import Effectful (Eff, (:>))
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Reader.Static (Reader, asks)
import Prelude hiding (Reader, asks)

import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Publishing (Pub, publish)
import Hoard.Events.ImmutableTipRefreshTriggered (ImmutableTipRefreshTriggered (..))
import Hoard.Types.Environment (CardanoNodeIntegrationConfig (..), Config (..))


-- | Starts all periodic triggers.
runTriggers :: (Concurrent :> es, Conc :> es, Pub :> es, Reader Config :> es) => Eff es ()
runTriggers = do
    refreshInterval <- asks $ (.cardanoNodeIntegration.immutableTipRefreshSeconds)
    every refreshInterval $ publish ImmutableTipRefreshTriggered


-- | Runs an action repeatedly every @delay@ seconds in a background thread, starting
-- immediately.
every :: (Concurrent :> es, Conc :> es) => Int -> Eff es () -> Eff es ()
every delay action = do
    Conc.fork_ $ forever $ do
        threadDelay (delay * 1000000)
        action
