module Hoard.Triggers (runTriggers, every) where

import Effectful (Eff, (:>))
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Reader.Static (Reader, asks)
import Prelude hiding (Reader, asks)

import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Publishing (Pub, publish)
import Hoard.Events.ImmutableTipRefreshTriggered (ImmutableTipRefreshTriggered (..))
import Hoard.Types.Environment (CardanoNodeIntegrationConfig (..), Config (..))

import Hoard.Effects.Conc qualified as Conc


-- | Starts all periodic triggers.
runTriggers :: (Conc :> es, Concurrent :> es, Pub ImmutableTipRefreshTriggered :> es, Reader Config :> es) => Eff es ()
runTriggers = do
    refreshInterval <- asks $ (.cardanoNodeIntegration.immutableTipRefreshSeconds)
    Conc.fork_ $ every refreshInterval $ publish ImmutableTipRefreshTriggered


-- | Runs an action repeatedly every @delay@ seconds, starting immediately.
-- Returns Void since it runs forever. Caller is responsible for forking.
every :: (Concurrent :> es) => Int -> Eff es () -> Eff es Void
every delay action = forever $ do
    threadDelay (delay * 1000000)
    action
