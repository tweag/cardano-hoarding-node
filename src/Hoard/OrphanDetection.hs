module Hoard.OrphanDetection (run, runListeners) where

import Effectful (Eff, (:>))
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.State.Static.Shared (State)
import Prelude hiding (State)

import Hoard.BlockFetch.Events (BlockReceived)
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Monitoring.Tracing (Tracing, addEvent, withSpan)
import Hoard.Effects.NodeToClient (NodeToClient)
import Hoard.Effects.Publishing (Sub)
import Hoard.Effects.Publishing qualified as Sub
import Hoard.Listeners.ImmutableTipRefreshTriggeredListener (ImmutableTipRefreshed)
import Hoard.OrphanDetection.Listeners qualified as Listeners
import Hoard.Types.HoardState (HoardState)


-- | Run the orphan detection system
run
    :: ( BlockRepo :> es
       , Conc :> es
       , Clock :> es
       , Tracing :> es
       , NodeToClient :> es
       , State HoardState :> es
       , Sub BlockReceived :> es
       , Sub ImmutableTipRefreshed :> es
       )
    => Eff es ()
run = withSpan "orphan_detection" $ do
    addEvent "initializing" []
    runListeners


-- | Start orphan detection listeners
runListeners
    :: ( BlockRepo :> es
       , Conc :> es
       , Clock :> es
       , Tracing :> es
       , NodeToClient :> es
       , State HoardState :> es
       , Sub BlockReceived :> es
       , Sub ImmutableTipRefreshed :> es
       )
    => Eff es ()
runListeners = withSpan "listeners" $ do
    _ <- Conc.fork_ $ Sub.listen $ withErrorHandling "block_received_classifier" Listeners.blockReceivedClassifier
    _ <- Conc.fork_ $ Sub.listen $ withErrorHandling "immutable_tip_ager" Listeners.immutableTipUpdatedAger
    pure ()
  where
    withErrorHandling listenerName listener event =
        withSpan listenerName $
            runErrorNoCallStack (listener event) >>= \case
                Left err -> addEvent "listener_error" [("error", err)]
                Right () -> pure ()
