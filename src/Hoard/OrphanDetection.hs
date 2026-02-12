module Hoard.OrphanDetection (OrphanDetection (..)) where

import Effectful (Eff, (:>))
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.State.Static.Shared (State)
import Prelude hiding (State)

import Hoard.BlockFetch.Events (BlockReceived)
import Hoard.Component (Component (..), Listener)
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Monitoring.Tracing (Tracing, addEvent, withSpan)
import Hoard.Effects.NodeToClient (NodeToClient)
import Hoard.Effects.Publishing (Sub)
import Hoard.Listeners.ImmutableTipRefreshTriggeredListener (ImmutableTipRefreshed)
import Hoard.Types.HoardState (HoardState)

import Hoard.Effects.Publishing qualified as Sub
import Hoard.OrphanDetection.Listeners qualified as Listeners


data OrphanDetection = OrphanDetection


instance Component OrphanDetection es where
    type
        Effects OrphanDetection es =
            ( BlockRepo :> es
            , Conc :> es
            , Clock :> es
            , Tracing :> es
            , NodeToClient :> es
            , State HoardState :> es
            , Sub BlockReceived :> es
            , Sub ImmutableTipRefreshed :> es
            )


    listeners :: (Effects OrphanDetection es) => Eff es [Listener es]
    listeners =
        pure
            [ Sub.listen $ withErrorHandling "block_received_classifier" Listeners.blockReceivedClassifier
            , Sub.listen $ withErrorHandling "immutable_tip_ager" Listeners.immutableTipUpdatedAger
            ]
      where
        withErrorHandling listenerName listener event =
            withSpan listenerName
                $ runErrorNoCallStack (listener event) >>= \case
                    Left err -> addEvent "listener_error" [("error", err)]
                    Right () -> pure ()
