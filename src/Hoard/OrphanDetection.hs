module Hoard.OrphanDetection (component) where

import Effectful ((:>))
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.State.Static.Shared (State)
import Prelude hiding (State)

import Hoard.Component (Component (..), defaultComponent)
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Monitoring.Tracing (Tracing, addEvent, withSpan)
import Hoard.Effects.NodeToClient (NodeToClient)
import Hoard.Effects.Publishing (Sub)
import Hoard.Events.BlockFetch (BlockReceived)
import Hoard.Listeners.ImmutableTipRefreshTriggeredListener (ImmutableTipRefreshed)
import Hoard.Types.HoardState (HoardState)

import Hoard.Effects.Publishing qualified as Sub
import Hoard.OrphanDetection.Listeners qualified as Listeners


component
    :: ( BlockRepo :> es
       , Clock :> es
       , NodeToClient :> es
       , State HoardState :> es
       , Sub BlockReceived :> es
       , Sub ImmutableTipRefreshed :> es
       , Tracing :> es
       )
    => Component es
component =
    defaultComponent
        { name = "OrphanDetection"
        , listeners =
            let
                withErrorHandling listenerName listener event =
                    withSpan listenerName
                        $ runErrorNoCallStack (listener event) >>= \case
                            Left err -> addEvent "listener_error" [("error", err)]
                            Right () -> pure ()
            in
                pure
                    [ Sub.listen $ withErrorHandling "block_received_classifier" Listeners.blockReceivedClassifier
                    , Sub.listen $ withErrorHandling "immutable_tip_ager" Listeners.immutableTipUpdatedAger
                    ]
        }
