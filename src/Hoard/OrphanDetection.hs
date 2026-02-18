module Hoard.OrphanDetection (component) where

import Effectful ((:>))
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.State.Static.Shared (State)
import Prelude hiding (State)

import Hoard.Component (Component (..), defaultComponent)
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Monitoring.Tracing (Tracing)
import Hoard.Effects.NodeToClient (NodeToClient)
import Hoard.Effects.Publishing (Sub)
import Hoard.Events.BlockFetch (BlockReceived)
import Hoard.Listeners.ImmutableTipRefreshTriggeredListener (ImmutableTipRefreshed)
import Hoard.Types.HoardState (HoardState)

import Hoard.Effects.Log qualified as Log
import Hoard.Effects.Publishing qualified as Sub
import Hoard.OrphanDetection.Listeners qualified as Listeners


component
    :: ( BlockRepo :> es
       , Clock :> es
       , Log :> es
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
                withErrorHandling listener event =
                    Log.withNamespace "OrphanDetection"
                        $ runErrorNoCallStack (listener event) >>= \case
                            Left err -> Log.err $ "Listener failed with error " <> err
                            Right () -> pure ()
            in
                pure
                    [ Sub.listen $ withErrorHandling Listeners.blockReceivedClassifier
                    , Sub.listen $ withErrorHandling Listeners.immutableTipUpdatedAger
                    ]
        }
