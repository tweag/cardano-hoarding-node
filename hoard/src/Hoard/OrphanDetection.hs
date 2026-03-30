module Hoard.OrphanDetection (component, Config (..)) where

import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Shared (State)

import Atelier.Component (Component (..), defaultComponent)
import Atelier.Effects.Clock (Clock)
import Atelier.Effects.Log (Log)
import Atelier.Effects.Monitoring.Tracing (Tracing)
import Atelier.Effects.Publishing (Sub)
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.NodeToClient (NodeToClient)
import Hoard.Events.BlockFetch (BlockReceived)
import Hoard.OrphanDetection.Config (Config (..))
import Hoard.Types.HoardState (HoardState)

import Atelier.Effects.Log qualified as Log
import Atelier.Effects.Publishing qualified as Sub
import Hoard.ImmutableTip qualified as ImmutableTip
import Hoard.OrphanDetection.Listeners qualified as Listeners


component
    :: ( BlockRepo :> es
       , Clock :> es
       , Log :> es
       , NodeToClient :> es
       , Reader Config :> es
       , State HoardState :> es
       , Sub BlockReceived :> es
       , Sub ImmutableTip.Refreshed :> es
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
                    [ Sub.listen_ $ withErrorHandling Listeners.blockReceivedClassifier
                    , Sub.listen_ $ withErrorHandling Listeners.immutableTipUpdatedAger
                    ]
        }
