module Hoard.Core (Core (..)) where

import Effectful (Eff, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Reader.Static (Reader, asks)
import Effectful.State.Static.Shared (State)
import Prelude hiding (Reader, State, asks)

import Hoard.Component (Component (..), Listener, Trigger)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.HoardStateRepo (HoardStateRepo)
import Hoard.Effects.Monitoring.Tracing (Tracing)
import Hoard.Effects.NodeToClient (NodeToClient)
import Hoard.Effects.Publishing (Pub, Sub, listen, publish)
import Hoard.Events.ImmutableTipRefreshTriggered (ImmutableTipRefreshTriggered (..))
import Hoard.Listeners.ImmutableTipRefreshTriggeredListener (ImmutableTipRefreshed, immutableTipRefreshTriggeredListener, immutableTipRefreshedListener)
import Hoard.Listeners.NetworkEventListener (protocolErrorListener)
import Hoard.Network.Events (ProtocolError)
import Hoard.Triggers (every)
import Hoard.Types.Environment (CardanoNodeIntegrationConfig (..), Config (..))
import Hoard.Types.HoardState (HoardState)


data Core = Core


instance Component Core es where
    type
        Effects Core es =
            ( Conc :> es
            , Concurrent :> es
            , Tracing :> es
            , NodeToClient :> es
            , State HoardState :> es
            , Sub ProtocolError :> es
            , Sub ImmutableTipRefreshTriggered :> es
            , Sub ImmutableTipRefreshed :> es
            , Pub ImmutableTipRefreshed :> es
            , Pub ImmutableTipRefreshTriggered :> es
            , HoardStateRepo :> es
            , Reader Config :> es
            )


    listeners :: (Effects Core es) => Eff es [Listener es]
    listeners =
        pure
            [ listen protocolErrorListener
            , listen immutableTipRefreshTriggeredListener
            , listen immutableTipRefreshedListener
            ]


    triggers :: (Effects Core es) => Eff es [Trigger es]
    triggers = do
        refreshInterval <- asks $ (.cardanoNodeIntegration.immutableTipRefreshSeconds)
        pure [every refreshInterval $ publish ImmutableTipRefreshTriggered]
