module Hoard.Core (component) where

import Effectful (IOE, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Reader.Static (Reader, asks)
import Effectful.State.Static.Shared (State, modify)
import Prelude hiding (Reader, State, asks, modify)

import Hoard.Component (Component (..), defaultComponent)
import Hoard.Effects.HoardStateRepo (HoardStateRepo)
import Hoard.Effects.Monitoring.Tracing (Tracing, addEvent)
import Hoard.Effects.NodeToClient (NodeToClient)
import Hoard.Effects.Publishing (Pub, Sub, listen, publish)
import Hoard.Events.ImmutableTipRefreshTriggered (ImmutableTipRefreshTriggered (..))
import Hoard.Listeners.ImmutableTipRefreshTriggeredListener (ImmutableTipRefreshed, immutableTipRefreshTriggeredListener, immutableTipRefreshedListener)
import Hoard.Listeners.NetworkEventListener (protocolErrorListener)
import Hoard.Network.Events (ProtocolError)
import Hoard.Setup (setFileDescriptorLimit)
import Hoard.Triggers (every)
import Hoard.Types.Environment (CardanoNodeIntegrationConfig (..), Config (..))
import Hoard.Types.HoardState (HoardState (immutableTip))

import Hoard.Effects.HoardStateRepo qualified as HoardStateRepo


component
    :: ( Concurrent :> es
       , HoardStateRepo :> es
       , IOE :> es
       , NodeToClient :> es
       , Pub ImmutableTipRefreshTriggered :> es
       , Pub ImmutableTipRefreshed :> es
       , Reader Config :> es
       , State HoardState :> es
       , Sub ImmutableTipRefreshTriggered :> es
       , Sub ImmutableTipRefreshed :> es
       , Sub ProtocolError :> es
       , Tracing :> es
       )
    => Component es
component =
    defaultComponent
        { name = "Core"
        , listeners =
            pure
                [ listen protocolErrorListener
                , listen immutableTipRefreshTriggeredListener
                , listen immutableTipRefreshedListener
                ]
        , triggers = do
            refreshInterval <- asks $ (.cardanoNodeIntegration.immutableTipRefreshSeconds)
            pure
                [ every refreshInterval $ publish ImmutableTipRefreshTriggered
                ]
        , setup = do
            -- Set file descriptor limits
            setFileDescriptorLimit

            -- Load immutable tip from DB and set in state
            immutableTip <- HoardStateRepo.getImmutableTip
            modify (\hoardState -> hoardState {immutableTip = immutableTip})
        , start = do
            -- Trigger initial immutable tip refresh now that listeners are registered
            publish ImmutableTipRefreshTriggered
            addEvent "initial_tip_refresh_triggered" []
        }
