module Hoard.Core (Core (..)) where

import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Reader.Static (Reader, asks)
import Effectful.State.Static.Shared (State, modify)
import Prelude hiding (Reader, State, asks, modify)

import Hoard.Component (Component (..))
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.HoardStateRepo (HoardStateRepo)
import Hoard.Effects.Monitoring.Tracing (Tracing, addEvent, withSpan)
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
            , IOE :> es
            )


    listeners =
        pure
            [ listen protocolErrorListener
            , listen immutableTipRefreshTriggeredListener
            , listen immutableTipRefreshedListener
            ]


    triggers = do
        refreshInterval <- asks $ (.cardanoNodeIntegration.immutableTipRefreshSeconds)
        pure
            [ every refreshInterval $ publish ImmutableTipRefreshTriggered
            ]


    setup :: (Effects Core es) => Eff es ()
    setup = withSpan "core:setup" $ do
        addEvent "setup_started" []

        -- Set file descriptor limits
        setFileDescriptorLimit

        -- Load immutable tip from DB and set in state
        immutableTip <- HoardStateRepo.getImmutableTip
        modify (\hoardState -> hoardState {immutableTip = immutableTip})

        addEvent "setup_completed" []


    start :: (Effects Core es) => Eff es ()
    start = withSpan "core:start" $ do
        addEvent "start_phase" []
        -- Trigger initial immutable tip refresh now that listeners are registered
        publish ImmutableTipRefreshTriggered
        addEvent "initial_tip_refresh_triggered" []
