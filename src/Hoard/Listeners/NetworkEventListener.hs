module Hoard.Listeners.NetworkEventListener
    ( connectionEstablishedListener
    , connectionLostListener
    , handshakeCompletedListener
    , protocolErrorListener
    ) where

import Effectful (Eff, (:>))

import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Network.Events
    ( ConnectionEstablished (..)
    , ConnectionLost (..)
    , HandshakeCompleted (..)
    , ProtocolError (..)
    )


-- | Listener that logs connection established events
connectionEstablishedListener
    :: (Log :> es)
    => ConnectionEstablished
    -> Eff es ()
connectionEstablishedListener event = do
    Log.info $ "🔗 Connection established with peer at " <> show event.timestamp


-- | Listener that logs connection lost events
connectionLostListener
    :: (Log :> es)
    => ConnectionLost
    -> Eff es ()
connectionLostListener event = do
    Log.info $ "💔 Connection lost: " <> event.reason <> " at " <> show event.timestamp


-- | Listener that logs handshake completed events
handshakeCompletedListener
    :: (Log :> es)
    => HandshakeCompleted
    -> Eff es ()
handshakeCompletedListener event = do
    Log.info $ "🤝 Handshake completed with version " <> show event.version


-- | Listener that logs protocol error events
protocolErrorListener
    :: (Log :> es)
    => ProtocolError
    -> Eff es ()
protocolErrorListener event = do
    Log.warn $ "❌ Protocol error: " <> event.errorMessage
