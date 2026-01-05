module Hoard.Listeners.NetworkEventListener (networkEventListener) where

import Effectful (Eff)

import Hoard.Effects.Log qualified as Log
import Hoard.Network.Events
    ( ConnectionEstablishedData (..)
    , ConnectionLostData (..)
    , HandshakeCompletedData (..)
    , NetworkEvent (..)
    , ProtocolErrorData (..)
    )


-- | Listener that logs network events
networkEventListener :: (_) => NetworkEvent -> Eff es ()
networkEventListener = \case
    ConnectionEstablished dat -> do
        Log.info $ "🔗 Connection established with peer at " <> show dat.timestamp
    ConnectionLost dat -> do
        Log.info $ "💔 Connection lost: " <> dat.reason <> " at " <> show dat.timestamp
    HandshakeCompleted dat -> do
        Log.info $ "🤝 Handshake completed with version " <> show dat.version
    ProtocolError dat -> do
        Log.warn $ "❌ Protocol error: " <> dat.errorMessage
