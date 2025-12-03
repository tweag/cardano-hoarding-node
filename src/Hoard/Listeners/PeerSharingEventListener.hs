module Hoard.Listeners.PeerSharingEventListener (peerSharingEventListener) where

import Effectful (Eff, (:>))

import Hoard.Data.Peer (PeerAddress (..))
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Network.Events
    ( PeerSharingEvent (..)
    , PeerSharingFailedData (..)
    , PeerSharingStartedData (..)
    , PeersReceivedData (..)
    )


-- | Listener that logs peer sharing events
peerSharingEventListener :: (Log :> es) => PeerSharingEvent -> Eff es ()
peerSharingEventListener = \case
    PeerSharingStarted dat -> do
        Log.info $ "🔍 PeerSharing protocol started at " <> show dat.timestamp
    PeersReceived dat -> do
        Log.info $ "📡 Received " <> show (length dat.peerAddresses) <> " peer addresses from remote peer:"
        forM_ dat.peerAddresses $ \addr ->
            Log.debug $ "   - " <> show addr.host <> ":" <> show addr.port
    PeerSharingFailed dat -> do
        Log.warn $ "❌ PeerSharing failed: " <> dat.errorMessage
