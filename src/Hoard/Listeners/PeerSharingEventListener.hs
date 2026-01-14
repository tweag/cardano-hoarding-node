module Hoard.Listeners.PeerSharingEventListener
    ( peerSharingStartedListener
    , peersReceivedLogListener
    , peerSharingFailedListener
    ) where

import Effectful (Eff, (:>))

import Hoard.Data.Peer (PeerAddress (..))
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Network.Events
    ( PeerSharingFailed (..)
    , PeerSharingStarted (..)
    , PeersReceived (..)
    )


-- | Listener that logs PeerSharing started events
peerSharingStartedListener :: (Log :> es) => PeerSharingStarted -> Eff es ()
peerSharingStartedListener event = do
    Log.info $ "🔍 PeerSharing protocol started at " <> show event.timestamp


-- | Listener that logs peer addresses received events
peersReceivedLogListener :: (Log :> es) => PeersReceived -> Eff es ()
peersReceivedLogListener event = do
    Log.info $ "📡 Received " <> show (length event.peerAddresses) <> " peer addresses from remote peer:"
    forM_ event.peerAddresses $ \addr ->
        Log.debug $ "   - " <> show addr.host <> ":" <> show addr.port


-- | Listener that logs PeerSharing failed events
peerSharingFailedListener :: (Log :> es) => PeerSharingFailed -> Eff es ()
peerSharingFailedListener event = do
    Log.warn $ "❌ PeerSharing failed: " <> event.errorMessage
