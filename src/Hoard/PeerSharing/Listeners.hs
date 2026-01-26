module Hoard.PeerSharing.Listeners
    ( peerSharingStarted
    , peersReceived
    , peerSharingFailed
    ) where

import Effectful (Eff, (:>))

import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.PeerRepo (PeerRepo, upsertPeers)
import Hoard.PeerSharing.Events
    ( PeerSharingFailed (..)
    , PeerSharingStarted (..)
    , PeersReceived (..)
    )


-- | Listener that logs PeerSharing started events
peerSharingStarted :: (Log :> es) => PeerSharingStarted -> Eff es ()
peerSharingStarted event = do
    Log.info $ "🔍 PeerSharing protocol started at " <> show event.timestamp


-- | Processes PeersReceived events and upserts the peer information into
-- the database.
peersReceived :: (Log :> es, PeerRepo :> es) => PeersReceived -> Eff es ()
peersReceived event = do
    Log.info $ "📡 Received " <> show (length event.peerAddresses) <> " peer addresses from remote peer:"
    forM_ event.peerAddresses $ \addr ->
        Log.debug $ "   - " <> show addr.host <> ":" <> show addr.port
    void $ upsertPeers event.peerAddresses event.peer.address event.timestamp


-- | Listener that logs PeerSharing failed events
peerSharingFailed :: (Log :> es) => PeerSharingFailed -> Eff es ()
peerSharingFailed event = do
    Log.warn $ "❌ PeerSharing failed: " <> event.errorMessage
