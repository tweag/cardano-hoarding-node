module Hoard.PeerSharing.Listeners
    ( peerSharingStarted
    , peersReceived
    , peerSharingFailed
    ) where

import Effectful (Eff, (:>))

import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.Effects.Monitoring.Tracing (Tracing, addAttribute, addEvent, withSpan)
import Hoard.Effects.PeerRepo (PeerRepo, upsertPeers)
import Hoard.PeerSharing.Events
    ( PeerSharingFailed (..)
    , PeerSharingStarted (..)
    , PeersReceived (..)
    )


-- | Listener that logs PeerSharing started events
peerSharingStarted :: (Tracing :> es) => PeerSharingStarted -> Eff es ()
peerSharingStarted event = do
    addEvent "peer_sharing_started" [("timestamp", show event.timestamp)]


-- | Processes PeersReceived events and upserts the peer information into
-- the database.
peersReceived :: (Tracing :> es, PeerRepo :> es) => PeersReceived -> Eff es ()
peersReceived event = withSpan "peers_received" $ do
    addAttribute "peers.count" (show $ length event.peerAddresses)
    addEvent "peers_received" [("count", show $ length event.peerAddresses)]
    forM_ event.peerAddresses $ \addr ->
        addEvent "peer_address" [("host", show addr.host), ("port", show addr.port)]
    void $ upsertPeers event.peerAddresses event.peer.address event.timestamp


-- | Listener that logs PeerSharing failed events
peerSharingFailed :: (Tracing :> es) => PeerSharingFailed -> Eff es ()
peerSharingFailed event = do
    addEvent "peer_sharing_failed" [("error", event.errorMessage)]
