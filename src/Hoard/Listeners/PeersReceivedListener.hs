module Hoard.Listeners.PeersReceivedListener (peersReceivedListener) where

import Effectful (Eff)

import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.PeerRepo (upsertPeers)
import Hoard.Network.Events (PeerSharingEvent (..), PeersReceivedData (..))


-- | Handler that persists discovered peers to the database
--
-- Processes PeersReceived events and upserts the peer information into
-- the database.
peersReceivedListener :: (_) => PeerSharingEvent -> Eff es ()
peersReceivedListener = \case
    PeersReceived dat -> void $ upsertPeers dat.peerAddresses dat.peer.address dat.timestamp
    _ -> pure () -- Ignore other PeerSharing events
