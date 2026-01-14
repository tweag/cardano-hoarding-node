module Hoard.Listeners.PeersReceivedListener (peersReceivedListener) where

import Effectful (Eff, (:>))

import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.PeerRepo (PeerRepo, upsertPeers)
import Hoard.Network.Events (PeersReceived (..))


-- | Handler that persists discovered peers to the database
--
-- Processes PeersReceived events and upserts the peer information into
-- the database.
peersReceivedListener :: (PeerRepo :> es) => PeersReceived -> Eff es ()
peersReceivedListener event =
    void $ upsertPeers event.peerAddresses event.peer.address event.timestamp
