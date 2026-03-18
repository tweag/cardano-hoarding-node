module Hoard.Events.PeerSharing
    ( PeersReceived (..)
    ) where

import Hoard.Data.Peer (Peer, PeerAddress)


--------------------------------------------------------------------------------
-- PeerSharing Protocol Events
--------------------------------------------------------------------------------

-- | Events from the PeerSharing mini-protocol.
--
-- PeerSharing allows nodes to discover new peers by requesting peer addresses
-- from connected nodes.
data PeersReceived = PeersReceived
    { peer :: Peer
    , peerAddresses :: Set PeerAddress -- The peer addresses we received
    }
    deriving (Show)
