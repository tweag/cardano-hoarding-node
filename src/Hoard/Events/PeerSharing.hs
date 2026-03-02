module Hoard.Events.PeerSharing
    ( PeersReceived (..)
    ) where

import Data.Time (UTCTime)

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
    , timestamp :: UTCTime
    , peerAddresses :: Set PeerAddress -- The peer addresses we received
    }
    deriving (Show, Typeable)
