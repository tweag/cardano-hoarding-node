module Hoard.Data.Peer
    ( Peer (..)
    , PeerAddress (..)
    , sockAddrToPeerAddress
    , peerAddressToSockAddr
    )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Time (UTCTime)
import Network.Socket (SockAddr)

import Data.IP qualified as IP
import Hoard.Data.ID (ID)
import Hoard.Types.NodeIP (NodeIP (..))
import Prelude hiding (id)


-- | Represents a peer address (host:port)
data PeerAddress = PeerAddress
    { host :: NodeIP
    , port :: Int
    }
    deriving stock (Eq, Ord, Generic, Show)
    deriving (FromJSON, ToJSON)


-- | Represents a peer in the P2P network
data Peer = Peer
    { id :: ID Peer
    , address :: PeerAddress
    , firstDiscovered :: UTCTime
    , lastSeen :: UTCTime
    , lastConnected :: Maybe UTCTime
    , discoveredVia :: Text
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON, ToJSON)


-- | Convert a SockAddr to a PeerAddress
--
-- Handles IPv4 and IPv6 addresses uniformly using the iproute library.
-- Returns Nothing for Unix domain sockets or invalid addresses.
sockAddrToPeerAddress :: SockAddr -> Maybe PeerAddress
sockAddrToPeerAddress sockAddr = do
    (host', portNum) <- IP.fromSockAddr sockAddr
    let port = fromIntegral portNum
        host = NodeIP host'
    pure PeerAddress {host, port}


peerAddressToSockAddr :: PeerAddress -> SockAddr
peerAddressToSockAddr addr =
    IP.toSockAddr (getNodeIP addr.host, fromIntegral $ addr.port)
