module Hoard.Data.Peer
    ( Peer (..)
    , PeerAddress (..)
    , sockAddrToPeerAddress
    )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.IP (fromSockAddr)
import Data.Time (UTCTime)
import Network.Socket (SockAddr)

import Hoard.Data.ID (ID)
import Hoard.Types.NodeIP (NodeIP (..))
import Prelude hiding (id)


-- | Represents a peer address (host:port)
data PeerAddress = PeerAddress
    { host :: NodeIP
    , port :: Int
    }
    deriving stock (Eq, Generic, Show)


-- | Represents a peer in the P2P network
data Peer = Peer
    { id :: ID Peer
    , address :: NodeIP
    , port :: Int
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
    (host', portNum) <- fromSockAddr sockAddr
    let port = fromIntegral portNum
        host = NodeIP host'
    pure PeerAddress {host, port}
