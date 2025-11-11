module Hoard.Data.Peer
    ( Peer (..)
    , PeerAddress (..)
    , sockAddrToPeerAddress
    )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.IP (fromSockAddr)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Network.Socket (SockAddr)

import Data.Text qualified as T

import Hoard.Data.ID (ID)


-- | Represents a peer address (host:port)
data PeerAddress = PeerAddress
    { host :: Text
    , port :: Int
    }
    deriving stock (Eq, Generic, Show)


-- | Represents a peer in the P2P network
data Peer = Peer
    { id :: ID Peer
    , address :: Text
    , port :: Int
    , firstDiscovered :: UTCTime
    , lastSeen :: UTCTime
    , lastConnected :: Maybe UTCTime
    , discoveredVia :: Text
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)


-- | Convert a SockAddr to a PeerAddress
--
-- Handles IPv4 and IPv6 addresses uniformly using the iproute library.
-- Returns Nothing for Unix domain sockets or invalid addresses.
sockAddrToPeerAddress :: SockAddr -> Maybe PeerAddress
sockAddrToPeerAddress sockAddr = do
    (ip, portNum) <- fromSockAddr sockAddr
    let host = T.pack $ show ip
        port = fromIntegral portNum
    pure PeerAddress {host, port}
