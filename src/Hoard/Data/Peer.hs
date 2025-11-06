module Hoard.Data.Peer
    ( Peer (..)
    , PeerAddress (..)
    , sockAddrToPeerAddress
    )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.List (intercalate)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Network.Socket (HostAddress, HostAddress6, SockAddr (..))
import Numeric (showHex)

import Data.Text qualified as T
import Network.Socket qualified as Socket

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
-- Handles IPv4 and IPv6 addresses from the network socket types.
-- Returns Nothing for Unix domain sockets.
sockAddrToPeerAddress :: SockAddr -> Maybe PeerAddress
sockAddrToPeerAddress = \case
    SockAddrInet portNum hostAddr ->
        let port = fromIntegral portNum
            host = hostAddressToText hostAddr
        in  Just PeerAddress {host, port}
    SockAddrInet6 portNum _flow hostAddr6 _scope ->
        let port = fromIntegral portNum
            host = hostAddress6ToText hostAddr6
        in  Just PeerAddress {host, port}
    SockAddrUnix _ -> Nothing -- We don't handle Unix sockets
  where
    -- Convert IPv4 HostAddress to text representation (e.g., "192.168.1.1")
    hostAddressToText :: HostAddress -> Text
    hostAddressToText addr =
        let (a, b, c, d) = Socket.hostAddressToTuple addr
        in  T.pack $ show a <> "." <> show b <> "." <> show c <> "." <> show d

    -- Convert IPv6 HostAddress6 to text representation (e.g., "[2a05:d014:1cfa:bc01::]")
    --
    -- Note: This uses a simple hex formatting without zero compression.
    -- The brackets are included to match the expected format for IPv6 in host:port notation.
    hostAddress6ToText :: HostAddress6 -> Text
    hostAddress6ToText addr =
        let (a, b, c, d, e, f, g, h) = Socket.hostAddress6ToTuple addr
            parts = [a, b, c, d, e, f, g, h]
            formatted = intercalate ":" $ map (\x -> showHex x "") parts
        in  T.pack $ "[" <> formatted <> "]"
