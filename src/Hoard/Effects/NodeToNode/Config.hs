module Hoard.Effects.NodeToNode.Config
    ( -- * Connection config
      NodeToNodeConfig (..)

      -- * Mini-protocol configs
    , BlockFetchConfig (..)
    , ChainSyncConfig (..)
    , KeepAliveConfig (..)
    , PeerSharingConfig (..)

      -- * Unified protocols config
    , ProtocolsConfig (..)
    ) where

import Data.Aeson (FromJSON (..))
import Data.Default (Default (..))
import Data.Time (NominalDiffTime)

import Hoard.Types.QuietSnake (QuietSnake (..))


-- | Connection-level configuration (TCP handshake timeout etc.)
data NodeToNodeConfig = NodeToNodeConfig
    { connectionTimeoutSeconds :: NominalDiffTime
    -- ^ Timeout for the TCP connection handshake. If the socket doesn't
    -- connect within this duration, the connection attempt fails. This does
    -- NOT affect already-established connections.
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via (QuietSnake NodeToNodeConfig)


instance Default NodeToNodeConfig where
    def =
        NodeToNodeConfig
            { connectionTimeoutSeconds = 10
            }


data BlockFetchConfig = BlockFetchConfig
    { batchSize :: Int
    -- ^ Number of block fetch requests to batch
    , batchTimeoutMicroseconds :: Int
    -- ^ Timeout for batching block fetch requests
    , maximumIngressQueue :: Int
    -- ^ Max bytes queued in ingress queue
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake BlockFetchConfig


instance Default BlockFetchConfig where
    def =
        BlockFetchConfig
            { batchSize = 10
            , batchTimeoutMicroseconds = 10_000_000 -- 10 seconds
            , maximumIngressQueue = 393216 -- 384 KiB
            }


data ChainSyncConfig = ChainSyncConfig
    { maximumIngressQueue :: Int
    -- ^ Max bytes queued in ingress queue
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake ChainSyncConfig


instance Default ChainSyncConfig where
    def =
        ChainSyncConfig
            { maximumIngressQueue = 1200
            }


data KeepAliveConfig = KeepAliveConfig
    { intervalMicroseconds :: Int
    -- ^ Interval between keepalive messages
    , maximumIngressQueue :: Int
    -- ^ Max bytes queued in ingress queue
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake KeepAliveConfig


instance Default KeepAliveConfig where
    def =
        KeepAliveConfig
            { intervalMicroseconds = 10_000_000
            , maximumIngressQueue = 1000
            }


data PeerSharingConfig = PeerSharingConfig
    { requestIntervalMicroseconds :: Int
    -- ^ Interval between peer sharing requests
    , requestAmount :: Int
    -- ^ Number of peer addresses to request per query
    , maximumIngressQueue :: Int
    -- ^ Max bytes queued in ingress queue
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake PeerSharingConfig


instance Default PeerSharingConfig where
    def =
        PeerSharingConfig
            { requestIntervalMicroseconds = 3600_000_000 -- 1 hour
            , requestAmount = 100
            , maximumIngressQueue = 1000
            }


-- | Unified configuration for all Cardano mini-protocols.
data ProtocolsConfig = ProtocolsConfig
    { blockFetch :: BlockFetchConfig
    , chainSync :: ChainSyncConfig
    , keepAlive :: KeepAliveConfig
    , peerSharing :: PeerSharingConfig
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake ProtocolsConfig
