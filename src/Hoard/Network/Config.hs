-- |
-- Module: Hoard.Network.Config
-- Description: Configuration types for the Network effect
--
-- This module defines configuration for the Network effect and its associated
-- mini-protocols.
module Hoard.Network.Config
    ( -- * Network configuration
      NetworkConfig (..)
    , ChainSyncConfig (..)
    , BlockFetchConfig (..)

      -- * Defaults
    , defaultNetworkConfig
    , previewTestnetConfig
    ) where

import Data.Word (Word16)
import Ouroboros.Network.Magic (NetworkMagic (..))


-- Placeholder types - will be replaced with proper Cardano block types
type Point = ()


--------------------------------------------------------------------------------
-- Configuration Types
--------------------------------------------------------------------------------

-- | Top-level network configuration.
--
-- Configures connection behavior and protocol-specific settings.
-- Fields use clean names thanks to DuplicateRecordFields.
data NetworkConfig = NetworkConfig
    { networkMagic :: NetworkMagic
    -- ^ Network identifier (mainnet, testnet, etc.)
    , connectTimeout :: Int
    -- ^ Connection timeout in microseconds
    , keepAliveInterval :: Int
    -- ^ KeepAlive ping interval in microseconds
    , chainSyncConfig :: ChainSyncConfig
    -- ^ ChainSync protocol configuration
    , blockFetchConfig :: BlockFetchConfig
    -- ^ BlockFetch protocol configuration
    }
    deriving (Show)


-- | Configuration for the ChainSync mini-protocol.
--
-- ChainSync downloads headers from peers and handles chain reorganizations.
data ChainSyncConfig = ChainSyncConfig
    { startingPoint :: Maybe Point
    -- ^ Optional starting point for synchronization
    , intersectionPoints :: [Point]
    -- ^ Known points to find intersection with peer's chain
    , pipelineDepth :: Word16
    -- ^ Pipeline depth for performance (number of requests in flight)
    }
    deriving (Show)


-- | Configuration for the BlockFetch mini-protocol.
--
-- BlockFetch downloads block bodies after ChainSync provides headers.
data BlockFetchConfig = BlockFetchConfig
    { maxInFlight :: Int
    -- ^ Maximum number of concurrent block requests
    , blockRange :: Maybe (Point, Point)
    -- ^ Optional range constraint for block fetching
    }
    deriving (Show)


--------------------------------------------------------------------------------
-- Default Configurations
--------------------------------------------------------------------------------

-- | Sensible default configuration.
--
-- Uses Preview testnet settings with conservative timeouts.
defaultNetworkConfig :: NetworkConfig
defaultNetworkConfig =
    NetworkConfig
        { networkMagic = NetworkMagic 2 -- Preview testnet
        , connectTimeout = 30_000_000 -- 30 seconds
        , keepAliveInterval = 10_000_000 -- 10 seconds
        , chainSyncConfig =
            ChainSyncConfig
                { startingPoint = Nothing
                , intersectionPoints = []
                , pipelineDepth = 10
                }
        , blockFetchConfig =
            BlockFetchConfig
                { maxInFlight = 100
                , blockRange = Nothing
                }
        }


-- | Preview testnet specific configuration.
--
-- This is an alias for defaultNetworkConfig but makes intent clear.
previewTestnetConfig :: NetworkConfig
previewTestnetConfig = defaultNetworkConfig
