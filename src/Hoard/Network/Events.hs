-- |
-- Module: Hoard.Network.Events
-- Description: Event types for network operations and protocol activity
--
-- This module defines all event types published by the Network effect and its
-- associated mini-protocols (ChainSync, BlockFetch, KeepAlive).
module Hoard.Network.Events
    ( -- * Network lifecycle events
      NetworkEvent (..)
    , ConnectionEstablishedData (..)
    , ConnectionLostData (..)
    , HandshakeCompletedData (..)
    , ProtocolErrorData (..)

      -- * Protocol-specific events
    , ChainSyncEvent (..)
    , ChainSyncStartedData (..)
    , HeaderReceivedData (..)
    , RollBackwardData (..)
    , RollForwardData (..)
    , ChainSyncIntersectionFoundData (..)
    , BlockFetchEvent (..)
    , BlockFetchStartedData (..)
    , BlockRequestedData (..)
    , BlockReceivedData (..)
    , BlockFetchFailedData (..)
    , BlockBatchCompletedData (..)
    ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Ouroboros.Network.NodeToNode (NodeToNodeVersion)

import Hoard.Data.Peer (Peer)


-- Note: We'll use placeholder types for now since we don't have Cardano block types yet
-- These will be replaced with proper types from ouroboros-consensus
type CardanoBlock = ()
type Header = ()
type Point = ()


--------------------------------------------------------------------------------
-- Network Lifecycle Events
--------------------------------------------------------------------------------

-- | Events related to connection lifecycle and protocol negotiation.
--
-- These events are published by the Network effect handler during connection
-- establishment, version negotiation, and disconnection.
data NetworkEvent
    = ConnectionEstablished ConnectionEstablishedData
    | ConnectionLost ConnectionLostData
    | HandshakeCompleted HandshakeCompletedData
    | ProtocolError ProtocolErrorData
    deriving (Show, Typeable)


data ConnectionEstablishedData = ConnectionEstablishedData
    { peer :: Peer
    , version :: NodeToNodeVersion
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)


data ConnectionLostData = ConnectionLostData
    { peer :: Peer
    , reason :: Text
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)


data HandshakeCompletedData = HandshakeCompletedData
    { peer :: Peer
    , version :: NodeToNodeVersion
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)


data ProtocolErrorData = ProtocolErrorData
    { peer :: Peer
    , errorMessage :: Text
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)


--------------------------------------------------------------------------------
-- ChainSync Protocol Events
--------------------------------------------------------------------------------

-- | Events from the ChainSync mini-protocol.
--
-- ChainSync is responsible for synchronizing chain headers with peers,
-- handling forks and rollbacks.
data ChainSyncEvent
    = ChainSyncStarted ChainSyncStartedData
    | HeaderReceived HeaderReceivedData
    | RollBackward RollBackwardData
    | RollForward RollForwardData
    | ChainSyncIntersectionFound ChainSyncIntersectionFoundData
    deriving (Show, Typeable)


data ChainSyncStartedData = ChainSyncStartedData
    { peer :: Peer
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)


data HeaderReceivedData = HeaderReceivedData
    { peer :: Peer
    , header :: Header
    , point :: Point
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)


data RollBackwardData = RollBackwardData
    { peer :: Peer
    , point :: Point
    , blocksRolledBack :: Int
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)


data RollForwardData = RollForwardData
    { peer :: Peer
    , header :: Header
    , point :: Point
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)


data ChainSyncIntersectionFoundData = ChainSyncIntersectionFoundData
    { peer :: Peer
    , point :: Point
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)


--------------------------------------------------------------------------------
-- BlockFetch Protocol Events
--------------------------------------------------------------------------------

-- | Events from the BlockFetch mini-protocol.
--
-- BlockFetch is responsible for downloading block bodies after ChainSync
-- has provided the headers.
data BlockFetchEvent
    = BlockFetchStarted BlockFetchStartedData
    | BlockRequested BlockRequestedData
    | BlockReceived BlockReceivedData
    | BlockFetchFailed BlockFetchFailedData
    | BlockBatchCompleted BlockBatchCompletedData
    deriving (Show, Typeable)


data BlockFetchStartedData = BlockFetchStartedData
    { peer :: Peer
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)


data BlockRequestedData = BlockRequestedData
    { peer :: Peer
    , point :: Point
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)


data BlockReceivedData = BlockReceivedData
    { peer :: Peer
    , block :: CardanoBlock
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)


data BlockFetchFailedData = BlockFetchFailedData
    { peer :: Peer
    , point :: Point
    , errorMessage :: Text
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)


data BlockBatchCompletedData = BlockBatchCompletedData
    { peer :: Peer
    , blockCount :: Int
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)
