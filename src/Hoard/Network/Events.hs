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
    , PeerSharingEvent (..)
    , PeerSharingStartedData (..)
    , PeersReceivedData (..)
    , PeerSharingFailedData (..)

      -- * Type aliases for Cardano block types
    , CardanoBlock'
    , Header'
    , Point'
    , Tip'
    ) where

import Data.Time (UTCTime)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, Header, StandardCrypto)
import Ouroboros.Network.Block (Point, Tip)
import Ouroboros.Network.NodeToNode (NodeToNodeVersion)

import Hoard.Data.Peer (PeerAddress)


-- | Type aliases for Cardano block types used throughout the network events.
--
-- These use StandardCrypto which is the standard cryptographic primitives
-- used in the Cardano mainnet and testnets.
type CardanoBlock' = CardanoBlock StandardCrypto


type Header' = Header CardanoBlock'
type Point' = Point CardanoBlock'
type Tip' = Tip CardanoBlock'


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
    { peer :: PeerAddress
    , version :: NodeToNodeVersion
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)


data ConnectionLostData = ConnectionLostData
    { peer :: PeerAddress
    , reason :: Text
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)


data HandshakeCompletedData = HandshakeCompletedData
    { peer :: PeerAddress
    , version :: NodeToNodeVersion
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)


data ProtocolErrorData = ProtocolErrorData
    { peer :: PeerAddress
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
    deriving (Typeable)


data ChainSyncStartedData = ChainSyncStartedData
    { peer :: PeerAddress
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)


data HeaderReceivedData = HeaderReceivedData
    { peer :: PeerAddress
    , header :: Header'
    , point :: Point'
    , tip :: Tip'
    , timestamp :: UTCTime
    }
    deriving (Typeable)


data RollBackwardData = RollBackwardData
    { peer :: PeerAddress
    , point :: Point'
    , tip :: Tip'
    , timestamp :: UTCTime
    }
    deriving (Typeable)


data RollForwardData = RollForwardData
    { peer :: PeerAddress
    , header :: Header'
    , point :: Point'
    , tip :: Tip'
    , timestamp :: UTCTime
    }
    deriving (Typeable)


data ChainSyncIntersectionFoundData = ChainSyncIntersectionFoundData
    { peer :: PeerAddress
    , point :: Point'
    , tip :: Tip'
    , timestamp :: UTCTime
    }
    deriving (Typeable)


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
    deriving (Typeable)


data BlockFetchStartedData = BlockFetchStartedData
    { peer :: PeerAddress
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)


data BlockRequestedData = BlockRequestedData
    { peer :: PeerAddress
    , point :: Point'
    , timestamp :: UTCTime
    }
    deriving (Typeable)


data BlockReceivedData = BlockReceivedData
    { peer :: PeerAddress
    , block :: CardanoBlock'
    , timestamp :: UTCTime
    }
    deriving (Typeable)


data BlockFetchFailedData = BlockFetchFailedData
    { peer :: PeerAddress
    , point :: Point'
    , errorMessage :: Text
    , timestamp :: UTCTime
    }
    deriving (Typeable)


data BlockBatchCompletedData = BlockBatchCompletedData
    { peer :: PeerAddress
    , blockCount :: Int
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)


--------------------------------------------------------------------------------
-- PeerSharing Protocol Events
--------------------------------------------------------------------------------

-- | Events from the PeerSharing mini-protocol.
--
-- PeerSharing allows nodes to discover new peers by requesting peer addresses
-- from connected nodes.
data PeerSharingEvent
    = PeerSharingStarted PeerSharingStartedData
    | PeersReceived PeersReceivedData
    | PeerSharingFailed PeerSharingFailedData
    deriving (Show, Typeable)


data PeerSharingStartedData = PeerSharingStartedData
    { peer :: PeerAddress
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)


data PeersReceivedData = PeersReceivedData
    { peer :: PeerAddress -- The peer we requested from
    , peerAddresses :: Set PeerAddress -- The peer addresses we received
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)


data PeerSharingFailedData = PeerSharingFailedData
    { peer :: PeerAddress
    , errorMessage :: Text
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)
