-- |
-- Module: Hoard.Network.Events
-- Description: Event types for network operations and protocol activity
--
-- This module defines all event types published by the Network effect and its
-- associated mini-protocols (ChainSync, BlockFetch, KeepAlive).
module Hoard.Network.Events
    ( -- * Network lifecycle events
      ProtocolError (..)

      -- * Protocol-specific events

      -- ** Chain sync
    , ChainSyncStarted (..)
    , HeaderReceived (..)
    , RollBackward (..)
    , RollForward (..)
    , ChainSyncIntersectionFound (..)

      -- ** Block fetch
    , BlockFetchRequest (..)
    , BlockFetchStarted (..)
    , BlockReceived (..)
    , BlockFetchFailed (..)
    , BlockBatchCompleted (..)

      -- ** Peer sharing
    , PeerSharingStarted (..)
    , PeersReceived (..)
    , PeerSharingFailed (..)
    ) where

import Data.Time (UTCTime)

import Hoard.Data.Peer (Peer, PeerAddress)
import Hoard.Types.Cardano (CardanoBlock, CardanoHeader, CardanoPoint, CardanoTip)


--------------------------------------------------------------------------------
-- Network Lifecycle Events
--------------------------------------------------------------------------------

data ProtocolError = ProtocolError
    { peer :: Peer
    , timestamp :: UTCTime
    , errorMessage :: Text
    }
    deriving (Show, Typeable)


--------------------------------------------------------------------------------
-- ChainSync Protocol Events
--------------------------------------------------------------------------------

-- | Events from the ChainSync mini-protocol.
--
-- ChainSync is responsible for synchronizing chain headers with peers,
-- handling forks and rollbacks.
data ChainSyncStarted = ChainSyncStarted
    { peer :: Peer
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)


data HeaderReceived = HeaderReceived
    { peer :: Peer
    , timestamp :: UTCTime
    , header :: CardanoHeader
    , tip :: CardanoTip
    }
    deriving (Typeable)


data RollBackward = RollBackward
    { peer :: Peer
    , timestamp :: UTCTime
    , point :: CardanoPoint
    , tip :: CardanoTip
    }
    deriving (Typeable)


data RollForward = RollForward
    { peer :: Peer
    , timestamp :: UTCTime
    , header :: CardanoHeader
    , point :: CardanoPoint
    , tip :: CardanoTip
    }
    deriving (Typeable)


data ChainSyncIntersectionFound = ChainSyncIntersectionFound
    { peer :: Peer
    , timestamp :: UTCTime
    , point :: CardanoPoint
    , tip :: CardanoTip
    }
    deriving (Typeable)


--------------------------------------------------------------------------------
-- BlockFetch Protocol Events
--------------------------------------------------------------------------------

-- | Events from the BlockFetch mini-protocol.
--
-- BlockFetch is responsible for downloading block bodies after ChainSync
-- has provided the headers.
data BlockFetchStarted = BlockFetchStarted
    { peer :: Peer
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)


-- | A request to fetch a single block.
data BlockFetchRequest = BlockFetchRequest
    { peer :: Peer
    , timestamp :: UTCTime
    , header :: CardanoHeader
    }
    deriving (Typeable)


data BlockReceived = BlockReceived
    { peer :: Peer
    , timestamp :: UTCTime
    , block :: CardanoBlock
    }
    deriving (Typeable)


data BlockFetchFailed = BlockFetchFailed
    { peer :: Peer
    , timestamp :: UTCTime
    , header :: CardanoHeader
    , errorMessage :: Text
    }
    deriving (Typeable)


data BlockBatchCompleted = BlockBatchCompleted
    { peer :: Peer
    , timestamp :: UTCTime
    , blockCount :: Int
    }
    deriving (Show, Typeable)


--------------------------------------------------------------------------------
-- PeerSharing Protocol Events
--------------------------------------------------------------------------------

-- | Events from the PeerSharing mini-protocol.
--
-- PeerSharing allows nodes to discover new peers by requesting peer addresses
-- from connected nodes.
data PeerSharingStarted = PeerSharingStarted
    { peer :: Peer
    , timestamp :: UTCTime
    }
    deriving (Show, Typeable)


data PeersReceived = PeersReceived
    { peer :: Peer
    , timestamp :: UTCTime
    , peerAddresses :: Set PeerAddress -- The peer addresses we received
    }
    deriving (Show, Typeable)


data PeerSharingFailed = PeerSharingFailed
    { peer :: Peer
    , timestamp :: UTCTime
    , errorMessage :: Text
    }
    deriving (Show, Typeable)
