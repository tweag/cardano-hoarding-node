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

      -- ** Peer sharing
    , PeerSharingStarted (..)
    , PeersReceived (..)
    , PeerSharingFailed (..)
    ) where

import Data.Time (UTCTime)

import Hoard.Data.Peer (Peer, PeerAddress)


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
