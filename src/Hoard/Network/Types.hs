-- |
-- Module: Hoard.Network.Types
-- Description: Core types for network connections
--
-- This module defines the types used by the Network effect.
module Hoard.Network.Types
    ( -- * Connection types
      Connection (..)
    ) where

import Data.Time (UTCTime)
import Ouroboros.Network.NodeToNode (NodeToNodeVersion)

import Hoard.Data.Peer (Peer)


--------------------------------------------------------------------------------
-- Connection Types
--------------------------------------------------------------------------------

-- | Active connection to a Cardano peer.
--
-- This represents an established connection with all mini-protocols running.
-- The connection is self-managing - once created, it runs autonomously and
-- publishes events as data arrives.
--
-- Note: When using ouroboros-network's connectTo, the socket and protocol threads
-- are managed internally by the library and not directly accessible. This type
-- only tracks the metadata about the connection.
--
-- Fields can be accessed cleanly using RecordDotSyntax:
-- @
--   liftIO $ putStrLn $ "Connected to " <> show conn.peer.address
--   liftIO $ putStrLn $ "Protocol version: " <> show conn.version
-- @
data Connection = Connection
    { peer :: Peer
    -- ^ The peer this connection is established with
    , version :: NodeToNodeVersion
    -- ^ The negotiated node-to-node protocol version
    , started :: UTCTime
    -- ^ When this connection was established
    }
