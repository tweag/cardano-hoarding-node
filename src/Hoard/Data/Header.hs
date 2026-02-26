module Hoard.Data.Header
    ( Header (..)
    , HeaderReceipt (..)
    )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Time (UTCTime)

import Hoard.Data.BlockHash (BlockHash)
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer)


-- | Represents a block header from the Cardano blockchain
--
-- Stores unique headers indexed by block hash. In Cardano, the block hash
-- is the hash of the header itself, which uniquely identifies the block.
data Header = Header
    { hash :: BlockHash
    , slotNumber :: Word64
    , blockNumber :: Word64
    , firstSeenAt :: UTCTime
    }
    deriving (FromJSON, ToJSON)
    deriving stock (Eq, Generic, Show)


-- | Represents a receipt of a header from a specific peer
--
-- Tracks each time a header was received from a peer, enabling
-- many-to-many relationship between headers and peers.
data HeaderReceipt = HeaderReceipt
    { id :: ID HeaderReceipt
    , hash :: BlockHash
    , peerId :: ID Peer
    , receivedAt :: UTCTime
    }
    deriving (FromJSON, ToJSON)
    deriving stock (Eq, Generic, Show)
