module Hoard.Data.Header
    ( Header (..)
    , HeaderReceipt (..)
    , BlockHash (..)
    )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Time (UTCTime)
import Rel8 (DBEq, DBOrd, DBType)

import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer)
import Prelude hiding (id)


-- | Newtype wrapper for block hash
newtype BlockHash = BlockHash Text
    deriving stock (Eq, Ord, Generic, Show)
    deriving newtype (FromJSON, ToJSON, DBEq, DBOrd, DBType)


-- | Represents a block header from the Cardano blockchain
--
-- Stores unique headers indexed by block hash. Fields marked TODO
-- require era-specific extraction and will be added later.
data Header = Header
    { blockHash :: BlockHash
    , slotNumber :: Int64
    , blockNumber :: Int64
    , firstSeenAt :: UTCTime
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON, ToJSON)


-- | Represents a receipt of a header from a specific peer
--
-- Tracks each time a header was received from a peer, enabling
-- many-to-many relationship between headers and peers.
data HeaderReceipt = HeaderReceipt
    { id :: ID HeaderReceipt
    , blockHash :: BlockHash
    , peerId :: ID Peer
    , receivedAt :: UTCTime
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON, ToJSON)
