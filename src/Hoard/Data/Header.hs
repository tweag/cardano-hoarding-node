module Hoard.Data.Header
    ( Header (..)
    , HeaderReceipt (..)
    , BlockHash (..)
    , HeaderHash (..)
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


-- | Newtype wrapper for header hash
newtype HeaderHash = HeaderHash Text
    deriving stock (Eq, Ord, Generic, Show)
    deriving newtype (FromJSON, ToJSON, DBEq, DBOrd, DBType)


-- | Represents a block header from the Cardano blockchain
--
-- Stores unique headers indexed by header hash. The headerHash is the hash
-- of the header itself and serves as the primary key. The blockHash is the
-- hash of the block that this header belongs to.
data Header = Header
    { headerHash :: HeaderHash
    , blockHash :: BlockHash
    , slotNumber :: Word64
    , blockNumber :: Word64
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
    , headerHash :: HeaderHash
    , peerId :: ID Peer
    , receivedAt :: UTCTime
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON, ToJSON)
