module Hoard.DB.Schemas.HeaderReceipts
    ( Row (..)
    , schema
    , headerReceiptFromRow
    , rowFromHeaderReceipt
    )
where

import Data.Time (UTCTime)
import Rel8
    ( Column
    , Name
    , Rel8able
    , Result
    , TableSchema
    )

import Hoard.DB.Schema (mkSchema)
import Hoard.Data.Header (BlockHash, HeaderReceipt (..))
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer)


data Row f = Row
    { id :: Column f (ID HeaderReceipt)
    , blockHash :: Column f BlockHash
    , peerId :: Column f (ID Peer)
    , receivedAt :: Column f UTCTime
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)


deriving instance Eq (Row Result)


deriving instance Show (Row Result)


-- | Table schema for header receipts
schema :: TableSchema (Row Name)
schema = mkSchema "header_receipts"


-- | Convert a database row to a HeaderReceipt domain type
headerReceiptFromRow :: Row Result -> HeaderReceipt
headerReceiptFromRow row =
    HeaderReceipt
        { id = row.id
        , blockHash = row.blockHash
        , peerId = row.peerId
        , receivedAt = row.receivedAt
        }


-- | Convert a HeaderReceipt domain type to a database row
rowFromHeaderReceipt :: HeaderReceipt -> Row Result
rowFromHeaderReceipt receipt =
    Row
        { id = receipt.id
        , blockHash = receipt.blockHash
        , peerId = receipt.peerId
        , receivedAt = receipt.receivedAt
        }
