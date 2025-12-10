module Hoard.DB.Schemas.Headers
    ( Row (..)
    , schema
    , headerFromRow
    , rowFromHeader
    )
where

import Data.Time (UTCTime)
import Rel8
    ( Column
    , Expr
    , Name
    , Rel8able
    , Result
    , TableSchema
    , lit
    )

import Hoard.DB.Schema (mkSchema)
import Hoard.Data.Header (BlockHash (..), Header (..), HeaderHash (..))


data Row f = Row
    { headerHash :: Column f HeaderHash
    , blockHash :: Column f BlockHash
    , slotNumber :: Column f Int64
    , blockNumber :: Column f Int64
    , firstSeenAt :: Column f UTCTime
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)


deriving instance Eq (Row Result)


deriving instance Show (Row Result)


-- | Table schema for headers
schema :: TableSchema (Row Name)
schema = mkSchema "headers"


-- | Convert a database row to a Header domain type
headerFromRow :: Row Result -> Header
headerFromRow row =
    Header
        { headerHash = row.headerHash
        , blockHash = row.blockHash
        , slotNumber = fromIntegral row.slotNumber
        , blockNumber = fromIntegral row.blockNumber
        , firstSeenAt = row.firstSeenAt
        }


-- | Convert a Header domain type to a database row
rowFromHeader :: Header -> Row Expr
rowFromHeader header =
    Row
        { headerHash = lit header.headerHash
        , blockHash = lit header.blockHash
        , slotNumber = lit $ fromIntegral header.slotNumber
        , blockNumber = lit $ fromIntegral header.blockNumber
        , firstSeenAt = lit header.firstSeenAt
        }
