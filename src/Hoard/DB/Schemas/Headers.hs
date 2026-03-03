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
import Hoard.Data.BlockHash (BlockHash)
import Hoard.Data.Eras (BlockEra, headerToEra)
import Hoard.Data.Header (Header (..), decodeCardanoHeader, encodeCardanoHeader)


data Row f = Row
    { hash :: Column f BlockHash
    , headerData :: Column f ByteString
    , headerEra :: Column f BlockEra
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
headerFromRow :: Row Result -> Either Text Header
headerFromRow row = do
    headerData <- decodeCardanoHeader row.headerEra row.headerData
    pure
        $ Header
            { hash = row.hash
            , headerData
            , slotNumber = fromIntegral row.slotNumber
            , blockNumber = fromIntegral row.blockNumber
            , firstSeenAt = row.firstSeenAt
            }


-- | Convert a Header domain type to a database row
rowFromHeader :: Header -> Row Expr
rowFromHeader header =
    Row
        { hash = lit header.hash
        , headerData = lit $ encodeCardanoHeader header.headerData
        , headerEra = lit $ headerToEra $ header.headerData
        , slotNumber = lit $ fromIntegral header.slotNumber
        , blockNumber = lit $ fromIntegral header.blockNumber
        , firstSeenAt = lit header.firstSeenAt
        }
