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
    , Name
    , Rel8able
    , Result
    , TableSchema
    )

import Hoard.DB.Schema (mkSchema)
import Hoard.Data.Header (Header (..))
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer)


data Row f = Row
    { id :: Column f (ID Header)
    , receivedAt :: Column f UTCTime
    , receivedFromPeerId :: Column f (ID Peer)
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
        { id = row.id
        , receivedAt = row.receivedAt
        , receivedFromPeerId = row.receivedFromPeerId
        }


-- | Convert a Header domain type to a database row
rowFromHeader :: Header -> Row Result
rowFromHeader header =
    Row
        { id = header.id
        , receivedAt = header.receivedAt
        , receivedFromPeerId = header.receivedFromPeerId
        }
