module Hoard.DB.Schemas.BlockTags (Row (..), schema) where

import Data.Time (UTCTime)
import Rel8 (Column, Name, Rel8able, Result, TableSchema)

import Hoard.DB.Schema (mkSchema)
import Hoard.Data.BlockHash (BlockHash)
import Hoard.Data.BlockTag (BlockTag)
import Hoard.Data.ID (ID)


data Row f = Row
    { id :: Column f (ID BlockTag)
    , blockHash :: Column f BlockHash
    , tag :: Column f BlockTag
    , taggedAt :: Column f UTCTime
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)


deriving instance Eq (Row Result)
deriving instance Show (Row Result)


schema :: TableSchema (Row Name)
schema = mkSchema "block_tags"
