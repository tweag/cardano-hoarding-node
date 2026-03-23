module Hoard.DB.Schemas.HeaderTags (Row (..), schema, hashHasTag) where

import Data.Time (UTCTime)
import Rel8 (Column, Name, Rel8able, Result, TableSchema, where_, (==.))

import Rel8 qualified

import Hoard.DB.Schema (mkSchema)
import Hoard.Data.BlockHash (BlockHash)
import Hoard.Data.HeaderTag (HeaderTag)
import Hoard.Data.ID (ID)


data Row f = Row
    { id :: Column f (ID HeaderTag)
    , hash :: Column f BlockHash
    , tag :: Column f HeaderTag
    , taggedAt :: Column f UTCTime
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)


deriving stock instance Eq (Row Result)
deriving stock instance Show (Row Result)


schema :: TableSchema (Row Name)
schema = mkSchema "header_tags"


hashHasTag :: Rel8.Expr BlockHash -> Rel8.Query (Rel8.Expr Bool)
hashHasTag hash = Rel8.exists $ do
    tagRow <- Rel8.each schema
    where_ $ tagRow.hash ==. hash
    pure tagRow
