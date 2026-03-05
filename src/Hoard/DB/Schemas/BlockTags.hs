module Hoard.DB.Schemas.BlockTags (Row (..), schema, insertTagsStatement) where

import Data.Time (UTCTime)
import Hasql.Statement (Statement)
import Rel8 (Column, Name, Rel8able, Result, TableSchema, lit)

import Rel8 qualified

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


insertTagsStatement :: [BlockHash] -> [BlockTag] -> Statement () ()
insertTagsStatement hashes tags =
    Rel8.run_
        $ Rel8.insert
            Rel8.Insert
                { into = schema
                , rows = Rel8.values [mkRow hash tag | hash <- hashes, tag <- tags]
                , onConflict = Rel8.DoNothing
                , returning = Rel8.NoReturning
                }
  where
    mkRow hash tag =
        Row
            { id = Rel8.unsafeDefault
            , blockHash = lit hash
            , tag = lit tag
            , taggedAt = Rel8.unsafeDefault
            }
