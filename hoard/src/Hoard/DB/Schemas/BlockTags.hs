module Hoard.DB.Schemas.BlockTags (Row (..), schema, insertTagsStatement, hashHasTag) where

import Data.Time (UTCTime)
import Hasql.Statement (Statement)
import Rel8 (Column, Name, Rel8able, Result, TableSchema, lit, where_, (==.))

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


deriving stock instance Eq (Row Result)
deriving stock instance Show (Row Result)


schema :: TableSchema (Row Name)
schema = mkSchema "block_tags"


hashHasTag :: Rel8.Expr BlockHash -> Rel8.Query (Rel8.Expr Bool)
hashHasTag hash = Rel8.exists $ do
    tagRow <- Rel8.each schema
    where_ $ tagRow.blockHash ==. hash
    pure tagRow


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
