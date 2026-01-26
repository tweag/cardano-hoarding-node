module Hoard.DB.Schema
    ( mkSchema
    , hoardSchema
    , countRows
    )
where

import Rel8
    ( Name
    , Rel8able
    , TableSchema (..)
    , namesFromLabelsWith
    )
import Text.Casing (quietSnake)

import Data.List.NonEmpty qualified as NonEmpty
import Effectful
import Hoard.Effects.DBRead (DBRead, runQuery)
import Rel8 qualified


-- | Default schema name for the application
hoardSchema :: String
hoardSchema = "hoard"


-- | Create a TableSchema for a table in the hoard schema
-- Automatically converts field names from camelCase to snake_case
mkSchema
    :: forall row
     . (Rel8able row)
    => String
    -> TableSchema (row Name)
mkSchema tableName =
    TableSchema
        { name =
            Rel8.QualifiedName
                { name = tableName
                , schema = Just hoardSchema
                }
        , columns =
            namesFromLabelsWith
                @(row Name)
                (quietSnake . NonEmpty.last)
        }


countRows :: (Rel8able row, DBRead :> es) => TableSchema (row Name) -> Eff es Int
countRows schema =
    runQuery ("countRows[" <> toText schema.name.name <> "]") $
        fmap fromIntegral $
            Rel8.run1 $
                Rel8.select $
                    Rel8.aggregate Rel8.countStar $
                        Rel8.each schema
