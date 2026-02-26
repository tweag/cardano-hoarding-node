module Hoard.DB.Schema
    ( mkSchema
    , hoardSchema
    , countRows
    , countRowsWhere
    )
where

import Rel8
    ( Expr
    , Name
    , Rel8able
    , TableSchema (..)
    , namesFromLabelsWith
    , where_
    )
import Text.Casing (quietSnake)

import Data.List.NonEmpty qualified as NonEmpty
import Rel8 qualified

import Hoard.Effects.DBRead (DBRead, runQuery)


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


countRows :: (DBRead :> es, Rel8able row) => TableSchema (row Name) -> Eff es Int
countRows schema =
    runQuery ("countRows[" <> toText schema.name.name <> "]")
        $ fmap fromIntegral
        $ Rel8.run1
        $ Rel8.select
        $ Rel8.aggregate Rel8.countStar
        $ Rel8.each schema


-- | Count rows in a table that satisfy a predicate
countRowsWhere
    :: (DBRead :> es, Rel8able row)
    => TableSchema (row Name)
    -> (row Expr -> Expr Bool)
    -> Eff es Int
countRowsWhere schema predicate =
    runQuery ("countRowsWhere[" <> toText schema.name.name <> "]")
        $ fmap fromIntegral
        $ Rel8.run1
        $ Rel8.select
        $ Rel8.aggregate Rel8.countStar
        $ do
            row <- Rel8.each schema
            where_ $ predicate row
            pure row
