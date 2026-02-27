module Hoard.Data.BlockTag (BlockTag (..)) where

import Rel8 (DBEnum, DBType)

import Rel8 qualified


data BlockTag
    = CorruptBlockIntegrity
    deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
    deriving (DBType) via Rel8.Enum BlockTag


instance DBEnum BlockTag where
    enumTypeName =
        Rel8.QualifiedName
            { name = "block_tag_kind"
            , schema = Just "hoard"
            }
