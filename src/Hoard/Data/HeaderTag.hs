module Hoard.Data.HeaderTag (HeaderTag (..)) where

import Rel8 (DBEnum, DBType)

import Rel8 qualified


data HeaderTag
    = CorruptHeaderIntegrity
    deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
    deriving (DBType) via Rel8.Enum HeaderTag


instance DBEnum HeaderTag where
    enumTypeName =
        Rel8.QualifiedName
            { name = "header_tag_kind"
            , schema = Just "hoard"
            }
