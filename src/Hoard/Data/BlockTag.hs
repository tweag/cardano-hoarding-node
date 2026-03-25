module Hoard.Data.BlockTag (BlockTag (..)) where

import Rel8 (DBEnum, DBEq, DBType)
import Servant (FromHttpApiData, ToHttpApiData)

import Rel8 qualified

import Atelier.Types.HttpApiDataReadShow (HttpApiDataReadShow (..))


data BlockTag
    = CorruptBlockIntegrity
    | OutsideOfRequestedRange
    | HeaderBlockMismatch
    | SlotDispute
    deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
    deriving (FromHttpApiData, ToHttpApiData) via HttpApiDataReadShow BlockTag
    deriving (DBEq, DBType) via Rel8.Enum BlockTag


instance DBEnum BlockTag where
    enumTypeName =
        Rel8.QualifiedName
            { name = "block_tag_kind"
            , schema = Just "hoard"
            }
