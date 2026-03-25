module Hoard.Data.HeaderTag (HeaderTag (..)) where

import Rel8 (DBEnum, DBEq, DBType)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)

import Rel8 qualified

import Atelier.Types.HttpApiDataReadShow (HttpApiDataReadShow (..))


data HeaderTag
    = CorruptHeaderIntegrity
    deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
    deriving (FromHttpApiData, ToHttpApiData) via HttpApiDataReadShow HeaderTag
    deriving (DBEq, DBType) via Rel8.Enum HeaderTag


instance DBEnum HeaderTag where
    enumTypeName =
        Rel8.QualifiedName
            { name = "header_tag_kind"
            , schema = Just "hoard"
            }
