module Hoard.DB.Schemas.HoardState
    ( Row (..)
    , Unit (..)
    , schema
    ) where

import Rel8
    ( Column
    , DBType (typeInformation)
    , Name
    , QualifiedName
    , ReadShow (ReadShow)
    , Rel8able
    , Result
    , TableSchema
    , TypeInformation (typeName)
    , TypeName (name)
    )

import Rel8 qualified

import Hoard.DB.Schema (mkSchema)
import Hoard.Types.Cardano (ChainPoint)


data Row f = Row
    { unit :: Column f Unit
    , immutableTip :: Column f ChainPoint
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)


deriving instance Eq (Row Result)
deriving instance Show (Row Result)


data Unit
    = Unit
    deriving (Eq, Read, Show)


instance DBType Unit where
    typeInformation =
        coerce
            (typeInformation @(ReadShow Unit))
                { typeName = "unit" & (overName . overSchema) (const $ Just "hoard")
                }


schema :: TableSchema (Row Name)
schema = mkSchema "hoard_state"


overName :: (QualifiedName -> QualifiedName) -> TypeName -> TypeName
overName f a = a {name = f a.name}


overSchema :: (Maybe String -> Maybe String) -> QualifiedName -> QualifiedName
overSchema f a = a {Rel8.schema = f a.schema}
