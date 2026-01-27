module Hoard.DB.Schemas.HoardState
    ( Row (..)
    , schema
    ) where

import Rel8
    ( Column
    , Name
    , Rel8able
    , Result
    , TableSchema
    )

import Hoard.DB.Schema (mkSchema)
import Hoard.Types.HoardState (ChainPoint)


data Row f = Row
    { immutableTip :: Column f ChainPoint
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)


deriving instance Eq (Row Result)
deriving instance Show (Row Result)


schema :: TableSchema (Row Name)
schema = mkSchema "hoard_state"
