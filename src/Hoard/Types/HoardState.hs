module Hoard.Types.HoardState (HoardState (..)) where

import Cardano.Api (ChainPoint (ChainPointAtGenesis))
import Data.Default (Default (..))


newtype HoardState = HoardState
    { immutableTip :: ChainPoint
    }
    deriving (Eq, Show)


instance Default HoardState where
    def =
        HoardState
            { immutableTip = ChainPointAtGenesis
            }
