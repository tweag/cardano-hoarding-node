module Hoard.Types.HoardState (HoardState (..)) where

import Cardano.Api qualified as C
import Data.Default (Default (..))
import Data.Set qualified as S

import Hoard.Data.BlockHash (BlockHash)
import Hoard.Types.Cardano (ChainPoint (ChainPoint))


-- | Application state
data HoardState = HoardState
    { immutableTip :: ChainPoint
    , blocksBeingClassified :: Set BlockHash
    }
    deriving (Eq, Show)


instance Default HoardState where
    def =
        HoardState
            { immutableTip = ChainPoint C.ChainPointAtGenesis
            , blocksBeingClassified = S.empty
            }
