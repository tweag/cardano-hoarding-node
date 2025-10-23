module Hoard.Types.HoardState (HoardState (..)) where

import Data.Default (Default (..))
import Data.Map.Strict (Map)

import Data.Map.Strict qualified as M

import Hoard.Types.Collector (CollectorHandle, CollectorId)


-- | Application state
data HoardState = HoardState
    { collectors :: Map CollectorId CollectorHandle
    }
    deriving (Eq, Show)


instance Default HoardState where
    def =
        HoardState
            { collectors = M.empty
            }
