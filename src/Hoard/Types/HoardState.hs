module Hoard.Types.HoardState (HoardState (..)) where

import Data.Default (Default (..))
import Data.Set (Set)

import Data.Set qualified as S

import Hoard.Types.Collector (Peer)


-- | Application state
data HoardState = HoardState
    { peers :: Set Peer
    }
    deriving (Eq, Show)


instance Default HoardState where
    def =
        HoardState
            { peers = S.empty
            }
