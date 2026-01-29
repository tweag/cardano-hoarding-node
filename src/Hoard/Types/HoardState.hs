module Hoard.Types.HoardState (HoardState (..)) where

import Cardano.Api qualified as C
import Data.Default (Default (..))
import Data.Set qualified as S

import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer)
import Hoard.Types.Cardano (ChainPoint (ChainPoint))


-- | Application state
data HoardState = HoardState
    { connectedPeers :: Set (ID Peer)
    , immutableTip :: ChainPoint
    }
    deriving (Eq, Show)


instance Default HoardState where
    def =
        HoardState
            { connectedPeers = S.empty
            , immutableTip = ChainPoint C.ChainPointAtGenesis
            }
