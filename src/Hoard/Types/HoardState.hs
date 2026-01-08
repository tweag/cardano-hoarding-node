module Hoard.Types.HoardState (HoardState (..)) where

import Data.Default (Default (..))

import Data.Set qualified as S

import Cardano.Api (ChainPoint (ChainPointAtGenesis))
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer)


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
            , immutableTip = ChainPointAtGenesis
            }
