module Hoard.Types.HoardState (HoardState (..), ChainPoint (..)) where

import Cardano.Api qualified as C
import Data.Default (Default (..))
import Data.Set qualified as S

import Data.Aeson (FromJSON, ToJSON)
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer)
import Rel8 (DBType, JSONBEncoded (JSONBEncoded))


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


newtype ChainPoint = ChainPoint C.ChainPoint
    deriving (Show)
    deriving (FromJSON, ToJSON, Eq, Ord) via ChainPoint
    deriving (DBType) via JSONBEncoded ChainPoint
