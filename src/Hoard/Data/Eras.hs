module Hoard.Data.Eras
    ( BlockEra (..)
    , blockToEra
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))
import Rel8 (DBEq, DBType, ReadShow (..))

import Hoard.Types.Cardano (CardanoBlock)
import Hoard.Types.JsonReadShow (JsonReadShow (..))


blockToEra :: CardanoBlock -> BlockEra
blockToEra = \case
    BlockByron _ -> Byron
    BlockAllegra _ -> Allegra
    BlockAlonzo _ -> Alonzo
    BlockMary _ -> Mary
    BlockShelley _ -> Shelley
    BlockBabbage _ -> Babbage
    BlockConway _ -> Conway
    BlockDijkstra _ -> Dijkstra


data BlockEra
    = Byron
    | Allegra
    | Alonzo
    | Mary
    | Shelley
    | Babbage
    | Conway
    | Dijkstra
    deriving (Eq, Show, Enum, Bounded, Read, Generic)
    deriving (DBType, DBEq) via (ReadShow BlockEra)
    deriving (FromJSON, ToJSON) via (JsonReadShow BlockEra)
