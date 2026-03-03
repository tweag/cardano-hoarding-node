module Hoard.Data.Eras
    ( BlockEra (..)
    , blockToEra
    , headerToEra
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (..), Header (..))
import Rel8 (DBEq, DBType, ReadShow (..))

import Hoard.Types.Cardano (CardanoBlock, CardanoHeader)
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


headerToEra :: CardanoHeader -> BlockEra
headerToEra = \case
    HeaderByron _ -> Byron
    HeaderAllegra _ -> Allegra
    HeaderAlonzo _ -> Alonzo
    HeaderMary _ -> Mary
    HeaderShelley _ -> Shelley
    HeaderBabbage _ -> Babbage
    HeaderConway _ -> Conway
    HeaderDijkstra _ -> Dijkstra


data BlockEra
    = Byron
    | Allegra
    | Alonzo
    | Mary
    | Shelley
    | Babbage
    | Conway
    | Dijkstra
    deriving (Bounded, Enum, Eq, Generic, Read, Show)
    deriving (FromJSON, ToJSON) via (JsonReadShow BlockEra)
    deriving (DBEq, DBType) via (ReadShow BlockEra)
