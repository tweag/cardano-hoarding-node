module Unit.Hoard.Data.Util (blocks) where

import Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))
import Test.Consensus.Shelley.Examples
    ( examplesAllegra
    , examplesAlonzo
    , examplesBabbage
    , examplesConway
    , examplesDijkstra
    , examplesMary
    , examplesShelley
    )
import Test.Util.Serialisation.Examples (Examples (..))

import Data.List qualified as List
import Test.Consensus.Byron.Examples qualified as Byron

import Hoard.Data.Eras (BlockEra (..))
import Hoard.Types.Cardano (CardanoBlock)


blocks :: [(BlockEra, CardanoBlock)]
blocks =
    universe <&> \x -> case x of
        Byron -> (x, mkBlock BlockByron Byron.examples)
        Allegra -> (x, mkBlock BlockAllegra examplesAllegra)
        Alonzo -> (x, mkBlock BlockAlonzo examplesAlonzo)
        Mary -> (x, mkBlock BlockMary examplesMary)
        Shelley -> (x, mkBlock BlockShelley examplesShelley)
        Babbage -> (x, mkBlock BlockBabbage examplesBabbage)
        Conway -> (x, mkBlock BlockConway examplesConway)
        Dijkstra -> (x, mkBlock BlockDijkstra examplesDijkstra)
  where
    mkBlock mk examples = mk $ snd $ List.head $ examples.exampleBlock
