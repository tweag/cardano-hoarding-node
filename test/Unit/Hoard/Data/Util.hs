module Unit.Hoard.Data.Util (blocks, headers) where

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
import Ouroboros.Consensus.Byron.Ledger.Block qualified as Byron
import Ouroboros.Consensus.Byron.Ledger.Serialisation qualified as Byron
import Ouroboros.Consensus.Cardano.Block qualified as O
import Test.Consensus.Byron.Examples qualified as Byron

import Hoard.Data.Eras (BlockEra (..))
import Hoard.Types.Cardano (CardanoBlock, CardanoHeader)


blocks :: [(BlockEra, CardanoBlock)]
blocks =
    universe <&> \x -> case x of
        Byron -> (x, mkBlock O.BlockByron Byron.examples)
        Allegra -> (x, mkBlock O.BlockAllegra examplesAllegra)
        Alonzo -> (x, mkBlock O.BlockAlonzo examplesAlonzo)
        Mary -> (x, mkBlock O.BlockMary examplesMary)
        Shelley -> (x, mkBlock O.BlockShelley examplesShelley)
        Babbage -> (x, mkBlock O.BlockBabbage examplesBabbage)
        Conway -> (x, mkBlock O.BlockConway examplesConway)
        Dijkstra -> (x, mkBlock O.BlockDijkstra examplesDijkstra)
  where
    mkBlock mk examples = mk $ snd $ List.head $ examples.exampleBlock


headers :: [(BlockEra, CardanoHeader)]
headers =
    universe <&> \x -> case x of
        Byron -> (x, O.HeaderByron byronHeader)
        Allegra -> (x, mkHeader O.HeaderAllegra examplesAllegra)
        Alonzo -> (x, mkHeader O.HeaderAlonzo examplesAlonzo)
        Mary -> (x, mkHeader O.HeaderMary examplesMary)
        Shelley -> (x, mkHeader O.HeaderShelley examplesShelley)
        Babbage -> (x, mkHeader O.HeaderBabbage examplesBabbage)
        Conway -> (x, mkHeader O.HeaderConway examplesConway)
        Dijkstra -> (x, mkHeader O.HeaderDijkstra examplesDijkstra)
  where
    mkHeader mk examples = mk $ snd $ List.head $ examples.exampleHeader
    -- When deserializing a byron header, we don't know its corresponding
    -- block's proper size, but the `byronHeaderBlockSizeHint` is used when
    -- comparing headers using their `Eq` instance, so we just set it to
    -- `fakeByronBlockSizeHint` for simplicity's sake. The hint is not used for
    -- any particularly critical logic, and only helps inform the cardano node
    -- on how to best handle the header and block.
    byronHeader =
        (snd $ List.head $ Byron.examples.exampleHeader)
            { Byron.byronHeaderBlockSizeHint = Byron.fakeByronBlockSizeHint
            }
