module Unit.Hoard.Data.HeaderSpec (spec_Header) where

import Test.Consensus.Shelley.Examples
    ( examplesAllegra
    , examplesAlonzo
    , examplesBabbage
    , examplesConway
    , examplesDijkstra
    , examplesMary
    , examplesShelley
    )
import Test.Hspec
import Test.Util.Serialisation.Examples (Examples (..))

import Data.List qualified as List
import Ouroboros.Consensus.Byron.Ledger.Block qualified as Byron
import Ouroboros.Consensus.Byron.Ledger.Serialisation qualified as Byron
import Ouroboros.Consensus.Cardano.Block qualified as O
import Test.Consensus.Byron.Examples qualified as Byron

import Hoard.Data.Eras (BlockEra (..))
import Hoard.Data.Header (decodeCardanoHeader, encodeCardanoHeader)
import Hoard.Types.Cardano (CardanoHeader)


spec_Header :: Spec
spec_Header =
    describe "CardanoHeader serialising roundtrip" $ do
        forM_ blocks $ \(blockEra, block) ->
            it ("can decode encoded " <> show blockEra <> " block") $ do
                decodeCardanoHeader blockEra (encodeCardanoHeader block) `shouldBe` Right block


blocks :: [(BlockEra, CardanoHeader)]
blocks =
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
