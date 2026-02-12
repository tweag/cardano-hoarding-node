module Unit.Hoard.Data.BlockSpec (spec_Block) where

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
import Test.Hspec
import Test.Util.Serialisation.Examples (Examples (..))

import Relude.Unsafe qualified
import Test.Consensus.Byron.Examples qualified as Byron

import Hoard.Data.Block (decodeCardanoBlock, encodeCardanoBlock)
import Hoard.Data.Eras (BlockEra (..))
import Hoard.Types.Cardano (CardanoBlock)


spec_Block :: Spec
spec_Block =
    describe "CardanoBlock serialising roundtrip" $ do
        forM_ blocks $ \(blockEra, block) ->
            it ("can decode encoded " <> show blockEra <> " block") $ do
                decodeCardanoBlock blockEra (encodeCardanoBlock block) `shouldBe` Right block


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
    mkBlock mk examples = mk $ snd $ Relude.Unsafe.head $ examples.exampleBlock
