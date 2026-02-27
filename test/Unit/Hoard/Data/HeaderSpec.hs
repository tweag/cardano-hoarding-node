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
import Ouroboros.Consensus.Cardano.Block qualified as O

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
    catMaybes
        $ universe <&> \x -> case x of
            Byron -> Nothing
            Allegra -> Just (x, mkHeader O.HeaderAllegra examplesAllegra)
            Alonzo -> Just (x, mkHeader O.HeaderAlonzo examplesAlonzo)
            Mary -> Just (x, mkHeader O.HeaderMary examplesMary)
            Shelley -> Just (x, mkHeader O.HeaderShelley examplesShelley)
            Babbage -> Just (x, mkHeader O.HeaderBabbage examplesBabbage)
            Conway -> Just (x, mkHeader O.HeaderConway examplesConway)
            Dijkstra -> Just (x, mkHeader O.HeaderDijkstra examplesDijkstra)
  where
    mkHeader mk examples = mk $ snd $ List.head $ examples.exampleHeader
