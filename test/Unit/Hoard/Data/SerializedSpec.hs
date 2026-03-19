module Unit.Hoard.Data.SerializedSpec (spec_Serialized) where

import Test.Hspec

import Hoard.Data.Serialized
    ( decodeCardanoBlock
    , decodeCardanoHeader
    , encodeCardanoBlock
    , encodeCardanoHeader
    )
import Unit.Hoard.Data.Util (blocks, headers)


spec_Serialized :: Spec
spec_Serialized = do
    describe "Header" testHeader
    describe "Block" testBlock


testHeader :: Spec
testHeader = do
    describe "CardanoHeader serialising roundtrip" $ do
        forM_ headers $ \(blockEra, header) ->
            it ("can decode encoded " <> show blockEra <> " header") $ do
                decodeCardanoHeader blockEra (encodeCardanoHeader header) `shouldBe` Right header


testBlock :: Spec
testBlock = do
    describe "CardanoBlock serialising roundtrip"
        $ forM_ blocks
        $ \(blockEra, block) ->
            it ("can decode encoded " <> show blockEra <> " block")
                $ decodeCardanoBlock blockEra (encodeCardanoBlock block) `shouldBe` Right block
