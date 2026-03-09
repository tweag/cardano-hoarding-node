module Unit.Hoard.Data.BlockSpec (spec_Block) where

import Test.Hspec

import Hoard.Data.Block (decodeCardanoBlock, encodeCardanoBlock)
import Unit.Hoard.Data.Util (blocks)


spec_Block :: Spec
spec_Block =
    describe "CardanoBlock serialising roundtrip"
        $ forM_ blocks
        $ \(blockEra, block) ->
            it ("can decode encoded " <> show blockEra <> " block")
                $ decodeCardanoBlock blockEra (encodeCardanoBlock block) `shouldBe` Right block
