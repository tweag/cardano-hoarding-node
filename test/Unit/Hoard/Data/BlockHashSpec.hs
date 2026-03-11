module Unit.Hoard.Data.BlockHashSpec (spec_BlockHash) where

import Ouroboros.Consensus.Block (getHeader)
import Test.Hspec

import Hoard.Data.BlockHash (mkBlockHash)
import Unit.Hoard.Data.Util (blocks)


spec_BlockHash :: Spec
spec_BlockHash = describe "mkBlockHash" do
    forM_ blocks \(era, block) ->
        it ("should return the same hash for a block and its header for " <> show era)
            $ mkBlockHash block `shouldBe` mkBlockHash (getHeader block)
