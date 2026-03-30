module Integration.Hoard.API.BlocksSpec (spec_BlocksIntegration) where

import Test.Hspec

import Hoard.API (Routes (..))
import Hoard.TestHelpers (withServer)
import Hoard.TestHelpers.Database (withCleanTestDatabase)

import Hoard.API.Blocks qualified as Blocks
import Hoard.Data.BlockTag qualified as BlockTag


spec_BlocksIntegration :: Spec
spec_BlocksIntegration =
    describe "Blocks API Integration Tests" do
        withCleanTestDatabase do
            describe "GET /blocks" do
                describe "with no filtering" do
                    it "returns blocks" $ \pools -> withServer pools \client -> do
                        _ <- client.blocks.getBlocks Nothing Nothing []
                        pass
                    it "accepts minSlot query parameter" \pools -> withServer pools \client -> do
                        _ <- client.blocks.getBlocks (Just 1000) Nothing []
                        pass
                    it "accepts maxSlot query parameter" \pools -> withServer pools \client -> do
                        _ <- client.blocks.getBlocks Nothing (Just 2000) []
                        pass
                    it "accepts both slot parameters" \pools -> withServer pools \client -> do
                        _ <- client.blocks.getBlocks (Just 1000) (Just 2000) []
                        pass
                    it "accepts block tag list" \pools -> withServer pools \client -> do
                        _ <- client.blocks.getBlocks Nothing Nothing [BlockTag.SlotDispute]
                        pass
            describe "GET /blocks/disputes" do
                it "returns violations list" $ \config -> withServer config $ \client -> do
                    _ <- client.blocks.getSlotDisputes Nothing Nothing
                    pass

                it "accepts minSlot query parameter" $ \config -> withServer config $ \client -> do
                    _ <- Blocks.getSlotDisputes client.blocks (Just 1000) Nothing
                    pass

                it "accepts maxSlot query parameter" $ \config -> withServer config $ \client -> do
                    _ <- Blocks.getSlotDisputes client.blocks Nothing (Just 2000)
                    pass

                it "accepts both slot parameters" $ \config -> withServer config $ \client -> do
                    _ <- Blocks.getSlotDisputes client.blocks (Just 1000) (Just 2000)
                    pass
