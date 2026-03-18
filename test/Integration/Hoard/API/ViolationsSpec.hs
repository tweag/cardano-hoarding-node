module Integration.Hoard.API.ViolationsSpec (spec_ViolationsIntegration) where

import Test.Hspec

import Hoard.API (Routes (..))
import Hoard.TestHelpers (withServer)
import Hoard.TestHelpers.Database (withCleanTestDatabase)


spec_ViolationsIntegration :: Spec
spec_ViolationsIntegration =
    describe "Violations API Integration Tests" do
        withCleanTestDatabase do
            describe "GET /violations" do
                it "returns violations list" $ \config -> withServer config $ \client -> do
                    _ <- client.violations Nothing Nothing
                    pass

                it "accepts minSlot query parameter" $ \config -> withServer config $ \client -> do
                    _ <- client.violations (Just 1000) Nothing
                    pass

                it "accepts maxSlot query parameter" $ \config -> withServer config $ \client -> do
                    _ <- client.violations Nothing (Just 2000)
                    pass

                it "accepts both slot parameters" $ \config -> withServer config $ \client -> do
                    _ <- client.violations (Just 1000) (Just 2000)
                    pass
