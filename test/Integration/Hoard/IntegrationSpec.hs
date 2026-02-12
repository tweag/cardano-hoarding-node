module Integration.Hoard.IntegrationSpec (spec_ViolationsIntegration) where

import Test.Hspec

import Hoard.API (Routes (..))
import Hoard.OrphanDetection.Data (BlockClassification (..))
import Hoard.TestHelpers (client, withEffectStackServer)


spec_ViolationsIntegration :: Spec
spec_ViolationsIntegration = describe "Violations API Integration Tests" $ do
    describe "GET /violations" $ do
        it "returns violations list"
            $ void @IO
            $ withEffectStackServer
            $ \_ runClient -> do
                result <- runClient (client.violations Nothing Nothing Nothing)
                liftIO $ result `shouldSatisfy` isRight

        it "accepts classification query parameter"
            $ void @IO
            $ withEffectStackServer
            $ \_ runClient -> do
                result <- runClient (client.violations (Just Canonical) Nothing Nothing)
                liftIO $ result `shouldSatisfy` isRight

        it "accepts minSlot query parameter"
            $ void @IO
            $ withEffectStackServer
            $ \_ runClient -> do
                result <- runClient (client.violations Nothing (Just 1000) Nothing)
                liftIO $ result `shouldSatisfy` isRight

        it "accepts maxSlot query parameter"
            $ void @IO
            $ withEffectStackServer
            $ \_ runClient -> do
                result <- runClient (client.violations Nothing Nothing (Just 2000))
                liftIO $ result `shouldSatisfy` isRight

        it "accepts all query parameters together"
            $ void @IO
            $ withEffectStackServer
            $ \_ runClient -> do
                result <- runClient (client.violations (Just Canonical) (Just 1000) (Just 2000))
                liftIO $ result `shouldSatisfy` isRight
