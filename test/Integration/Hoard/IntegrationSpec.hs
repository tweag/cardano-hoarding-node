module Integration.Hoard.IntegrationSpec (spec_ViolationsIntegration) where

import Test.Hspec

import Hoard.API (Routes (..))
import Hoard.TestHelpers (client, withEffectStackServer)


spec_ViolationsIntegration :: Spec
spec_ViolationsIntegration = describe "Violations API Integration Tests" $ do
    describe "GET /violations" $ do
        it "returns violations list"
            $ void @IO
            $ withEffectStackServer
            $ \_ runClient -> do
                result <- runClient (client.violations Nothing Nothing)
                liftIO $ result `shouldSatisfy` isRight

        it "accepts minSlot query parameter"
            $ void @IO
            $ withEffectStackServer
            $ \_ runClient -> do
                result <- runClient (client.violations (Just 1000) Nothing)
                liftIO $ result `shouldSatisfy` isRight

        it "accepts maxSlot query parameter"
            $ void @IO
            $ withEffectStackServer
            $ \_ runClient -> do
                result <- runClient (client.violations Nothing (Just 2000))
                liftIO $ result `shouldSatisfy` isRight

        it "accepts both slot parameters"
            $ void @IO
            $ withEffectStackServer
            $ \_ runClient -> do
                result <- runClient (client.violations (Just 1000) (Just 2000))
                liftIO $ result `shouldSatisfy` isRight
