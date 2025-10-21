module Integration.Hoard.IntegrationSpec (spec_HeaderIntegration) where

import Data.Either (isRight)
import Effectful (liftIO)
import Effectful.Concurrent (threadDelay)
import Hoard.API (Routes (..))
import Hoard.Events.HeaderReceived (HeaderReceived (..))
import Hoard.TestHelpers (client, filterEvents, withEffectStackServer)
import Hoard.Types.Header (Header (..))
import Test.Hspec

spec_HeaderIntegration :: Spec
spec_HeaderIntegration = describe "Header Integration Tests" $ do
  describe "POST /header" $ do
    it "receives header data and emits HeaderReceived event" $ do
      -- Setup: Create test header
      let testHeader = Header {info = "test header info"}

      (_, _, events) <- withEffectStackServer $ \_ runClient -> do
        result <- runClient (client.receiveHeader testHeader)
        liftIO $ result `shouldSatisfy` isRight
        threadDelay 2_000_000

      case filterEvents events of
        [HeaderReceived header] ->
          header `shouldBe` testHeader
        xs ->
          expectationFailure $ "Expected 1 HeaderReceived event, got: " <> show (length xs)

    it "handles multiple sequential requests" $ do
      -- Setup: Create multiple test headers
      let testHeader1 = Header {info = "first header"}
      let testHeader2 = Header {info = "second header"}

      -- Setup: Start test app
      (_, _, events) <- withEffectStackServer $ \_ runClient -> do
        -- Send first request
        result1 <- runClient (client.receiveHeader testHeader1)
        liftIO $ result1 `shouldSatisfy` isRight

        -- Send second request
        result2 <- runClient (client.receiveHeader testHeader2)
        liftIO $ result2 `shouldSatisfy` isRight

      -- Assert events contain correct data (in order)
      case filterEvents events of
        [HeaderReceived h1, HeaderReceived h2] -> do
          h1 `shouldBe` testHeader1
          h2 `shouldBe` testHeader2
        _ ->
          expectationFailure $ "Expected 2 HeaderReceived events, got: " <> show (length events)
