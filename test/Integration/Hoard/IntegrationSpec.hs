module Integration.Hoard.IntegrationSpec (spec_HeaderIntegration) where

import Data.Either (isRight)
import Hoard.API (Routes (..))
import Hoard.Events.HeaderReceived (HeaderReceived (..))
import Hoard.TestHelpers
import Hoard.Types.Header (Header (..))
import Test.Hspec

spec_HeaderIntegration :: Spec
spec_HeaderIntegration = describe "Header Integration Tests" $ do
  describe "POST /header" $ do
    it "receives header data and emits HeaderReceived event" $ do
      -- Setup: Create test header
      let testHeader = Header {info = "test header info"}

      -- Setup: Create event capture
      headerReceivedEvents <- observer @HeaderReceived

      -- Setup: Start test app with custom listener
      withTestApp (testConfig {captures = [capture headerReceivedEvents]}) $ \_ runClient -> do
        -- Send request via servant client
        result <- runClient (client.receiveHeader testHeader)

        -- Assert request succeeded
        result `shouldSatisfy` isRight

        -- Wait for at least 1 event (with 100ms timeout)
        [HeaderReceived receivedHeader] <- waitForEvents headerReceivedEvents 1 0.1

        -- Assert event contains the test data
        receivedHeader `shouldBe` testHeader

    it "handles multiple sequential requests" $ do
      -- Setup: Create multiple test headers
      let testHeader1 = Header {info = "first header"}
      let testHeader2 = Header {info = "second header"}

      -- Setup: Create event capture
      headerReceivedEvents <- observer @HeaderReceived

      -- Setup: Start test app
      withTestApp (testConfig {captures = [capture headerReceivedEvents]}) $ \_ runClient -> do
        -- Send first request
        result1 <- runClient (client.receiveHeader testHeader1)
        result1 `shouldSatisfy` isRight

        -- Send second request
        result2 <- runClient (client.receiveHeader testHeader2)
        result2 `shouldSatisfy` isRight

        -- Wait for at least 2 events (with 200ms timeout)
        events <- waitForEvents headerReceivedEvents 2 0.2

        -- Assert we got two events
        length events `shouldBe` 2

        -- Assert events contain correct data (in order)
        case events of
          [HeaderReceived h1, HeaderReceived h2] -> do
            h1 `shouldBe` testHeader1
            h2 `shouldBe` testHeader2
          _ ->
            expectationFailure $ "Expected 2 HeaderReceived events, got: " <> show (length events)
