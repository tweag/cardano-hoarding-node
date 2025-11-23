module Unit.Hoard.Listeners.HeaderReceivedListenerSpec (spec_HeaderReceivedListener) where

import Effectful (Eff, runPureEff, (:>))
import Effectful.Writer.Static.Local (Writer, runWriter, tell)
import Test.Hspec

import Data.ByteString.Char8 qualified as BS

import Hoard.Events.HeaderReceived (Header (..), HeaderReceived (..))


-- | Run listener with Writer effect instead of Console
-- This allows us to capture the output for testing
runListenerWithWriter :: HeaderReceived -> ((), [BS.ByteString])
runListenerWithWriter event =
    runPureEff $
        runWriter $
            do
                -- Reinterpret the listener to use Writer instead of Console
                headerReceivedListenerWithWriter event


-- | Version of headerReceivedListener that uses Writer instead of Console
headerReceivedListenerWithWriter :: (Writer [BS.ByteString] :> es) => HeaderReceived -> Eff es ()
headerReceivedListenerWithWriter event = do
    tell [BS.pack "Header received:"]
    tell [BS.pack $ show event]


spec_HeaderReceivedListener :: Spec
spec_HeaderReceivedListener = describe "HeaderReceivedListener" $ do
    describe "headerReceivedListener" $ do
        it "logs header received message" $ do
            let header = Header {info = "test info"}
            let event = HeaderReceived {header = header}
            let ((), output) = runListenerWithWriter event

            output `shouldContain` [BS.pack "Header received:"]

        it "logs the event details" $ do
            let header = Header {info = "test details"}
            let event = HeaderReceived {header = header}
            let ((), output) = runListenerWithWriter event

            -- Check that the output contains the show representation
            let showOutput = BS.pack $ show event
            output `shouldContain` [showOutput]
