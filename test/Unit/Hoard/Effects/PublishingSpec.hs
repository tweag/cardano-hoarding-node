module Unit.Hoard.Effects.PublishingSpec (spec_Pub) where

import Effectful (runPureEff)
import Test.Hspec (Spec, context, describe, expectationFailure, it, shouldBe)

import Data.Dynamic (fromDynamic)
import Effectful.Writer.Static.Shared (runWriter)
import Hoard.Effects.Publishing (publish, runPubWriter)


data TestEvent = TestEvent Text


spec_Pub :: Spec
spec_Pub = do
    describe "Writer implementation" $ do
        context "no events published" do
            it "doesn't record events" $ do
                let ((), events) = runPureEff . runWriter . runPubWriter $ do
                        pure ()

                length events `shouldBe` 0

        context "events published" do
            it "records events" $ do
                let ((), events) = runPureEff . runWriter . runPubWriter $ do
                        publish $ TestEvent "payload"
                        pure ()

                length events `shouldBe` 1
                case mapMaybe fromDynamic events of
                    [TestEvent payload] ->
                        payload `shouldBe` "payload"
                    xs ->
                        expectationFailure $ "Expected 1 TestEvent event, got: " <> show (length xs)
