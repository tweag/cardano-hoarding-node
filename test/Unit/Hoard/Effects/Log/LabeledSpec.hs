module Unit.Hoard.Effects.Log.LabeledSpec (spec_LabeledBomber) where

import Effectful (runPureEff)
import Effectful.Writer.Static.Shared (execWriter)
import Test.Hspec (Spec, describe, it, shouldBe)

import Hoard.Effects.Log qualified as Log
import Hoard.Effects.Log.Labeled (LabeledLogger (..))
import Hoard.Effects.Log.Labeled qualified as Labeled


spec_LabeledBomber :: Spec
spec_LabeledBomber = do
    it "prepends a label to the logged message" do
        let (msgs :: [Text]) = runLog do
                Log.debug "foo"
                let logger = Labeled.labeled "foo: "
                logger.debug "bar"

        msgs `shouldBe` ["foo", "foo: bar"]

    describe "appendLabel" do
        it "appends a label to the current label" do
            let (msgs :: [Text]) = runLog do
                    let logger = Labeled.labeled "foo: "
                    logger.debug "bar"
                    let subLogger = logger.appendLabel "quack: "
                    subLogger.debug "qux"

            msgs `shouldBe` ["foo: bar", "foo: quack: qux"]
  where
    runLog = fmap Log.text . runPureEff . execWriter @[Log.Message] . Log.runLogWriter
