module Integration.Hoard.SchemaSpec (spec_Schema) where

import Effectful (runEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Reader.Static (runReader)
import Rel8 qualified
import Test.Hspec
import Prelude hiding (runReader)

import Hoard.DB.Schemas.Blocks qualified as BlocksSchema
import Hoard.DB.Schemas.Headers qualified as HeaderReceiptsSchema
import Hoard.DB.Schemas.Headers qualified as HeadersSchema
import Hoard.DB.Schemas.Peers qualified as PeersSchema
import Hoard.Effects.DBRead (runDBRead, runQuery)
import Hoard.TestHelpers.Database (TestConfig (..), withCleanTestDatabase)


spec_Schema :: Spec
spec_Schema = withCleanTestDatabase $ do
    describe "Schema" $ do
        it "is correctly mapped" $ \config -> do
            weakTestSchema config PeersSchema.schema
            weakTestSchema config HeaderReceiptsSchema.schema
            weakTestSchema config HeadersSchema.schema
            weakTestSchema config BlocksSchema.schema
  where
    -- Helper function to test that a schema is correctly mapped
    -- Similar to weakTestSchema - verifies schema can be queried without errors
    weakTestSchema config schema = do
        let query = Rel8.run $ Rel8.select $ Rel8.each schema
        result <-
            runEff
                . runErrorNoCallStack @Text
                . runReader config.pools
                . runDBRead
                $ runQuery "weak-schema-test" query

        case result of
            Right _ -> pure ()
            Left err -> expectationFailure $ "Schema validation failed: " <> show err
