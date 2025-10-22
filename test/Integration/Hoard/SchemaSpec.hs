module Integration.Hoard.SchemaSpec (spec_Schema) where

import Data.Text (Text)
import Effectful (runEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Hoard.DB.Schemas.Peers qualified as PeersSchema
import Hoard.Effects.DBRead (runDBRead, runQuery)
import Hoard.TestHelpers.Database (TestConfig (..), withCleanTestDatabase)
import Hoard.Types.DBConfig (DBPools (..))
import Rel8 qualified
import Test.Hspec

spec_Schema :: Spec
spec_Schema = withCleanTestDatabase $ do
  describe "Schema" $ do
    it "is correctly mapped" $ \config -> do
      weakTestSchema config PeersSchema.schema
  where
    -- Helper function to test that a schema is correctly mapped
    -- Similar to weakTestSchema - verifies schema can be queried without errors
    weakTestSchema config schema = do
      let query = Rel8.run $ Rel8.select $ Rel8.each schema
      result <-
        runEff
          . runErrorNoCallStack @Text
          . runDBRead config.pools.readerPool
          $ runQuery "weak-schema-test" query

      case result of
        Right _ -> pure ()
        Left err -> expectationFailure $ "Schema validation failed: " <> show err
