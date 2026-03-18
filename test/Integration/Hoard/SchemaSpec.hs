module Integration.Hoard.SchemaSpec (spec_Schema) where

import Data.Time (UTCTime (..))
import Effectful (runEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Reader.Static (runReader)
import Test.Hspec

import Rel8 qualified

import Atelier.Effects.Clock (runClockConst)
import Atelier.Effects.Monitoring.Metrics (runMetricsNoOp)
import Atelier.Effects.Monitoring.Tracing (runTracingNoOp)
import Hoard.Effects.DB (runDBRead, runQuery)
import Hoard.TestHelpers.Database (withCleanTestDatabase)

import Hoard.DB.Schemas.BlockTags qualified as BlockTagsSchema
import Hoard.DB.Schemas.Blocks qualified as BlocksSchema
import Hoard.DB.Schemas.Headers qualified as HeaderReceiptsSchema
import Hoard.DB.Schemas.Headers qualified as HeadersSchema
import Hoard.DB.Schemas.HoardState qualified as HoadStateSchema
import Hoard.DB.Schemas.Peers qualified as PeersSchema
import Hoard.DB.Schemas.SelectedPeers qualified as SelectedPeersSchema


spec_Schema :: Spec
spec_Schema = withCleanTestDatabase $ do
    let testTime = UTCTime (toEnum 0) 0

    describe "Schema" $ do
        it "is correctly mapped" $ \pools -> do
            weakTestSchema pools testTime PeersSchema.schema
            weakTestSchema pools testTime HeaderReceiptsSchema.schema
            weakTestSchema pools testTime HeadersSchema.schema
            weakTestSchema pools testTime BlocksSchema.schema
            weakTestSchema pools testTime HoadStateSchema.schema
            weakTestSchema pools testTime BlockTagsSchema.schema
            weakTestSchema pools testTime SelectedPeersSchema.schema
  where
    -- Helper function to test that a schema is correctly mapped
    -- Similar to weakTestSchema - verifies schema can be queried without errors
    weakTestSchema pools testTime schema = do
        let query = Rel8.run $ Rel8.select $ Rel8.each schema
        result <-
            runEff
                . runErrorNoCallStack @Text
                . runReader pools
                . runMetricsNoOp
                . runTracingNoOp
                . runClockConst testTime
                . runDBRead
                $ runQuery "weak-schema-test" query

        case result of
            Right _ -> pure ()
            Left err -> expectationFailure $ "Schema validation failed: " <> show err
