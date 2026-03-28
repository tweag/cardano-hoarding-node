module Integration.Hoard.DB.HoardStatePersistenceSpec (spec_HoardStatePersistence) where

import Effectful (runEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Reader.Static (runReader)
import Test.Hspec

import Cardano.Api qualified as C

import Atelier.Effects.Clock (runClock)
import Atelier.Effects.Monitoring.Metrics (runMetricsNoOp)
import Atelier.Effects.Monitoring.Tracing (runTracingNoOp)
import Hoard.Effects.DB (runDBRead, runDBWrite, runRel8Read, runRel8Write)
import Hoard.Effects.HoardStateRepo (getImmutableTip, persistImmutableTip, runHoardStateRepo)
import Hoard.TestHelpers.Database (withCleanTestDatabase)
import Hoard.Types.Cardano (ChainPoint (..))

import Atelier.Effects.Log qualified as Log


spec_HoardStatePersistence :: Spec
spec_HoardStatePersistence = do
    let run pools action =
            runEff
                . Log.runLogNoOp
                . runErrorNoCallStack @Text
                . runReader pools
                . runMetricsNoOp
                . runTracingNoOp
                . runClock
                . runDBRead
                . runClock
                . runDBWrite
                . runRel8Read
                . runRel8Write
                . runHoardStateRepo
                $ action

    withCleanTestDatabase $ describe "HoardState persistence (database)" $ do
        it "returns genesis chain point when no state exists" $ \pools -> do
            result <- run pools getImmutableTip
            case result of
                Right tip -> tip `shouldBe` ChainPoint C.ChainPointAtGenesis
                Left err -> expectationFailure $ show err

        it "persists and retrieves immutable tip" $ \pools -> do
            let tip = ChainPoint C.ChainPointAtGenesis
            Right () <- run pools $ persistImmutableTip tip
            result <- run pools getImmutableTip
            case result of
                Right retrieved -> retrieved `shouldBe` tip
                Left err -> expectationFailure $ show err

        it "overwrites previous tip on subsequent persist" $ \pools -> do
            Right () <- run pools $ persistImmutableTip (ChainPoint C.ChainPointAtGenesis)
            Right () <- run pools $ persistImmutableTip (ChainPoint C.ChainPointAtGenesis)
            result <- run pools getImmutableTip
            result `shouldSatisfy` isRight
