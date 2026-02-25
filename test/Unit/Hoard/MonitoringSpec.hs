module Unit.Hoard.MonitoringSpec (spec_Monitoring) where

import Data.Time (UTCTime (..))
import Effectful (Eff, runEff, (:>))
import Effectful.Concurrent.MVar (Concurrent, newEmptyMVar, runConcurrent)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Reader.Static (runReader)
import Effectful.State.Static.Shared (evalState)
import Effectful.Writer.Static.Shared (execWriter)
import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude hiding (evalState, newEmptyMVar, runReader)

import Cardano.Api qualified as C
import Data.UUID qualified as UUID

import Hoard.Data.ID (ID (..))
import Hoard.Effects.Clock (runClock)
import Hoard.Effects.DBRead (runDBRead)
import Hoard.Effects.Log (Message (..), Severity (..), runLogWriter)
import Hoard.Effects.Monitoring.Metrics (runMetricsNoOp)
import Hoard.Effects.Monitoring.Tracing (runTracingNoOp)
import Hoard.PeerManager.Peers (Connection (..), ConnectionState (..), Peers (..))
import Hoard.TestHelpers.Database (TestConfig (..), withCleanTestDatabase)
import Hoard.Types.Cardano (ChainPoint (ChainPoint))
import Hoard.Types.HoardState (HoardState (..))

import Hoard.Monitoring qualified as Monitoring


spec_Monitoring :: Spec
spec_Monitoring = withCleanTestDatabase $ do
    describe "listener" do
        it "reports correct number of peers, immutable tip, and block count" $ \config -> do
            let testTime = UTCTime (toEnum 0) 0
            peers <- runEff . runConcurrent $ mkPeers testTime 3
            logs <-
                runEff
                    . fmap (fmap (\m -> (m.severity, m.text)))
                    . execWriter @[Message]
                    . runLogWriter
                    . runErrorNoCallStack @Text
                    . runReader config.pools
                    . runMetricsNoOp
                    . runTracingNoOp
                    . runClock
                    . runDBRead
                    . evalState
                        HoardState
                            { immutableTip = ChainPoint C.ChainPointAtGenesis
                            , blocksBeingClassified = mempty
                            }
                    . evalState peers
                    $ Monitoring.listener Monitoring.Poll
            logs `shouldBe` [(INFO, "Current peer connections: 1 | Pending peer connections: 2 | Immutable tip slot: genesis | Blocks in DB: 0 | Unclassified blocks: 0 | Blocks being classified: 0")]


mkPeers :: (Concurrent :> es) => UTCTime -> Int -> Eff es Peers
mkPeers connectedAt n = do
    terminator <- newEmptyMVar
    pure
        $ Peers
        $ fromList
            [ ( ID $ UUID.fromWords64 (fromIntegral i) (fromIntegral i + 1)
              , Connection
                    { connectedAt
                    , terminator
                    , state = if i < (n `div` 2) then Connected else Connecting
                    }
              )
            | i <- [0 .. (n - 1)]
            ]
