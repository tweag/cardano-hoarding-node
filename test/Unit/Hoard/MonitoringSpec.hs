module Unit.Hoard.MonitoringSpec (spec_Monitoring) where

import Cardano.Api (ChainPoint (..))
import Data.Default (def)
import Data.UUID qualified as UUID
import Effectful (runEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Reader.Static (runReader)
import Effectful.State.Static.Shared (evalState)
import Effectful.Writer.Static.Shared (execWriter)
import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude hiding (evalState, runReader)

import Hoard.Data.ID (ID (..))
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.DBRead (runDBRead)
import Hoard.Effects.Log (Message (..), runLogWriter)
import Hoard.Monitoring qualified as Monitoring
import Hoard.TestHelpers.Database (TestConfig (..), withCleanTestDatabase)
import Hoard.Types.Environment (Severity (..))
import Hoard.Types.HoardState (HoardState (..))


spec_Monitoring :: Spec
spec_Monitoring = withCleanTestDatabase $ do
    describe "listener" do
        it "reports correct number of peers, immutable tip, and block count" $ \config -> do
            logs <-
                runEff
                    . fmap (fmap (\m -> (m.severity, m.text)))
                    . execWriter @[Message]
                    . runLogWriter
                    . runErrorNoCallStack @Text
                    . runReader config.pools
                    . runDBRead
                    . evalState
                        ( def
                            { connectedPeers = fromList $ mkPeerIDs 3
                            , immutableTip = ChainPointAtGenesis
                            }
                        )
                    $ Monitoring.listener Monitoring.Poll
            logs `shouldBe` [(INFO, "Currently connected to 3 peers | Immutable tip slot: genesis | Blocks in DB: 0")]


mkPeerIDs :: Int -> [ID Peer]
mkPeerIDs n =
    [ ID $ UUID.fromWords64 (fromIntegral i) (fromIntegral i + 1)
    | i <- [0 .. (n - 1)]
    ]
