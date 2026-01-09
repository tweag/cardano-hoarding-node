module Unit.Hoard.MonitoringSpec (spec_Monitoring) where

import Data.Default (def)
import Data.UUID qualified as UUID
import Effectful (runPureEff)
import Effectful.State.Static.Shared (evalState)
import Effectful.Writer.Static.Shared (execWriter)
import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude hiding (evalState)

import Hoard.Data.ID (ID (..))
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.Log (Message (..), runLogWriter)
import Hoard.Monitoring qualified as Monitoring
import Hoard.Types.Environment (Severity (..))
import Hoard.Types.HoardState (HoardState (..))


spec_Monitoring :: Spec
spec_Monitoring = do
    describe "listener" do
        it "reports correct number of peers" do
            let logs =
                    fmap (\m -> (m.severity, m.text))
                        . runPureEff
                        . execWriter @[Message]
                        . runLogWriter
                        . evalState
                            ( def
                                { connectedPeers = fromList $ mkPeerIDs 3
                                }
                            )
                        $ Monitoring.listener Monitoring.Poll
            logs `shouldBe` [(INFO, "Currently connected to 3 peers")]


mkPeerIDs :: Int -> [ID Peer]
mkPeerIDs n =
    [ ID $ UUID.fromWords64 (fromIntegral i) (fromIntegral i + 1)
    | i <- [0 .. (n - 1)]
    ]
