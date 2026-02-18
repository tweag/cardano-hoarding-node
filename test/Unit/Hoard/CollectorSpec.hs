module Unit.Hoard.CollectorSpec (spec_Collector) where

import Data.Time (UTCTime (..))
import Effectful (runPureEff)
import Effectful.State.Static.Shared (evalState)
import Effectful.Writer.Static.Shared (runWriter)
import Ouroboros.Consensus.Block (GetHeader (getHeader))
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (BlockShelley))
import Ouroboros.Network.Block (Tip (TipGenesis))
import Relude.Unsafe (head, read)
import Test.Consensus.Shelley.Examples (examplesShelley)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Util.Serialisation.Examples (Examples (..))
import Prelude hiding (evalState, execState)

import Data.UUID qualified as UUID

import Hoard.Collector (filterHeaderReceived)
import Hoard.Data.Block (Block (..))
import Hoard.Data.BlockHash (blockHashFromHeader)
import Hoard.Data.ID (ID (..))
import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.Data.PoolID (PoolID (..))
import Hoard.Effects.BlockRepo (runBlockRepoState)
import Hoard.Effects.Log (runLogNoOp)
import Hoard.Effects.Monitoring.Tracing (runTracingNoOp)
import Hoard.Effects.Publishing (runPubWriter)
import Hoard.Effects.Verifier (runAllValidVerifier)
import Hoard.Events.ChainSync (HeaderReceived (..))
import Hoard.Types.Cardano (CardanoBlock, CardanoHeader)

import Hoard.Events.BlockFetch qualified as BlockFetch


testBlock :: CardanoBlock
testBlock = BlockShelley $ snd $ Relude.Unsafe.head examplesShelley.exampleBlock


testHeader :: CardanoHeader
testHeader = getHeader testBlock


epoch :: UTCTime
epoch = UTCTime (read "1970-01-01") 0


headerReceived :: HeaderReceived
headerReceived =
    HeaderReceived
        { peer = testPeer
        , header = testHeader
        , timestamp = epoch
        , tip = TipGenesis
        }


dbBlock :: Block
dbBlock =
    Block
        { hash = blockHashFromHeader testHeader
        , slotNumber = 1
        , poolId = PoolID "1"
        , blockData = testBlock
        , validationStatus = ""
        , validationReason = ""
        , firstSeen = epoch
        , classification = Nothing
        , classifiedAt = Nothing
        }


testPeer :: Peer
testPeer =
    Peer
        { id = ID $ UUID.fromWords64 1 2
        , address = PeerAddress (read "192.168.0.1") 3001
        , firstDiscovered = epoch
        , lastSeen = epoch
        , lastConnected = Nothing
        , lastFailureTime = Nothing
        , discoveredVia = "testing"
        }


spec_Collector :: Spec
spec_Collector = do
    describe "filterHeaderReceived" do
        it "should not issue request for existing block" do
            let reqs = runEff [dbBlock]
            reqs `shouldBe` []

        it "should issue request for missing block" do
            let reqs = runEff []
            reqs
                `shouldBe` [ BlockFetch.Request
                                { peer = testPeer
                                , timestamp = epoch
                                , header = testHeader
                                }
                           ]
  where
    runEff :: [Block] -> [BlockFetch.Request]
    runEff db =
        let ((), events) =
                runPureEff
                    . runLogNoOp
                    . runTracingNoOp
                    . runWriter
                    . runPubWriter @BlockFetch.Request
                    . evalState db
                    . runBlockRepoState
                    . runAllValidVerifier
                    $ filterHeaderReceived testPeer.id headerReceived
        in  events
