module Unit.Hoard.Collectors.ListenersSpec (spec_Collectors_Listeners) where

import Data.Dynamic (fromDynamic)
import Data.Time (UTCTime (..))
import Data.UUID qualified as UUID
import Effectful (runPureEff)
import Effectful.State.Static.Shared (evalState, execState)
import Effectful.Writer.Static.Shared (runWriter)
import Ouroboros.Consensus.Block (GetHeader (getHeader))
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (BlockShelley))
import Ouroboros.Network.Block (Tip (TipGenesis))
import Relude.Unsafe (head, read)
import Test.Consensus.Shelley.Examples (examplesShelley)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Util.Serialisation.Examples (Examples (..))
import Prelude hiding (evalState, execState)

import Hoard.BlockFetch.Events (BlockFetchRequest (..))
import Hoard.ChainSync.Events (HeaderReceived (..))
import Hoard.Collectors.Listeners (pickBlockFetchRequest)
import Hoard.Collectors.State (BlocksBeingFetched (..))
import Hoard.Data.Block (Block (..))
import Hoard.Data.BlockHash (BlockHash, blockHashFromHeader)
import Hoard.Data.ID (ID (..))
import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.Data.PoolID (PoolID (..))
import Hoard.Effects.BlockRepo (runBlockRepoState)
import Hoard.Effects.Input (runInputConst)
import Hoard.Effects.Publishing (runPubWriter)
import Hoard.Types.Cardano (CardanoBlock, CardanoHeader)


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
        , isCanonical = False
        , firstSeen = epoch
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


spec_Collectors_Listeners :: Spec
spec_Collectors_Listeners = do
    describe "pickBlockFetchRequest" do
        it "should not issue request for existing block" do
            let (blocksBeingFetched, reqs) = runEff mempty [dbBlock]

            reqs `shouldBe` []
            blocksBeingFetched `shouldBe` BlocksBeingFetched mempty

        it "should issue request for missing block" do
            let (blocksBeingFetched, reqs) = runEff mempty []
            reqs
                `shouldBe` [ BlockFetchRequest
                                { peer = testPeer
                                , timestamp = epoch
                                , header = testHeader
                                }
                           ]
            blocksBeingFetched
                `shouldBe` BlocksBeingFetched (one $ blockHashFromHeader testHeader)

        it "should not issue request for block already being fetched" do
            let initialBbf = one $ blockHashFromHeader testHeader
                (blocksBeingFetched, reqs) = runEff initialBbf []
            reqs `shouldBe` []
            blocksBeingFetched `shouldBe` BlocksBeingFetched initialBbf
  where
    runEff :: Set BlockHash -> [Block] -> (BlocksBeingFetched, [BlockFetchRequest])
    runEff bbf db =
        let (blocksBeingFetched, events) =
                runPureEff
                    . runWriter
                    . runPubWriter
                    . runInputConst headerReceived
                    . execState (BlocksBeingFetched bbf)
                    . evalState db
                    . runBlockRepoState
                    $ pickBlockFetchRequest testPeer.id headerReceived
            reqs = mapMaybe fromDynamic events
        in  (blocksBeingFetched, reqs)
