{-# LANGUAGE PatternSynonyms #-}

module Unit.Hoard.SentrySpec (spec_Sentry) where

import Cardano.Ledger.Block (Block (..))
import Data.Default (Default, def)
import Data.UUID (UUID)
import Effectful (runPureEff)
import Effectful.Reader.Static (runReader)
import Effectful.Writer.Static.Shared (execWriter, runWriter)
import Ouroboros.Consensus.Block (SlotNo (..), getHeader, headerPoint)
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))
import Ouroboros.Consensus.Protocol.Praos.Header (Header (..), HeaderBody (..))
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock (..), mkShelleyBlock)
import Ouroboros.Network.Protocol.BlockFetch.Type (ChainRange (..))
import Test.Consensus.Shelley.Examples (examplesBabbage, examplesShelley)
import Test.Hspec
import Test.Util.Serialisation.Examples (Examples (..))
import Text.Read (read)

import Data.List qualified as List
import Data.UUID qualified as UUID

import Atelier.Effects.Monitoring.Tracing (runTracingNoOp)
import Atelier.Effects.Publishing (runPubWriter)
import Atelier.Effects.Quota (runQuotaConst)
import Hoard.Data.ID (ID (..))
import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.Events.BlockFetch (BlockReceived (..))
import Hoard.Sentry
    ( AdversarialBehavior (..)
    , AdversarialSeverity (..)
    , AdversarialThresholds (..)
    , Config (..)
    , ReceivedBlockOutsideRequestedRange (..)
    , ReceivedMismatchingBlock (..)
    , duplicateBlockGuard
    , headerBlockMismatchGuard
    , receivedBlockIsOutsideRequestedRangeGuard
    )
import Hoard.Types.Cardano (CardanoBlock, CardanoHeader, CardanoPoint)


spec_Sentry :: Spec
spec_Sentry = do
    describe "headerBlockMismatchGuard" testHeaderBlockMismatchGuard
    describe "duplicateBlockGuard" testDuplicateBlockGuard
    describe "receivedBlockIsOutsideRequestedRangeGuard" testReceivedBlockIsOutsideRequestedRangeGuard


testDuplicateBlockGuard :: Spec
testDuplicateBlockGuard = do
    describe "with non-duplicate block" do
        it "should not flag the peer" do
            let msgs =
                    runDuplicateBlockGuard
                        def
                            { warningThreshold = 1
                            , criticalThreshold = 5
                            , quota = 0
                            }

            msgs `shouldMatchList` []

    describe "with quota > warningThreshold and quota <= criticalThreshold" do
        it "should flag the peer" do
            let msgs =
                    runDuplicateBlockGuard
                        def
                            { warningThreshold = 1
                            , criticalThreshold = 5
                            , quota = 2
                            }

            length msgs `shouldBe` 1
            let msg = List.head msgs
            msg.severity `shouldBe` Minor

    describe "with quota > criticalThreshold" do
        it "should flag the peer" do
            let msgs =
                    runDuplicateBlockGuard
                        def
                            { warningThreshold = 1
                            , criticalThreshold = 2
                            , quota = 3
                            }

            length msgs `shouldBe` 1
            let msg = List.head msgs
            msg.severity `shouldBe` Critical

    describe "with quota == warningThreshold" do
        it "should not flag the peer" do
            let msgs =
                    runDuplicateBlockGuard
                        def
                            { warningThreshold = 2
                            , criticalThreshold = 5
                            , quota = 2
                            }

            msgs `shouldBe` []

    describe "with quota == criticalThreshold" do
        it "should flag as minor (not critical)" do
            let msgs =
                    runDuplicateBlockGuard
                        def
                            { warningThreshold = 1
                            , criticalThreshold = 2
                            , quota = 2
                            }

            length msgs `shouldBe` 1
            let msg = List.head msgs
            msg.severity `shouldBe` Minor

    describe "with duplicate block" do
        it "should include the correct peer in the published warning message" do
            let msgs =
                    runDuplicateBlockGuard
                        def
                            { warningThreshold = 1
                            , criticalThreshold = 3
                            , quota = 2
                            }

            let msg = List.head msgs
            msg.peer `shouldBe` mockPeer

    describe "with critical severity" do
        it "should include the correct peer in the published critical message" do
            let msgs =
                    runDuplicateBlockGuard
                        def
                            { warningThreshold = 1
                            , criticalThreshold = 2
                            , quota = 3
                            }

            let msg = List.head msgs
            msg.severity `shouldBe` Critical
            msg.peer `shouldBe` mockPeer
  where
    runDuplicateBlockGuard run =
        runPureEff
            . runTracingNoOp
            . runReader
                def
                    { duplicateBlocks =
                        AdversarialThresholds
                            { warningThreshold = run.warningThreshold
                            , criticalThreshold = run.criticalThreshold
                            }
                    }
            . runQuotaConst run.quota
            . execWriter @[AdversarialBehavior]
            . runPubWriter @AdversarialBehavior
            $ duplicateBlockGuard blockReceived
    blockReceived =
        BlockReceived
            { peer = mockPeer
            , block = block1
            , requestId = mockUUID
            , range = mockChainRange
            , headerWithSameSlotNumber = Nothing
            }


testHeaderBlockMismatchGuard :: Spec
testHeaderBlockMismatchGuard = do
    describe "with quota < warningThreshold" do
        it "should not flag block with mismatching header" do
            let msgs =
                    runHeaderBlockMismatchGuard
                        Run
                            { warningThreshold = 2
                            , criticalThreshold = 5
                            , quota = 1
                            , block = block1
                            , header = Just header2
                            }
            msgs `shouldBe` ([], [])

    describe "with quota > criticalThreshold" do
        it "should flag block as critical with mismatching header" do
            let (adversarialMsgs, mismatchMsgs) =
                    runHeaderBlockMismatchGuard
                        Run
                            { warningThreshold = 1
                            , criticalThreshold = 2
                            , quota = 3
                            , block = block1
                            , header = Just header2
                            }

            length mismatchMsgs `shouldBe` 1
            length adversarialMsgs `shouldBe` 1
            let adversarial = List.head adversarialMsgs
            adversarial.severity `shouldBe` Critical

    describe "with quota > warningThreshold and quota <= criticalThreshold" do
        it "should flag block as minor with mismatching header" do
            let (adversarialMsgs, mismatchMsgs) =
                    runHeaderBlockMismatchGuard
                        Run
                            { warningThreshold = 1
                            , criticalThreshold = 10
                            , quota = 5
                            , block = block1
                            , header = Just header2
                            }

            length mismatchMsgs `shouldBe` 1
            length adversarialMsgs `shouldBe` 1
            let adversarial = List.head adversarialMsgs
            adversarial.severity `shouldBe` Minor

    describe "with quota == criticalThreshold" do
        it "should flag as minor (not critical) when quota equals critical threshold" do
            let (adversarialMsgs, _) =
                    runHeaderBlockMismatchGuard
                        Run
                            { warningThreshold = 1
                            , criticalThreshold = 5
                            , quota = 5
                            , block = block1
                            , header = Just header2
                            }

            length adversarialMsgs `shouldBe` 1
            let adversarial = List.head adversarialMsgs
            adversarial.severity `shouldBe` Minor

    describe "with quota == criticalThreshold + 1" do
        it "should flag as critical" do
            let (adversarialMsgs, _) =
                    runHeaderBlockMismatchGuard
                        Run
                            { warningThreshold = 1
                            , criticalThreshold = 5
                            , quota = 6
                            , block = block1
                            , header = Just header2
                            }

            length adversarialMsgs `shouldBe` 1
            let adversarial = List.head adversarialMsgs
            adversarial.severity `shouldBe` Critical

    describe "with quota == warningThreshold" do
        it "should not flag" do
            let msgs =
                    runHeaderBlockMismatchGuard
                        Run
                            { warningThreshold = 3
                            , criticalThreshold = 5
                            , quota = 3
                            , block = block1
                            , header = Just header2
                            }
            msgs `shouldBe` ([], [])

    describe "with quota == warningThreshold + 1" do
        it "should flag as minor" do
            let (adversarialMsgs, mismatchMsgs) =
                    runHeaderBlockMismatchGuard
                        Run
                            { warningThreshold = 3
                            , criticalThreshold = 10
                            , quota = 4
                            , block = block1
                            , header = Just header2
                            }

            length mismatchMsgs `shouldBe` 1
            length adversarialMsgs `shouldBe` 1
            let adversarial = List.head adversarialMsgs
            adversarial.severity `shouldBe` Minor

    describe "with no associated header" do
        it "should not flag block without associated header" do
            let msgs =
                    runHeaderBlockMismatchGuard
                        Run
                            { warningThreshold = 1
                            , criticalThreshold = 10
                            , quota = 10
                            , block = block1
                            , header = Nothing
                            }
            msgs `shouldBe` ([], [])

    describe "with matching associated header" do
        it "should not flag block with matching header" do
            let msgs =
                    runHeaderBlockMismatchGuard
                        Run
                            { warningThreshold = 1
                            , criticalThreshold = 5
                            , quota = 100
                            , block = block1
                            , header = Just header1
                            }
            msgs `shouldBe` ([], [])

    describe "with mismatch" do
        it "should publish ReceivedMismatchingBlock with correct peer and block" do
            let (_, mismatchMsgs) =
                    runHeaderBlockMismatchGuard
                        Run
                            { warningThreshold = 1
                            , criticalThreshold = 5
                            , quota = 100
                            , block = block1
                            , header = Just header2
                            }

            let mismatch = List.head mismatchMsgs
            mismatch.peer `shouldBe` mockPeer
            mismatch.block `shouldBe` block1

    describe "with critical severity" do
        it "should publish AdversarialBehavior with correct peer and description" do
            let (adversarialMsgs, _) =
                    runHeaderBlockMismatchGuard
                        Run
                            { warningThreshold = 1
                            , criticalThreshold = 5
                            , quota = 100
                            , block = block1
                            , header = Just header2
                            }

            let adversarial = List.head adversarialMsgs
            adversarial.peer `shouldBe` mockPeer
            adversarial.description `shouldBe` "exceeded header-block hash mismatch Critical threshold"

    describe "with minor severity" do
        it "should publish AdversarialBehavior with correct peer and description" do
            let (adversarialMsgs, _) =
                    runHeaderBlockMismatchGuard
                        Run
                            { warningThreshold = 1
                            , criticalThreshold = 10
                            , quota = 5
                            , block = block1
                            , header = (Just header2)
                            }

            let adversarial = List.head adversarialMsgs
            adversarial.peer `shouldBe` mockPeer
            adversarial.description `shouldBe` "exceeded header-block hash mismatch Minor threshold"
  where
    runHeaderBlockMismatchGuard run =
        runPureEff
            . runTracingNoOp
            . runReader
                def
                    { hashMismatch =
                        AdversarialThresholds
                            { warningThreshold = run.warningThreshold
                            , criticalThreshold = run.criticalThreshold
                            }
                    }
            . runQuotaConst run.quota
            . runWriter @[ReceivedMismatchingBlock]
            . execWriter @[AdversarialBehavior]
            . runPubWriter @ReceivedMismatchingBlock
            . runPubWriter @AdversarialBehavior
            . headerBlockMismatchGuard
            $ mkBlockReceived run.block run.header
    mkBlockReceived block headerWithSameSlotNumber =
        BlockReceived
            { peer = mockPeer
            , block
            , requestId = mockUUID
            , range = mockChainRange
            , headerWithSameSlotNumber
            }


testReceivedBlockIsOutsideRequestedRangeGuard :: Spec
testReceivedBlockIsOutsideRequestedRangeGuard = do
    describe "with block in requested range" do
        it "should not flag" do
            let msgs = runGuard block1 block1Point block1Point
            msgs `shouldBe` ([], [])

    -- block2 (Babbage) has a higher slot than block1 (Shelley), so block1 is before the range
    describe "with block before start of range" do
        it "should flag" do
            let (adversarialMsgs, outsideMsgs) = runGuard block1 block2Point block2Point
            length adversarialMsgs `shouldBe` 1
            length outsideMsgs `shouldBe` 1

    -- block2 (Babbage) has a higher slot than block1 (Shelley), so block2 is after the range
    describe "with block after end of range" do
        it "should flag block after range end" do
            let (adversarialMsgs, outsideMsgs) = runGuard block2 block1Point block1Point
            length adversarialMsgs `shouldBe` 1
            length outsideMsgs `shouldBe` 1

    describe "with duplicate block" do
        describe "with Minor severity" do
            it "should publish AdversarialBehavior" do
                let (adversarialMsgs, _) = runGuard block2 block1Point block1Point
                let msg = List.head adversarialMsgs
                msg.severity `shouldBe` Minor

        it "should publish AdversarialBehavior with correct peer" do
            let (adversarialMsgs, _) = runGuard block2 block1Point block1Point
            let msg = List.head adversarialMsgs
            msg.peer `shouldBe` mockPeer

        it "should publish ReceivedBlockOutsideRequestedRange with correct peer and block" do
            let (_, outsideMsgs) = runGuard block2 block1Point block1Point
            let msg = List.head outsideMsgs
            msg.peer `shouldBe` mockPeer
            msg.block `shouldBe` block2

    describe "with block at start of range" do
        it "should not flag block at exact range start boundary" do
            -- Range spans block1's slot to block2's slot; block1 is at the start
            let msgs = runGuard block1 block1Point block2Point
            msgs `shouldBe` ([], [])

    describe "with block at end of range" do
        it "should not flag block at exact range end boundary" do
            -- Range spans block1's slot to block2's slot; block2 is at the end
            let msgs = runGuard block2 block1Point block2Point
            msgs `shouldBe` ([], [])
  where
    block1Point = headerPoint $ getHeader block1
    block2Point = headerPoint $ getHeader block2
    runGuard block point1 point2 =
        runPureEff
            . runTracingNoOp
            . runWriter @[ReceivedBlockOutsideRequestedRange]
            . execWriter @[AdversarialBehavior]
            . runPubWriter @ReceivedBlockOutsideRequestedRange
            . runPubWriter @AdversarialBehavior
            . receivedBlockIsOutsideRequestedRangeGuard
            $ BlockReceived
                { peer = mockPeer
                , block
                , requestId = mockUUID
                , range = ChainRange point1 point2
                , headerWithSameSlotNumber = Nothing
                }


data Run = Run
    { warningThreshold :: Word
    , criticalThreshold :: Word
    , quota :: Int
    , block :: CardanoBlock
    , header :: Maybe CardanoHeader
    }


instance Default Run where
    def =
        Run
            { warningThreshold = 1
            , criticalThreshold = 5
            , quota = 0
            , block = block1
            , header = Nothing
            }


block1 :: CardanoBlock
block1 = BlockShelley $ snd $ List.head $ examplesShelley.exampleBlock


header1 :: CardanoHeader
header1 = getHeader block1


block2 :: CardanoBlock
block2 =
    let original = snd $ List.head $ examplesBabbage.exampleBlock
        Block hdr body = original.shelleyBlockRaw
        Header hdrBody sig = hdr
        newHdrBody = hdrBody {hbSlotNo = SlotNo 9999}
        newRawBlock = Block (Header newHdrBody sig) body
    in  BlockBabbage (mkShelleyBlock newRawBlock)


header2 :: CardanoHeader
header2 = getHeader block2


mockPeer :: Peer
mockPeer =
    Peer
        { id = ID mockUUID
        , address = PeerAddress (read "192.168.1.1") 3001
        , firstDiscovered = now
        , lastSeen = now
        , lastConnected = Nothing
        , lastFailureTime = Nothing
        , discoveredVia = "test"
        }
  where
    now = read "1970-01-01 01:00:00Z"


mockUUID :: UUID
mockUUID = UUID.fromWords64 0 1


mockChainRange :: ChainRange CardanoPoint
mockChainRange = ChainRange mockPoint mockPoint


mockPoint :: CardanoPoint
mockPoint = headerPoint $ getHeader block1
