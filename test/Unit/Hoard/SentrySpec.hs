{-# LANGUAGE PatternSynonyms #-}

module Unit.Hoard.SentrySpec (spec_Sentry) where

import Data.Default (def)
import Data.UUID (UUID)
import Effectful (runPureEff)
import Effectful.Reader.Static (runReader)
import Effectful.Writer.Static.Shared (execWriter, runWriter)
import Cardano.Ledger.Block (Block (..))
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

import Hoard.Data.ID (ID (..))
import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.Effects.Monitoring.Tracing (runTracingNoOp)
import Hoard.Effects.Publishing (runPubWriter)
import Hoard.Effects.Quota (runQuotaConst)
import Hoard.Events.BlockFetch (BlockReceived (..))
import Hoard.Sentry
    ( AdversarialBehavior (..)
    , AdversarialSeverity (..)
    , AdversarialThresholds (..)
    , Config (..)
    , ReceivedBlockOutsideRequestedRange (..)
    , ReceivedMismatchingBlock (..)
    , duplicateBlockGuard
    , headerBlockHashMismatchGuard
    , receivedBlockIsOutsideRequestedRangeGuard
    )
import Hoard.Types.Cardano (CardanoBlock, CardanoHeader, CardanoPoint)


spec_Sentry :: Spec
spec_Sentry = do
    describe "headerBlockHashMismatchGuard" testHeaderBlockHashMismatchGuard
    describe "duplicateBlockGuard" testDuplicateBlockGuard
    describe "receivedBlockIsOutsideRequestedRangeGuard" testReceivedBlockIsOutsideRequestedRangeGuard


testDuplicateBlockGuard :: Spec
testDuplicateBlockGuard = do
    it "should not flag the peer if block is not duplicate" do
        let msgs =
                runPureEff
                    . runTracingNoOp
                    . runReader def
                    . runQuotaConst 1
                    . execWriter @[AdversarialBehavior]
                    . runPubWriter @AdversarialBehavior
                    $ duplicateBlockGuard blockReceived
        msgs `shouldMatchList` []

    it "should flag the peer if block breaches the warning threshold" do
        let msgs =
                runPureEff
                    . runTracingNoOp
                    . runReader
                        def
                            { duplicateBlocks =
                                AdversarialThresholds
                                    { warningThreshold = 1
                                    , criticalThreshold = 5
                                    }
                            }
                    . runQuotaConst 2
                    . execWriter @[AdversarialBehavior]
                    . runPubWriter @AdversarialBehavior
                    $ duplicateBlockGuard blockReceived
        length msgs `shouldBe` 1
        let msg = List.head msgs
        msg.severity `shouldBe` Minor

    it "should flag the peer if block breaches the critical threshold" do
        let msgs =
                runPureEff
                    . runTracingNoOp
                    . runReader
                        def
                            { duplicateBlocks =
                                AdversarialThresholds
                                    { warningThreshold = 1
                                    , criticalThreshold = 2
                                    }
                            }
                    . runQuotaConst 3
                    . execWriter @[AdversarialBehavior]
                    . runPubWriter @AdversarialBehavior
                    $ duplicateBlockGuard blockReceived
        length msgs `shouldBe` 1
        let msg = List.head msgs
        msg.severity `shouldBe` Critical

    it "should not flag the peer if block count equals the warning threshold" do
        let msgs =
                runPureEff
                    . runTracingNoOp
                    . runReader
                        def
                            { duplicateBlocks =
                                AdversarialThresholds
                                    { warningThreshold = 2
                                    , criticalThreshold = 5
                                    }
                            }
                    . runQuotaConst 2
                    . execWriter @[AdversarialBehavior]
                    . runPubWriter @AdversarialBehavior
                    $ duplicateBlockGuard blockReceived
        msgs `shouldBe` []

    it "should flag as minor (not critical) when block count equals the critical threshold" do
        let msgs =
                runPureEff
                    . runTracingNoOp
                    . runReader
                        def
                            { duplicateBlocks =
                                AdversarialThresholds
                                    { warningThreshold = 1
                                    , criticalThreshold = 5
                                    }
                            }
                    . runQuotaConst 5
                    . execWriter @[AdversarialBehavior]
                    . runPubWriter @AdversarialBehavior
                    $ duplicateBlockGuard blockReceived
        length msgs `shouldBe` 1
        let msg = List.head msgs
        msg.severity `shouldBe` Minor

    it "should include the correct peer in the published warning message" do
        let msgs =
                runPureEff
                    . runTracingNoOp
                    . runReader
                        def
                            { duplicateBlocks =
                                AdversarialThresholds
                                    { warningThreshold = 1
                                    , criticalThreshold = 5
                                    }
                            }
                    . runQuotaConst 2
                    . execWriter @[AdversarialBehavior]
                    . runPubWriter @AdversarialBehavior
                    $ duplicateBlockGuard blockReceived
        let msg = List.head msgs
        msg.peer `shouldBe` mockPeer

    it "should include the correct peer in the published critical message" do
        let msgs =
                runPureEff
                    . runTracingNoOp
                    . runReader
                        def
                            { duplicateBlocks =
                                AdversarialThresholds
                                    { warningThreshold = 1
                                    , criticalThreshold = 2
                                    }
                            }
                    . runQuotaConst 3
                    . execWriter @[AdversarialBehavior]
                    . runPubWriter @AdversarialBehavior
                    $ duplicateBlockGuard blockReceived
        let msg = List.head msgs
        msg.peer `shouldBe` mockPeer
  where
    blockReceived =
        BlockReceived
            { peer = mockPeer
            , block = block1
            , requestId = mockUUID
            , range = mockChainRange
            , headerWithSameSlotNumber = Nothing
            }


testHeaderBlockHashMismatchGuard :: Spec
testHeaderBlockHashMismatchGuard = do
    it "should not flag block without associated header" do
        let msgs =
                runPureEff
                    . runTracingNoOp
                    . runReader @Config def
                    . runQuotaConst 10
                    . collectPubs
                    . headerBlockHashMismatchGuard
                    $ mkBlockReceived block1 Nothing
        msgs `shouldBe` ([], [])

    it "should not flag block with mismatching header if below quota" do
        let msgs =
                runPureEff
                    . runTracingNoOp
                    . runReader @Config def
                    . runQuotaConst 0
                    . collectPubs
                    . headerBlockHashMismatchGuard
                    . mkBlockReceived block1
                    $ Just header2
        msgs `shouldBe` ([], [])

    it "should not flag block with matching header" do
        let msgs =
                runPureEff
                    . runTracingNoOp
                    . runReader @Config def
                    . runQuotaConst 100
                    . collectPubs
                    . headerBlockHashMismatchGuard
                    . mkBlockReceived block1
                    $ Just header1
        msgs `shouldBe` ([], [])

    it "should flag block as critical with mismatching header over critical threshold" do
        let (adversarialMsgs, mismatchMsgs) =
                runPureEff
                    . runTracingNoOp
                    . runReader @Config def
                    . runQuotaConst 100
                    . collectPubs
                    . headerBlockHashMismatchGuard
                    . mkBlockReceived block1
                    $ Just header2

        length mismatchMsgs `shouldBe` 1
        length adversarialMsgs `shouldBe` 1
        let adversarial = List.head adversarialMsgs
        adversarial.severity `shouldBe` Critical

    it "should flag block as minor with mismatching header over warning threshold" do
        let (adversarialMsgs, mismatchMsgs) =
                runPureEff
                    . runTracingNoOp
                    . runReader @Config
                        def
                            { hashMismatch =
                                AdversarialThresholds
                                    { warningThreshold = 1
                                    , criticalThreshold = 10
                                    }
                            }
                    . runQuotaConst 5
                    . collectPubs
                    . headerBlockHashMismatchGuard
                    . mkBlockReceived block1
                    $ Just header2

        length mismatchMsgs `shouldBe` 1
        length adversarialMsgs `shouldBe` 1
        let adversarial = List.head adversarialMsgs
        adversarial.severity `shouldBe` Minor

    -- Boundary: quota == criticalThreshold uses >, so this should be Minor not Critical
    it "should flag as minor (not critical) when quota equals critical threshold" do
        let (adversarialMsgs, _) =
                runPureEff
                    . runTracingNoOp
                    . runReader @Config def -- criticalThreshold = 5
                    . runQuotaConst 5
                    . collectPubs
                    . headerBlockHashMismatchGuard
                    . mkBlockReceived block1
                    $ Just header2

        length adversarialMsgs `shouldBe` 1
        let adversarial = List.head adversarialMsgs
        adversarial.severity `shouldBe` Minor

    -- Boundary: quota == criticalThreshold + 1 should be Critical
    it "should flag as critical when quota is one above critical threshold" do
        let (adversarialMsgs, _) =
                runPureEff
                    . runTracingNoOp
                    . runReader @Config def -- criticalThreshold = 5
                    . runQuotaConst 6
                    . collectPubs
                    . headerBlockHashMismatchGuard
                    . mkBlockReceived block1
                    $ Just header2

        length adversarialMsgs `shouldBe` 1
        let adversarial = List.head adversarialMsgs
        adversarial.severity `shouldBe` Critical

    -- Boundary: quota == warningThreshold uses >, so exactly at threshold should not flag
    it "should not flag when quota equals a non-zero warning threshold" do
        let msgs =
                runPureEff
                    . runTracingNoOp
                    . runReader @Config
                        def
                            { hashMismatch =
                                AdversarialThresholds
                                    { warningThreshold = 3
                                    , criticalThreshold = 10
                                    }
                            }
                    . runQuotaConst 3
                    . collectPubs
                    . headerBlockHashMismatchGuard
                    . mkBlockReceived block1
                    $ Just header2
        msgs `shouldBe` ([], [])

    -- Boundary: quota == warningThreshold + 1 should flag as Minor
    it "should flag as minor when quota is one above a non-zero warning threshold" do
        let (adversarialMsgs, mismatchMsgs) =
                runPureEff
                    . runTracingNoOp
                    . runReader @Config
                        def
                            { hashMismatch =
                                AdversarialThresholds
                                    { warningThreshold = 3
                                    , criticalThreshold = 10
                                    }
                            }
                    . runQuotaConst 4
                    . collectPubs
                    . headerBlockHashMismatchGuard
                    . mkBlockReceived block1
                    $ Just header2

        length mismatchMsgs `shouldBe` 1
        length adversarialMsgs `shouldBe` 1
        let adversarial = List.head adversarialMsgs
        adversarial.severity `shouldBe` Minor

    it "should publish ReceivedMismatchingBlock with correct peer and block" do
        let (_, mismatchMsgs) =
                runPureEff
                    . runTracingNoOp
                    . runReader @Config def
                    . runQuotaConst 100
                    . collectPubs
                    . headerBlockHashMismatchGuard
                    . mkBlockReceived block1
                    $ Just header2

        let mismatch = List.head mismatchMsgs
        mismatch.peer `shouldBe` mockPeer
        mismatch.block `shouldBe` block1

    it "should publish AdversarialBehavior with correct peer and description when critical" do
        let (adversarialMsgs, _) =
                runPureEff
                    . runTracingNoOp
                    . runReader @Config def
                    . runQuotaConst 100
                    . collectPubs
                    . headerBlockHashMismatchGuard
                    . mkBlockReceived block1
                    $ Just header2

        let adversarial = List.head adversarialMsgs
        adversarial.peer `shouldBe` mockPeer
        adversarial.description `shouldBe` "exceeded header-block hash mismatch Critical threshold"

    it "should publish AdversarialBehavior with correct peer and description when minor" do
        let (adversarialMsgs, _) =
                runPureEff
                    . runTracingNoOp
                    . runReader @Config
                        def
                            { hashMismatch =
                                AdversarialThresholds
                                    { warningThreshold = 1
                                    , criticalThreshold = 10
                                    }
                            }
                    . runQuotaConst 5
                    . collectPubs
                    . headerBlockHashMismatchGuard
                    . mkBlockReceived block1
                    $ Just header2

        let adversarial = List.head adversarialMsgs
        adversarial.peer `shouldBe` mockPeer
        adversarial.description `shouldBe` "exceeded header-block hash mismatch Minor threshold"
  where
    collectPubs =
        runWriter @[ReceivedMismatchingBlock]
            . execWriter @[AdversarialBehavior]
            . runPubWriter @ReceivedMismatchingBlock
            . runPubWriter @AdversarialBehavior
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
    it "should not flag block within range" do
        let msgs =
                runPureEff
                    . runTracingNoOp
                    . collectPubs
                    . receivedBlockIsOutsideRequestedRangeGuard
                    $ mkBlockReceived block1 (ChainRange mockPoint mockPoint)
        msgs `shouldBe` ([], [])

    -- block2 (Babbage) has a higher slot than block1 (Shelley), so block1 is before the range
    it "should flag block before range start" do
        let (adversarialMsgs, outsideMsgs) =
                runPureEff
                    . runTracingNoOp
                    . collectPubs
                    . receivedBlockIsOutsideRequestedRangeGuard
                    $ mkBlockReceived block1 (ChainRange block2Point block2Point)
        length adversarialMsgs `shouldBe` 1
        length outsideMsgs `shouldBe` 1

    -- block2 (Babbage) has a higher slot than block1 (Shelley), so block2 is after the range
    it "should flag block after range end" do
        let (adversarialMsgs, outsideMsgs) =
                runPureEff
                    . runTracingNoOp
                    . collectPubs
                    . receivedBlockIsOutsideRequestedRangeGuard
                    $ mkBlockReceived block2 (ChainRange mockPoint mockPoint)
        length adversarialMsgs `shouldBe` 1
        length outsideMsgs `shouldBe` 1

    it "should publish AdversarialBehavior with severity Minor" do
        let (adversarialMsgs, _) =
                runPureEff
                    . runTracingNoOp
                    . collectPubs
                    . receivedBlockIsOutsideRequestedRangeGuard
                    $ mkBlockReceived block2 (ChainRange mockPoint mockPoint)
        let msg = List.head adversarialMsgs
        msg.severity `shouldBe` Minor

    it "should publish AdversarialBehavior with correct peer" do
        let (adversarialMsgs, _) =
                runPureEff
                    . runTracingNoOp
                    . collectPubs
                    . receivedBlockIsOutsideRequestedRangeGuard
                    $ mkBlockReceived block2 (ChainRange mockPoint mockPoint)
        let msg = List.head adversarialMsgs
        msg.peer `shouldBe` mockPeer

    it "should publish ReceivedBlockOutsideRequestedRange with correct peer and block" do
        let (_, outsideMsgs) =
                runPureEff
                    . runTracingNoOp
                    . collectPubs
                    . receivedBlockIsOutsideRequestedRangeGuard
                    $ mkBlockReceived block2 (ChainRange mockPoint mockPoint)
        let msg = List.head outsideMsgs
        msg.peer `shouldBe` mockPeer
        msg.block `shouldBe` block2

    it "should not flag block at exact range start boundary" do
        -- Range spans block1's slot to block2's slot; block1 is at the start
        let msgs =
                runPureEff
                    . runTracingNoOp
                    . collectPubs
                    . receivedBlockIsOutsideRequestedRangeGuard
                    $ mkBlockReceived block1 (ChainRange mockPoint block2Point)
        msgs `shouldBe` ([], [])

    it "should not flag block at exact range end boundary" do
        -- Range spans block1's slot to block2's slot; block2 is at the end
        let msgs =
                runPureEff
                    . runTracingNoOp
                    . collectPubs
                    . receivedBlockIsOutsideRequestedRangeGuard
                    $ mkBlockReceived block2 (ChainRange mockPoint block2Point)
        msgs `shouldBe` ([], [])
  where
    block2Point = headerPoint $ getHeader block2
    collectPubs =
        runWriter @[ReceivedBlockOutsideRequestedRange]
            . execWriter @[AdversarialBehavior]
            . runPubWriter @ReceivedBlockOutsideRequestedRange
            . runPubWriter @AdversarialBehavior
    mkBlockReceived block range =
        BlockReceived
            { peer = mockPeer
            , block
            , requestId = mockUUID
            , range
            , headerWithSameSlotNumber = Nothing
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
        newHdrBody = hdrBody{hbSlotNo = SlotNo 9999}
        newRawBlock = Block (Header newHdrBody sig) body
     in BlockBabbage (mkShelleyBlock newRawBlock)


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
