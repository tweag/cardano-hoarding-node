module Integration.Hoard.DB.EvictionSpec (spec_Eviction) where

import Data.List ((!!))
import Data.Maybe (fromJust)
import Data.Time (UTCTime (..))
import Effectful (runEff, runPureEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Reader.Static (runReader)
import Hasql.Statement (Statement)
import Ouroboros.Consensus.Block (GetHeader (getHeader))
import Ouroboros.Consensus.Cardano.Block
    ( HardForkBlock
        ( BlockAllegra
        , BlockAlonzo
        , BlockBabbage
        , BlockConway
        , BlockMary
        , BlockShelley
        )
    )
import Test.Consensus.Shelley.Examples
    ( examplesAllegra
    , examplesAlonzo
    , examplesBabbage
    , examplesConway
    , examplesMary
    , examplesShelley
    )
import Test.Hspec
import Test.Util.Serialisation.Examples (Examples (..))
import Text.Read (read)

import Data.List qualified as List
import Data.UUID.V4 qualified as UUID
import Ouroboros.Consensus.Cardano.Block qualified as O
import Rel8 qualified

import Atelier.Effects.Clock (runClock)
import Atelier.Effects.Monitoring.Metrics (runMetricsNoOp)
import Atelier.Effects.Monitoring.Tracing (runTracingNoOp)
import Hoard.API.Data.BlockViolation (SlotDispute (..))
import Hoard.Data.Block (Block (..))
import Hoard.Data.BlockHash (BlockHash (..), mkBlockHash)
import Hoard.Data.Header (Header (..))
import Hoard.Data.HeaderTag (HeaderTag (..))
import Hoard.Data.ID (ID (..))
import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.Data.PoolID (PoolID (..))
import Hoard.Effects.BlockRepo (classifyBlock, evictBlocks, getSlotDisputesInRange, insertBlocks, runBlockRepo, tagBlock)
import Hoard.Effects.DB (runDBRead, runDBWrite, runQuery)
import Hoard.Effects.HeaderRepo (evictHeaders, runHeaderRepo, tagHeader, upsertHeader)
import Hoard.Effects.PeerRepo (PeerRepo, runPeerRepo, upsertPeers)
import Hoard.Effects.Verifier (Validity (Valid), Verified)
import Hoard.OrphanDetection.Data (BlockClassification (..))
import Hoard.TestHelpers.Database (withCleanTestDatabase)
import Hoard.Types.Cardano (CardanoBlock, CardanoHeader)
import Prelude hiding (head)

import Atelier.Effects.Log qualified as Log
import Hoard.DB.Schemas.HeaderReceipts qualified as HeaderReceiptsSchema
import Hoard.Data.BlockTag qualified as BlockTag
import Hoard.Effects.Verifier qualified as Verifier


spec_Eviction :: Spec
spec_Eviction = do
    let runDB config action =
            runEff
                . Log.runLogNoOp
                . runErrorNoCallStack @Text
                . runReader config
                . runMetricsNoOp
                . runTracingNoOp
                . runClock
                . runDBRead
                . runDBWrite
                . runConcurrent
                . runBlockRepo
                . runPeerRepo
                . runHeaderRepo
                $ action

    withCleanTestDatabase $ describe "Eviction (database)" $ do
        context "when there are no blocks"
            $ it "evicts nothing"
            $ \config -> do
                result <- runDB config evictBlocks
                result `shouldBe` Right 0

        context "when all canonical blocks have no orphan at their slot" $ do
            it "evicts all canonical blocks" $ \config -> do
                -- Insert 3 canonical blocks at different slots (no orphans)
                let blocks = [mkTestBlock 1, mkTestBlock 2, mkTestBlock 3]
                result <- runDB config $ do
                    insertBlocks blocks
                    classifyBlock (blockHash 1) Canonical epoch
                    classifyBlock (blockHash 2) Canonical epoch
                    classifyBlock (blockHash 3) Canonical epoch
                    evictBlocks
                result `shouldBe` Right 3

            it "keeps orphaned blocks" $ \config -> do
                -- canonical10 (slot 10) - no orphan at slot 10 → evicted
                -- orphan10 (slot 20) - orphan → kept
                -- canonical11 (slot 11) - no orphan at slot 11 → evicted
                let canonical10 = mkTestBlock 10
                    orphan10 = mkTestBlock 20 -- different block hash, different slot
                    canonical11 = mkTestBlock 11
                result <- runDB config $ do
                    insertBlocks [canonical10, orphan10, canonical11]
                    classifyBlock (blockHash 10) Canonical epoch
                    classifyBlock (blockHash 20) Orphaned epoch
                    classifyBlock (blockHash 11) Canonical epoch
                    evictBlocks
                result `shouldBe` Right 2

        context "when a canonical block has an orphan at the same slot"
            $ it "keeps the canonical block"
            $ \config -> do
                let canonicalBlock = mkTestBlockAtSlot 0 100 -- hash from index 0, slot 100
                    orphanBlock = mkTestBlockAtSlot 1 100 -- hash from index 1, same slot 100
                result <- runDB config $ do
                    insertBlocks [canonicalBlock, orphanBlock]
                    classifyBlock (blockHash' canonicalBlock) Canonical epoch
                    classifyBlock (blockHash' orphanBlock) Orphaned epoch
                    evictBlocks
                result `shouldBe` Right 0

        context "when blocks are unclassified"
            $ it "does not evict them"
            $ \config -> do
                let blocks = [mkTestBlock 1, mkTestBlock 2]
                result <- runDB config $ do
                    insertBlocks blocks
                    evictBlocks
                result `shouldBe` Right 0

        context "when a canonical block's header has tags"
            $ it "does not evict it"
            $ \config -> do
                peer <- mkTestPeer
                result <- runDB config $ do
                    insertBlocks [mkTestBlock 1]
                    classifyBlock (blockHash 1) Canonical epoch
                    -- header_tags has a FK to headers(hash), so we must insert the header first
                    persistedPeer <- upsertOnePeer peer
                    upsertHeader (makeValidHeader $ mkMatchingHeader 1) persistedPeer epoch
                    tagHeader (blockHash 1) [CorruptHeaderIntegrity]
                    evictBlocks
                result `shouldBe` Right 0

        context "when blocks are orphaned"
            $ it "does not evict them"
            $ \config -> do
                let blocks = [mkTestBlock 1, mkTestBlock 2]
                result <- runDB config $ do
                    insertBlocks blocks
                    classifyBlock (blockHash 1) Orphaned epoch
                    classifyBlock (blockHash 2) Orphaned epoch
                    evictBlocks
                result `shouldBe` Right 0

    withCleanTestDatabase $ describe "Header eviction (database)" $ do
        context "when there are no headers"
            $ it "evicts nothing"
            $ \config -> do
                result <- runDB config evictHeaders
                result `shouldBe` Right 0

        context "when headers have no tags" $ do
            it "evicts all of them" $ \config -> do
                peer <- mkTestPeer
                result <- runDB config $ do
                    persistedPeer <- upsertOnePeer peer
                    upsertHeader (makeValidHeader $ mkHeader "hash1" 1) persistedPeer epoch
                    upsertHeader (makeValidHeader $ mkHeader "hash2" 2) persistedPeer epoch
                    evictHeaders
                result `shouldBe` Right 2

            it "also removes their receipts" $ \config -> do
                peer <- mkTestPeer
                _ <- runDB config $ do
                    persistedPeer <- upsertOnePeer peer
                    upsertHeader (makeValidHeader $ mkHeader "hash1" 1) persistedPeer epoch
                    evictHeaders
                receiptCount <- runDB config $ runQuery "count-receipts" countReceiptsStmt
                receiptCount `shouldBe` Right 0

        context "when headers have tags" $ do
            it "does not evict them" $ \config -> do
                peer <- mkTestPeer
                result <- runDB config $ do
                    persistedPeer <- upsertOnePeer peer
                    upsertHeader (makeValidHeader $ mkHeader "hash1" 1) persistedPeer epoch
                    tagHeader (BlockHash "hash1") [CorruptHeaderIntegrity]
                    evictHeaders
                result `shouldBe` Right 0

            it "preserves their receipts" $ \config -> do
                peer <- mkTestPeer
                _ <- runDB config $ do
                    persistedPeer <- upsertOnePeer peer
                    upsertHeader (makeValidHeader $ mkHeader "hash1" 1) persistedPeer epoch
                    tagHeader (BlockHash "hash1") [CorruptHeaderIntegrity]
                    evictHeaders
                receiptCount <- runDB config $ runQuery "count-receipts-tagged" countReceiptsStmt
                receiptCount `shouldBe` Right 1

        context "when some headers have tags and others do not"
            $ it "evicts only the untagged ones"
            $ \config -> do
                peer <- mkTestPeer
                result <- runDB config $ do
                    persistedPeer <- upsertOnePeer peer
                    upsertHeader (makeValidHeader $ mkHeader "hash1" 1) persistedPeer epoch
                    upsertHeader (makeValidHeader $ mkHeader "hash2" 2) persistedPeer epoch
                    tagHeader (BlockHash "hash1") [CorruptHeaderIntegrity]
                    evictHeaders
                result `shouldBe` Right 1

        context "when a header's block hash has block tags"
            $ it "does not evict it"
            $ \config -> do
                peer <- mkTestPeer
                result <- runDB config $ do
                    persistedPeer <- upsertOnePeer peer
                    upsertHeader (makeValidHeader $ mkHeader "hash1" 1) persistedPeer epoch
                    -- Tag the block with the same hash (no block row needed — no FK on block_tags)
                    tagBlock (BlockHash "hash1") [BlockTag.SlotDispute]
                    evictHeaders
                result `shouldBe` Right 0

    withCleanTestDatabase $ describe "getSlotDisputesInRange (database)" $ do
        it "returns empty list when no disputes exist" $ \config -> do
            result <- runDB config $ getSlotDisputesInRange Nothing Nothing
            result `shouldBe` Right []

        it "returns a dispute when canonical and orphan exist at the same slot" $ \config -> do
            let canonicalBlock = mkTestBlockAtSlot 0 100
                orphanBlock = mkTestBlockAtSlot 1 100
            result <- runDB config $ do
                insertBlocks [canonicalBlock, orphanBlock]
                classifyBlock (blockHash' canonicalBlock) Canonical epoch
                classifyBlock (blockHash' orphanBlock) Orphaned epoch
                getSlotDisputesInRange Nothing Nothing
            case result of
                Right disputes -> map (.slotNumber) disputes `shouldBe` ([100] :: [Int64])
                Left err -> expectationFailure $ "getSlotDisputesInRange failed: " <> show err

        it "filters disputes by slot range" $ \config -> do
            let canonical10 = mkTestBlockAtSlot 0 10
                orphan10 = mkTestBlockAtSlot 1 10
                canonical20 = mkTestBlockAtSlot 2 20
                orphan20 = mkTestBlockAtSlot 3 20
            result <- runDB config $ do
                insertBlocks [canonical10, orphan10, canonical20, orphan20]
                classifyBlock (blockHash' canonical10) Canonical epoch
                classifyBlock (blockHash' orphan10) Orphaned epoch
                classifyBlock (blockHash' canonical20) Canonical epoch
                classifyBlock (blockHash' orphan20) Orphaned epoch
                getSlotDisputesInRange (Just 15) Nothing
            case result of
                Right disputes -> map (.slotNumber) disputes `shouldBe` ([20] :: [Int64])
                Left err -> expectationFailure $ "getSlotDisputesInRange failed: " <> show err


-- | Block hash for a test block created with mkTestBlock n
blockHash :: Int64 -> BlockHash
blockHash n = (.hash) $ Verifier.getVerified (mkTestBlock n)


-- | Block hash from a verified block
blockHash' :: Verified Valid Block -> BlockHash
blockHash' = (.hash) . Verifier.getVerified


-- | Create a test block with slot number = n (and hash derived from n mod examples)
mkTestBlock :: Int64 -> Verified Valid Block
mkTestBlock n = mkTestBlockAtSlot (fromIntegral n) n


-- | Create a test block using the nth example cardano block, at the given slot number
mkTestBlockAtSlot :: Int -> Int64 -> Verified Valid Block
mkTestBlockAtSlot exampleIdx slotNumber =
    let cardanoBlock = exampleBlocks !! (exampleIdx `mod` length exampleBlocks)
        header = getHeader cardanoBlock
    in  runPureEff
            $ Verifier.runAllValidVerifier
            $ fmap (fromJust . rightToMaybe)
            $ Verifier.verifyBlock
            $ Block
                { hash = mkBlockHash header
                , slotNumber = slotNumber
                , poolId = PoolID "test-pool"
                , blockData = cardanoBlock
                , validationStatus = ""
                , validationReason = ""
                , firstSeen = epoch
                , classification = Nothing
                , classifiedAt = Nothing
                }


exampleBlocks :: [CardanoBlock]
exampleBlocks =
    [ BlockShelley $ snd $ List.head examplesShelley.exampleBlock
    , BlockAllegra $ snd $ List.head examplesAllegra.exampleBlock
    , BlockMary $ snd $ List.head examplesMary.exampleBlock
    , BlockAlonzo $ snd $ List.head examplesAlonzo.exampleBlock
    , BlockBabbage $ snd $ List.head examplesBabbage.exampleBlock
    , BlockConway $ snd $ List.head examplesConway.exampleBlock
    ]


-- | Build a Header whose hash matches mkTestBlock n (same example cardano block)
mkMatchingHeader :: Int64 -> Header
mkMatchingHeader n =
    let cardanoBlock = exampleBlocks !! (fromIntegral n `mod` length exampleBlocks)
        cardanoHeader = getHeader cardanoBlock
    in  Header
            { hash = mkBlockHash cardanoHeader
            , headerData = cardanoHeader
            , slotNumber = fromIntegral n
            , blockNumber = fromIntegral n
            , firstSeenAt = epoch
            }


mkHeader :: Text -> Word64 -> Header
mkHeader hash slot =
    Header
        { hash = BlockHash hash
        , headerData = exampleHdr
        , slotNumber = slot
        , blockNumber = slot
        , firstSeenAt = epoch
        }


exampleHdr :: CardanoHeader
exampleHdr = O.HeaderShelley . snd . List.head $ examplesShelley.exampleHeader


countReceiptsStmt :: Statement () Int
countReceiptsStmt = fmap length $ Rel8.run $ Rel8.select $ Rel8.each HeaderReceiptsSchema.schema


makeValidHeader :: Header -> Verified 'Valid Header
makeValidHeader =
    fromJust
        . rightToMaybe
        . runPureEff
        . Verifier.runAllValidVerifier
        . Verifier.verifyHeader


upsertOnePeer :: (PeerRepo :> es) => Peer -> Eff es Peer
upsertOnePeer peer = do
    upsertedPeers <- upsertPeers (fromList [peer.address]) peer.address epoch
    case toList upsertedPeers of
        [p] -> pure p
        _ -> error "Expected exactly one peer from upsert"


mkTestPeer :: IO Peer
mkTestPeer = do
    uuid <- UUID.nextRandom
    pure
        Peer
            { id = ID uuid
            , address = PeerAddress (read "127.0.0.1") 3001
            , firstDiscovered = epoch
            , lastSeen = epoch
            , lastConnected = Nothing
            , lastFailureTime = Nothing
            , discoveredVia = "test"
            }


epoch :: UTCTime
epoch = UTCTime (read "1970-01-01") 0
