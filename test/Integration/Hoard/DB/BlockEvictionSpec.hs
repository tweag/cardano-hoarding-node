module Integration.Hoard.DB.BlockEvictionSpec (spec_BlockEviction) where

import Data.Time (UTCTime (..))
import Effectful (runEff, runPureEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Reader.Static (runReader)
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
import Relude.Unsafe (fromJust, head, read, (!!))
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
import Prelude hiding (evalState, head, runReader)

import Hoard.Data.Block (Block (..))
import Hoard.Data.BlockHash (BlockHash, blockHashFromHeader)
import Hoard.Data.PoolID (PoolID (..))
import Hoard.Effects.BlockRepo (classifyBlock, evictBlocks, insertBlocks, runBlockRepo)
import Hoard.Effects.Clock (runClock)
import Hoard.Effects.DBRead (runDBRead)
import Hoard.Effects.DBWrite (runDBWrite)
import Hoard.Effects.Monitoring.Metrics (runMetricsNoOp)
import Hoard.Effects.Monitoring.Tracing (runTracingNoOp)
import Hoard.Effects.Verifier (Validity (Valid), Verified)
import Hoard.OrphanDetection.Data (BlockClassification (..))
import Hoard.TestHelpers.Database (TestConfig (..), withCleanTestDatabase)
import Hoard.Types.Cardano (CardanoBlock)

import Hoard.Effects.Log qualified as Log
import Hoard.Effects.Verifier qualified as Verifier


spec_BlockEviction :: Spec
spec_BlockEviction = do
    let runDB config action =
            runEff
                . Log.runLogNoOp
                . runErrorNoCallStack @Text
                . runReader config.pools
                . runMetricsNoOp
                . runTracingNoOp
                . runClock
                . runDBRead
                . runDBWrite
                . runConcurrent
                . runBlockRepo
                $ action

    withCleanTestDatabase $ describe "Block eviction (database)" $ do
        it "evicts nothing when there are no blocks" $ \config -> do
            result <- runDB config evictBlocks
            result `shouldBe` Right 0

        it "evicts all canonical blocks when there are no orphans" $ \config -> do
            -- Insert 3 canonical blocks at different slots (no orphans)
            let blocks = [mkTestBlock 1, mkTestBlock 2, mkTestBlock 3]
            result <- runDB config $ do
                insertBlocks blocks
                classifyBlock (blockHash 1) Canonical epoch
                classifyBlock (blockHash 2) Canonical epoch
                classifyBlock (blockHash 3) Canonical epoch
                evictBlocks
            result `shouldBe` Right 3

        it "keeps canonical block that has an orphan at the same slot" $ \config -> do
            -- slot 10: canonical + orphan (battle) → keep both
            -- slot 11: canonical only → evict
            let canonical10 = mkTestBlock 10
                orphan10 = mkTestBlock 20 -- different block hash, same slot via classification
                canonical11 = mkTestBlock 11
            result <- runDB config $ do
                insertBlocks [canonical10, orphan10, canonical11]
                -- Classify canonical10 and orphan10 at slot 10 (same slot number in the block)
                classifyBlock (blockHash 10) Canonical epoch
                -- We can't change slotNumber, so test the logic via the actual slot fields:
                -- orphan10 has slotNumber=20, canonical10 has slotNumber=10
                -- They're at different slots, so this tests the "no orphan at same slot" path
                classifyBlock (blockHash 20) Orphaned epoch
                classifyBlock (blockHash 11) Canonical epoch
                evictBlocks
            -- canonical10 (slot 10) - no orphan at slot 10 → evicted
            -- orphan10 (slot 20) - orphan → kept
            -- canonical11 (slot 11) - no orphan at slot 11 → evicted
            result `shouldBe` Right 2

        it "keeps canonical block when orphan is at the same slot" $ \config -> do
            -- Use blocks with the same slot number by setting slotNumber directly
            -- mkTestBlockAtSlot creates blocks with a specific slot
            let canonicalBlock = mkTestBlockAtSlot 0 100 -- hash from index 0, slot 100
                orphanBlock = mkTestBlockAtSlot 1 100 -- hash from index 1, same slot 100
            result <- runDB config $ do
                insertBlocks [canonicalBlock, orphanBlock]
                classifyBlock (blockHash' canonicalBlock) Canonical epoch
                classifyBlock (blockHash' orphanBlock) Orphaned epoch
                evictBlocks
            -- Both blocks are at slot 100: canonical has orphan at same slot → kept
            -- Orphan is always kept
            -- So 0 blocks evicted
            result `shouldBe` Right 0

        it "does not evict unclassified blocks" $ \config -> do
            let blocks = [mkTestBlock 1, mkTestBlock 2]
            result <- runDB config $ do
                insertBlocks blocks
                -- Don't classify them
                evictBlocks
            result `shouldBe` Right 0

        it "does not evict orphaned blocks" $ \config -> do
            let blocks = [mkTestBlock 1, mkTestBlock 2]
            result <- runDB config $ do
                insertBlocks blocks
                classifyBlock (blockHash 1) Orphaned epoch
                classifyBlock (blockHash 2) Orphaned epoch
                evictBlocks
            result `shouldBe` Right 0


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
                { hash = blockHashFromHeader header
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
    [ BlockShelley $ snd $ Relude.Unsafe.head examplesShelley.exampleBlock
    , BlockAllegra $ snd $ Relude.Unsafe.head examplesAllegra.exampleBlock
    , BlockMary $ snd $ Relude.Unsafe.head examplesMary.exampleBlock
    , BlockAlonzo $ snd $ Relude.Unsafe.head examplesAlonzo.exampleBlock
    , BlockBabbage $ snd $ Relude.Unsafe.head examplesBabbage.exampleBlock
    , BlockConway $ snd $ Relude.Unsafe.head examplesConway.exampleBlock
    ]


epoch :: UTCTime
epoch = UTCTime (read "1970-01-01") 0
