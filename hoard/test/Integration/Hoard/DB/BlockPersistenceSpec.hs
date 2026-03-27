module Integration.Hoard.DB.BlockPersistenceSpec (spec_BlockPersistence) where

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
import Data.Set qualified as Set
import Rel8 qualified

import Atelier.Effects.Clock (runClock)
import Atelier.Effects.Monitoring.Metrics (runMetricsNoOp)
import Atelier.Effects.Monitoring.Tracing (runTracingNoOp)
import Hoard.Data.Block (Block (..))
import Hoard.Data.BlockHash (mkBlockHash)
import Hoard.Data.PoolID (PoolID (..))
import Hoard.Effects.BlockRepo (blockExists, getBlock, getUnclassifiedBlocksBeforeSlot, insertBlocks, runBlockRepo)
import Hoard.Effects.DB (runDBRead, runDBWrite, runQuery, runRel8Read, runRel8Write)
import Hoard.Effects.Verifier (Validity (Valid), Verified)
import Hoard.TestHelpers.Database (withCleanTestDatabase)
import Hoard.Types.Cardano (CardanoBlock)
import Prelude hiding (head)

import Atelier.Effects.Log qualified as Log
import Hoard.DB.Schemas.Blocks qualified as BlocksSchema
import Hoard.Effects.Verifier qualified as Verified
import Hoard.Effects.Verifier qualified as Verifier


spec_BlockPersistence :: Spec
spec_BlockPersistence = do
    let runWrite pools action =
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
                . runConcurrent
                . runRel8Read
                . runRel8Write
                . runBlockRepo
                $ action

    let runRead pools action =
            runEff
                . runErrorNoCallStack @Text
                . runReader pools
                . runMetricsNoOp
                . runTracingNoOp
                . runClock
                . runDBRead
                $ action

    withCleanTestDatabase $ describe "Block persistence (database)" $ do
        it "inserts blocks successfully" $ \pools -> do
            let block = mkTestBlock 1

            -- Insert block
            result <- runWrite pools $ insertBlocks [block]

            result `shouldSatisfy` isRight

            -- Verify block was persisted
            blockCount <- runRead pools $ runQuery "count-blocks" countBlocksStmt
            case blockCount of
                Right count -> count `shouldBe` 1
                Left err -> expectationFailure $ "Block count query failed: " <> show err

        it "blockExists returns True for inserted block" $ \pools -> do
            let verifiedBlock = mkTestBlock 2
                block = Verifier.getVerified verifiedBlock

            -- Insert block and check if it exists
            result <- runWrite pools $ do
                insertBlocks [verifiedBlock]
                blockExists block.hash

            case result of
                Right exists -> exists `shouldBe` True
                Left err -> expectationFailure $ "blockExists query failed: " <> show err

        it "blockExists returns False for non-existent block" $ \pools -> do
            let existingBlock = mkTestBlock 3
            let nonExistentBlock = Verifier.getVerified $ mkTestBlock 4

            -- Insert only existingBlock and check if nonExistentBlock exists (it shouldn't)
            result <- runWrite pools $ do
                insertBlocks [existingBlock]
                blockExists nonExistentBlock.hash

            case result of
                Right exists -> exists `shouldBe` False
                Left err -> expectationFailure $ "blockExists query failed: " <> show err

        it "blockExists handles multiple concurrent queries correctly" $ \pools -> do
            let verifiedBlock1 = mkTestBlock 5
                block1 = Verifier.getVerified verifiedBlock1
            let verifiedBlock2 = mkTestBlock 6
                block2 = Verifier.getVerified verifiedBlock2
            let verifiedBlock3 = mkTestBlock 7
                block3 = Verifier.getVerified verifiedBlock3

            -- Insert blocks and check existence of all three
            results <- runWrite pools $ do
                insertBlocks [verifiedBlock1, verifiedBlock2]
                exists1 <- blockExists block1.hash
                exists2 <- blockExists block2.hash
                exists3 <- blockExists block3.hash
                pure (exists1, exists2, exists3)

            case results of
                Right (exists1, exists2, exists3) -> do
                    exists1 `shouldBe` True
                    exists2 `shouldBe` True
                    exists3 `shouldBe` False
                Left err -> expectationFailure $ "blockExists queries failed: " <> show err

        it "handles duplicate block inserts correctly (DoNothing on conflict)" $ \pools -> do
            let verifiedBlock = mkTestBlock 8
                block = Verified.getVerified verifiedBlock

            -- Insert block twice (second insert should be ignored)
            _ <- runWrite pools $ do
                insertBlocks [verifiedBlock]
                insertBlocks [verifiedBlock]

            -- Should still have only 1 block
            blockCount <- runRead pools $ runQuery "count-blocks-after-dup" countBlocksStmt
            case blockCount of
                Right count -> count `shouldBe` 1
                Left err -> expectationFailure $ "Block count query failed: " <> show err

            -- blockExists should still return True
            result <- runWrite pools $ blockExists block.hash
            case result of
                Right exists -> exists `shouldBe` True
                Left err -> expectationFailure $ "blockExists query failed: " <> show err

        it "inserts multiple blocks in batch" $ \pools -> do
            let blocks =
                    [ mkTestBlock 9
                    , mkTestBlock 10
                    , mkTestBlock 11
                    , mkTestBlock 12
                    ]

            -- Insert all blocks at once
            result <- runWrite pools $ insertBlocks blocks

            result `shouldSatisfy` isRight

            -- Verify all blocks were persisted
            blockCount <- runRead pools $ runQuery "count-batch-blocks" countBlocksStmt
            case blockCount of
                Right count -> count `shouldBe` 4
                Left err -> expectationFailure $ "Block count query failed: " <> show err

            -- Verify blockExists works for all inserted blocks
            existsResults <- runWrite pools $ do
                traverse blockExists (map ((.hash) . Verified.getVerified) blocks)

            case existsResults of
                Right results -> all (== True) results `shouldBe` True
                Left err -> expectationFailure $ "blockExists queries failed: " <> show err

        it "getBlock returns the block when it exists" $ \pools -> do
            let verifiedBlock = mkTestBlock 13
                block = Verifier.getVerified verifiedBlock
                header = getHeader block.blockData

            result <- runWrite pools $ do
                insertBlocks [verifiedBlock]
                getBlock header

            case result of
                Right (Just found) -> found.hash `shouldBe` block.hash
                Right Nothing -> expectationFailure "Expected block to be found but got Nothing"
                Left err -> expectationFailure $ "getBlock failed: " <> show err

        it "getBlock returns Nothing for a non-existent block" $ \pools -> do
            let verifiedBlock = mkTestBlock 14
                block = Verifier.getVerified verifiedBlock
                header = getHeader block.blockData

            result <- runWrite pools $ getBlock header

            case result of
                Right Nothing -> pure ()
                Right (Just _) -> expectationFailure "Expected Nothing but found a block"
                Left err -> expectationFailure $ "getBlock failed: " <> show err

        it "getUnclassifiedBlocksBeforeSlot returns only blocks with slotNumber < slot" $ \pools -> do
            let block5 = mkTestBlock 5
                block10 = mkTestBlock 10
                block15 = mkTestBlock 15

            result <- runWrite pools $ do
                insertBlocks [block5, block10, block15]
                getUnclassifiedBlocksBeforeSlot 12 100 mempty

            case result of
                Right blocks -> map (.slotNumber) blocks `shouldMatchList` [5, 10]
                Left err -> expectationFailure $ "getUnclassifiedBlocksBeforeSlot failed: " <> show err

        it "getUnclassifiedBlocksBeforeSlot respects the limit" $ \pools -> do
            let blocks = map mkTestBlock [1, 2, 3, 4, 5]

            result <- runWrite pools $ do
                insertBlocks blocks
                getUnclassifiedBlocksBeforeSlot 100 2 mempty

            case result of
                Right found -> length found `shouldBe` 2
                Left err -> expectationFailure $ "getUnclassifiedBlocksBeforeSlot failed: " <> show err

        it "getUnclassifiedBlocksBeforeSlot excludes specified hashes" $ \pools -> do
            let block1 = mkTestBlock 1
                block2 = mkTestBlock 2
                block3 = mkTestBlock 3
                hash2 = (.hash) $ Verifier.getVerified block2

            result <- runWrite pools $ do
                insertBlocks [block1, block2, block3]
                getUnclassifiedBlocksBeforeSlot 100 100 (Set.fromList [hash2])

            case result of
                Right found -> map (.hash) found `shouldNotContain` [hash2]
                Left err -> expectationFailure $ "getUnclassifiedBlocksBeforeSlot failed: " <> show err


-- Helper functions

-- Get test blocks from different eras to ensure unique hashes
-- We use the first block from each era
exampleBlocks :: [CardanoBlock]
exampleBlocks =
    [ BlockShelley $ snd $ List.head examplesShelley.exampleBlock
    , BlockAllegra $ snd $ List.head examplesAllegra.exampleBlock
    , BlockMary $ snd $ List.head examplesMary.exampleBlock
    , BlockAlonzo $ snd $ List.head examplesAlonzo.exampleBlock
    , BlockBabbage $ snd $ List.head examplesBabbage.exampleBlock
    , BlockConway $ snd $ List.head examplesConway.exampleBlock
    ]


-- Create a test block using the nth example (cycles if n exceeds available examples)
mkTestBlock :: Int64 -> Verified Valid Block
mkTestBlock n =
    let cardanoBlock = exampleBlocks !! fromIntegral (n `mod` fromIntegral (length exampleBlocks))
        header = getHeader cardanoBlock
    in  runPureEff
            $ Verifier.runAllValidVerifier
            $ fmap (fromJust . rightToMaybe)
            $ Verifier.verifyBlock
            $ Block
                { hash = mkBlockHash header
                , slotNumber = n
                , poolId = PoolID "test-pool"
                , blockData = cardanoBlock
                , validationStatus = ""
                , validationReason = ""
                , firstSeen = epoch
                , classification = Nothing
                , classifiedAt = Nothing
                }


epoch :: UTCTime
epoch = UTCTime (read "1970-01-01") 0


countBlocksStmt :: Statement () Int
countBlocksStmt = fmap length $ Rel8.run $ Rel8.select $ Rel8.each BlocksSchema.schema
