module Integration.Hoard.DB.BlockPersistenceSpec (spec_BlockPersistence) where

import Data.Time (UTCTime (..))
import Effectful (runEff)
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
import Relude.Unsafe (head, read, (!!))
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
import Prelude hiding (head, runReader)

import Rel8 qualified

import Hoard.Data.Block (Block (..))
import Hoard.Data.BlockHash (blockHashFromHeader)
import Hoard.Data.PoolID (PoolID (..))
import Hoard.Effects.BlockRepo (blockExists, insertBlocks, runBlockRepo)
import Hoard.Effects.Clock (runClock)
import Hoard.Effects.DBRead (runDBRead, runQuery)
import Hoard.Effects.DBWrite (runDBWrite)
import Hoard.Effects.Monitoring.Metrics (runMetricsNoOp)
import Hoard.Effects.Monitoring.Tracing (runTracingNoOp)
import Hoard.TestHelpers.Database (TestConfig (..), withCleanTestDatabase)
import Hoard.Types.Cardano (CardanoBlock)

import Hoard.DB.Schemas.Blocks qualified as BlocksSchema
import Hoard.Effects.Log qualified as Log


spec_BlockPersistence :: Spec
spec_BlockPersistence = do
    let runWrite config action =
            runEff
                . Log.runLogNoOp
                . runErrorNoCallStack @Text
                . runReader config.pools
                . runMetricsNoOp
                . runTracingNoOp
                . runClock
                . runDBRead
                . runClock
                . runDBWrite
                . runConcurrent
                . runBlockRepo
                $ action

    let runRead config action =
            runEff
                . runErrorNoCallStack @Text
                . runReader config.pools
                . runMetricsNoOp
                . runTracingNoOp
                . runClock
                . runDBRead
                $ action

    withCleanTestDatabase $ describe "Block persistence (database)" $ do
        it "inserts blocks successfully" $ \config -> do
            let block = mkTestBlock 1

            -- Insert block
            result <- runWrite config $ insertBlocks [block]

            result `shouldSatisfy` isRight

            -- Verify block was persisted
            blockCount <- runRead config $ runQuery "count-blocks" countBlocksStmt
            case blockCount of
                Right count -> count `shouldBe` 1
                Left err -> expectationFailure $ "Block count query failed: " <> show err

        it "blockExists returns True for inserted block" $ \config -> do
            let block = mkTestBlock 2

            -- Insert block and check if it exists
            result <- runWrite config $ do
                insertBlocks [block]
                blockExists block.hash

            case result of
                Right exists -> exists `shouldBe` True
                Left err -> expectationFailure $ "blockExists query failed: " <> show err

        it "blockExists returns False for non-existent block" $ \config -> do
            let existingBlock = mkTestBlock 3
            let nonExistentBlock = mkTestBlock 4

            -- Insert only existingBlock and check if nonExistentBlock exists (it shouldn't)
            result <- runWrite config $ do
                insertBlocks [existingBlock]
                blockExists nonExistentBlock.hash

            case result of
                Right exists -> exists `shouldBe` False
                Left err -> expectationFailure $ "blockExists query failed: " <> show err

        it "blockExists handles multiple concurrent queries correctly" $ \config -> do
            let block1 = mkTestBlock 5
            let block2 = mkTestBlock 6
            let block3 = mkTestBlock 7

            -- Insert blocks and check existence of all three
            results <- runWrite config $ do
                insertBlocks [block1, block2]
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

        it "handles duplicate block inserts correctly (DoNothing on conflict)" $ \config -> do
            let block = mkTestBlock 8

            -- Insert block twice (second insert should be ignored)
            _ <- runWrite config $ do
                insertBlocks [block]
                insertBlocks [block]

            -- Should still have only 1 block
            blockCount <- runRead config $ runQuery "count-blocks-after-dup" countBlocksStmt
            case blockCount of
                Right count -> count `shouldBe` 1
                Left err -> expectationFailure $ "Block count query failed: " <> show err

            -- blockExists should still return True
            result <- runWrite config $ blockExists block.hash
            case result of
                Right exists -> exists `shouldBe` True
                Left err -> expectationFailure $ "blockExists query failed: " <> show err

        it "inserts multiple blocks in batch" $ \config -> do
            let blocks =
                    [ mkTestBlock 9
                    , mkTestBlock 10
                    , mkTestBlock 11
                    , mkTestBlock 12
                    ]

            -- Insert all blocks at once
            result <- runWrite config $ insertBlocks blocks

            result `shouldSatisfy` isRight

            -- Verify all blocks were persisted
            blockCount <- runRead config $ runQuery "count-batch-blocks" countBlocksStmt
            case blockCount of
                Right count -> count `shouldBe` 4
                Left err -> expectationFailure $ "Block count query failed: " <> show err

            -- Verify blockExists works for all inserted blocks
            existsResults <- runWrite config $ do
                traverse blockExists (map (.hash) blocks)

            case existsResults of
                Right results -> all (== True) results `shouldBe` True
                Left err -> expectationFailure $ "blockExists queries failed: " <> show err


-- Helper functions

-- Get test blocks from different eras to ensure unique hashes
-- We use the first block from each era
exampleBlocks :: [CardanoBlock]
exampleBlocks =
    [ BlockShelley $ snd $ Relude.Unsafe.head examplesShelley.exampleBlock
    , BlockAllegra $ snd $ Relude.Unsafe.head examplesAllegra.exampleBlock
    , BlockMary $ snd $ Relude.Unsafe.head examplesMary.exampleBlock
    , BlockAlonzo $ snd $ Relude.Unsafe.head examplesAlonzo.exampleBlock
    , BlockBabbage $ snd $ Relude.Unsafe.head examplesBabbage.exampleBlock
    , BlockConway $ snd $ Relude.Unsafe.head examplesConway.exampleBlock
    ]


-- Create a test block using the nth example (cycles if n exceeds available examples)
mkTestBlock :: Int64 -> Block
mkTestBlock n =
    let cardanoBlock = exampleBlocks !! fromIntegral (n `mod` fromIntegral (length exampleBlocks))
        header = getHeader cardanoBlock
    in  Block
            { hash = blockHashFromHeader header
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
