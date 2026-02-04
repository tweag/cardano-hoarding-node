module Hoard.Effects.BlockRepo
    ( BlockRepo
    , insertBlocks
    , getBlock
    , blockExists
    , classifyBlock
    , getUnclassifiedBlocksBeforeSlot
    , getViolations
    , runBlockRepo
    , runBlockRepoState
    ) where

import Data.Set qualified as Set
import Data.Time (UTCTime)
import Effectful (Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpretWith, interpret_)
import Effectful.TH (makeEffect)
import Hasql.Transaction (Transaction)
import Hasql.Transaction qualified as TX
import Rel8 (isNull, lit, where_, (&&.), (<.), (<=.), (==.), (>=.))
import Rel8 qualified
import Prelude hiding (Reader, State, ask, atomically, gets, modify, newTVarIO, runReader)

import Effectful.State.Static.Shared (State, gets, modify)
import Hasql.Statement (Statement)
import Hoard.DB.Schemas.Blocks (rowFromBlock)
import Hoard.DB.Schemas.Blocks qualified as Blocks
import Hoard.DB.Schemas.HeaderReceipts qualified as HeaderReceipts
import Hoard.Data.Block (Block (..))
import Hoard.Data.BlockHash (BlockHash, blockHashFromHeader)
import Hoard.Data.BlockViolation (BlockViolation, blockToViolation)
import Hoard.Effects.DBRead (DBRead, runQuery)
import Hoard.Effects.DBWrite (DBWrite, runTransaction)
import Hoard.Effects.Monitoring.Tracing (Tracing, addAttribute, addEvent, withSpan)
import Hoard.OrphanDetection.Data (BlockClassification)
import Hoard.Types.Cardano (CardanoHeader)


data BlockRepo :: Effect where
    InsertBlocks :: [Block] -> BlockRepo m ()
    GetBlock :: CardanoHeader -> BlockRepo m (Maybe Block)
    BlockExists :: BlockHash -> BlockRepo m Bool
    ClassifyBlock :: BlockHash -> BlockClassification -> UTCTime -> BlockRepo m ()
    GetUnclassifiedBlocksBeforeSlot :: Int64 -> Int -> Set BlockHash -> BlockRepo m [Block]
    GetViolations :: Maybe BlockClassification -> Maybe Int64 -> Maybe Int64 -> BlockRepo m [BlockViolation]


makeEffect ''BlockRepo


runBlockRepo :: (DBRead :> es, DBWrite :> es, Tracing :> es) => Eff (BlockRepo : es) a -> Eff es a
runBlockRepo action = do
    interpretWith action $ \_env -> \case
        InsertBlocks blocks -> withSpan "block_repo.insert_blocks" $ do
            addAttribute "blocks.count" (show $ length blocks)
            runTransaction "insert-blocks" $
                insertBlocksTrans blocks
        GetBlock header -> withSpan "block_repo.get_block" $ do
            let hash = blockHashFromHeader header
            addAttribute "block.hash" (show hash)
            runQuery "get-block" $
                getBlockQuery header
        BlockExists blockHash -> withSpan "block_repo.block_exists" $ do
            addAttribute "block.hash" (show blockHash)
            runQuery "block-exists" $ blockExistsQuery blockHash
        ClassifyBlock blockHash classification timestamp -> withSpan "block_repo.classify_block" $ do
            addAttribute "block.hash" (show blockHash)
            addAttribute "block.classification" (show classification)
            runTransaction "classify-block" $
                classifyBlockTrans blockHash classification timestamp
        GetUnclassifiedBlocksBeforeSlot slot limit excludeHashes -> withSpan "block_repo.get_unclassified_blocks" $ do
            addAttribute "slot" (show slot)
            addAttribute "limit" (show limit)
            addAttribute "exclude.count" (show $ Set.size excludeHashes)
            runQuery "get-unclassified-blocks" $
                getUnclassifiedBlocksQuery slot limit excludeHashes
        GetViolations mbClassification mbMinSlot mbMaxSlot -> withSpan "block_repo.get_violations" $ do
            addAttribute "filter.classification" (show mbClassification)
            addAttribute "filter.min_slot" (show mbMinSlot)
            addAttribute "filter.max_slot" (show mbMaxSlot)
            results <- runQuery "get-violations" $ getViolationsQuery mbClassification mbMinSlot mbMaxSlot

            -- Log parsing errors before discarding them
            let (parseErrors, violations) = partitionEithers results
            addAttribute "parse.errors.count" (show $ length parseErrors)
            forM_ parseErrors $ \err -> addEvent "parse_error" [("error", err)]
            pure violations


insertBlocksTrans :: [Block] -> Transaction ()
insertBlocksTrans blocks =
    TX.statement ()
        . Rel8.run_
        $ Rel8.insert
            Rel8.Insert
                { into = Blocks.schema
                , rows = Rel8.values $ rowFromBlock <$> blocks
                , onConflict = Rel8.DoNothing
                , returning = Rel8.NoReturning
                }


getBlockQuery :: CardanoHeader -> Statement () (Maybe Block)
getBlockQuery header =
    fmap (extractSingleBlock . rights . fmap Blocks.blockFromRow)
        . Rel8.run
        . Rel8.select
        $ do
            block <- Rel8.each Blocks.schema
            where_ $
                block.hash ==. (lit $ blockHashFromHeader header)
            pure block
  where
    -- The unique constraint over the `hash` column ensures we get either 1 or
    -- 0 rows from the above query.
    extractSingleBlock = \case
        [x] -> Just x
        _ -> Nothing


blockExistsQuery :: BlockHash -> Statement () Bool
blockExistsQuery blockHash =
    fmap (not . null)
        . Rel8.run
        . Rel8.select
        $ Rel8.limit 1
        $ do
            block <- Rel8.each Blocks.schema
            where_ $ block.hash ==. lit blockHash
            pure block.hash


classifyBlockTrans :: BlockHash -> BlockClassification -> UTCTime -> Transaction ()
classifyBlockTrans blockHash classification timestamp =
    TX.statement ()
        . Rel8.run_
        $ Rel8.update
            Rel8.Update
                { target = Blocks.schema
                , from = pure ()
                , updateWhere = \_ row -> row.hash ==. lit blockHash
                , set = \_ row ->
                    row
                        { Blocks.classification = lit (Just classification)
                        , Blocks.classifiedAt = lit (Just timestamp)
                        }
                , returning = Rel8.NoReturning
                }


getUnclassifiedBlocksQuery :: Int64 -> Int -> Set BlockHash -> Statement () [Block]
getUnclassifiedBlocksQuery slot limit excludeHashes =
    fmap (rights . fmap Blocks.blockFromRow)
        . Rel8.run
        . Rel8.select
        $ Rel8.limit (fromIntegral limit)
        $ do
            block <- Rel8.each Blocks.schema
            where_ $
                Rel8.isNull block.classification
                    &&. block.slotNumber <. lit slot
                    &&. excludeHashesCondition block.hash
            pure block
  where
    -- Build exclusion condition: hash NOT IN (excludeHashes)
    excludeHashesCondition blockHash =
        case toList excludeHashes of
            [] -> lit True -- No exclusions
            hashes -> Rel8.not_ (blockHash `Rel8.in_` fmap lit hashes)


-- | Helper to create an optional filter predicate
-- If the Maybe value is Nothing, returns True (no filter)
-- Otherwise applies the predicate function
optionalFilter :: Maybe a -> (a -> Rel8.Expr Bool) -> Rel8.Expr Bool
optionalFilter Nothing _ = lit True
optionalFilter (Just val) f = f val


-- Query blocks with given filters and their receipts
-- Returns Either values so parsing errors can be logged before being discarded
getViolationsQuery :: Maybe BlockClassification -> Maybe Int64 -> Maybe Int64 -> Statement () [Either Text BlockViolation]
getViolationsQuery mbClassification mbMinSlot mbMaxSlot =
    (fmap . fmap $ parseRow) . Rel8.run . Rel8.select $ do
        block <- Rel8.each Blocks.schema

        -- Apply block filters
        where_ $
            optionalFilter mbClassification (\cls -> block.classification ==. lit (Just cls))
                &&. optionalFilter mbMinSlot (\minSlot -> block.slotNumber >=. lit minSlot)
                &&. optionalFilter mbMaxSlot (\maxSlot -> block.slotNumber <=. lit maxSlot)

        -- Get all receipts for this block
        receipts <- Rel8.many $ do
            receipt <- Rel8.each HeaderReceipts.schema
            where_ $ receipt.hash ==. block.hash
            pure receipt

        pure (block, receipts)
  where
    parseRow :: (Blocks.Row Rel8.Result, [HeaderReceipts.Row Rel8.Result]) -> Either Text BlockViolation
    parseRow (blockRow, receiptRows) = do
        block <- Blocks.blockFromRow blockRow
        let receipts = fmap HeaderReceipts.headerReceiptFromRow receiptRows
        pure $ blockToViolation block receipts


runBlockRepoState :: (State [Block] :> es) => Eff (BlockRepo : es) a -> Eff es a
runBlockRepoState = interpret_ \case
    InsertBlocks blocks -> modify $ (blocks <>)
    GetBlock header -> gets $ find ((blockHashFromHeader header ==) . (.hash))
    BlockExists blockHash -> gets $ isJust . find ((blockHash ==) . (.hash))
    ClassifyBlock blockHash classification timestamp ->
        modify $ fmap $ \block ->
            if block.hash == blockHash
                then block {classification = Just classification, classifiedAt = Just timestamp}
                else block
    GetUnclassifiedBlocksBeforeSlot slot limit excludeHashes ->
        gets $
            take limit
                . filter
                    ( \block ->
                        isNothing block.classification
                            && block.slotNumber < slot
                            && Set.notMember block.hash excludeHashes
                    )
    GetViolations mbClassification mbMinSlot mbMaxSlot -> do
        blocks <- gets $ filter $ \block ->
            maybe True (\cls -> block.classification == Just cls) mbClassification
                && maybe True (\minSlot -> block.slotNumber >= minSlot) mbMinSlot
                && maybe True (\maxSlot -> block.slotNumber <= maxSlot) mbMaxSlot
        -- In test state, return empty receipts list and convert to DTOs
        pure $ fmap (\block -> blockToViolation block []) blocks
