module Hoard.Effects.BlockRepo
    ( BlockRepo
    , insertBlocks
    , getBlock
    , classifyBlock
    , getUnclassifiedBlocksBeforeSlot
    , getViolations
    , runBlockRepo
    , runBlockRepoState
    ) where

import Data.Set qualified as Set
import Data.Time (UTCTime)
import Effectful (Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.TH (makeEffect)
import Hasql.Transaction (Transaction)
import Hasql.Transaction qualified as TX
import Rel8 (isNull, lit, where_, (&&.), (<.), (<=.), (==.), (>=.))
import Rel8 qualified
import Prelude hiding (State, gets, modify)

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
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.OrphanDetection.Data (BlockClassification)
import Hoard.Types.Cardano (CardanoHeader)


data BlockRepo :: Effect where
    InsertBlocks :: [Block] -> BlockRepo m ()
    GetBlock :: CardanoHeader -> BlockRepo m (Maybe Block)
    ClassifyBlock :: BlockHash -> BlockClassification -> UTCTime -> BlockRepo m ()
    GetUnclassifiedBlocksBeforeSlot :: Int64 -> Int -> Set BlockHash -> BlockRepo m [Block]
    GetViolations :: Maybe BlockClassification -> Maybe Int64 -> Maybe Int64 -> BlockRepo m [BlockViolation]


makeEffect ''BlockRepo


runBlockRepo :: (DBRead :> es, DBWrite :> es, Log :> es) => Eff (BlockRepo : es) a -> Eff es a
runBlockRepo = interpret_ $ \case
    InsertBlocks blocks ->
        runTransaction "insert-blocks" $
            insertBlocksTrans blocks
    GetBlock header ->
        runQuery "get-block" $
            getBlockQuery header
    ClassifyBlock blockHash classification timestamp ->
        runTransaction "classify-block" $
            classifyBlockTrans blockHash classification timestamp
    GetUnclassifiedBlocksBeforeSlot slot limit excludeHashes ->
        runQuery "get-unclassified-blocks" $
            getUnclassifiedBlocksQuery slot limit excludeHashes
    GetViolations mbClassification mbMinSlot mbMaxSlot -> do
        results <- runQuery "get-violations" $ getViolationsQuery mbClassification mbMinSlot mbMaxSlot
        -- Log parsing errors before discarding them
        let (errs, violations) = partitionEithers results
        forM_ errs $ \err ->
            Log.warn $ "Failed to parse block violation from database: " <> err
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
