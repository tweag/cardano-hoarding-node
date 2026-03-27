module Hoard.Effects.BlockRepo
    ( BlockRepo
    , insertBlocks
    , getBlock
    , blockExists
    , classifyBlock
    , getUnclassifiedBlocksBeforeSlot
    , getSlotDisputesInRange
    , evictBlocks
    , tagBlock
    , getBlocks
    , runBlockRepo
    , runBlockRepoState
    ) where

import Data.List (partition)
import Data.Time (UTCTime)
import Effectful (Effect)
import Effectful.Concurrent (Concurrent)
import Effectful.Dispatch.Dynamic (interpret_, reinterpretWith)
import Effectful.State.Static.Shared (State, get, gets, modify, put)
import Effectful.TH (makeEffect)
import Rel8 (in_, isNull, lit, where_, (&&.), (<.), (<=.), (==.), (>=.))

import Data.Set qualified as Set
import Hasql.Transaction qualified as TX
import Rel8 qualified

import Atelier.Effects.Monitoring.Tracing (Tracing)
import Hoard.API.Data.BlockViolation (SlotDispute, blockToViolation, groupIntoDisputes)
import Hoard.DB.Schemas.Blocks (rowFromBlock)
import Hoard.Data.Block (Block (..))
import Hoard.Data.BlockHash (BlockHash (..), mkBlockHash)
import Hoard.Data.BlockTag (BlockTag (..))
import Hoard.Effects.DB (Rel8Read, Rel8Write, delete_, insert_, select, selectTx, transact, update_)
import Hoard.Effects.Verifier (Validity (Valid), Verified, getVerified)
import Hoard.OrphanDetection.Data (BlockClassification (..))
import Hoard.Types.Cardano (CardanoHeader)
import Hoard.Types.SlotRange (SlotRange (..))

import Atelier.Effects.Cache.Singleflight qualified as Singleflight
import Hoard.DB.Schemas.BlockTags qualified as BlockTags
import Hoard.DB.Schemas.Blocks qualified as Blocks
import Hoard.DB.Schemas.HeaderReceipts qualified as HeaderReceipts
import Hoard.DB.Schemas.HeaderTags qualified as HeaderTags


data BlockRepo :: Effect where
    InsertBlocks :: [Verified 'Valid Block] -> BlockRepo m ()
    GetBlock :: CardanoHeader -> BlockRepo m (Maybe Block)
    BlockExists :: BlockHash -> BlockRepo m Bool
    ClassifyBlock :: BlockHash -> BlockClassification -> UTCTime -> BlockRepo m ()
    GetSlotDisputesInRange :: Maybe Int64 -> Maybe Int64 -> BlockRepo m [SlotDispute]
    GetUnclassifiedBlocksBeforeSlot :: Int64 -> Int -> Set BlockHash -> BlockRepo m [Block]
    EvictBlocks :: BlockRepo m Int
    TagBlock :: BlockHash -> [BlockTag] -> BlockRepo m ()
    GetBlocks :: SlotRange -> [BlockTag] -> BlockRepo m [Block]


makeEffect ''BlockRepo


runBlockRepo :: (Concurrent :> es, Rel8Read :> es, Rel8Write :> es, Tracing :> es) => Eff (BlockRepo : es) a -> Eff es a
runBlockRepo action =
    reinterpretWith (Singleflight.runSingleflight @BlockHash @Bool) action $ \_env -> \case
        InsertBlocks blocks -> do
            transact "insert_blocks"
                $ insert_
                    Rel8.Insert
                        { into = Blocks.schema
                        , rows = Rel8.values $ rowFromBlock <$> getVerified <$> blocks
                        , onConflict = Rel8.DoNothing
                        , returning = Rel8.NoReturning
                        }
            -- Pre-populate cache: we know these blocks exist after insertion
            Singleflight.updateCache (map ((,True) . (.hash) . getVerified) blocks)
        GetBlock header -> do
            rows <- select "get_block" $ do
                block <- Rel8.each Blocks.schema
                where_ $ block.hash ==. lit (mkBlockHash header)
                pure block
            pure $ extractSingleBlock . rights $ fmap Blocks.blockFromRow rows
          where
            extractSingleBlock = \case
                [x] -> Just x
                _ -> Nothing
        BlockExists blockHash ->
            Singleflight.withCache blockHash $ do
                rows <- select "block_exists" $ Rel8.limit 1 $ do
                    block <- Rel8.each Blocks.schema
                    where_ $ block.hash ==. lit blockHash
                    pure block.hash
                pure $ not (null rows)
        ClassifyBlock blockHash classification timestamp ->
            transact "classify_block" $ do
                update_
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
                when (classification == Orphaned) $ do
                    hashesAtSlot <- selectTx $ do
                        targetBlock <- Rel8.each Blocks.schema
                        where_ $ targetBlock.hash ==. lit blockHash
                        block <- Rel8.each Blocks.schema
                        where_ $ block.slotNumber ==. targetBlock.slotNumber
                        pure block.hash
                    TX.statement () (BlockTags.insertTagsStatement hashesAtSlot [SlotDispute])
        GetSlotDisputesInRange mbMinSlot mbMaxSlot -> do
            rows <- select "get_violations" $ do
                block <- Rel8.each Blocks.schema
                where_
                    $ Rel8.not_ (isNull block.classification)
                        &&. optionalFilter mbMinSlot (\minSlot -> block.slotNumber >=. lit minSlot)
                        &&. optionalFilter mbMaxSlot (\maxSlot -> block.slotNumber <=. lit maxSlot)
                hasOrphanAtSlot <- orphanExistsAtSlot (Rel8.each Blocks.schema) block.slotNumber
                where_ hasOrphanAtSlot
                receipts <- Rel8.many $ do
                    receipt <- Rel8.each HeaderReceipts.schema
                    where_ $ receipt.hash ==. block.hash
                    pure receipt
                pure (block, receipts)
            let (_, disputes) = partitionEithers $ fmap parseRow rows
            pure $ groupIntoDisputes disputes
          where
            parseRow (blockRow, receiptRows) = do
                block <- Blocks.blockFromRow blockRow
                pure $ blockToViolation block (fmap HeaderReceipts.headerReceiptFromRow receiptRows)
        GetUnclassifiedBlocksBeforeSlot slot limit excludeHashes -> do
            rows <- select "get_unclassified_blocks"
                $ Rel8.limit (fromIntegral limit)
                $ do
                    block <- Rel8.each Blocks.schema
                    where_
                        $ Rel8.isNull block.classification
                            &&. block.slotNumber <. lit slot
                            &&. excludeHashesCondition block.hash
                    pure block
            pure $ rights $ fmap Blocks.blockFromRow rows
          where
            excludeHashesCondition blockHash =
                case toList excludeHashes of
                    [] -> lit True
                    hashes -> Rel8.not_ (blockHash `Rel8.in_` fmap lit hashes)
        EvictBlocks -> do
            hashes <- transact "evict_blocks" $ do
                hashes <- selectTx $ do
                    block <- Rel8.each Blocks.schema
                    where_ $ block.classification ==. lit (Just Canonical)
                    hasOrphanAtSameSlot <- orphanExistsAtSlot (Rel8.each Blocks.schema) block.slotNumber
                    where_ $ Rel8.not_ hasOrphanAtSameSlot
                    hasBlockTag <- BlockTags.hashHasTag block.hash
                    where_ $ Rel8.not_ hasBlockTag
                    hasHeaderTag <- HeaderTags.hashHasTag block.hash
                    where_ $ Rel8.not_ hasHeaderTag
                    pure block.hash
                unless (null hashes)
                    $ delete_
                        Rel8.Delete
                            { from = Blocks.schema
                            , using = pure ()
                            , deleteWhere = \_ row -> row.hash `Rel8.in_` fmap lit hashes
                            , returning = Rel8.NoReturning
                            }
                pure hashes
            -- Remove evicted blocks from cache so future lookups hit the DB
            Singleflight.removeFromCache hashes
            pure $ length hashes
        TagBlock hash tags ->
            transact "tag_block"
                $ TX.statement () (BlockTags.insertTagsStatement [hash] tags)
        GetBlocks (SlotRange mFromSlot mToSlot) tags -> do
            rows <- select "get_blocks_in_range" $ do
                block <- Rel8.each Blocks.schema
                tagsMatch <-
                    if length tags > 0 then
                        Rel8.exists
                            $ Rel8.filter (\tag -> block.hash ==. tag.blockHash &&. tag.tag `in_` litTags)
                                =<< Rel8.each BlockTags.schema
                    else
                        pure $ lit True
                where_
                    $ block.slotNumber >=. lit fromSlot
                        &&. block.slotNumber <=. lit toSlot
                        &&. tagsMatch
                pure block
            pure $ mapMaybe (rightToMaybe . Blocks.blockFromRow) rows
          where
            fromSlot = fromMaybe minBound mFromSlot
            toSlot = fromMaybe maxBound mToSlot
            litTags = lit <$> tags


-- | Helper to create an optional filter predicate
-- If the Maybe value is Nothing, returns True (no filter)
-- Otherwise applies the predicate function
optionalFilter :: Maybe a -> (a -> Rel8.Expr Bool) -> Rel8.Expr Bool
optionalFilter Nothing _ = lit True
optionalFilter (Just val) f = f val


orphanExistsAtSlot :: Rel8.Query (Blocks.Row Rel8.Expr) -> Rel8.Expr Int64 -> Rel8.Query (Rel8.Expr Bool)
orphanExistsAtSlot orphans slot = Rel8.exists $ do
    orphan <- orphans
    where_ $ orphan.classification ==. lit (Just Orphaned) &&. orphan.slotNumber ==. slot
    pure orphan


runBlockRepoState
    :: ( State (Set (BlockHash, BlockTag)) :> es
       , State [Block] :> es
       )
    => Eff (BlockRepo : es) a -> Eff es a
runBlockRepoState = interpret_ \case
    InsertBlocks blocks -> modify $ (fmap getVerified blocks <>)
    GetBlock header -> gets $ find ((mkBlockHash header ==) . (.hash))
    BlockExists blockHash -> gets @[Block] $ isJust . find ((blockHash ==) . (.hash))
    ClassifyBlock blockHash classification timestamp -> do
        modify $ fmap $ \block ->
            if block.hash == blockHash then
                block {classification = Just classification, classifiedAt = Just timestamp}
            else
                block
        when (classification == Orphaned) $ do
            blocks <- get @[Block]
            let hashesAtSlot = case find ((blockHash ==) . (.hash)) blocks of
                    Nothing -> [blockHash]
                    Just b -> map (.hash) $ filter (\b' -> b'.slotNumber == b.slotNumber) blocks
            modify @(Set (BlockHash, BlockTag)) $ Set.union $ Set.fromList $ (,SlotDispute) <$> hashesAtSlot
    GetSlotDisputesInRange mbMinSlot mbMaxSlot -> do
        blocks <- gets $ filter $ \block ->
            isJust block.classification
                && maybe True (\minSlot -> block.slotNumber >= minSlot) mbMinSlot
                && maybe True (\maxSlot -> block.slotNumber <= maxSlot) mbMaxSlot
        -- In test state, return empty receipts list and convert to DTOs
        pure $ groupIntoDisputes $ fmap (\block -> blockToViolation block []) blocks
    GetUnclassifiedBlocksBeforeSlot slot limit excludeHashes ->
        gets
            $ take limit
                . filter
                    ( \block ->
                        isNothing block.classification
                            && block.slotNumber < slot
                            && Set.notMember block.hash excludeHashes
                    )
    EvictBlocks -> do
        blocks <- get
        tags <- get @(Set (BlockHash, BlockTag))
        let taggedHashes = Set.map fst tags
            orphanedSlots = map (.slotNumber) $ filter (\b -> b.classification == Just Orphaned) blocks
            isEvictable b =
                b.classification == Just Canonical
                    && b.slotNumber `notElem` orphanedSlots
                    && Set.notMember b.hash taggedHashes
            (evictable, keep) = partition isEvictable blocks
        put keep
        pure (length evictable)
    TagBlock hash tags ->
        modify @(Set (BlockHash, BlockTag)) $ Set.union $ Set.fromList $ (hash,) <$> tags
    GetBlocks (SlotRange mFromSlot mToSlot) filteredTags -> do
        let fromSlot = fromMaybe minBound mFromSlot
            toSlot = fromMaybe maxBound mToSlot
        tags <- get @(Set (BlockHash, BlockTag))
        gets @[Block]
            $ filter \b ->
                b.slotNumber >= fromSlot
                    && b.slotNumber <= toSlot
                    && all (\t -> (b.hash, t) `Set.member` tags) filteredTags
