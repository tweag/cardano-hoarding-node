module Hoard.BlockFetch.Listeners
    ( blockFetchStarted
    , blockReceived
    , blockFetchFailed
    , blockBatchCompleted
    ) where

import Cardano.Api.LedgerState ()
import Effectful (Eff, (:>))
import Ouroboros.Consensus.Block
    ( SlotNo (..)
    , blockSlot
    , getHeader
    )
import Prelude hiding (State, gets, modify, state)

import Hoard.BlockFetch.Events
    ( BlockBatchCompleted (..)
    , BlockFetchFailed (..)
    , BlockFetchStarted (..)
    , BlockReceived (..)
    )
import Hoard.Data.Block (Block (..))
import Hoard.Data.BlockHash (blockHashFromHeader)
import Hoard.Data.PoolID (mkPoolID)
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.BlockRepo qualified as BlockRepo
import Hoard.Effects.Monitoring.Metrics (Metrics)
import Hoard.Effects.Monitoring.Metrics.Definitions (recordBlockFetchFailure, recordBlockReceived)
import Hoard.Effects.Monitoring.Tracing (Tracing, addAttribute, addEvent, withSpan)


-- | Listener that handles BlockFetch started events
blockFetchStarted :: (Tracing :> es) => BlockFetchStarted -> Eff es ()
blockFetchStarted event = do
    addEvent "block_fetch_started" [("timestamp", show event.timestamp)]


-- | Listener that handles block received events
--
-- Extracts block data and persists it to the database.
blockReceived :: (Tracing :> es, BlockRepo :> es, Metrics :> es) => BlockReceived -> Eff es ()
blockReceived event = withSpan "block_received" $ do
    let block = extractBlockData event
    addAttribute "block.hash" (show block.hash)
    addAttribute "block.slot" (show block.slotNumber)
    addEvent "block_received" [("slot", show block.slotNumber), ("hash", show block.hash)]
    recordBlockReceived
    BlockRepo.insertBlocks [block]
    addEvent "block_persisted" [("hash", show block.hash)]


-- | Listener that handles block fetch failed events
blockFetchFailed :: (Tracing :> es, Metrics :> es) => BlockFetchFailed -> Eff es ()
blockFetchFailed event = do
    recordBlockFetchFailure
    addEvent "block_fetch_failed" [("error", event.errorMessage)]


-- | Listener that handles block batch completed events
blockBatchCompleted :: (Tracing :> es) => BlockBatchCompleted -> Eff es ()
blockBatchCompleted event = do
    addEvent "block_batch_completed" [("count", show event.blockCount)]


-- | Extract block data from a BlockReceived event.
-- Assumes the block has not been validated.
extractBlockData :: BlockReceived -> Block
extractBlockData event =
    Block
        { hash = blockHashFromHeader $ getHeader event.block
        , slotNumber = fromIntegral $ unSlotNo $ blockSlot $ event.block
        , poolId = mkPoolID event.block
        , blockData = event.block
        , validationStatus = "" -- Block has yet to be validated
        , validationReason = "" -- Block has yet to be validated
        , firstSeen = event.timestamp
        , classification = Nothing -- Block has yet to be classified
        , classifiedAt = Nothing -- Block has yet to be classified
        }
