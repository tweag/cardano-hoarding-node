module Hoard.BlockFetch.Listeners
    ( blockFetchStarted
    , blockReceived
    , blockFetchFailed
    , blockBatchCompleted
    ) where

import Cardano.Api.LedgerState ()
import Data.Set qualified as Set
import Effectful (Eff, (:>))
import Effectful.State.Static.Shared (State, modify)
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
import Hoard.Collectors.State (BlocksBeingFetched (..))
import Hoard.Data.Block (Block (..))
import Hoard.Data.BlockHash (blockHashFromHeader)
import Hoard.Data.PoolID (mkPoolID)
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.BlockRepo qualified as BlockRepo
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log


-- | Listener that handles BlockFetch started events
blockFetchStarted :: (Log :> es) => BlockFetchStarted -> Eff es ()
blockFetchStarted event = do
    Log.info $ "🧱  BlockFetch protocol started at " <> show event.timestamp


-- | Listener that handles block received events
--
-- Extracts block data and persists it to the database.
blockReceived :: (Log :> es, State BlocksBeingFetched :> es, BlockRepo :> es) => BlockReceived -> Eff es ()
blockReceived event = do
    let block = extractBlockData event
    Log.info $ "📦 Block received at slot " <> show block.slotNumber <> " (hash: " <> show block.hash <> ")"
    BlockRepo.insertBlocks [block]
    modify $ coerce Set.delete block.hash
    Log.debug $ "Persisted block: " <> show block.hash


-- | Listener that handles block fetch failed events
blockFetchFailed :: (Log :> es, State BlocksBeingFetched :> es) => BlockFetchFailed -> Eff es ()
blockFetchFailed event = do
    modify $ coerce Set.delete (blockHashFromHeader event.header)
    Log.warn $ "❗ Failed to fetch block from: " <> event.errorMessage


-- | Listener that handles block batch completed events
blockBatchCompleted :: (Log :> es) => BlockBatchCompleted -> Eff es ()
blockBatchCompleted event = do
    Log.info $ "✅ Finished fetching " <> show event.blockCount <> " blocks in block batch"


-- | Extract block data from a BlockReceived event.
-- Assumes the block is not in the canonical chain and has not been validated.
extractBlockData :: BlockReceived -> Block
extractBlockData event =
    Block
        { hash = blockHashFromHeader $ getHeader event.block
        , slotNumber = fromIntegral $ unSlotNo $ blockSlot $ event.block
        , poolId = mkPoolID event.block
        , blockData = event.block
        , validationStatus = "" -- Block has yet to be validated
        , validationReason = "" -- Block has yet to be validated
        , isCanonical = False -- Default to False until proven otherwise.
        , firstSeen = event.timestamp
        }
