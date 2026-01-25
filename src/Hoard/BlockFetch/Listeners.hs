module Hoard.BlockFetch.Listeners
    ( blockFetchStarted
    , blockReceived
    , blockFetchFailed
    , blockBatchCompleted
    , pickBlockFetchRequest
    ) where

import Cardano.Api.LedgerState ()
import Data.Set qualified as S
import Data.Set qualified as Set
import Effectful (Eff, (:>))
import Effectful.State.Static.Shared (State, modify, state)
import Ouroboros.Consensus.Block
    ( SlotNo (..)
    , blockSlot
    , getHeader
    )
import Prelude hiding (State, modify, state)

import Hoard.BlockFetch.Events
    ( BlockBatchCompleted (..)
    , BlockFetchFailed (..)
    , BlockFetchRequest (..)
    , BlockFetchStarted (..)
    , BlockReceived (..)
    )
import Hoard.ChainSync.Events (HeaderReceived (..))
import Hoard.Collectors.State (BlocksBeingFetched (..))
import Hoard.Data.Block (Block (..))
import Hoard.Data.BlockHash (blockHashFromHeader)
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer (..))
import Hoard.Data.PoolID (mkPoolID)
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.BlockRepo qualified as BlockRepo
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.Publishing (Pub, publish)


-- | Listener that handles BlockFetch started events
blockFetchStarted :: (Log :> es) => BlockFetchStarted -> Eff es ()
blockFetchStarted event = do
    Log.info $ "🧱  BlockFetch protocol started at " <> show event.timestamp


-- | Listener that handles block received events
--
-- Extracts block data and persists it to the database.
blockReceived :: (Log :> es, State BlocksBeingFetched :> es, BlockRepo :> es) => BlockReceived -> Eff es ()
blockReceived event = do
    Log.info "📦 Block received!"
    let block = extractBlockData event
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


-- | Re-emit `HeaderReceived` events as `BlockFetchRequests`.
--
-- Filters events by peer ID and publishes block fetch requests for headers
-- that are not already being fetched and not in the database.
pickBlockFetchRequest
    :: ( BlockRepo :> es
       , State BlocksBeingFetched :> es
       , Pub :> es
       )
    => ID Peer
    -> HeaderReceived
    -> Eff es ()
pickBlockFetchRequest myPeerId event =
    when (event.peer.id == myPeerId) do
        let hash = blockHashFromHeader event.header
        -- This check can be implemented in 2 thread-safe ways, as far as we know:
        -- 1. Keep the database check inside the `stateM` operation and ensure
        --    thread-safety between checking the DB and the `blocksBeingFetched`
        --    state.
        -- 2. Check the in-memory `blocksBeingFetched` state first, update
        --    `blocksBeingFetched` if the hash is not in there, then check the
        --    database, removing the hash from `blocksBeingFetched` if it is
        --    already in the database.
        --
        -- We decided to use option 2 to prevent having the database check inside
        -- `state`/`stateM`, which would drastically increase the time spent in the
        -- critical section `state`/`stateM` provides us.
        fetchTheBlock <- state \s ->
            if hash `S.notMember` s.blocksBeingFetched
                then
                    (True, BlocksBeingFetched $ S.insert hash s.blocksBeingFetched)
                else
                    (False, s)
        when fetchTheBlock do
            existingBlock <- BlockRepo.getBlock event.header
            if (isNothing existingBlock)
                then
                    publish
                        BlockFetchRequest
                            { timestamp = event.timestamp
                            , header = event.header
                            , peer = event.peer
                            }
                else modify \s -> BlocksBeingFetched $ S.delete hash s.blocksBeingFetched


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
