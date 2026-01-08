module Hoard.Listeners.BlockFetchEventListener
    ( blockFetchStartedListener
    , blockReceivedListener
    , blockFetchFailedListener
    , blockBatchCompletedListener
    ) where

import Data.Set qualified as S
import Effectful (Eff, (:>))
import Effectful.State.Static.Shared (State, modify)
import Prelude hiding (State, modify)

import Hoard.Collectors.State (BlocksBeingFetched (..))
import Hoard.Data.Block (Block (..))
import Hoard.Data.Block.Extract (extractBlockData)
import Hoard.Data.BlockHash (blockHashFromHeader)
import Hoard.Effects.BlockRepo (BlockRepo, insertBlocks)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Network.Events
    ( BlockBatchCompleted (..)
    , BlockFetchFailed (..)
    , BlockFetchStarted (..)
    , BlockReceived (..)
    )


-- | Listener that handles BlockFetch started events
blockFetchStartedListener :: (Log :> es) => BlockFetchStarted -> Eff es ()
blockFetchStartedListener event = do
    Log.info $ "🧱  BlockFetch protocol started at " <> show event.timestamp


-- | Listener that handles block received events
--
-- Extracts block data and persists it to the database.
blockReceivedListener :: (Log :> es, BlockRepo :> es, State BlocksBeingFetched :> es) => BlockReceived -> Eff es ()
blockReceivedListener event = do
    Log.info "📦 Block received!"
    let block = extractBlockData event
    insertBlocks [block]
    modify $ coerce S.delete block.hash
    Log.debug $ "Persisted block: " <> show block.hash


-- | Listener that handles block fetch failed events
blockFetchFailedListener :: (Log :> es, State BlocksBeingFetched :> es) => BlockFetchFailed -> Eff es ()
blockFetchFailedListener event = do
    modify $ coerce S.delete (blockHashFromHeader event.header)
    Log.warn $ "❗ Failed to fetch block from: " <> event.errorMessage


-- | Listener that handles block batch completed events
blockBatchCompletedListener :: (Log :> es) => BlockBatchCompleted -> Eff es ()
blockBatchCompletedListener event = do
    Log.info $ "✅ Finished fetching " <> show event.blockCount <> " blocks in block batch"
