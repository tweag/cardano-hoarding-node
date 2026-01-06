module Hoard.Listeners.BlockFetchEventListener (blockFetchEventListener) where

import Effectful (Eff, (:>))

import Hoard.Data.Block (Block (..))
import Hoard.Data.Block.Extract (extractBlockData)
import Hoard.Effects.BlockRepo (BlockRepo, insertBlocks)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Network.Events
    ( BlockBatchCompletedData (..)
    , BlockFetchEvent (..)
    , BlockFetchFailedData (..)
    , BlockFetchStartedData (..)
    )


-- | Listener that handles chain sync events
--
-- For HeaderReceived events, extracts header data and persists it to the database.
blockFetchEventListener :: (Log :> es, BlockRepo :> es) => BlockFetchEvent -> Eff es ()
blockFetchEventListener = \case
    BlockFetchStarted dat -> do
        Log.info $ "🧱  BlockFetch protocol started at " <> show dat.timestamp
    BlockReceived dat -> do
        Log.info "📦 Block received!"
        let block = extractBlockData dat
        insertBlocks [block]
        Log.debug $ "Persisted block: " <> show block.hash
    BlockFetchFailed dat -> do
        Log.warn $ "❗ Failed to fetch block from: " <> dat.errorMessage
    BlockBatchCompleted dat -> do
        Log.info $ "✅ Finished fetching " <> show dat.blockCount <> " blocks in block batch"
