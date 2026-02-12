module Hoard.ChainSync.Listeners
    ( chainSyncHeaderReceived
    , chainSyncStarted
    , chainSyncRollBackward
    , chainSyncRollForward
    , chainSyncIntersectionFound
    ) where

import Cardano.Api.LedgerState ()
import Effectful (Eff, (:>))
import Ouroboros.Consensus.Block (BlockNo (..), SlotNo (..))
import Ouroboros.Consensus.Block.Abstract (blockNo, blockSlot)

import Hoard.ChainSync.Events
    ( ChainSyncIntersectionFound (..)
    , ChainSyncStarted (..)
    , HeaderReceived (..)
    , RollBackward (..)
    , RollForward (..)
    )
import Hoard.Data.BlockHash (blockHashFromHeader)
import Hoard.Data.Header (Header (..))
import Hoard.Effects.HeaderRepo (HeaderRepo, upsertHeader)
import Hoard.Effects.Monitoring.Metrics (Metrics)
import Hoard.Effects.Monitoring.Metrics.Definitions (recordChainSyncRollback, recordChainSyncRollforward, recordHeaderReceived)
import Hoard.Effects.Monitoring.Tracing (Tracing, addAttribute, addEvent, withSpan)


-- | Listener that handles ChainSync HeaderReceived events
--
-- Extracts header data and persists it to the database.
chainSyncHeaderReceived
    :: (HeaderRepo :> es, Metrics :> es, Tracing :> es)
    => HeaderReceived
    -> Eff es ()
chainSyncHeaderReceived event = withSpan "header_received" $ do
    recordHeaderReceived
    -- Extract header data from the event
    let header = extractHeaderData event
    addAttribute "header.hash" (show header.hash)
    addAttribute "header.slot" (show header.slotNumber)
    addAttribute "header.block_number" (show header.blockNumber)
    addEvent "header_received" [("hash", show header.hash)]
    -- Upsert the header and record receipt
    upsertHeader header event.peer event.timestamp
    addEvent "header_persisted" [("hash", show header.hash)]


-- | Listener that handles ChainSync started events
chainSyncStarted
    :: (Tracing :> es)
    => ChainSyncStarted
    -> Eff es ()
chainSyncStarted event = do
    addEvent "chain_sync_started" [("timestamp", show event.timestamp)]


-- | Listener that handles ChainSync rollback events
chainSyncRollBackward
    :: (Metrics :> es, Tracing :> es)
    => RollBackward
    -> Eff es ()
chainSyncRollBackward _event = do
    recordChainSyncRollback
    addEvent "chain_sync_rollback" []


-- | Listener that handles ChainSync roll forward events
chainSyncRollForward
    :: (Metrics :> es, Tracing :> es)
    => RollForward
    -> Eff es ()
chainSyncRollForward _event = do
    recordChainSyncRollforward
    addEvent "chain_sync_rollforward" []


-- | Listener that handles ChainSync intersection found events
chainSyncIntersectionFound
    :: (Tracing :> es)
    => ChainSyncIntersectionFound
    -> Eff es ()
chainSyncIntersectionFound _event = do
    addEvent "chain_sync_intersection_found" []


-- | Extract header data from a HeaderReceivedData event
extractHeaderData :: HeaderReceived -> Header
extractHeaderData event =
    Header
        { hash = blockHashFromHeader event.header
        , slotNumber = unSlotNo $ blockSlot event.header
        , blockNumber = unBlockNo $ blockNo event.header
        , firstSeenAt = event.timestamp
        }
