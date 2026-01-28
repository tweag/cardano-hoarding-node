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
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.Metrics (Metrics)
import Hoard.Effects.Metrics.Definitions (recordChainSyncRollback, recordChainSyncRollforward, recordHeaderReceived)


-- | Listener that handles ChainSync HeaderReceived events
--
-- Extracts header data and persists it to the database.
chainSyncHeaderReceived
    :: (Log :> es, HeaderRepo :> es, Metrics :> es)
    => HeaderReceived
    -> Eff es ()
chainSyncHeaderReceived event = do
    Log.debug "📦 Header received!"
    recordHeaderReceived
    -- Extract header data from the event
    let header = extractHeaderData event
    -- Upsert the header and record receipt
    upsertHeader header event.peer event.timestamp
    Log.debug $ "Persisted header: " <> show header.hash


-- | Listener that handles ChainSync started events
chainSyncStarted
    :: (Log :> es)
    => ChainSyncStarted
    -> Eff es ()
chainSyncStarted event = do
    Log.debug $ "⛓️  ChainSync protocol started at " <> show event.timestamp


-- | Listener that handles ChainSync rollback events
chainSyncRollBackward
    :: (Log :> es, Metrics :> es)
    => RollBackward
    -> Eff es ()
chainSyncRollBackward _event = do
    recordChainSyncRollback
    Log.info "⏪ Rollback occurred"


-- | Listener that handles ChainSync roll forward events
chainSyncRollForward
    :: (Log :> es, Metrics :> es)
    => RollForward
    -> Eff es ()
chainSyncRollForward _event = do
    recordChainSyncRollforward
    Log.info "⏩ RollForward occurred"


-- | Listener that handles ChainSync intersection found events
chainSyncIntersectionFound
    :: (Log :> es)
    => ChainSyncIntersectionFound
    -> Eff es ()
chainSyncIntersectionFound _event = do
    Log.info "🎯 ChainSync intersection found"


-- | Extract header data from a HeaderReceivedData event
extractHeaderData :: HeaderReceived -> Header
extractHeaderData event =
    Header
        { hash = blockHashFromHeader event.header
        , slotNumber = unSlotNo $ blockSlot event.header
        , blockNumber = unBlockNo $ blockNo event.header
        , firstSeenAt = event.timestamp
        }
