module Hoard.Listeners.ChainSyncEventListener
    ( chainSyncHeaderReceivedListener
    , chainSyncStartedListener
    , chainSyncRollBackwardListener
    , chainSyncRollForwardListener
    , chainSyncIntersectionFoundListener
    ) where

import Effectful (Eff, (:>))

import Hoard.Data.Header (Header (..))
import Hoard.Data.Header.Extract (extractHeaderData)
import Hoard.Effects.HeaderRepo (HeaderRepo, upsertHeader)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Network.Events
    ( ChainSyncIntersectionFound (..)
    , ChainSyncStarted (..)
    , HeaderReceived (..)
    , RollBackward (..)
    , RollForward (..)
    )


-- | Listener that handles ChainSync HeaderReceived events
--
-- Extracts header data and persists it to the database.
chainSyncHeaderReceivedListener
    :: (Log :> es, HeaderRepo :> es)
    => HeaderReceived
    -> Eff es ()
chainSyncHeaderReceivedListener event = do
    Log.debug "📦 Header received!"
    -- Extract header data from the event
    let header = extractHeaderData event
    -- Upsert the header and record receipt
    upsertHeader header event.peer event.timestamp
    Log.debug $ "Persisted header: " <> show header.hash


-- | Listener that handles ChainSync started events
chainSyncStartedListener
    :: (Log :> es)
    => ChainSyncStarted
    -> Eff es ()
chainSyncStartedListener event = do
    Log.info $ "⛓️  ChainSync protocol started at " <> show event.timestamp


-- | Listener that handles ChainSync rollback events
chainSyncRollBackwardListener
    :: (Log :> es)
    => RollBackward
    -> Eff es ()
chainSyncRollBackwardListener _event = do
    Log.info "⏪ Rollback occurred"


-- | Listener that handles ChainSync roll forward events
chainSyncRollForwardListener
    :: (Log :> es)
    => RollForward
    -> Eff es ()
chainSyncRollForwardListener _event = do
    Log.info "⏩ RollForward occurred"


-- | Listener that handles ChainSync intersection found events
chainSyncIntersectionFoundListener
    :: (Log :> es)
    => ChainSyncIntersectionFound
    -> Eff es ()
chainSyncIntersectionFoundListener _event = do
    Log.info "🎯 ChainSync intersection found"
