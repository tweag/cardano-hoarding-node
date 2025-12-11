module Hoard.Listeners.ChainSyncEventListener (chainSyncEventListener) where

import Effectful (Eff, (:>))

import Hoard.Data.Header (Header (..))
import Hoard.Data.Header.Extract (extractHeaderData)
import Hoard.Effects.HeaderRepo (HeaderRepo, upsertHeader)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Network.Events
    ( ChainSyncEvent (..)
    , ChainSyncStartedData (..)
    , HeaderReceivedData (..)
    )


-- | Listener that handles chain sync events
--
-- For HeaderReceived events, extracts header data and persists it to the database.
chainSyncEventListener :: (Log :> es, HeaderRepo :> es) => ChainSyncEvent -> Eff es ()
chainSyncEventListener = \case
    ChainSyncStarted dat -> do
        Log.info $ "⛓️  ChainSync protocol started at " <> show dat.timestamp
    HeaderReceived dat -> do
        Log.info "📦 Header received!"
        -- Extract header data from the event
        let header = extractHeaderData dat
        -- Upsert the header and record receipt (peer will be created if needed)
        upsertHeader header dat.peer dat.timestamp
        Log.debug $ "Persisted header: " <> show header.hash
    RollBackward _dat -> do
        Log.info "⏪ Rollback occurred"
    RollForward _dat -> do
        Log.info "⏩ RollForward occurred"
    ChainSyncIntersectionFound _dat -> do
        Log.info "🎯 ChainSync intersection found"
