module Hoard.Listeners.ChainSyncEventListener (chainSyncEventListener) where

import Effectful (Eff, (:>))

import Data.Set qualified as Set
import Hoard.Data.Header (Header (..))
import Hoard.Data.Header.Extract (extractHeaderData)
import Hoard.Effects.HeaderRepo (HeaderRepo, upsertHeader)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.PeerRepo (PeerRepo, upsertPeers)
import Hoard.Network.Events
    ( ChainSyncEvent (..)
    , ChainSyncStartedData (..)
    , HeaderReceivedData (..)
    )


-- | Listener that handles chain sync events
--
-- For HeaderReceived events, extracts header data and persists it to the database.
chainSyncEventListener :: (Log :> es, PeerRepo :> es, HeaderRepo :> es) => ChainSyncEvent -> Eff es ()
chainSyncEventListener = \case
    ChainSyncStarted dat -> do
        Log.info $ "⛓️  ChainSync protocol started at " <> show dat.timestamp
    HeaderReceived dat -> do
        Log.info "📦 Header received!"
        -- 1. Upsert the peer first to ensure it exists
        upsertPeers (Set.singleton dat.peer) dat.peer dat.timestamp
        -- 2. Extract header data from the Cardano header and point
        let header = extractHeaderData dat.header dat.timestamp
        -- 3. Upsert the header and record receipt
        upsertHeader header dat.peer dat.timestamp
        Log.debug $ "Persisted header: " <> show (header.blockHash)
    RollBackward _dat -> do
        Log.info "⏪ Rollback occurred"
    RollForward _dat -> do
        Log.info "⏩ RollForward occurred"
    ChainSyncIntersectionFound _dat -> do
        Log.info "🎯 ChainSync intersection found"
