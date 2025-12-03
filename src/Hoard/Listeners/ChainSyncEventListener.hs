module Hoard.Listeners.ChainSyncEventListener (chainSyncEventListener) where

import Effectful (Eff, (:>))

import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Network.Events
    ( ChainSyncEvent (..)
    , ChainSyncStartedData (..)
    )


-- | Listener that logs chain sync events
chainSyncEventListener :: (Log :> es) => ChainSyncEvent -> Eff es ()
chainSyncEventListener = \case
    ChainSyncStarted dat -> do
        Log.info $ "⛓️  ChainSync protocol started at " <> show dat.timestamp
    HeaderReceived _dat -> do
        Log.info "📦 Header received!"
    RollBackward _dat -> do
        Log.info "⏪ Rollback occurred"
    RollForward _dat -> do
        Log.info "⏩ RollForward occurred"
    ChainSyncIntersectionFound _dat -> do
        Log.info "🎯 ChainSync intersection found"
