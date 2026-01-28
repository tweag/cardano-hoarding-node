module Hoard.Listeners.ImmutableTipRefreshTriggeredListener (immutableTipRefreshTriggeredListener, refreshImmutableTip) where

import Effectful (Eff, (:>))
import Effectful.State.Static.Shared (State, modify)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.NodeToClient (NodeToClient)
import Hoard.Effects.NodeToClient qualified as NodeToClient
import Hoard.Events.ImmutableTipRefreshTriggered (ImmutableTipRefreshTriggered (..))
import Hoard.Types.HoardState (HoardState (..))
import Prelude hiding (State, modify)


-- | Fetches the immutable tip from the node and updates HoardState.
immutableTipRefreshTriggeredListener :: (NodeToClient :> es, State HoardState :> es, Log :> es) => ImmutableTipRefreshTriggered -> Eff es ()
immutableTipRefreshTriggeredListener ImmutableTipRefreshTriggered = refreshImmutableTip


-- | Fetch the immutable tip from the cardano-node and store it in HoardState.
--
-- This is called regularly and during application setup to initialize the immutable tip
-- before we start connecting to peers.
-- If fetching the tip fails due to a connection error, it retries once to reconnect and fetch.
refreshImmutableTip :: (Log :> es, NodeToClient :> es, State HoardState :> es) => Eff es ()
refreshImmutableTip = do
    Log.info "Fetching immutable tip from cardano-node..."
    NodeToClient.immutableTip >>= \case
        Nothing -> Log.warn "Failed to fetch immutable tip from cardano-node (connection may be down)"
        Just tip -> do
            Log.info ("Immutable tip updated: " <> show tip)
            modify (\hoardState -> hoardState {immutableTip = tip})
