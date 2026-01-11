module Hoard.Listeners.ImmutableTipRefreshTriggeredListener (immutableTipRefreshTriggeredListener, refreshImmutableTip) where

import Effectful (Eff, (:>))
import Effectful.State.Static.Shared (State, modify)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.NodeToClient (ConnectionError (..), NodeToClient, connect)
import Hoard.Effects.NodeToClient qualified as N
import Hoard.Events.ImmutableTipRefreshTriggered (ImmutableTipRefreshTriggered (..))
import Hoard.Types.HoardState (HoardState (..))
import Prelude hiding (State, modify)


-- | Fetches the immutable tip from the node and updates HoardState.
immutableTipRefreshTriggeredListener :: (NodeToClient ConnectionError :> es, State HoardState :> es, Log :> es) => ImmutableTipRefreshTriggered -> Eff es ()
immutableTipRefreshTriggeredListener ImmutableTipRefreshTriggered = refreshImmutableTip


-- | Fetch the immutable tip from the cardano-node and store it in HoardState.
--
-- This is called regularly and during application setup to initialize the immutable tip
-- before we start connecting to peers.
-- If fetching the tip fails due to a connection error, it attempts to reconnect. So the next call has a chance to succeed.
refreshImmutableTip :: (Log :> es, NodeToClient ConnectionError :> es, State HoardState :> es) => Eff es ()
refreshImmutableTip = do
    Log.info "Fetching immutable tip from cardano-node..."
    N.immutableTip >>= \case
        Left e -> do
            case e of
                UninitializedConnection -> Log.warn $ "`connectToLocalNode` error. uninitialized connection. connecting..."
                ConnectionException e' -> Log.warn $ "`connectToLocalNode` error. " <> toText (displayException e') <> ". reconnecting..."
            connect
        Right tip ->
            do
                Log.info $ "Immutable tip: " <> show tip
                modify (\hoardState -> hoardState {immutableTip = tip})
