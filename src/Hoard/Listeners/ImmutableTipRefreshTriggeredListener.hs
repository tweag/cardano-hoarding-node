module Hoard.Listeners.ImmutableTipRefreshTriggeredListener
    ( ImmutableTipRefreshed (..)
    , immutableTipRefreshTriggeredListener
    , refreshImmutableTip
    , immutableTipRefreshedListener
    ) where

import Effectful (Eff, (:>))
import Effectful.State.Static.Shared (State, gets, modifyM)
import Hoard.Effects.HoardStateRepo (HoardStateRepo, persistImmutableTip)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.NodeToClient (NodeToClient)
import Hoard.Effects.NodeToClient qualified as NodeToClient
import Hoard.Effects.Publishing (Pub, publish)
import Hoard.Events.ImmutableTipRefreshTriggered (ImmutableTipRefreshTriggered (..))
import Hoard.Types.HoardState (HoardState (..))
import Prelude hiding (State, gets, modify)


-- | Fetches the immutable tip from the node and updates HoardState.
immutableTipRefreshTriggeredListener :: (NodeToClient :> es, State HoardState :> es, Log :> es, Pub :> es) => ImmutableTipRefreshTriggered -> Eff es ()
immutableTipRefreshTriggeredListener ImmutableTipRefreshTriggered = refreshImmutableTip


-- | Fetch the immutable tip from the cardano-node and store it in HoardState.
--
-- This is called regularly and during application setup to initialize the immutable tip
-- before we start connecting to peers.
-- If fetching the tip fails due to a connection error, it retries once to reconnect and fetch.
refreshImmutableTip :: (Log :> es, NodeToClient :> es, State HoardState :> es, Pub :> es) => Eff es ()
refreshImmutableTip = do
    Log.info "Fetching immutable tip from cardano-node..."
    NodeToClient.immutableTip >>= \case
        Nothing -> Log.warn "Failed to fetch immutable tip from cardano-node (connection may be down)"
        Just tip -> do
            Log.info ("Immutable tip updated: " <> show tip)
            modifyM
                ( \hoardState ->
                    if hoardState.immutableTip < tip
                        then publish ImmutableTipRefreshed $> hoardState {immutableTip = tip}
                        else Log.warn "observed a regressed immutable tip." $> hoardState
                )


data ImmutableTipRefreshed = ImmutableTipRefreshed


-- | Persist the updated immutable tip to the database.
immutableTipRefreshedListener :: (HoardStateRepo :> es, State HoardState :> es, Log :> es) => ImmutableTipRefreshed -> Eff es ()
immutableTipRefreshedListener ImmutableTipRefreshed =
    do
        Log.debug "persisting immutable tip."
        persistImmutableTip =<< gets (.immutableTip)
