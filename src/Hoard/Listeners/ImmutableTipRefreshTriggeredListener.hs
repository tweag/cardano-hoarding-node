module Hoard.Listeners.ImmutableTipRefreshTriggeredListener
    ( ImmutableTipRefreshed (..)
    , immutableTipRefreshTriggeredListener
    , immutableTipRefreshedListener
    ) where

import Effectful.State.Static.Shared (State, gets, modifyM)

import Hoard.Effects.HoardStateRepo (HoardStateRepo, persistImmutableTip)
import Hoard.Effects.Monitoring.Tracing (SpanStatus (..), Tracing, setStatus, withSpan)
import Hoard.Effects.NodeToClient (NodeToClient)
import Hoard.Effects.Publishing (Pub, publish)
import Hoard.Events.ImmutableTipRefreshTriggered
    ( ImmutableTipRefreshTriggered (ImmutableTipRefreshTriggered)
    )
import Hoard.Types.HoardState (HoardState (immutableTip))

import Hoard.Effects.NodeToClient qualified as NodeToClient


-- | Fetch the immutable tip from the cardano-node and store it in HoardState.
--
-- This is called regularly and during application setup to initialize the immutable tip
-- before we start connecting to peers.
-- If fetching the tip fails due to a connection error, it retries once to reconnect and fetch.
immutableTipRefreshTriggeredListener
    :: ( NodeToClient :> es
       , Pub ImmutableTipRefreshed :> es
       , State HoardState :> es
       , Tracing :> es
       )
    => ImmutableTipRefreshTriggered -> Eff es ()
immutableTipRefreshTriggeredListener ImmutableTipRefreshTriggered = withSpan "immutable_tip.refresh" do
    NodeToClient.immutableTip >>= \case
        Nothing ->
            setStatus $ Error "Fetch failed: connection may be down"
        Just tip -> do
            setStatus Ok
            modifyM $ \hoardState ->
                if hoardState.immutableTip < tip then
                    publish ImmutableTipRefreshed $> hoardState {immutableTip = tip}
                else
                    pure hoardState


data ImmutableTipRefreshed = ImmutableTipRefreshed


-- | Persist the updated immutable tip to the database.
immutableTipRefreshedListener
    :: ( HoardStateRepo :> es
       , State HoardState :> es
       , Tracing :> es
       )
    => ImmutableTipRefreshed -> Eff es ()
immutableTipRefreshedListener ImmutableTipRefreshed = withSpan "immutable_tip.persist" do
    tip <- gets (.immutableTip)
    persistImmutableTip tip
