module Hoard.Listeners.ImmutableTipRefreshTriggeredListener (immutableTipRefreshTriggeredListener) where

import Effectful (Eff, (:>))
import Effectful.State.Static.Shared (State, modify)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.NodeToClient (NodeToClient, immutableTipWith1Retry)
import Hoard.Events.ImmutableTipRefreshTriggered (ImmutableTipRefreshTriggered (..))
import Hoard.Types.HoardState (HoardState (..))
import Prelude hiding (State, modify)


-- | Fetches the immutable tip from the node and updates HoardState.
immutableTipRefreshTriggeredListener :: (NodeToClient :> es, State HoardState :> es, Log :> es) => ImmutableTipRefreshTriggered -> Eff es ()
immutableTipRefreshTriggeredListener ImmutableTipRefreshTriggered = do
    Log.info "Fetching immutable tip from cardano-node..."
    immutableTipWith1Retry >>= \case
        Left _ -> pure ()
        Right tip -> do
            Log.info ("Immutable tip: " <> show tip)
            modify (\hoardState -> hoardState {immutableTip = tip})
