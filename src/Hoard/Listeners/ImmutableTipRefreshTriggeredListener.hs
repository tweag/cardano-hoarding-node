module Hoard.Listeners.ImmutableTipRefreshTriggeredListener (immutableTipRefreshTriggeredListener) where

import Effectful (Eff, (:>))
import Effectful.State.Static.Shared (State, modify)
import Hoard.Effects.NodeToClient (NodeToClient)
import Hoard.Effects.NodeToClient qualified as N
import Hoard.Events.ImmutableTipRefreshTriggered (ImmutableTipRefreshTriggered (..))
import Hoard.Types.HoardState (HoardState (..))
import Prelude hiding (State, modify)


-- | Fetches the immutable tip from the node and updates HoardState.
immutableTipRefreshTriggeredListener :: (NodeToClient :> es, State HoardState :> es) => ImmutableTipRefreshTriggered -> Eff es ()
immutableTipRefreshTriggeredListener ImmutableTipRefreshTriggered = do
    tip <- N.immutableTip
    modify (\hoardState -> hoardState {immutableTip = tip})
