module Hoard.Listeners (runListeners) where

import Effectful.State.Static.Shared (State)

import Hoard.Effects.Conc (Conc)
import Hoard.Effects.HoardStateRepo (HoardStateRepo)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Monitoring.Tracing (Tracing)
import Hoard.Effects.NodeToClient (NodeToClient)
import Hoard.Effects.Publishing (Pub, Sub, listen)
import Hoard.Events.ImmutableTipRefreshTriggered (ImmutableTipRefreshTriggered)
import Hoard.Events.Network (ProtocolError)
import Hoard.Listeners.ImmutableTipRefreshTriggeredListener (ImmutableTipRefreshed, immutableTipRefreshTriggeredListener, immutableTipRefreshedListener)
import Hoard.Listeners.NetworkEventListener (protocolErrorListener)
import Hoard.Types.HoardState (HoardState)

import Hoard.Effects.Conc qualified as Conc


runListeners
    :: ( Conc :> es
       , HoardStateRepo :> es
       , Log :> es
       , NodeToClient :> es
       , Pub ImmutableTipRefreshed :> es
       , State HoardState :> es
       , Sub ImmutableTipRefreshTriggered :> es
       , Sub ImmutableTipRefreshed :> es
       , Sub ProtocolError :> es
       , Tracing :> es
       )
    => Eff es ()
runListeners = do
    _ <- Conc.fork $ listen protocolErrorListener
    _ <- Conc.fork $ listen immutableTipRefreshTriggeredListener
    _ <- Conc.fork $ listen immutableTipRefreshedListener
    pure ()
