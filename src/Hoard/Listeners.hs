module Hoard.Listeners (runListeners) where

import Effectful (Eff, (:>))
import Effectful.State.Static.Shared (State)
import Prelude hiding (Reader, State)

import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.HoardStateRepo (HoardStateRepo)
import Hoard.Effects.Monitoring.Tracing (Tracing)
import Hoard.Effects.NodeToClient (NodeToClient)
import Hoard.Effects.Publishing (Pub, Sub, listen)
import Hoard.Events.ImmutableTipRefreshTriggered (ImmutableTipRefreshTriggered)
import Hoard.Listeners.ImmutableTipRefreshTriggeredListener (ImmutableTipRefreshed, immutableTipRefreshTriggeredListener, immutableTipRefreshedListener)
import Hoard.Listeners.NetworkEventListener (protocolErrorListener)
import Hoard.Network.Events (ProtocolError)
import Hoard.Types.HoardState (HoardState)


runListeners
    :: ( Conc :> es
       , Tracing :> es
       , NodeToClient :> es
       , State HoardState :> es
       , Sub ProtocolError :> es
       , Sub ImmutableTipRefreshTriggered :> es
       , Sub ImmutableTipRefreshed :> es
       , Pub ImmutableTipRefreshed :> es
       , HoardStateRepo :> es
       )
    => Eff es ()
runListeners = do
    _ <- Conc.fork $ listen protocolErrorListener
    _ <- Conc.fork $ listen immutableTipRefreshTriggeredListener
    _ <- Conc.fork $ listen immutableTipRefreshedListener
    pure ()
