module Hoard.Listeners (runListeners) where

import Effectful (Eff, (:>))
import Effectful.State.Static.Shared (State)
import Prelude hiding (Reader, State)

import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Log (Log)
import Hoard.Effects.NodeToClient (NodeToClient)
import Hoard.Effects.Publishing (Sub, listen)
import Hoard.Listeners.HeaderReceivedListener (headerReceivedListener)
import Hoard.Listeners.ImmutableTipRefreshTriggeredListener (immutableTipRefreshTriggeredListener)
import Hoard.Listeners.NetworkEventListener (protocolErrorListener)
import Hoard.Types.HoardState (HoardState)


runListeners
    :: ( Conc :> es
       , Log :> es
       , NodeToClient :> es
       , State HoardState :> es
       , Sub :> es
       )
    => Eff es ()
runListeners = do
    _ <- Conc.fork $ listen headerReceivedListener
    _ <- Conc.fork $ listen protocolErrorListener
    _ <- Conc.fork $ listen immutableTipRefreshTriggeredListener
    pure ()
