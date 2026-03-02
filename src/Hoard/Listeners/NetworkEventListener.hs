module Hoard.Listeners.NetworkEventListener (protocolErrorListener) where

import Hoard.Effects.Log (Log)
import Hoard.Events.Network (ProtocolError (..))

import Hoard.Effects.Log qualified as Log


-- | Listener that logs protocol error events
protocolErrorListener
    :: (Log :> es)
    => ProtocolError
    -> Eff es ()
protocolErrorListener event =
    Log.err $ "Protocol error: " <> event.errorMessage
