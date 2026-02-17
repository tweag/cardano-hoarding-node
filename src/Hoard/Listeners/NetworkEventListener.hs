module Hoard.Listeners.NetworkEventListener (protocolErrorListener) where

import Effectful (Eff, (:>))

import Hoard.Effects.Monitoring.Tracing (Tracing, addEvent)
import Hoard.Events.Network
    ( ProtocolError (..)
    )


-- | Listener that logs protocol error events
protocolErrorListener
    :: (Tracing :> es)
    => ProtocolError
    -> Eff es ()
protocolErrorListener event = do
    addEvent "protocol_error_event" [("error", event.errorMessage)]
