module Hoard.Listeners.HeaderReceivedListener (headerReceivedListener) where

import Effectful (Eff, (:>))

import Hoard.Effects.Monitoring.Tracing (Tracing, addEvent)
import Hoard.Events.HeaderReceived (HeaderReceived (..))


-- | Listener that logs when a header is received
headerReceivedListener :: (Tracing :> es) => HeaderReceived -> Eff es ()
headerReceivedListener event = do
    addEvent "header_received_event" [("details", show event)]
