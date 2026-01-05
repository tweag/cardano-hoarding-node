module Hoard.Listeners.HeaderReceivedListener (headerReceivedListener) where

import Effectful (Eff)

import Hoard.Effects.Log qualified as Log
import Hoard.Events.HeaderReceived (HeaderReceived (..))


-- | Listener that logs when a header is received
headerReceivedListener :: (_) => HeaderReceived -> Eff es ()
headerReceivedListener event = do
    Log.debug "Header received:"
    Log.debug $ show event
