module Hoard.Listeners.NetworkEventListener (protocolErrorListener) where

import Effectful (Eff, (:>))

import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Network.Events
    ( ProtocolError (..)
    )


-- | Listener that logs protocol error events
protocolErrorListener
    :: (Log :> es)
    => ProtocolError
    -> Eff es ()
protocolErrorListener event = do
    Log.warn $ "❌ Protocol error: " <> event.errorMessage
