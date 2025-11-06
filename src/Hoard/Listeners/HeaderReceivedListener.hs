module Hoard.Listeners.HeaderReceivedListener (headerReceivedListener) where

import Effectful (Eff, (:>))
import Prelude hiding (putStrLn)

import Data.Text qualified as T
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Events.HeaderReceived (HeaderReceived (..))


-- | Listener that logs when a header is received
headerReceivedListener :: (Log :> es) => HeaderReceived -> Eff es ()
headerReceivedListener event = do
    Log.debug "Header received:"
    Log.debug $ T.pack $ show event
