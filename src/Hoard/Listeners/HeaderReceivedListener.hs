module Hoard.Listeners.HeaderReceivedListener (headerReceivedListener) where

import Effectful (Eff, (:>))
import Effectful.Console.ByteString (Console, putStrLn)
import Prelude hiding (putStrLn)

import Data.ByteString.Char8 qualified as BS

import Hoard.Events.HeaderReceived (HeaderReceived (..))


-- | Listener that logs when a header is received
headerReceivedListener :: (Console :> es) => HeaderReceived -> Eff es ()
headerReceivedListener event = do
    putStrLn $ BS.pack "Header received:"
    putStrLn $ BS.pack $ show event
