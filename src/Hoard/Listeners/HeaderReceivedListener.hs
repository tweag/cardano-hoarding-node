module Hoard.Listeners.HeaderReceivedListener (headerReceivedListener) where

import Data.ByteString.Char8 qualified as BS
import Effectful (Eff, (:>))
import Effectful.Console.ByteString (Console, putStrLn)
import Hoard.Events.HeaderReceived (HeaderReceived (..))
import Prelude hiding (putStrLn)

-- | Listener that logs when a header is received
headerReceivedListener :: (Console :> es) => HeaderReceived -> Eff es ()
headerReceivedListener event = do
  putStrLn $ BS.pack "Header received:"
  putStrLn $ BS.pack $ show event
