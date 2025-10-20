module Hoard.Listeners.HeaderReceivedListener (headerReceivedListener) where

import Control.Monad.Trans (lift)
import Data.ByteString.Char8 qualified as BS
import Effectful ((:>))
import Effectful.Console.ByteString (Console, putStrLn)
import Hoard.App (App)
import Hoard.Events.HeaderReceived (HeaderReceived (..))
import Prelude hiding (putStrLn)

-- | Listener that logs when a header is received
headerReceivedListener :: (Console :> es) => HeaderReceived -> App es ()
headerReceivedListener event = lift $ do
  putStrLn $ BS.pack "Header received:"
  putStrLn $ BS.pack $ show event
