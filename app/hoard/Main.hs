module Main (main) where

import Control.Concurrent.STM (newTQueueIO)
import Effectful.Concurrent qualified as Eff
import Hoard.App (addListener, startEventSystem)
import Hoard.Effects (Config (Config), runEffectStack)
import Hoard.Listeners.HeaderReceivedListener (headerReceivedListener)
import Hoard.Server (ServerConfig (..), runServer)
import Hoard.Server qualified as Server

main :: IO ()
main = do
  -- Create event queue for communication between server and Eve
  eventQueue <- newTQueueIO

  -- Start background threads in the effect stack
  putStrLn "Starting Hoard..."
  runEffectStack (Config eventQueue) $ do
    -- Fork the HTTP server
    let serverConfig = ServerConfig {port = 3000, host = "0.0.0.0", Server.eventQueue = eventQueue}
    _ <- Eff.forkIO $ runServer serverConfig

    -- Run the Eve app (blocks until app exits)
    _finalState <- startEventSystem eventQueue $ do
      addListener headerReceivedListener

    pure ()
