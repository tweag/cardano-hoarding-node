module Main (main) where

import Control.Concurrent.STM (newTQueueIO)
import Effectful.Concurrent qualified as Eff
import Hoard.App (addListener, startEventSystem)
import Hoard.Effects (Config (..), runEffectStack)
import Hoard.Listeners.HeaderReceivedListener (headerReceivedListener)
import Hoard.Server (ServerConfig (..), runServer)
import Hoard.Server qualified as Server
import Hoard.Types.DBConfig (acquireDatabasePools, devConfig)

main :: IO ()
main = do
  config <- loadConfig

  -- Start background threads in the effect stack
  putStrLn "Starting Hoard..."
  runEffectStack config $ do
    -- Fork the HTTP server
    let serverConfig =
          ServerConfig
            { port = 3000,
              host = "0.0.0.0",
              Server.eventQueue = config.eventQueue,
              Server.dbPools = config.dbPools
            }
    _ <- Eff.forkIO $ runServer serverConfig

    -- Run the Eve app (blocks until app exits)
    _finalState <- startEventSystem config.eventQueue $ do
      addListener headerReceivedListener

    pure ()

loadConfig :: IO Config
loadConfig = do
  eventQueue <- newTQueueIO
  let (readerConfig, writerConfig) = devConfig
  dbPools <- acquireDatabasePools readerConfig writerConfig
  pure $ Config eventQueue dbPools
