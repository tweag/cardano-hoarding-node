module Main (main) where

import Control.Concurrent.Chan.Unagi (newChan)
import Effectful.Concurrent qualified as Eff
import Hoard.Effects (Config (..), channelsFromPair, runEffectStack)
import Hoard.Effects.Sub (listen)
import Hoard.Listeners.HeaderReceivedListener (headerReceivedListener)
import Hoard.Server (ServerConfig (..), runServer)
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
              config
            }
    _ <- Eff.forkIO $ runServer serverConfig

    -- Blocks indefinitely
    listen headerReceivedListener

    pure ()

loadConfig :: IO Config
loadConfig = do
  let (readerConfig, writerConfig) = devConfig
  dbPools <- acquireDatabasePools readerConfig writerConfig
  channels <- channelsFromPair <$> newChan
  pure $
    Config
      { dbPools,
        channels
      }
