module Main (main) where

import Control.Concurrent.Chan.Unagi (newChan)

import Hoard.Effects (Config (..), runEffectStack)
import Hoard.Effects.Sub (listen)
import Hoard.Listeners.HeaderReceivedListener (headerReceivedListener)
import Hoard.Server (ServerConfig (..), runServer)
import Hoard.Types.DBConfig (acquireDatabasePools, devConfig)

import Hoard.Effects.Conc qualified as Conc


main :: IO ()
main = do
    config <- loadConfig

    -- Start background threads in the effect stack
    putStrLn "Starting Hoard..."
    runEffectStack config $ do
        -- Fork the HTTP server
        let serverConfig =
                ServerConfig
                    { port = 3000
                    , host = "0.0.0.0"
                    , config
                    }
        _ <- Conc.fork $ runServer serverConfig
        _ <- Conc.fork $ listen headerReceivedListener

        Conc.awaitAll


loadConfig :: IO Config
loadConfig = do
    let (readerConfig, writerConfig) = devConfig
    dbPools <- acquireDatabasePools readerConfig writerConfig
    (inChan, _) <- newChan
    pure
        $ Config
            { dbPools
            , inChan
            }
