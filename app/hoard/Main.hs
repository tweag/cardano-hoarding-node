module Main (main) where

import Data.Maybe (fromMaybe)
import Ouroboros.Network.IOManager (withIOManager)

import Data.Text qualified as T
import Options.Applicative qualified as Opt

import Hoard.CLI.Options (Options (..), optsParser)
import Hoard.Config.Loader (loadConfig)
import Hoard.Effects (Config (..), ServerConfig (..), runEffectStack)
import Hoard.Effects.Sub (listen)
import Hoard.Listeners.HeaderReceivedListener (headerReceivedListener)
import Hoard.Types.Environment (Environment (..))

import Hoard.Effects.Conc qualified as Conc
import Hoard.Server qualified as Server


main :: IO ()
main = withIOManager $ \ioManager -> do
    options <- Opt.execParser optsParser

    -- Determine environment: CLI flag > default to Dev
    let env = fromMaybe Dev options.environment

    putStrLn $ "Loading configuration for environment: " <> show env
    config <- loadConfig ioManager env

    -- Start background threads in the effect stack
    putStrLn $ "Starting Hoard on " <> T.unpack config.server.host <> ":" <> show config.server.port <> "..."
    runEffectStack config $ do
        -- Fork the HTTP server
        _ <- Conc.fork $ Server.runServer config
        _ <- Conc.fork $ listen headerReceivedListener

        Conc.awaitAll
