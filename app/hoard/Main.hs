module Main (main) where

import Ouroboros.Network.IOManager (withIOManager)

import Options.Applicative qualified as Opt

import Hoard.CLI.Options (Options (..), optsParser)
import Hoard.Config.Loader (loadConfig)
import Hoard.Effects (Config (..), runEffectStack)
import Hoard.Listeners (runListeners)
import Hoard.Types.Environment (Environment (..))

import Hoard.Data.ProtocolInfo (ProtocolConfigPath (..))
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Input (runInputConst)
import Hoard.Server (runServer)


main :: IO ()
main = withIOManager $ \ioManager -> do
    options <- Opt.execParser optsParser

    -- Determine environment: CLI flag > default to Dev
    let env = fromMaybe Dev options.environment

    putTextLn $ "Loading configuration for environment: " <> show env
    config <- loadConfig ioManager env

    runEffectStack config
        . runInputConst config.ioManager
        . runInputConst (ProtocolConfigPath config.protocolConfigPath)
        $ do
            runServer config
            runListeners
            Conc.awaitAll
