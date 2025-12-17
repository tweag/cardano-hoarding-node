module Main (main) where

import Ouroboros.Network.IOManager (withIOManager)

import Options.Applicative qualified as Opt

import Hoard.CLI.Options (Options (..), optsParser)
import Hoard.Config.Loader (loadEnv)
import Hoard.Effects (runEffectStack)
import Hoard.Listeners (runListeners)
import Hoard.Types.Deployment (Deployment (..))

import Hoard.Collector (runCollectors)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Server (runServer)


main :: IO ()
main = withIOManager $ \ioManager -> do
    options <- Opt.execParser optsParser

    -- Determine deployment: CLI flag > default to Dev
    let deployment = fromMaybe Dev options.deployment

    putTextLn $ "Loading configuration for deployment: " <> show deployment
    env <- loadEnv ioManager deployment

    runEffectStack env $ do
        runServer env
        runListeners
        runCollectors
        Conc.awaitAll
