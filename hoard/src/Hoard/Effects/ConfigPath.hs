{-# LANGUAGE AllowAmbiguousTypes #-}

module Hoard.Effects.ConfigPath
    ( envOverrides
    , deepMerge
    , LoadedConfig (..)
    , loadHoardConfig
    , runConfigRoot
    , runConfig
    ) where

import Data.Aeson (Value (..))
import Effectful (IOE, runEff)
import Effectful.Reader.Static (Reader, asks, runReader)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import Data.Aeson.KeyMap qualified as KM
import Data.Yaml qualified as Yaml

import Atelier.Config (LoadedConfig (..), deepMerge, envOverrides, runConfig)
import Atelier.Effects.Env (runEnv)
import Hoard.Effects.Options (Options)
import Hoard.Types.Deployment (Deployment (..), deploymentName)

import Hoard.Effects.Options qualified as Options


-- | Load the full configuration for a deployment.
-- Priority (highest to lowest): env vars > secrets/{env}.yaml > config/{env}.yaml.
loadHoardConfig :: Deployment -> IO LoadedConfig
loadHoardConfig deployment = do
    let env = toString (deploymentName deployment)
        configYaml = "config" </> env <> ".yaml"
        secretsYaml = "secrets" </> env <> ".yaml"
    base <- loadOptional configYaml
    secrets <- loadOptional secretsYaml
    envVars <- runEff $ runEnv $ envOverrides "HOARD"
    pure $ LoadedConfig $ deepMerge (deepMerge base secrets) envVars
  where
    loadOptional path =
        doesFileExist path >>= \case
            True -> Yaml.decodeFileThrow path
            False -> pure (Object KM.empty)


runConfigRoot :: (IOE :> es, Reader Options :> es) => Eff (Reader LoadedConfig : es) a -> Eff es a
runConfigRoot eff = do
    deployment <- asks $ fromMaybe Dev . Options.deployment
    root <- liftIO $ loadHoardConfig deployment
    runReader root eff
