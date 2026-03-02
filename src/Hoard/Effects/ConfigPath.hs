{-# LANGUAGE AllowAmbiguousTypes #-}

module Hoard.Effects.ConfigPath
    ( ConfigPath
    , runConfigPath
    , loadYaml
    , AtKey (..)
    , runConfig
    ) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.String.Conversions (cs)
import Effectful (IOE)
import Effectful.Exception (throwIO)
import Effectful.Reader.Static (Reader, asks, runReader)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import System.FilePath ((</>))
import System.IO.Error (userError)

import Data.Yaml qualified as Yaml

import Hoard.Effects.Options (Options)
import Hoard.Types.Deployment (Deployment (..), deploymentName)

import Hoard.Effects.Options qualified as Options


newtype ConfigPath = ConfigPath FilePath
    deriving (Eq, FromJSON, Read, Show) via (FilePath)


runConfigPath :: (Reader Options :> es) => Eff (Reader ConfigPath : es) a -> Eff es a
runConfigPath eff = do
    path <- configPath
    runReader path eff


configPath :: (Reader Options :> es) => Eff es ConfigPath
configPath = do
    deployment <- asks $ fromMaybe Dev . Options.deployment
    pure $ ConfigPath $ "config" </> cs (deploymentName deployment) <> ".yaml"


newtype AtKey (key :: Symbol) a = AtKey a


instance (FromJSON a, KnownSymbol key) => FromJSON (AtKey key a) where
    parseJSON = withObject "config" $ \o ->
        AtKey <$> o .: fromString (symbolVal (Proxy :: Proxy key))


runConfig
    :: forall key r es a
     . (FromJSON r, IOE :> es, KnownSymbol key, Reader Options :> es)
    => Eff (Reader r : es) a
    -> Eff es a
runConfig eff = do
    path <- configPath
    AtKey r <- loadYaml @(AtKey key r) path
    runReader r eff


loadYaml :: forall a es. (FromJSON a, IOE :> es) => ConfigPath -> Eff es a
loadYaml (ConfigPath path) = do
    result <- liftIO $ Yaml.decodeFileEither path
    case result of
        Left err -> throwIO $ userError $ "Failed to parse " <> path <> ": " <> show err
        Right configFile -> pure configFile
