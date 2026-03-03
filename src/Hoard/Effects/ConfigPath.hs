{-# LANGUAGE AllowAmbiguousTypes #-}

module Hoard.Effects.ConfigPath
    ( ConfigPath (..)
    , runConfigPath
    , loadYaml
    , AtKey (..)
    , runConfig
    , withHotReload
    ) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.String.Conversions (cs)
import Effectful (IOE)
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Exception (throwIO, try)
import Effectful.Reader.Static (Reader, ask, asks, runReader)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import System.Directory (getModificationTime)
import System.FilePath ((</>))
import System.IO.Error (userError)

import Data.Yaml qualified as Yaml

import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Options (Options)
import Hoard.Types.Deployment (Deployment (..), deploymentName)

import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Log qualified as Log
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


data ReloadRequested = ReloadRequested
    deriving (Show)
    deriving anyclass (Exception)


-- | Run a computation, restarting it whenever the config file changes on disk.
--
-- Opens a Ki sub-scope (via 'Conc.scoped') for the computation and forks a
-- background thread that polls the config file's modification time once per
-- second. When a change is detected, it throws 'ReloadRequested' into the
-- scope, which kills all threads cleanly and restarts the whole block —
-- re-running every 'runConfig' call inside it with fresh values.
withHotReload
    :: (Conc :> es, Concurrent :> es, IOE :> es, Log :> es, Reader ConfigPath :> es)
    => Eff es a
    -> Eff es ()
withHotReload action = do
    ConfigPath path <- ask
    loop path
  where
    loop path = do
        _ <- try @ReloadRequested do
            Conc.scoped do
                Conc.fork_ (watchFile path)
                action
        loop path

    watchFile path = do
        mtime0 <- liftIO $ getModificationTime path
        forever do
            threadDelay 1_000_000
            mtime <- liftIO $ getModificationTime path
            when (mtime /= mtime0) do
                Log.info $ "Config file changed, reloading: " <> toText path
                throwIO ReloadRequested
