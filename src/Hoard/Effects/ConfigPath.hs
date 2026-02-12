module Hoard.Effects.ConfigPath
    ( ConfigPath
    , runConfigPath
    , loadYaml
    ) where

import Data.Aeson (FromJSON (..))
import Data.String.Conversions (cs)
import Data.Yaml qualified as Yaml
import Effectful (Eff, IOE, (:>))
import Effectful.Exception (throwIO)
import Effectful.Reader.Static (Reader, asks, runReader)
import System.FilePath ((</>))
import System.IO.Error (userError)
import Prelude hiding (Reader, asks, runReader)

import Hoard.Effects.Options (Options)
import Hoard.Effects.Options qualified as Options
import Hoard.Types.Deployment (Deployment (..), deploymentName)
import Hoard.Types.JsonReadShow (JsonReadShow (..))


newtype ConfigPath = ConfigPath FilePath
    deriving (Eq, Read, Show) via (FilePath)
    deriving (FromJSON) via (JsonReadShow ConfigPath)


runConfigPath :: (Reader Options :> es) => Eff (Reader ConfigPath : es) a -> Eff es a
runConfigPath eff = do
    deployment <- asks $ fromMaybe Dev . Options.deployment
    let configPath = ConfigPath $ "config" </> cs (deploymentName deployment) <> ".yaml"
    runReader configPath eff


loadYaml :: forall a es. (IOE :> es) => (FromJSON a) => ConfigPath -> Eff es a
loadYaml (ConfigPath path) = do
    result <- liftIO $ Yaml.decodeFileEither path
    case result of
        Left err -> throwIO $ userError $ "Failed to parse " <> path <> ": " <> show err
        Right configFile -> pure configFile
