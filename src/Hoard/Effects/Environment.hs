module Hoard.Effects.Environment
    ( loadEnv
    , runConfigReader
    , runHandlesReader
    , loadNodeConfig
    , loadProtocolInfo
    ) where

import Cardano.Api (File (..), NodeConfig, mkProtocolInfoCardano, readCardanoGenesisConfig, readNodeConfig)
import Data.Aeson (FromJSON (..))
import Data.Dynamic (Dynamic)
import Data.String.Conversions (cs)
import Data.Yaml qualified as Yaml
import Effectful (Eff, IOE, withSeqEffToIO, (:>))
import Effectful.Exception (throwIO)
import Effectful.Reader.Static (Reader, asks, runReader)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo)
import Ouroboros.Network.IOManager (IOManager, withIOManager)
import System.FilePath ((</>))
import System.IO.Error (userError)
import Prelude hiding (Reader, asks, runReader)

import Hoard.Effects.Chan (Chan, InChan)
import Hoard.Effects.Chan qualified as Chan
import Hoard.Effects.Options (Options)
import Hoard.Effects.Options qualified as Options
import Hoard.Types.Cardano (CardanoBlock)
import Hoard.Types.DBConfig (DBConfig (..), DBPools, PoolConfig (..), acquireDatabasePools)
import Hoard.Types.Deployment (Deployment (..), deploymentName)
import Hoard.Types.Environment (Config (..), Env (..), Handles (..), LogConfig, NodeSocketsConfig, ServerConfig (..))
import Hoard.Types.Environment qualified as Log (LogConfig (..), Severity (..), defaultLogConfig)
import Hoard.Types.QuietSnake (QuietSnake (..))


-- | Top-level config file structure (YAML)
data ConfigFile = ConfigFile
    { server :: ServerConfig
    , database :: DatabaseConfig
    , secretsFile :: String
    , protocolConfigPath :: FilePath
    , nodeSockets :: NodeSocketsConfig
    , logging :: LoggingConfig
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake ConfigFile


-- | Database configuration (non-sensitive connection details)
data DatabaseConfig = DatabaseConfig
    { host :: Text
    , port :: Word16
    , databaseName :: Text
    , pool :: PoolConfig
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake DatabaseConfig


-- | Secret configuration (sensitive values)
data SecretConfig = SecretConfig
    { database :: DBSecrets
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake SecretConfig


-- | Database secrets (passwords and sensitive auth data)
data DBSecrets = DBSecrets
    { users :: DBUserSecrets
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake DBSecrets


-- | Database user secrets for both reader and writer
data DBUserSecrets = DBUserSecrets
    { reader :: DBUserCredentials
    , writer :: DBUserCredentials
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake DBUserSecrets


-- | Database user credentials (username and password)
data DBUserCredentials = DBUserCredentials
    { user :: Text
    , password :: Text
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake DBUserCredentials


loadNodeConfig :: (IOE :> es) => FilePath -> Eff es NodeConfig
loadNodeConfig configPath = do
    let configFile = File configPath
    nodeConfigResult <- runExceptT $ readNodeConfig configFile
    case nodeConfigResult of
        Left err -> error $ "Failed to read node config: " <> err
        Right cfg -> pure cfg


-- | Load the Cardano protocol info from config files.
-- This is needed to get the CodecConfig for creating proper codecs.
loadProtocolInfo :: (IOE :> es) => NodeConfig -> Eff es (ProtocolInfo CardanoBlock)
loadProtocolInfo nodeConfig = do
    -- Load GenesisConfig
    genesisConfigResult <- runExceptT $ readCardanoGenesisConfig nodeConfig
    genesisConfig <- case genesisConfigResult of
        Left err -> error $ "Failed to read genesis config: " <> show err
        Right cfg -> pure cfg

    -- Create ProtocolInfo
    let (protocolInfo, _mkBlockForging) = mkProtocolInfoCardano genesisConfig
    pure protocolInfo


-- | Convert config types to DBConfig for database connection
toDBConfig :: DatabaseConfig -> DBUserCredentials -> DBConfig
toDBConfig dbCfg credentials =
    DBConfig
        { host = dbCfg.host
        , port = dbCfg.port
        , user = credentials.user
        , password = credentials.password
        , databaseName = dbCfg.databaseName
        , pool = dbCfg.pool
        }


data LoggingConfig = LoggingConfig
    { minimumSeverity :: Log.Severity
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake LoggingConfig


loadLoggingConfig :: (IOE :> es) => ConfigFile -> Eff es Log.LogConfig
loadLoggingConfig configFile = do
    log <- (>>= readMaybe) <$> lookupEnv "LOG"
    logging <- (>>= readMaybe) <$> lookupEnv "LOGGING"
    debug <-
        (>>= \x -> if x == "0" then Nothing else Just Log.DEBUG)
            <$> lookupEnv "DEBUG"
    let minimumSeverity = fromMaybe configFile.logging.minimumSeverity $ debug <|> logging <|> log
    pure $ Log.defaultLogConfig {Log.minimumSeverity}


-- | Acquire runtime handles
acquireHandles :: (IOE :> es, Chan :> es) => IOManager -> ConfigFile -> SecretConfig -> Eff es Handles
acquireHandles ioManager configFile secrets = do
    databaseHost <- lookupNonEmpty "DB_HOST"
    databasePort <- (>>= readMaybe . toString) <$> lookupNonEmpty "DB_PORT"
    databaseName <- lookupNonEmpty "DB_NAME"
    let
        database =
            DatabaseConfig
                { host = fromMaybe configFile.database.host databaseHost
                , port = fromMaybe configFile.database.port databasePort
                , databaseName = fromMaybe configFile.database.databaseName databaseName
                , pool = configFile.database.pool
                }
    let readerConfig = toDBConfig database secrets.database.users.reader
    let writerConfig = toDBConfig database secrets.database.users.writer
    dbPools <- liftIO $ acquireDatabasePools readerConfig writerConfig
    (inChan, _) <- Chan.newChan

    pure
        Handles
            { ioManager
            , dbPools
            , inChan
            }


lookupNonEmpty :: (MonadIO m) => Text -> m (Maybe Text)
lookupNonEmpty n =
    lookupEnv (toString n) <&> \case
        Just [] -> Nothing
        s -> toText <$> s


loadYaml :: forall a es. (IOE :> es) => (FromJSON a) => String -> Eff es a
loadYaml path = do
    result <- liftIO $ Yaml.decodeFileEither path
    case result of
        Left err -> throwIO $ userError $ "Failed to parse " <> path <> ": " <> show err
        Right configFile -> pure configFile


-- | Load the full application environment for a given deployment
-- Loads both the public config YAML and the secrets YAML file,
-- then acquires all necessary runtime handles
loadEnv
    :: ( Chan :> es
       , IOE :> es
       , Reader Options :> es
       )
    => Eff (Reader Env : es) a
    -> Eff es a
loadEnv eff = withSeqEffToIO \unlift -> withIOManager \ioManager -> unlift do
    deployment <- asks $ fromMaybe Dev . Options.deployment
    putTextLn $ "Loading configuration for deployment: " <> show deployment
    configFile <- loadYaml @ConfigFile $ "config" </> cs (deploymentName deployment) <> ".yaml"
    secrets <- loadYaml @SecretConfig $ configFile.secretsFile
    nodeConfig <- loadNodeConfig configFile.protocolConfigPath
    protocolInfo <- loadProtocolInfo nodeConfig
    logging <- loadLoggingConfig configFile
    handles <- acquireHandles ioManager configFile secrets

    let config =
            Config
                { server = configFile.server
                , protocolInfo
                , nodeConfig
                , nodeSockets = configFile.nodeSockets
                , logging
                }
        env = Env {config, handles}

    runReader env eff


runConfigReader :: (Reader Env :> es) => Eff (Reader LogConfig : Reader Config : es) a -> Eff es a
runConfigReader eff = do
    cfg <- asks config
    runReader cfg
        . runReader cfg.logging
        $ eff


runHandlesReader :: (Reader Env :> es) => Eff (Reader (InChan Dynamic) : Reader DBPools : es) a -> Eff es a
runHandlesReader eff = do
    handles <- asks handles
    runReader handles.dbPools
        . runReader handles.inChan
        $ eff
