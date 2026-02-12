module Hoard.Effects.Environment
    ( loadEnv
    , runConfigReader
    , runHandlesReader
    , loadNodeConfig
    , loadProtocolInfo
    , loadTopology
    ) where

import Cardano.Api (File (..), NodeConfig, mkProtocolInfoCardano, readCardanoGenesisConfig, readNodeConfig)
import Data.Aeson (FromJSON (..), eitherDecodeFileStrict)
import Data.Default (def)
import Data.String.Conversions (cs)
import Data.Yaml qualified as Yaml
import Effectful (Eff, IOE, withSeqEffToIO, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Exception (throwIO)
import Effectful.Reader.Static (Reader, asks, runReader)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo)
import Ouroboros.Network.IOManager (IOManager, withIOManager)
import System.Directory (makeAbsolute)
import System.FilePath (takeDirectory, (</>))
import System.IO.Error (userError)
import Prelude hiding (Reader, asks, runReader)

import Hoard.BlockFetch.Config qualified as BlockFetch
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.NodeToNode.Config qualified as NodeToNode
import Hoard.Effects.Options (Options)
import Hoard.Effects.Options qualified as Options
import Hoard.PeerManager.Config qualified as PeerManager
import Hoard.Types.Cardano (CardanoBlock)
import Hoard.Types.DBConfig (DBConfig (..), DBPools, PoolConfig (..), acquireDatabasePools)
import Hoard.Types.Deployment (Deployment (..), deploymentName)
import Hoard.Types.Environment
    ( CardanoNodeIntegrationConfig
    , CardanoProtocolHandles (..)
    , CardanoProtocolsConfig
    , Config (..)
    , Env (..)
    , Handles (..)
    , MonitoringConfig
    , NodeSocketsConfig
    , PeerSnapshotFile (..)
    , ServerConfig (..)
    , Topology (..)
    , TracingConfig
    )
import Hoard.Types.QuietSnake (QuietSnake (..))


-- | Top-level config file structure (YAML)
data ConfigFile = ConfigFile
    { server :: ServerConfig
    , database :: DatabaseConfigFile
    , secretsFile :: String
    , protocolConfigPath :: FilePath
    , nodeSockets :: NodeSocketsConfig
    , logging :: LoggingConfig
    , tracing :: TracingConfig
    , maxFileDescriptors :: Maybe Word32
    , cardanoProtocols :: CardanoProtocolsConfig
    , monitoring :: MonitoringConfig
    , cardanoNodeIntegration :: CardanoNodeIntegrationConfig
    , cardanoProtocolHandles :: CardanoProtocolHandlesConfig
    , peerManager :: PeerManager.Config
    , nodeToNode :: NodeToNode.Config
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake ConfigFile


-- | Database configuration (non-sensitive connection details)
data DatabaseConfigFile = DatabaseConfigFile
    { host :: Text
    , port :: Word16
    , databaseName :: Text
    , pool :: PoolConfig
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake DatabaseConfigFile


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


newtype CardanoProtocolHandlesConfig = CardanoProtocolHandlesConfig
    { blockFetch :: BlockFetch.HandlesConfig
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake CardanoProtocolHandlesConfig


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


-- | Load topology configuration from the same directory as the protocol config
loadTopology :: (IOE :> es) => FilePath -> Eff es (Topology, PeerSnapshotFile)
loadTopology protocolConfigPath = do
    let configDir = takeDirectory protocolConfigPath
        topologyPath = configDir </> "topology.json"

    -- Load topology.json
    topologyResult <- liftIO $ eitherDecodeFileStrict topologyPath
    topologyData <- case topologyResult of
        Left err -> throwIO $ userError $ "Failed to parse " <> topologyPath <> ": " <> err
        Right topology -> pure topology

    -- Load peer-snapshot.json from the same directory
    let peerSnapshotPath = configDir </> topologyData.peerSnapshotFile
    peerSnapshotResult <- liftIO $ eitherDecodeFileStrict peerSnapshotPath
    peerSnapshot <- case peerSnapshotResult of
        Left err -> throwIO $ userError $ "Failed to parse " <> peerSnapshotPath <> ": " <> err
        Right snapshot -> pure (snapshot :: PeerSnapshotFile)

    -- Return both the topology data and the loaded peer snapshot
    pure (topologyData, peerSnapshot)


-- | Convert config types to DBConfig for database connection
toDBConfig :: DatabaseConfigFile -> DBUserCredentials -> DBConfig
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


loadLoggingConfig :: (IOE :> es) => ConfigFile -> Eff es Log.Config
loadLoggingConfig configFile = do
    log <- (>>= readMaybe) <$> lookupEnv "LOG"
    logging <- (>>= readMaybe) <$> lookupEnv "LOGGING"
    debug <-
        (>>= \x -> if x == "0" then Nothing else Just Log.DEBUG)
            <$> lookupEnv "DEBUG"
    let minimumSeverity = fromMaybe configFile.logging.minimumSeverity $ debug <|> logging <|> log
    pure $ def {Log.minimumSeverity}


loadCardanoProtocolHandles :: (Concurrent :> es) => ConfigFile -> Eff es CardanoProtocolHandles
loadCardanoProtocolHandles configFile = do
    blockFetch <- BlockFetch.loadHandles configFile.cardanoProtocolHandles.blockFetch
    pure
        CardanoProtocolHandles
            { blockFetch
            }


-- | Acquire runtime handles
acquireHandles :: (IOE :> es, Concurrent :> es) => IOManager -> ConfigFile -> SecretConfig -> Eff es Handles
acquireHandles ioManager configFile secrets = do
    databaseHost <- lookupNonEmpty "DB_HOST"
    databasePort <- (>>= readMaybe . toString) <$> lookupNonEmpty "DB_PORT"
    databaseName <- lookupNonEmpty "DB_NAME"
    -- Resolve relative paths to absolute for Unix socket connections
    resolvedHost <- liftIO $ makeAbsolute $ toString $ fromMaybe configFile.database.host databaseHost
    let
        database =
            DatabaseConfigFile
                { host = toText resolvedHost
                , port = fromMaybe configFile.database.port databasePort
                , databaseName = fromMaybe configFile.database.databaseName databaseName
                , pool = configFile.database.pool
                }
    let readerConfig = toDBConfig database secrets.database.users.reader
    let writerConfig = toDBConfig database secrets.database.users.writer
    dbPools <- liftIO $ acquireDatabasePools readerConfig writerConfig
    cardanoProtocols <- loadCardanoProtocolHandles configFile

    pure
        Handles
            { ioManager
            , dbPools
            , cardanoProtocols
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
    :: ( Concurrent :> es
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
    (topology, peerSnapshot) <- loadTopology configFile.protocolConfigPath
    logging <- loadLoggingConfig configFile
    handles <- acquireHandles ioManager configFile secrets

    let config =
            Config
                { server = configFile.server
                , protocolInfo
                , nodeConfig
                , nodeSockets = configFile.nodeSockets
                , logging
                , tracing = configFile.tracing
                , maxFileDescriptors = configFile.maxFileDescriptors
                , topology
                , peerSnapshot
                , peerManager = configFile.peerManager
                , cardanoProtocols = configFile.cardanoProtocols
                , monitoring = configFile.monitoring
                , cardanoNodeIntegration = configFile.cardanoNodeIntegration
                , nodeToNode = configFile.nodeToNode
                }
        env = Env {config, handles}

    runReader env eff


runConfigReader
    :: (Reader Env :> es)
    => Eff (Reader PeerManager.Config : Reader Log.Config : Reader Config : es) a
    -> Eff es a
runConfigReader eff = do
    cfg <- asks config
    runReader cfg
        . runReader cfg.logging
        . runReader cfg.peerManager
        $ eff


runHandlesReader :: (Reader Env :> es) => Eff (Reader DBPools : es) a -> Eff es a
runHandlesReader eff = do
    handles <- asks handles
    runReader handles.dbPools eff
