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
import Effectful (Eff, IOE, withSeqEffToIO, (:>))
import Effectful.Exception (throwIO)
import Effectful.Reader.Static (Reader, ask, asks, runReader)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo)
import Ouroboros.Network.IOManager (IOManager, withIOManager)
import System.Directory (makeAbsolute)
import System.FilePath (takeDirectory, (</>))
import System.IO.Error (userError)
import Prelude hiding (Reader, ask, asks, runReader)

import Hoard.Effects.ConfigPath (ConfigPath, loadYaml)
import Hoard.Types.Cardano (CardanoBlock)
import Hoard.Types.DBConfig (DBConfig (..), DBPools, PoolConfig (..), acquireDatabasePools)
import Hoard.Types.Environment
    ( CardanoNodeIntegrationConfig
    , CardanoProtocolsConfig
    , Config (..)
    , Env (..)
    , Handles (..)
    , NodeSocketsConfig
    , PeerSnapshotFile (..)
    , ServerConfig (..)
    , Topology (..)
    , TracingConfig
    )
import Hoard.Types.QuietSnake (QuietSnake (..))

import Hoard.Effects.Log qualified as Log
import Hoard.Effects.NodeToNode.Config qualified as NodeToNode
import Hoard.Effects.Quota.Config qualified as Quota
import Hoard.PeerManager.Config qualified as PeerManager


-- | Top-level config file structure (YAML)
data ConfigFile = ConfigFile
    { server :: ServerConfig
    , database :: DatabaseConfigFile
    , secretsFile :: ConfigPath
    , protocolConfigPath :: FilePath
    , nodeSockets :: NodeSocketsConfig
    , logging :: LoggingConfig
    , tracing :: TracingConfig
    , maxFileDescriptors :: Maybe Word32
    , cardanoProtocols :: CardanoProtocolsConfig
    , cardanoNodeIntegration :: CardanoNodeIntegrationConfig
    , peerManager :: PeerManager.Config
    , nodeToNode :: NodeToNode.Config
    , quota :: Quota.Config
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


-- | Acquire runtime handles
acquireHandles :: (IOE :> es) => IOManager -> ConfigFile -> SecretConfig -> Eff es Handles
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

    pure
        Handles
            { ioManager
            , dbPools
            }


lookupNonEmpty :: (MonadIO m) => Text -> m (Maybe Text)
lookupNonEmpty n =
    lookupEnv (toString n) <&> \case
        Just [] -> Nothing
        s -> toText <$> s


-- | Load the full application environment for a given deployment
-- Loads both the public config YAML and the secrets YAML file,
-- then acquires all necessary runtime handles
loadEnv
    :: ( IOE :> es
       , Reader ConfigPath :> es
       )
    => Eff (Reader Env : es) a
    -> Eff es a
loadEnv eff = withSeqEffToIO \unlift -> withIOManager \ioManager -> unlift do
    configPath <- ask @ConfigPath
    putTextLn $ "Loading configuration from: " <> show configPath
    configFile <- loadYaml @ConfigFile configPath
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
                , cardanoNodeIntegration = configFile.cardanoNodeIntegration
                , nodeToNode = configFile.nodeToNode
                , quota = configFile.quota
                }
        env = Env {config, handles}

    runReader env eff


runConfigReader
    :: (Reader Env :> es)
    => Eff (Reader Quota.Config : Reader NodeConfig : Reader PeerManager.Config : Reader Log.Config : Reader Config : es) a
    -> Eff es a
runConfigReader eff = do
    cfg <- asks config
    runReader cfg
        . runReader cfg.logging
        . runReader cfg.peerManager
        . runReader cfg.nodeConfig
        . runReader cfg.quota
        $ eff


runHandlesReader :: (Reader Env :> es) => Eff (Reader DBPools : es) a -> Eff es a
runHandlesReader eff = do
    handles <- asks handles
    runReader handles.dbPools eff
