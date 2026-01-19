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
import Data.String.Conversions (cs)
import Data.Time.Clock (NominalDiffTime)
import Data.Yaml qualified as Yaml
import Effectful (Eff, IOE, withSeqEffToIO, (:>))
import Effectful.Concurrent.QSem (Concurrent, QSem, newQSem)
import Effectful.Exception (throwIO)
import Effectful.Reader.Static (Reader, asks, runReader)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo)
import Ouroboros.Network.IOManager (IOManager, withIOManager)
import Ouroboros.Network.Mux (MiniProtocolLimits (..))
import Ouroboros.Network.NodeToNode (MiniProtocolParameters (..), defaultMiniProtocolParameters)
import System.FilePath (takeDirectory, (</>))
import System.IO.Error (userError)
import Prelude hiding (Reader, asks, runReader)

import Hoard.Effects.Options (Options)
import Hoard.Effects.Options qualified as Options
import Hoard.Types.Cardano (CardanoBlock)
import Hoard.Types.DBConfig (DBConfig (..), DBPools, PoolConfig (..), acquireDatabasePools)
import Hoard.Types.Deployment (Deployment (..), deploymentName)
import Hoard.Types.Environment (Config (..), Env (..), Handles (..), LogConfig, MiniProtocolConfig (..), NodeSocketsConfig, PeerSnapshotFile (..), ServerConfig (..), Topology (..))
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
    , maxFileDescriptors :: Maybe Word32
    , peerFailureCooldownSeconds :: NominalDiffTime
    , blockFetchQsemLimit :: Maybe Int
    , miniProtocols :: Maybe MiniProtocolConfigFile
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


data MiniProtocolConfigFile = MiniProtocolConfigFile
    { blockFetch :: Maybe MiniProtocolLimitsConfigFile
    , chainSync :: Maybe MiniProtocolLimitsConfigFile
    , txSubmission :: Maybe MiniProtocolLimitsConfigFile
    , keepAlive :: Maybe MiniProtocolLimitsConfigFile
    , peerSharing :: Maybe MiniProtocolLimitsConfigFile
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake MiniProtocolConfigFile


newtype MiniProtocolLimitsConfigFile = MiniProtocolLimitsConfigFile
    { maximumIngressQueue :: Int
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake MiniProtocolLimitsConfigFile


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


loadBlockFetchQSem :: (Concurrent :> es, IOE :> es) => ConfigFile -> Eff es QSem
loadBlockFetchQSem configFile = do
    qsemLimitEnv <- (>>= readMaybe) <$> lookupEnv "BLOCK_FETCH_QSEM_LIMIT"
    let qsemLimit = fromMaybe 10 $ qsemLimitEnv <|> configFile.blockFetchQsemLimit
    newQSem qsemLimit


loadMiniProtocolConfigs :: Maybe MiniProtocolConfigFile -> Eff es MiniProtocolConfig
loadMiniProtocolConfigs configFile = do
    pure $
        MiniProtocolConfig
            { blockFetch = mk defaultBlockFetch (.blockFetch)
            , chainSync = mk defaultChainSync (.chainSync)
            , txSubmission = mk defaultTxSubmission (.txSubmission)
            , keepAlive = mk defaultKeepAlive (.keepAlive)
            , peerSharing = mk defaultPeerSharing (.peerSharing)
            }
  where
    defaultBlockFetch =
        MiniProtocolLimits
            { -- 384KiB, taken from Cardano's high watermark limit for block
              -- fetch ingress queue limit.
              maximumIngressQueue = 402653184
            }
    defaultChainSync =
        MiniProtocolLimits
            { maximumIngressQueue = fromIntegral $ chainSyncPipeliningHighMark params * 4
            }
    defaultTxSubmission =
        MiniProtocolLimits
            { maximumIngressQueue = fromIntegral $ txSubmissionMaxUnacked params
            }
    defaultKeepAlive =
        MiniProtocolLimits
            { maximumIngressQueue = 1000 -- Reasonable default for keep alive
            }
    defaultPeerSharing =
        MiniProtocolLimits
            { maximumIngressQueue = 1000 -- Reasonable default for peer sharing
            }
    params = defaultMiniProtocolParameters
    mk def getter = fromMaybe def $ fmap (MiniProtocolLimits . (.maximumIngressQueue)) . getter =<< configFile


-- | Acquire runtime handles
acquireHandles :: (IOE :> es) => IOManager -> ConfigFile -> SecretConfig -> Eff es Handles
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
    blockFetchQSem <- loadBlockFetchQSem configFile
    miniProtocolConfig <- loadMiniProtocolConfigs configFile.miniProtocols

    let config =
            Config
                { server = configFile.server
                , protocolInfo
                , nodeConfig
                , nodeSockets = configFile.nodeSockets
                , logging
                , maxFileDescriptors = configFile.maxFileDescriptors
                , topology
                , peerSnapshot
                , peerFailureCooldown = configFile.peerFailureCooldownSeconds
                , blockFetchQSem
                , miniProtocolConfig
                }
        env = Env {config, handles}

    runReader env eff


runConfigReader :: (Reader Env :> es) => Eff (Reader LogConfig : Reader Config : es) a -> Eff es a
runConfigReader eff = do
    cfg <- asks config
    runReader cfg
        . runReader cfg.logging
        $ eff


runHandlesReader :: (Reader Env :> es) => Eff (Reader DBPools : es) a -> Eff es a
runHandlesReader eff = do
    handles <- asks handles
    runReader handles.dbPools eff
