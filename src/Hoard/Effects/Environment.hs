module Hoard.Effects.Environment
    ( loadEnv
    , loadNodeConfig
    , loadProtocolInfo
    , loadTopology
    ) where

import Cardano.Api (File (..), NodeConfig, mkProtocolInfoCardano, readCardanoGenesisConfig, readNodeConfig)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (FromJSON (..), Value, eitherDecodeFileStrict)
import Effectful (IOE, withSeqEffToIO)
import Effectful.Exception (throwIO)
import Effectful.Reader.Static (Reader, ask, runReader)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo)
import Ouroboros.Network.IOManager (IOManager, withIOManager)
import System.Directory (doesDirectoryExist, makeAbsolute)
import System.FilePath (takeDirectory, (</>))
import System.IO.Error (userError)

import Data.Aeson qualified as Aeson

import Hoard.Effects.ConfigPath (LoadedConfig (..))
import Hoard.Types.Cardano (CardanoBlock)
import Hoard.Types.DBConfig (DBConfig (..), DBPools, PoolConfig (..), acquireDatabasePools)
import Hoard.Types.Environment
    ( PeerSnapshotFile (..)
    , Topology (..)
    )
import Hoard.Types.QuietSnake (QuietSnake (..))


-- | Top-level config file structure — only the fields needed to bootstrap
-- the application environment. Component-specific configs are loaded separately.
-- Secrets (database passwords) are merged in from secrets/{env}.yaml by the config loader.
data ConfigFile = ConfigFile
    { database :: DatabaseConfigFile
    , protocolConfigPath :: FilePath
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


-- | Decode a value from the root config Value, throwing on parse failure.
decodeRoot :: forall a es. (FromJSON a) => Value -> Eff es a
decodeRoot v = case Aeson.fromJSON v of
    Aeson.Success a -> pure a
    Aeson.Error e -> throwIO $ userError $ "Failed to parse root config: " <> e


-- | Load the full application environment for a given deployment.
-- The root config Value has already been assembled from config/{env}.yaml,
-- secrets/{env}.yaml, and environment variables by the config loader.
--
-- Provides individual Reader effects for:
-- - IOManager (network IO manager)
-- - ProtocolInfo CardanoBlock (Cardano protocol info)
-- - NodeConfig (Cardano node config)
-- - PeerSnapshotFile (bootstrap peer snapshot)
-- - DBPools (database connection pools)
loadEnv
    :: ( IOE :> es
       , Reader LoadedConfig :> es
       )
    => Eff (Reader IOManager : Reader (ProtocolInfo CardanoBlock) : Reader NodeConfig : Reader PeerSnapshotFile : Reader DBPools : es) a
    -> Eff es a
loadEnv eff = withSeqEffToIO \unlift -> withIOManager \ioManager -> unlift do
    LoadedConfig root <- ask
    configFile <- decodeRoot @ConfigFile root
    secrets <- decodeRoot @SecretConfig root
    nodeConfig <- loadNodeConfig configFile.protocolConfigPath
    protocolInfo <- loadProtocolInfo nodeConfig
    (_, peerSnapshot) <- loadTopology configFile.protocolConfigPath

    resolvedHost <- liftIO $ do
        let rawHost = toString configFile.database.host
        absPath <- makeAbsolute rawHost
        isSocketDir <- doesDirectoryExist absPath
        pure $ if isSocketDir then absPath else rawHost
    let db = configFile.database
        database =
            DatabaseConfigFile
                { host = toText resolvedHost
                , port = db.port
                , databaseName = db.databaseName
                , pool = db.pool
                }
    let readerConfig = toDBConfig database secrets.database.users.reader
    let writerConfig = toDBConfig database secrets.database.users.writer
    dbPools <- liftIO $ acquireDatabasePools readerConfig writerConfig

    runReader dbPools
        . runReader peerSnapshot
        . runReader nodeConfig
        . runReader protocolInfo
        . runReader ioManager
        $ eff
