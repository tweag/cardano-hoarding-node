{-# LANGUAGE UndecidableInstances #-}

module Hoard.Config.Loader
    ( loadConfig
    )
where

import Control.Concurrent.Chan.Unagi (newChan)
import Control.Exception (throwIO)
import Data.Aeson (FromJSON (..))
import Ouroboros.Network.IOManager (IOManager)
import System.FilePath ((</>))
import System.IO.Error (userError)

import Data.Yaml qualified as Yaml

import Hoard.Effects (Config (..), ServerConfig (..))
import Hoard.Effects.Log qualified as Log
import Hoard.Types.DBConfig (DBConfig (..), PoolConfig (..), acquireDatabasePools)
import Hoard.Types.Environment (Environment, environmentName)
import Hoard.Types.QuietSnake (QuietSnake (..))


-- | Top-level config file structure (YAML)
data ConfigFile = ConfigFile
    { server :: ServerConfig
    , database :: DatabaseConfig
    , secretsFile :: String
    , protocolConfigPath :: FilePath
    , logging :: LoggingConfig
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON)


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


-- | Load the full application configuration for a given environment
-- Loads both the public config YAML and the secrets YAML file
loadConfig :: IOManager -> Environment -> IO Config
loadConfig ioManager env = do
    let envName = toString $ environmentName env

    -- Load non-sensitive config from YAML
    configFile <- loadYaml @ConfigFile $ "config" </> envName <> ".yaml"
    -- Load secrets from YAML (path specified in config file)
    -- Note: In production, this file should be decrypted by sops first
    secrets <- loadYaml @SecretConfig $ configFile.secretsFile

    -- Create DB configs by combining database config with user credentials
    let readerConfig = toDBConfig configFile.database secrets.database.users.reader
    let writerConfig = toDBConfig configFile.database secrets.database.users.writer

    -- Acquire database pools
    dbPools <- acquireDatabasePools readerConfig writerConfig

    logging <- do
        log <- (>>= readMaybe) <$> lookupEnv "LOG"
        logging <- (>>= readMaybe) <$> lookupEnv "LOGGING"
        debug <-
            (>>= \x -> if x == "0" then Nothing else Just Log.DEBUG)
                <$> lookupEnv "DEBUG"
        let minimumSeverity = fromMaybe configFile.logging.minimumSeverity $ debug <|> logging <|> log
        pure $ Log.defaultConfig {Log.minimumSeverity}

    -- Create pub/sub channel
    (inChan, _) <- newChan

    pure
        Config
            { ioManager
            , dbPools
            , inChan
            , server = configFile.server
            , protocolConfigPath = configFile.protocolConfigPath
            , logging
            }


loadYaml :: (FromJSON a) => String -> IO a
loadYaml path = do
    result <- Yaml.decodeFileEither path
    case result of
        Left err -> throwIO $ userError $ "Failed to parse " <> path <> ": " <> show err
        Right configFile -> pure configFile
