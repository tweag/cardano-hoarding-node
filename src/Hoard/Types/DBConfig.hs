module Hoard.Types.DBConfig
  ( DBConfig (..),
    DBPools (..),
    Port,
    portToWord16,
    acquireDatabasePool,
    acquireDatabasePools,
    devConfig,
  )
where

import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Word (Word16)
import Hasql.Connection qualified as Connection
import Hasql.Pool qualified as Pool

-- | Database port type
newtype Port = Port Word16
  deriving newtype (Show, Eq, Num)

-- | Convert Port to Word16
portToWord16 :: Port -> Word16
portToWord16 (Port w) = w

-- | Database configuration for a single user
data DBConfig = DBConfig
  { host :: Text,
    port :: Port,
    user :: Text,
    password :: Text,
    databaseName :: Text
  }
  deriving stock (Show, Eq)

-- | Separate connection pools for read and write operations
data DBPools = DBPools
  { readerPool :: Pool.Pool,
    writerPool :: Pool.Pool
  }

-- | Acquire a single database connection pool
acquireDatabasePool :: DBConfig -> IO Pool.Pool
acquireDatabasePool config = do
  let settings =
        Connection.settings
          (TE.encodeUtf8 config.host)
          (portToWord16 config.port)
          (TE.encodeUtf8 config.user)
          (TE.encodeUtf8 config.password)
          (TE.encodeUtf8 config.databaseName)

  -- Pool configuration:
  -- - Size: 10 connections
  -- - Acquisition timeout: 5 seconds
  -- - Aging timeout: 30 minutes (maximal connection lifetime)
  -- - Idleness timeout: 10 minutes (maximal connection idle time)
  Pool.acquire
    10 -- pool size
    5 -- acquisition timeout (seconds)
    (30 * 60) -- aging timeout (30 minutes in seconds)
    (10 * 60) -- idleness timeout (10 minutes in seconds)
    settings

-- | Acquire both read and write connection pools
-- Uses different database users for read-only and read-write operations
acquireDatabasePools ::
  -- | Read-only user config
  DBConfig ->
  -- | Read-write user config
  DBConfig ->
  IO DBPools
acquireDatabasePools readerConfig writerConfig = do
  readerPool <- acquireDatabasePool readerConfig
  writerPool <- acquireDatabasePool writerConfig
  pure $ DBPools {readerPool, writerPool}

-- | Development database configuration
-- Connects to the local postgres instance started with `nix run .#postgres`
-- Uses socket connection in ./postgres-data directory
-- Returns separate configs for reader and writer users
devConfig :: (DBConfig, DBConfig)
devConfig =
  ( -- Read-only user
    DBConfig
      { host = "/home/cgeorgii/code/cardano-hoarding-node/postgres-data",
        port = 5432,
        user = "hoard_reader",
        password = "",
        databaseName = "hoard_dev"
      },
    -- Read-write user
    DBConfig
      { host = "/home/cgeorgii/code/cardano-hoarding-node/postgres-data",
        port = 5432,
        user = "hoard_writer",
        password = "",
        databaseName = "hoard_dev"
      }
  )
