module Hoard.Types.DBConfig
    ( DBConfig (..)
    , DBPools (..)
    , acquireDatabasePool
    , acquireDatabasePools
    , devConfig
    )
where

import Data.Text (Text)
import Data.Word (Word16)

import Hasql.Connection.Setting qualified as Setting
import Hasql.Connection.Setting.Connection qualified as Connection
import Hasql.Connection.Setting.Connection.Param qualified as Param
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as Pool


-- | Database configuration for a single user
data DBConfig = DBConfig
    { host :: Text
    , port :: Word16
    , user :: Text
    , password :: Text
    , databaseName :: Text
    }
    deriving stock (Eq, Show)


-- | Separate connection pools for read and write operations
data DBPools = DBPools
    { readerPool :: Pool.Pool
    , writerPool :: Pool.Pool
    }


-- | Acquire a single database connection pool
acquireDatabasePool :: DBConfig -> IO Pool.Pool
acquireDatabasePool config = do
    -- Pool configuration:
    -- - Size: 10 connections
    -- - Acquisition timeout: 5 seconds
    -- - Aging timeout: 30 minutes (maximal connection lifetime)
    -- - Idleness timeout: 10 minutes (maximal connection idle time)
    let settings =
            [ Pool.staticConnectionSettings
                [ Setting.connection $
                    Connection.params
                        [ Param.host config.host
                        , Param.port config.port
                        , Param.user config.user
                        , Param.password config.password
                        , Param.dbname config.databaseName
                        ]
                ]
            , Pool.size 10
            , Pool.acquisitionTimeout 5 -- (seconds)
            , Pool.agingTimeout (30 * 60) -- (30 minutes in seconds)
            , Pool.idlenessTimeout (10 * 60) -- (10 minutes in seconds)
            ]

    Pool.acquire $ Pool.settings settings


-- | Acquire both read and write connection pools
-- Uses different database users for read-only and read-write operations
acquireDatabasePools
    :: DBConfig
    -- ^ Read-only user config
    -> DBConfig
    -- ^ Read-write user config
    -> IO DBPools
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
        { host = "/home/cgeorgii/code/cardano-hoarding-node/postgres-data"
        , port = 5432
        , user = "hoard_reader"
        , password = ""
        , databaseName = "hoard_dev"
        }
    , -- Read-write user
      DBConfig
        { host = "/home/cgeorgii/code/cardano-hoarding-node/postgres-data"
        , port = 5432
        , user = "hoard_writer"
        , password = ""
        , databaseName = "hoard_dev"
        }
    )
