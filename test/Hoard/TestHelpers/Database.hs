module Hoard.TestHelpers.Database
  ( TestConfig (..),
    withTestDatabase,
    cleanDatabase,
  )
where

import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (..))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDv4
import Database.PostgreSQL.Simple.Options (Options (..))
import Database.Postgres.Temp qualified as TmpPostgres
import Hasql.Decoders qualified as Decoders
import Hasql.Pool qualified as Pool
import Hasql.Session (Session, statement)
import Hasql.Statement (Statement (..))
import Hoard.Types.DBConfig (DBConfig (..), DBPools (..), acquireDatabasePool, acquireDatabasePools)
import System.Environment (getEnv)
import System.Exit (ExitCode (..))
import System.IO.Error (catchIOError)
import System.Process (readProcessWithExitCode)

-- | Test configuration including database pools and schema name
data TestConfig = TestConfig
  { pools :: DBPools,
    schemaName :: Text
  }

-- | Run an action with a temporary PostgreSQL database that has migrations applied.
-- The database is automatically cleaned up after the action completes.
--
-- This starts a temporary PostgreSQL server for the test suite and creates a test database.
-- You should clean tables between individual tests using 'cleanDatabase'.
withTestDatabase :: (TestConfig -> IO a) -> IO a
withTestDatabase action = do
  -- Get current system user (PostgreSQL will use this by default when user is not specified)
  currentUser <- catchIOError (getEnv "USER") (const $ pure "postgres")

  -- Start temporary PostgreSQL server
  TmpPostgres.withDbCache $ \dbCache -> do
    result <- TmpPostgres.withConfig (TmpPostgres.cacheConfig dbCache) $ \db -> do
      dbName <- generateUniqueDatabaseName

      -- Get connection info from the temporary server
      let Options {host = Last mHost, port = Last mPort, user = Last mUser} =
            TmpPostgres.toConnectionOptions db
          socketDir = fromMaybe "localhost" mHost
          portNum = fromIntegral (fromMaybe 5432 mPort)
          -- Use the actual tmp-postgres user (or current system user if not specified)
          tmpUser = fromMaybe currentUser mUser

          -- Config using the tmp-postgres default user
          tmpUserConfig =
            DBConfig
              { host = cs socketDir,
                port = portNum,
                user = cs tmpUser,
                password = "",
                databaseName = "postgres"
              }

          -- Admin config for postgres role (we'll create this role)
          adminConfig =
            DBConfig
              { host = cs socketDir,
                port = portNum,
                user = "postgres",
                password = "",
                databaseName = "postgres"
              }

      -- Setup required roles in the temporary PostgreSQL instance
      setupRoles tmpUserConfig

      -- Setup the test database and roles
      bracket
        (setupDatabase adminConfig dbName)
        (const $ cleanupDatabase adminConfig dbName)
        $ \() -> do
          -- Create configs for the test database with hoard users
          let readerConfig =
                DBConfig
                  { host = cs socketDir,
                    port = portNum,
                    user = "hoard_reader",
                    password = "",
                    databaseName = dbName
                  }
              writerConfig =
                DBConfig
                  { host = cs socketDir,
                    port = portNum,
                    user = "hoard_writer",
                    password = "",
                    databaseName = dbName
                  }

          pools <- acquireDatabasePools readerConfig writerConfig
          let testConfig =
                TestConfig
                  { pools = pools,
                    schemaName = "hoard"
                  }
          action testConfig

    either (error . show) pure result

-- | Create required PostgreSQL roles if they don't exist
setupRoles :: DBConfig -> IO ()
setupRoles config = do
  pool <- acquireDatabasePool config
  runOrThrow pool $ do
    -- Create postgres role (superuser) if it doesn't exist
    statement () $ Statement "CREATE ROLE postgres WITH SUPERUSER LOGIN" mempty Decoders.noResult False

-- | Set up a test database: create it and run migrations
setupDatabase :: DBConfig -> Text -> IO ()
setupDatabase adminConfig dbName = do
  pool <- acquireDatabasePool adminConfig

  -- Create the database
  createDatabase pool dbName

  -- Run sqitch migrations
  runSqitchMigrations adminConfig dbName

  -- Grant TRUNCATE privileges to hoard_writer for test cleanup
  grantTruncatePrivileges adminConfig dbName

-- | Create a database using hasql
createDatabase :: Pool.Pool -> Text -> IO ()
createDatabase pool dbName = do
  runOrThrow pool $ do
    statement () $ Statement (cs $ "CREATE DATABASE " <> dbName) mempty Decoders.noResult False

-- | Clean up: drop the test database
cleanupDatabase :: DBConfig -> Text -> IO ()
cleanupDatabase config dbName = do
  pool <- acquireDatabasePool config
  -- Ignore errors during cleanup
  _ <- Pool.use pool $ do
    statement () $ Statement (cs $ "DROP DATABASE IF EXISTS " <> dbName <> " WITH (FORCE)") mempty Decoders.noResult False
  pure ()

-- | Run sqitch migrations against the test database
runSqitchMigrations :: DBConfig -> Text -> IO ()
runSqitchMigrations config dbName = do
  -- Build sqitch connection string based on whether we're using TCP or socket
  let targetUri =
        "db:pg://"
          <> Text.unpack config.user
          <> "@/"
          <> Text.unpack dbName
          <> "?host="
          <> Text.unpack config.host
          <> "&port="
          <> show config.port
      args = ["deploy", targetUri]
  (exitCode, stdout, stderr) <- readProcessWithExitCode "sqitch" args ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure code ->
      error $
        "Failed to run sqitch migrations: "
          <> show code
          <> "\nstdout: "
          <> stdout
          <> "\nstderr: "
          <> stderr

-- | Grant TRUNCATE privileges to hoard_writer for test cleanup
-- This is only needed in test databases for the cleanDatabase function
grantTruncatePrivileges :: DBConfig -> Text -> IO ()
grantTruncatePrivileges config dbName = do
  pool <- acquireDatabasePool (config {databaseName = dbName})
  runOrThrow pool $ do
    statement () $ Statement "GRANT TRUNCATE ON ALL TABLES IN SCHEMA hoard TO hoard_writer" mempty Decoders.noResult False

-- | Clean the database by truncating all tables except system/migration tables.
-- Use this between tests to reset state while keeping the schema and migration data intact.
-- Excludes: schema_metadata (migration tracking)
cleanDatabase :: TestConfig -> IO ()
cleanDatabase config = do
  runOrThrow config.pools.writerPool $ do
    tableNames <- statement () (queryTableNames config.schemaName)
    unless (null tableNames) $ do
      let qualifiedNames = map (\t -> config.schemaName <> "." <> t) tableNames
      truncateTables (Text.intercalate "," qualifiedNames)
  where
    queryTableNames :: Text -> Statement () [Text]
    queryTableNames schema =
      Statement
        (cs $ "SELECT tablename FROM pg_tables WHERE schemaname = '" <> schema <> "' AND tablename != 'schema_metadata'")
        mempty
        (decodeList Decoders.text)
        True

    truncateTables :: Text -> Session ()
    truncateTables tableNames =
      statement () $
        Statement
          (cs $ "TRUNCATE TABLE " <> tableNames <> " CASCADE")
          mempty
          Decoders.noResult
          False

decodeList :: Decoders.Value a -> Decoders.Result [a]
decodeList val = Decoders.rowList $ Decoders.column $ Decoders.nonNullable val

runOrThrow :: Pool.Pool -> Session a -> IO a
runOrThrow pool sess =
  Pool.use pool sess
    >>= \case
      Left e -> error $ "Database operation failed: " <> show e
      Right a -> pure a

-- | Generate a unique valid database name for PostgreSQL (no hyphens, not starting with numbers)
generateUniqueDatabaseName :: IO Text
generateUniqueDatabaseName = do
  uuid <- UUIDv4.nextRandom
  pure $ "test_hoard_" <> Text.replace "-" "_" (UUID.toText uuid)
