module Hoard.TestHelpers.Database
  ( withTestDatabase,
    cleanDatabase,
  )
where

import Control.Exception (bracket)
import Control.Monad (unless)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDv4
import Hasql.Decoders qualified as Decoders
import Hasql.Pool qualified as Pool
import Hasql.Session (Session, statement)
import Hasql.Statement (Statement (..))
import Hoard.Types.DBConfig (DBConfig (..), DBPools, acquireDatabasePools)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

-- | Run an action with a temporary PostgreSQL database that has migrations applied.
-- The database is automatically cleaned up after the action completes.
--
-- This creates the database ONCE for the entire test suite - you should clean
-- tables between individual tests using 'cleanDatabase'.
withTestDatabase :: (DBPools -> IO a) -> IO a
withTestDatabase action = do
  dbName <- generateUniqueDatabaseName

  -- Use socket connection like devConfig
  let adminConfig =
        DBConfig
          { host = "/home/cgeorgii/code/cardano-hoarding-node/postgres-data",
            port = 5432,
            user = "postgres",
            password = "",
            databaseName = "postgres"
          }

  bracket
    (setupDatabase adminConfig dbName)
    (const $ cleanupDatabase adminConfig dbName)
    $ \() -> do
      -- Create configs for the test database with hoard users
      let readerConfig =
            DBConfig
              { host = "/home/cgeorgii/code/cardano-hoarding-node/postgres-data",
                port = 5432,
                user = "hoard_reader",
                password = "",
                databaseName = dbName
              }
          writerConfig =
            DBConfig
              { host = "/home/cgeorgii/code/cardano-hoarding-node/postgres-data",
                port = 5432,
                user = "hoard_writer",
                password = "",
                databaseName = dbName
              }

      pools <- acquireDatabasePools readerConfig writerConfig
      action pools

-- | Set up a test database: create it and run migrations
setupDatabase :: DBConfig -> Text -> IO ()
setupDatabase adminConfig dbName = do
  -- Create the database
  createDatabase adminConfig dbName

  -- Run sqitch migrations
  runSqitchMigrations dbName

-- | Create a database using psql
createDatabase :: DBConfig -> Text -> IO ()
createDatabase config dbName = do
  let connStr = makeConnectionString config
      args = [cs connStr, "-c", "CREATE DATABASE " <> Text.unpack dbName]
  (exitCode, stdout, stderr) <- readProcessWithExitCode "psql" args ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure code ->
      error $
        "Failed to create test database: "
          <> show code
          <> "\nstdout: "
          <> stdout
          <> "\nstderr: "
          <> stderr

-- | Clean up: drop the test database
cleanupDatabase :: DBConfig -> Text -> IO ()
cleanupDatabase config dbName = do
  let connStr = makeConnectionString config
      -- Use FORCE to drop even if there are active connections
      args = [cs connStr, "-c", "DROP DATABASE IF EXISTS " <> Text.unpack dbName <> " WITH (FORCE)"]
  (exitCode, _, _) <- readProcessWithExitCode "psql" args ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> pure () -- Ignore errors during cleanup

-- | Run sqitch migrations against the test database
runSqitchMigrations :: Text -> IO ()
runSqitchMigrations dbName = do
  -- Use socket connection with postgres user, like in sqitch.conf
  -- Format: db:pg://user@/database
  let targetUri = "db:pg://postgres@/" <> Text.unpack dbName
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

-- | Create a PostgreSQL connection string for psql from DBConfig
-- Uses the host as a socket directory if it starts with /
makeConnectionString :: DBConfig -> Text
makeConnectionString config =
  if "/" `Text.isPrefixOf` config.host
    then -- Socket connection
      "postgresql:///"
        <> config.databaseName
        <> "?host="
        <> config.host
        <> "&user="
        <> config.user
    else -- TCP connection
      "postgresql://"
        <> config.user
        <> "@"
        <> config.host
        <> ":"
        <> Text.pack (show config.port)
        <> "/"
        <> config.databaseName

-- | Clean the database by truncating all tables except system/migration tables.
-- Use this between tests to reset state while keeping the schema and migration data intact.
-- Excludes: schema_metadata (migration tracking)
cleanDatabase :: Pool.Pool -> IO ()
cleanDatabase pool = do
  runOrThrow pool $ do
    tableNames <- statement () queryTableNames
    unless (null tableNames) $
      truncateTables (Text.intercalate "," tableNames)
  where
    queryTableNames :: Statement () [Text]
    queryTableNames =
      Statement
        "SELECT tablename FROM pg_tables WHERE schemaname = 'hoard' AND tablename != 'schema_metadata'"
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
