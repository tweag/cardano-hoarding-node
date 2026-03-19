module Hoard.TestHelpers.Database
    ( withCleanTestDatabase
    )
where

import Data.String.Conversions (cs)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Hspec (Spec, SpecWith)

import Atelier.Effects.DB.Config (DBConfig (..), DBPools)
import Atelier.Testing.Database (TmpDbConfig (..))

import Atelier.Testing.Database qualified as Atelier


withCleanTestDatabase :: SpecWith DBPools -> Spec
withCleanTestDatabase =
    Atelier.withCleanTestDatabase
        TmpDbConfig
            { readerUser = "hoard_reader"
            , writerUser = "hoard_writer"
            , schemaName = "hoard"
            , excludedTables = ["schema_metadata"]
            , setupTemplate = runSqitchMigrations
            }


runSqitchMigrations :: DBConfig -> Text -> IO ()
runSqitchMigrations config dbName = do
    let targetUri =
            "db:pg://"
                <> toString config.user
                <> "@/"
                <> toString dbName
                <> "?host="
                <> toString config.host
                <> "&port="
                <> show config.port
        args = ["deploy", targetUri]
    (exitCode, stdoutput, stderroutput) <- readProcessWithExitCode "sqitch" args ""
    case exitCode of
        ExitSuccess -> pure ()
        ExitFailure code ->
            error
                $ cs
                $ "Failed to run sqitch migrations: "
                    <> show code
                    <> "\nstdout: "
                    <> stdoutput
                    <> "\nstderr: "
                    <> stderroutput
