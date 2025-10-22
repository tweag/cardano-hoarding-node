module Integration.DBEffects (spec_DBEffects) where

import Data.Int (Int32)
import Data.Text (Text)
import Effectful (runEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Statement qualified as Statement
import Hasql.Transaction qualified as TX
import Hoard.Effects.DBRead (runDBRead, runQuery)
import Hoard.Effects.DBWrite (runDBWrite, runTransaction)
import Hoard.TestHelpers.Database (TestConfig (..), cleanDatabase, withTestDatabase)
import Hoard.Types.DBConfig (DBPools (..))
import Test.Hspec

spec_DBEffects :: Spec
spec_DBEffects = aroundAll withTestDatabase $ beforeWith cleanAndReturn $ do
  describe "DBRead effect" $ do
    it "can read from the database" $ \config -> do
      result <-
        runEff
          . runErrorNoCallStack @Text
          . runDBRead config.pools.readerPool
          $ runQuery "count-metadata" countMetadataStmt

      result `shouldSatisfy` isRight
      case result of
        Right count -> count `shouldBe` 1 -- init_schema creates one row
        Left err -> expectationFailure $ "Query failed: " <> show err

    it "can read data that was written" $ \config -> do
      -- First write some data
      _ <-
        runEff
          . runErrorNoCallStack @Text
          . runDBWrite config.pools.writerPool
          $ runTransaction "insert-test"
          $ do
            TX.statement () insertTestStmt

      -- Then read it back
      result <-
        runEff
          . runErrorNoCallStack @Text
          . runDBRead config.pools.readerPool
          $ runQuery "count-after-insert" countMetadataStmt

      case result of
        Right count -> count `shouldBe` 2 -- initial + our test row
        Left err -> expectationFailure $ "Query failed: " <> show err

  describe "DBWrite effect" $ do
    it "can write to the database" $ \config -> do
      result <-
        runEff
          . runErrorNoCallStack @Text
          . runDBWrite config.pools.writerPool
          $ runTransaction "insert-test"
          $ do
            TX.statement () insertTestStmt

      result `shouldSatisfy` isRight

    it "can delete from the database" $ \config -> do
      -- First insert
      _ <-
        runEff
          . runErrorNoCallStack @Text
          . runDBWrite config.pools.writerPool
          $ runTransaction "insert"
          $ TX.statement () insertTestStmt

      -- Then delete
      result <-
        runEff
          . runErrorNoCallStack @Text
          . runDBWrite config.pools.writerPool
          $ runTransaction "delete"
          $ TX.statement () deleteTestStmt

      result `shouldSatisfy` isRight

      -- Verify deletion
      countResult <-
        runEff
          . runErrorNoCallStack @Text
          . runDBRead config.pools.readerPool
          $ runQuery "count-after-delete" countMetadataStmt

      case countResult of
        Right count -> count `shouldBe` 1 -- back to just initial row
        Left err -> expectationFailure $ "Count query failed: " <> show err

  describe "Permission separation" $ do
    it "reader pool cannot write to the database" $ \config -> do
      -- Try to write using the reader pool - this should fail
      result <-
        runEff
          . runErrorNoCallStack @Text
          . runDBRead config.pools.readerPool
          $ runQuery "illegal-insert" insertAsSelectStmt

      -- We expect this to fail with a permission error
      result `shouldSatisfy` isLeft

-- Helper to check if Either is Right
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

-- Helper to check if Either is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

-- | Count metadata entries
countMetadataStmt :: Statement.Statement () Int32
countMetadataStmt =
  Statement.Statement
    "SELECT COUNT(*) FROM hoard.schema_metadata"
    E.noParams
    (D.singleRow (D.column (D.nonNullable D.int4)))
    True

-- | Insert test metadata
insertTestStmt :: Statement.Statement () ()
insertTestStmt =
  Statement.Statement
    "INSERT INTO hoard.schema_metadata (version, description) VALUES ('test', 'Integration test entry')"
    E.noParams
    D.noResult
    True

-- | Delete test metadata
deleteTestStmt :: Statement.Statement () ()
deleteTestStmt =
  Statement.Statement
    "DELETE FROM hoard.schema_metadata WHERE version = 'test'"
    E.noParams
    D.noResult
    True

-- | Try to insert as a SELECT (this will fail with reader permissions)
insertAsSelectStmt :: Statement.Statement () Int32
insertAsSelectStmt =
  Statement.Statement
    "INSERT INTO hoard.schema_metadata (version, description) VALUES ('illegal', 'Should fail') RETURNING id"
    E.noParams
    (D.singleRow (D.column (D.nonNullable D.int4)))
    True

-- | Clean database before each test and return the config
cleanAndReturn :: TestConfig -> IO TestConfig
cleanAndReturn config = do
  cleanDatabase config
  pure config
