module Integration.Hoard.DB.HeaderPersistenceSpec (spec_HeaderPersistence) where

import Data.Time (getCurrentTime)
import Effectful (runEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Test.Hspec

import Hasql.Statement (Statement)
import Rel8 qualified

import Hoard.DB.Schemas.HeaderReceipts qualified as HeaderReceiptsSchema
import Hoard.DB.Schemas.Headers qualified as HeadersSchema
import Hoard.DB.Schemas.Peers qualified as PeersSchema
import Hoard.Data.Header (BlockHash (..), Header (..))
import Hoard.Data.Peer (PeerAddress (..))
import Hoard.Effects.DBRead (runDBRead, runQuery)
import Hoard.Effects.DBWrite (runDBWrite)
import Hoard.Effects.HeaderRepo (runHeaderRepo, upsertHeader)
import Hoard.Effects.Log qualified as Log
import Hoard.TestHelpers.Database (TestConfig (..), withCleanTestDatabase)
import Hoard.Types.DBConfig (DBPools (..))
import Text.Read (read)


spec_HeaderPersistence :: Spec
spec_HeaderPersistence = do
    let runWrite config action =
            runEff
                . Log.runLog Log.defaultConfig
                . runErrorNoCallStack @Text
                . runDBRead config.pools.readerPool
                . runDBWrite config.pools.writerPool
                . runHeaderRepo
                $ action

    let runRead config action =
            runEff
                . runErrorNoCallStack @Text
                . runDBRead config.pools.readerPool
                $ action

    withCleanTestDatabase $ describe "Header persistence (database)" $ do
        it "persists a header and creates peer if needed" $ \config -> do
            now <- getCurrentTime
            let peerAddr = PeerAddress (read "192.168.1.1") 3001
            let header =
                    Header
                        { hash = BlockHash "abc123def456"
                        , slotNumber = 12345
                        , blockNumber = 100
                        , firstSeenAt = now
                        }

            -- Upsert header (should also create peer)
            result <- runWrite config $ upsertHeader header peerAddr now

            result `shouldSatisfy` isRight

            -- Verify header was persisted
            headerCount <- runRead config $ runQuery "count-headers" countHeadersStmt
            case headerCount of
                Right count -> count `shouldBe` 1
                Left err -> expectationFailure $ "Header count query failed: " <> show err

            -- Verify peer was created
            peerCount <- runRead config $ runQuery "count-peers" countPeersStmt
            case peerCount of
                Right count -> count `shouldBe` 1
                Left err -> expectationFailure $ "Peer count query failed: " <> show err

            -- Verify receipt was recorded
            receiptCount <- runRead config $ runQuery "count-receipts" countReceiptsStmt
            case receiptCount of
                Right count -> count `shouldBe` 1
                Left err -> expectationFailure $ "Receipt count query failed: " <> show err

        it "handles duplicate headers correctly" $ \config -> do
            now <- getCurrentTime
            let peerAddr = PeerAddress (read "192.168.1.1") 3001
            let header =
                    Header
                        { hash = BlockHash "abc123def456"
                        , slotNumber = 12345
                        , blockNumber = 100
                        , firstSeenAt = now
                        }

            -- Insert header first time
            _ <- runWrite config $ upsertHeader header peerAddr now

            -- Insert same header again
            _ <- runWrite config $ upsertHeader header peerAddr now

            -- Should still have only 1 header
            headerCount <- runRead config $ runQuery "count-headers-after-dup" countHeadersStmt
            case headerCount of
                Right count -> count `shouldBe` 1
                Left err -> expectationFailure $ "Header count query failed: " <> show err

        it "records receipts from multiple peers for same header" $ \config -> do
            now <- getCurrentTime
            let peer1 = PeerAddress (read "192.168.1.1") 3001
            let peer2 = PeerAddress (read "192.168.1.2") 3002
            let header =
                    Header
                        { hash = BlockHash "abc123def456"
                        , slotNumber = 12345
                        , blockNumber = 100
                        , firstSeenAt = now
                        }

            -- Upsert header from peer1
            _ <- runWrite config $ upsertHeader header peer1 now

            -- Upsert same header from peer2
            _ <- runWrite config $ upsertHeader header peer2 now

            -- Should have 1 header
            headerCount <- runRead config $ runQuery "count-headers-multi" countHeadersStmt
            case headerCount of
                Right count -> count `shouldBe` 1
                Left err -> expectationFailure $ "Header count query failed: " <> show err

            -- Should have 2 peers
            peerCount <- runRead config $ runQuery "count-peers-multi" countPeersStmt
            case peerCount of
                Right count -> count `shouldBe` 2
                Left err -> expectationFailure $ "Peer count query failed: " <> show err

            -- Should have 2 receipts (one per peer)
            receiptCount <- runRead config $ runQuery "count-receipts-multi" countReceiptsStmt
            case receiptCount of
                Right count -> count `shouldBe` 2
                Left err -> expectationFailure $ "Receipt count query failed: " <> show err


-- Helper statements

countHeadersStmt :: Statement () Int
countHeadersStmt = fmap length $ Rel8.run $ Rel8.select $ Rel8.each HeadersSchema.schema


countPeersStmt :: Statement () Int
countPeersStmt = fmap length $ Rel8.run $ Rel8.select $ Rel8.each PeersSchema.schema


countReceiptsStmt :: Statement () Int
countReceiptsStmt = fmap length $ Rel8.run $ Rel8.select $ Rel8.each HeaderReceiptsSchema.schema
