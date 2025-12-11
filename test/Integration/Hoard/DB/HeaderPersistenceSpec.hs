module Integration.Hoard.DB.HeaderPersistenceSpec (spec_HeaderPersistence) where

import Data.Time (UTCTime, getCurrentTime)
import Effectful (runEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Test.Hspec

import Hasql.Statement (Statement)
import Rel8 qualified

import Data.UUID.V4 qualified as UUID
import Hoard.DB.Schemas.HeaderReceipts qualified as HeaderReceiptsSchema
import Hoard.DB.Schemas.Headers qualified as HeadersSchema
import Hoard.Data.Header (BlockHash (..), Header (..))
import Hoard.Data.ID (ID (..))
import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.Effects.DBRead (runDBRead, runQuery)
import Hoard.Effects.DBWrite (runDBWrite)
import Hoard.Effects.HeaderRepo (runHeaderRepo, upsertHeader)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.PeerRepo (runPeerRepo, upsertPeers)
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
                . runPeerRepo
                . runHeaderRepo
                $ action

    let runRead config action =
            runEff
                . runErrorNoCallStack @Text
                . runDBRead config.pools.readerPool
                $ action

    withCleanTestDatabase $ describe "Header persistence (database)" $ do
        it "persists a header and receipt" $ \config -> do
            now <- getCurrentTime
            peer <- mkTestPeer now
            let header =
                    Header
                        { hash = BlockHash "abc123def456"
                        , slotNumber = 12345
                        , blockNumber = 100
                        , firstSeenAt = now
                        }

            -- First, create the peer (upsertPeers returns the peer with DB-assigned ID)
            result <- runWrite config $ do
                upsertedPeers <- upsertPeers (fromList [peer.address]) peer.address now
                persistedPeer <- case toList upsertedPeers of
                    [p] -> pure p
                    _ -> error "Expected exactly one peer from upsert"
                -- Then upsert header with the persisted peer
                upsertHeader header persistedPeer now

            result `shouldSatisfy` isRight

            -- Verify header was persisted
            headerCount <- runRead config $ runQuery "count-headers" countHeadersStmt
            case headerCount of
                Right count -> count `shouldBe` 1
                Left err -> expectationFailure $ "Header count query failed: " <> show err

            -- Verify receipt was recorded
            receiptCount <- runRead config $ runQuery "count-receipts" countReceiptsStmt
            case receiptCount of
                Right count -> count `shouldBe` 1
                Left err -> expectationFailure $ "Receipt count query failed: " <> show err

        it "handles duplicate headers correctly" $ \config -> do
            now <- getCurrentTime
            peer <- mkTestPeer now
            let header =
                    Header
                        { hash = BlockHash "abc123def456"
                        , slotNumber = 12345
                        , blockNumber = 100
                        , firstSeenAt = now
                        }

            -- Create peer first (upsertPeers returns the peer with DB-assigned ID)
            _ <- runWrite config $ do
                upsertedPeers <- upsertPeers (fromList [peer.address]) peer.address now
                persistedPeer <- case toList upsertedPeers of
                    [p] -> pure p
                    _ -> error "Expected exactly one peer from upsert"
                -- Insert header first time
                _ <- upsertHeader header persistedPeer now
                -- Insert same header again
                upsertHeader header persistedPeer now

            -- Should still have only 1 header
            headerCount <- runRead config $ runQuery "count-headers-after-dup" countHeadersStmt
            case headerCount of
                Right count -> count `shouldBe` 1
                Left err -> expectationFailure $ "Header count query failed: " <> show err

        it "records receipts from multiple peers for same header" $ \config -> do
            now <- getCurrentTime
            peer1 <- mkTestPeerWithPort now 3001
            peer2 <- mkTestPeerWithPort now 3002
            let header =
                    Header
                        { hash = BlockHash "abc123def456"
                        , slotNumber = 12345
                        , blockNumber = 100
                        , firstSeenAt = now
                        }

            -- Create both peers (upsertPeers returns peers with DB-assigned IDs)
            _ <- runWrite config $ do
                upsertedPeers1 <- upsertPeers (fromList [peer1.address]) peer1.address now
                upsertedPeers2 <- upsertPeers (fromList [peer2.address]) peer2.address now
                persistedPeer1 <- case toList upsertedPeers1 of
                    [p] -> pure p
                    _ -> error "Expected exactly one peer from upsert"
                persistedPeer2 <- case toList upsertedPeers2 of
                    [p] -> pure p
                    _ -> error "Expected exactly one peer from upsert"
                -- Upsert header from peer1
                _ <- upsertHeader header persistedPeer1 now
                -- Upsert same header from peer2
                upsertHeader header persistedPeer2 now

            -- Should have 1 header
            headerCount <- runRead config $ runQuery "count-headers-multi" countHeadersStmt
            case headerCount of
                Right count -> count `shouldBe` 1
                Left err -> expectationFailure $ "Header count query failed: " <> show err

            -- Should have 2 receipts (one per peer)
            receiptCount <- runRead config $ runQuery "count-receipts-multi" countReceiptsStmt
            case receiptCount of
                Right count -> count `shouldBe` 2
                Left err -> expectationFailure $ "Receipt count query failed: " <> show err


-- Helper functions

mkTestPeer :: UTCTime -> IO Peer
mkTestPeer now = mkTestPeerWithPort now 3001


mkTestPeerWithPort :: UTCTime -> Int -> IO Peer
mkTestPeerWithPort now port = do
    uuid <- UUID.nextRandom
    pure
        Peer
            { id = ID uuid
            , address = PeerAddress (read "192.168.1.1") port
            , firstDiscovered = now
            , lastSeen = now
            , lastConnected = Nothing
            , discoveredVia = "test"
            }


-- Helper statements

countHeadersStmt :: Statement () Int
countHeadersStmt = fmap length $ Rel8.run $ Rel8.select $ Rel8.each HeadersSchema.schema


countReceiptsStmt :: Statement () Int
countReceiptsStmt = fmap length $ Rel8.run $ Rel8.select $ Rel8.each HeaderReceiptsSchema.schema
