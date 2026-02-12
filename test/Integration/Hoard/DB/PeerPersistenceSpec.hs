module Integration.Hoard.DB.PeerPersistenceSpec (spec_PeerPersistence) where

import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Effectful (runEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Reader.Static (runReader)
import Hasql.Statement (Statement)
import Test.Hspec
import Text.Read (read)
import Prelude hiding (runReader)

import Data.UUID.V4 qualified as UUID
import Rel8 qualified

import Hoard.Data.ID (ID (..))
import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.Effects.Clock (runClock)
import Hoard.Effects.DBRead (runDBRead, runQuery)
import Hoard.Effects.DBWrite (runDBWrite)
import Hoard.Effects.Monitoring.Metrics (runMetricsNoOp)
import Hoard.Effects.Monitoring.Tracing (runTracingNoOp)
import Hoard.Effects.PeerRepo (getPeerByAddress, runPeerRepo, upsertPeers)
import Hoard.TestHelpers.Database (TestConfig (..), withCleanTestDatabase)

import Hoard.DB.Schemas.Peers qualified as PeersSchema
import Hoard.Effects.Log qualified as Log


spec_PeerPersistence :: Spec
spec_PeerPersistence = do
    let runWrite config action =
            runEff
                . Log.runLogNoOp
                . runErrorNoCallStack @Text
                . runReader config.pools
                . runMetricsNoOp
                . runTracingNoOp
                . runClock
                . runDBRead
                . runClock
                . runDBWrite
                . runPeerRepo
                $ action

    let runRead config action =
            runEff
                . runErrorNoCallStack @Text
                . runReader config.pools
                . runMetricsNoOp
                . runTracingNoOp
                . runClock
                . runDBRead
                $ action

    withCleanTestDatabase $ describe "Peer persistence (database)" $ do
        it "inserts a new peer successfully" $ \config -> do
            now <- getCurrentTime
            sourcePeer <- mkTestSourcePeer now

            -- Insert peers
            result <-
                runWrite config $ do
                    upsertPeers
                        ( fromList
                            [ PeerAddress (read "192.168.1.1") 3001
                            , PeerAddress (read "192.168.1.2") 3002
                            ]
                        )
                        sourcePeer.address
                        now

            result `shouldSatisfy` isRight

            -- Query back to verify
            queryResult <-
                runEff
                    . runErrorNoCallStack @Text
                    . runReader config.pools
                    . runMetricsNoOp
                    . runTracingNoOp
                    . runClock
                    . runDBRead
                    $ runQuery "count-peers" countPeersStmt

            case queryResult of
                Right count -> count `shouldBe` 2
                Left err -> expectationFailure $ "Query failed: " <> show err

        it "handles duplicate peers correctly (upsert behavior)" $ \config -> do
            now <- getCurrentTime
            sourcePeer <- mkTestSourcePeer now

            -- Insert peer first time
            _ <-
                runWrite config
                    $ upsertPeers
                        (fromList [PeerAddress (read "192.168.1.1") 3001])
                        sourcePeer.address
                        now

            -- Wait a bit and insert same peer again with different timestamp
            let laterTime = addUTCTime 60 now -- 60 seconds later
            _ <-
                runWrite config
                    $ upsertPeers
                        (fromList [PeerAddress (read "192.168.1.1") 3001])
                        sourcePeer.address
                        laterTime

            -- Query to verify we still have only 1 peer
            queryResult <-
                runRead config $ runQuery "count-peers-after-upsert" countPeersStmt

            case queryResult of
                Right count -> count `shouldBe` 1
                Left err -> expectationFailure $ "Count query failed: " <> show err

            -- Query the peer to verify lastSeen was updated but firstDiscovered was not
            peerResult <-
                runRead config $ runQuery "get-peer" getPeerByAddressStmt

            case peerResult of
                Right peer -> do
                    peer.address.host `shouldBe` read "192.168.1.1"
                    peer.address.port `shouldBe` 3001
                    -- lastSeen should be updated to laterTime (approximately)
                    abs (diffTime peer.lastSeen laterTime) `shouldSatisfy` (< 1)
                    -- firstDiscovered should still be the original time
                    abs (diffTime peer.firstDiscovered now) `shouldSatisfy` (< 1)
                    -- discoveredVia should be unchanged
                    peer.discoveredVia `shouldBe` ("PeerSharing:" <> show sourcePeer.address.host <> ":" <> show sourcePeer.address.port)
                Left err -> expectationFailure $ "Peer query failed: " <> show err

        it "handles multiple peers in one batch" $ \config -> do
            now <- getCurrentTime
            sourcePeer <- mkTestSourcePeer now

            -- Insert multiple peers at once
            result <-
                runWrite config
                    $ upsertPeers
                        ( fromList
                            [ PeerAddress (read "192.168.1.1") 3001
                            , PeerAddress (read "192.168.1.2") 3002
                            , PeerAddress (read "192.168.1.3") 3003
                            , PeerAddress (read "10.0.0.1") 6000
                            ]
                        )
                        sourcePeer.address
                        now

            result `shouldSatisfy` isRight

            queryResult <-
                runRead config $ runQuery "count-batch" countPeersStmt

            case queryResult of
                Right count -> count `shouldBe` 4
                Left err -> expectationFailure $ "Query failed: " <> show err

        it "can persist and fetch a peer (round-trip test)" $ \config -> do
            now <- getCurrentTime
            sourcePeer <- mkTestSourcePeer now
            let testAddr = PeerAddress (read "10.20.30.40") 5000

            -- Insert a peer
            insertResult <-
                runWrite config
                    $ upsertPeers (fromList [testAddr]) sourcePeer.address now

            insertResult `shouldSatisfy` isRight

            -- Fetch it back using getPeerByAddress
            fetchResult <- runWrite config $ getPeerByAddress testAddr

            case fetchResult of
                Right (Just peer) -> do
                    -- Verify the peer data matches what we inserted
                    peer.address.host `shouldBe` testAddr.host
                    peer.address.port `shouldBe` testAddr.port
                    peer.discoveredVia `shouldBe` ("PeerSharing:" <> show sourcePeer.address.host <> ":" <> show sourcePeer.address.port)
                    -- Timestamps should be close to what we inserted
                    abs (diffTime peer.firstDiscovered now) `shouldSatisfy` (< 1)
                    abs (diffTime peer.lastSeen now) `shouldSatisfy` (< 1)
                Right Nothing -> expectationFailure "Expected to find the peer, but got Nothing"
                Left err -> expectationFailure $ "Fetch failed: " <> show err


-- Helper functions

mkTestSourcePeer :: UTCTime -> IO Peer
mkTestSourcePeer now = do
    uuid <- UUID.nextRandom
    pure
        Peer
            { id = ID uuid
            , address = PeerAddress (read "172.0.0.1") 3001
            , firstDiscovered = now
            , lastSeen = now
            , lastConnected = Nothing
            , lastFailureTime = Nothing
            , discoveredVia = "bootstrap"
            }


countPeersStmt :: Statement () Int
countPeersStmt = fmap length $ Rel8.run $ Rel8.select $ Rel8.each PeersSchema.schema


getPeerByAddressStmt :: Statement () Peer
getPeerByAddressStmt =
    fmap extractSinglePeer
        $ Rel8.run
        $ Rel8.select
        $ do
            peer <- Rel8.each PeersSchema.schema
            Rel8.where_ $ peer.address Rel8.==. Rel8.lit (read "192.168.1.1")
            pure peer
  where
    extractSinglePeer rows = case rows of
        [row] -> PeersSchema.peerFromRow row
        _ -> error "Expected exactly one peer"


diffTime :: UTCTime -> UTCTime -> Double
diffTime t1 t2 = realToFrac (diffUTCTime t1 t2)
