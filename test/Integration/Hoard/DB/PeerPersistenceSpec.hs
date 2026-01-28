module Integration.Hoard.DB.PeerPersistenceSpec (spec_PeerPersistence) where

import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.UUID.V4 qualified as UUID
import Effectful (runEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Reader.Static (runReader)
import Hasql.Statement (Statement)
import Rel8 qualified
import Test.Hspec
import Text.Read (read)
import Prelude hiding (runReader)

import Hoard.DB.Schemas.Peers qualified as PeersSchema
import Hoard.Data.ID (ID (..))
import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.Effects.Clock (runClockConst)
import Hoard.Effects.DBRead (runDBRead, runQuery)
import Hoard.Effects.DBWrite (runDBWrite)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.Metrics (runMetricsNoOp)
import Hoard.Effects.PeerRepo (getPeerByAddress, runPeerRepo, upsertPeers)
import Hoard.TestHelpers.Database (TestConfig (..), withCleanTestDatabase)


spec_PeerPersistence :: Spec
spec_PeerPersistence = do
    let runWrite config testTime action =
            runEff
                . Log.runLogNoOp
                . runErrorNoCallStack @Text
                . runReader config.pools
                . runMetricsNoOp
                . runClockConst testTime
                . runDBRead
                . runDBWrite
                . runPeerRepo
                $ action

    let runRead config testTime action =
            runEff
                . runErrorNoCallStack @Text
                . runReader config.pools
                . runMetricsNoOp
                . runClockConst testTime
                . runDBRead
                $ action

    withCleanTestDatabase $ describe "Peer persistence (database)" $ do
        it "inserts a new peer successfully" $ \config -> do
            now <- getCurrentTime
            sourcePeer <- mkTestSourcePeer now

            -- Insert peers
            result <-
                runWrite config now $ do
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
                    . runClockConst now
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
                runWrite config now $
                    upsertPeers
                        (fromList [PeerAddress (read "192.168.1.1") 3001])
                        sourcePeer.address
                        now

            -- Wait a bit and insert same peer again with different timestamp
            let laterTime = addUTCTime 60 now -- 60 seconds later
            _ <-
                runWrite config now $
                    upsertPeers
                        (fromList [PeerAddress (read "192.168.1.1") 3001])
                        sourcePeer.address
                        laterTime

            -- Query to verify we still have only 1 peer
            queryResult <-
                runRead config now $ runQuery "count-peers-after-upsert" countPeersStmt

            case queryResult of
                Right count -> count `shouldBe` 1
                Left err -> expectationFailure $ "Count query failed: " <> show err

            -- Query the peer to verify lastSeen was updated but firstDiscovered was not
            peerResult <-
                runRead config now $ runQuery "get-peer" getPeerByAddressStmt

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
                runWrite config now $
                    upsertPeers
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
                runRead config now $ runQuery "count-batch" countPeersStmt

            case queryResult of
                Right count -> count `shouldBe` 4
                Left err -> expectationFailure $ "Query failed: " <> show err

        it "can persist and fetch a peer (round-trip test)" $ \config -> do
            now <- getCurrentTime
            sourcePeer <- mkTestSourcePeer now
            let testAddr = PeerAddress (read "10.20.30.40") 5000

            -- Insert a peer
            insertResult <-
                runWrite config now $
                    upsertPeers (fromList [testAddr]) sourcePeer.address now

            insertResult `shouldSatisfy` isRight

            -- Fetch it back using getPeerByAddress
            fetchResult <- runWrite config now $ getPeerByAddress testAddr

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
    fmap extractSinglePeer $
        Rel8.run $
            Rel8.select $ do
                peer <- Rel8.each PeersSchema.schema
                Rel8.where_ $ peer.address Rel8.==. Rel8.lit (read "192.168.1.1")
                pure peer
  where
    extractSinglePeer rows = case rows of
        [row] -> PeersSchema.peerFromRow row
        _ -> error "Expected exactly one peer"


diffTime :: UTCTime -> UTCTime -> Double
diffTime t1 t2 = realToFrac (diffUTCTime t1 t2)
