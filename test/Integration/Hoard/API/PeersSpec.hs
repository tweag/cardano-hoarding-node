module Integration.Hoard.API.PeersSpec (spec_PeersAPI) where

import Test.Hspec
import Text.Read (read)

import Hoard.API (Routes (..))
import Hoard.API.Peers (PeersRoutes (..), PinPeerRequest (..))
import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.TestHelpers (withServer)
import Hoard.TestHelpers.Database (withCleanTestDatabase)


spec_PeersAPI :: Spec
spec_PeersAPI =
    describe "Peers API" do
        withCleanTestDatabase do
            let testAddr = PeerAddress (read "192.168.1.1") 3001
                testReq = [PinPeerRequest {peer = testAddr, note = Nothing}]

            describe "GET /peers/pinned" do
                context "no peers pinned" do
                    it "returns empty list" $ \config -> withServer config $ \client -> do
                        result <- client.peers.getPinned
                        liftIO $ result `shouldBe` []

            describe "POST /peers/pinned" do
                context "single peer" do
                    it "pins and returns it" $ \config -> withServer config $ \client -> do
                        peers <- client.peers.addPinned testReq
                        liftIO $ do
                            length peers `shouldBe` 1
                            map (.address) peers `shouldBe` [testAddr]

                context "already pinned" do
                    it "returns 200" $ \config -> withServer config $ \client -> do
                        _ <- client.peers.addPinned testReq
                        _ <- client.peers.addPinned testReq
                        pass

                context "multiple peers" do
                    it "pins all of them" $ \config -> withServer config $ \client -> do
                        let reqs =
                                [ PinPeerRequest {peer = PeerAddress (read "192.168.1.1") 3001, note = Nothing}
                                , PinPeerRequest {peer = PeerAddress (read "192.168.1.2") 3002, note = Just "second"}
                                ]
                        peers <- client.peers.addPinned reqs
                        liftIO $ length peers `shouldBe` 2

                context "after pinning" do
                    it "appears in GET /peers/pinned" $ \config -> withServer config $ \client -> do
                        _ <- client.peers.addPinned testReq
                        peers <- client.peers.getPinned
                        liftIO $ map (.address) peers `shouldContain` [testAddr]

            describe "DELETE /peers/pinned" do
                context "pinned peer" do
                    it "unpins it" $ \config -> withServer config $ \client -> do
                        _ <- client.peers.addPinned testReq
                        _ <- client.peers.removePinned [testAddr]
                        pass

                context "multiple pinned peers" do
                    it "unpins all of them" $ \config -> withServer config $ \client -> do
                        let reqs =
                                [ PinPeerRequest {peer = PeerAddress (read "192.168.1.1") 3001, note = Nothing}
                                , PinPeerRequest {peer = PeerAddress (read "192.168.1.2") 3002, note = Just "second"}
                                ]
                            addrs = map (.peer) reqs
                        _ <- client.peers.addPinned reqs
                        _ <- client.peers.removePinned addrs
                        pass

                context "peer not pinned" do
                    it "silently succeeds" $ \config -> withServer config $ \client -> do
                        _ <- client.peers.removePinned [testAddr]
                        pass

                context "peer address not found" do
                    it "silently succeeds" $ \config -> withServer config $ \client -> do
                        _ <- client.peers.removePinned [PeerAddress (read "10.0.0.99") 9999]
                        pass

                context "after unpinning" do
                    it "disappears from GET /peers/pinned" $ \config -> withServer config $ \client -> do
                        _ <- client.peers.addPinned testReq
                        _ <- client.peers.removePinned [testAddr]
                        peers <- client.peers.getPinned
                        liftIO $ map (.address) peers `shouldNotContain` [testAddr]
