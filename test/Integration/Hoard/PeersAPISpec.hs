module Integration.Hoard.PeersAPISpec (spec_PeersAPI) where

import Test.Hspec
import Text.Read (read)

import Hoard.API (Routes (..))
import Hoard.API.Peers (PeersRoutes (..), PinPeerRequest (..))
import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.TestHelpers (client, withEffectStackServer)


spec_PeersAPI :: Spec
spec_PeersAPI = describe "Peers API" $ do
    let testAddr = PeerAddress (read "192.168.1.1") 3001
        testReq = [PinPeerRequest {peer = testAddr, note = Nothing}]

    describe "GET /peers/pinned" $ do
        context "no peers pinned"
            $ it "returns empty list"
            $ void @IO
            $ withEffectStackServer
            $ \_ runClient -> do
                result <- runClient client.peers.getPinned
                liftIO $ result `shouldBe` Right []

    describe "POST /peers/pinned" $ do
        context "single peer"
            $ it "pins and returns it"
            $ void @IO
            $ withEffectStackServer
            $ \_ runClient -> do
                result <- runClient (client.peers.addPinned testReq)
                liftIO $ case result of
                    Right [peer] -> peer.address `shouldBe` testAddr
                    Right peers -> expectationFailure $ "Expected 1 peer, got " <> show (length peers)
                    Left err -> expectationFailure $ show err

        context "already pinned"
            $ it "returns 200"
            $ void @IO
            $ withEffectStackServer
            $ \_ runClient -> do
                _ <- runClient (client.peers.addPinned testReq)
                result <- runClient (client.peers.addPinned testReq)
                liftIO $ result `shouldSatisfy` isRight

        context "multiple peers"
            $ it "pins all of them"
            $ void @IO
            $ withEffectStackServer
            $ \_ runClient -> do
                let reqs =
                        [ PinPeerRequest {peer = PeerAddress (read "192.168.1.1") 3001, note = Nothing}
                        , PinPeerRequest {peer = PeerAddress (read "192.168.1.2") 3002, note = Just "second"}
                        ]
                result <- runClient (client.peers.addPinned reqs)
                liftIO $ case result of
                    Right peers -> length peers `shouldBe` 2
                    Left err -> expectationFailure $ show err

        context "after pinning"
            $ it "appears in GET /peers/pinned"
            $ void @IO
            $ withEffectStackServer
            $ \_ runClient -> do
                _ <- runClient (client.peers.addPinned testReq)
                result <- runClient client.peers.getPinned
                liftIO $ case result of
                    Right peers -> map (.address) peers `shouldContain` [testAddr]
                    Left err -> expectationFailure $ show err

    describe "DELETE /peers/pinned" $ do
        context "pinned peer"
            $ it "unpins it"
            $ void @IO
            $ withEffectStackServer
            $ \_ runClient -> do
                _ <- runClient (client.peers.addPinned testReq)
                result <- runClient (client.peers.removePinned [testAddr])
                liftIO $ result `shouldSatisfy` isRight

        context "multiple pinned peers"
            $ it "unpins all of them"
            $ void @IO
            $ withEffectStackServer
            $ \_ runClient -> do
                let reqs =
                        [ PinPeerRequest {peer = PeerAddress (read "192.168.1.1") 3001, note = Nothing}
                        , PinPeerRequest {peer = PeerAddress (read "192.168.1.2") 3002, note = Just "second"}
                        ]
                    addrs = map (.peer) reqs
                _ <- runClient (client.peers.addPinned reqs)
                result <- runClient (client.peers.removePinned addrs)
                liftIO $ result `shouldSatisfy` isRight

        context "peer not pinned"
            $ it "silently succeeds"
            $ void @IO
            $ withEffectStackServer
            $ \_ runClient -> do
                result <- runClient (client.peers.removePinned [testAddr])
                liftIO $ result `shouldSatisfy` isRight

        context "peer address not found"
            $ it "silently succeeds"
            $ void @IO
            $ withEffectStackServer
            $ \_ runClient -> do
                result <- runClient (client.peers.removePinned [PeerAddress (read "10.0.0.99") 9999])
                liftIO $ result `shouldSatisfy` isRight

        context "after unpinning"
            $ it "disappears from GET /peers/pinned"
            $ void @IO
            $ withEffectStackServer
            $ \_ runClient -> do
                _ <- runClient (client.peers.addPinned testReq)
                _ <- runClient (client.peers.removePinned [testAddr])
                result <- runClient client.peers.getPinned
                liftIO $ case result of
                    Right peers -> map (.address) peers `shouldNotContain` [testAddr]
                    Left err -> expectationFailure $ show err
