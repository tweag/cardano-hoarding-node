module Unit.Hoard.Data.PeerSpec (spec_Peer) where

import Network.Socket (SockAddr (..))
import Test.Hspec

import Network.Socket qualified as Socket

import Hoard.Data.Peer (PeerAddress (..), sockAddrToPeerAddress)
import Text.Read (read)


spec_Peer :: Spec
spec_Peer = describe "Peer address conversion" $ do
    describe "sockAddrToPeerAddress" $ do
        it "converts IPv4 SockAddr correctly" $ do
            -- 192.168.1.1:3001
            let hostAddr = Socket.tupleToHostAddress (192, 168, 1, 1)
                sockAddr = SockAddrInet 3001 hostAddr
            sockAddrToPeerAddress sockAddr `shouldBe` Just (PeerAddress (read "192.168.1.1") 3001)

            -- 10.0.0.1:6000
            let hostAddr2 = Socket.tupleToHostAddress (10, 0, 0, 1)
                sockAddr2 = SockAddrInet 6000 hostAddr2
            sockAddrToPeerAddress sockAddr2 `shouldBe` Just (PeerAddress (read "10.0.0.1") 6000)

        it "converts IPv6 SockAddr correctly with RFC 5952 zero compression" $ do
            -- ::1 (localhost IPv6) - should compress to ::1
            let hostAddr6 = Socket.tupleToHostAddress6 (0, 0, 0, 0, 0, 0, 0, 1)
                sockAddr = SockAddrInet6 8080 0 hostAddr6 0
            sockAddrToPeerAddress sockAddr `shouldBe` Just (PeerAddress (read "::1") 8080)

            -- 2a05:d014:1cfa:bc01:: - should compress trailing zeros
            let hostAddr6_2 = Socket.tupleToHostAddress6 (0x2a05, 0xd014, 0x1cfa, 0xbc01, 0, 0, 0, 0)
                sockAddr2 = SockAddrInet6 3001 0 hostAddr6_2 0
            sockAddrToPeerAddress sockAddr2 `shouldBe` Just (PeerAddress (read "2a05:d014:1cfa:bc01::") 3001)

        it "returns Nothing for Unix domain sockets" $ do
            let sockAddr = SockAddrUnix "/tmp/socket"
            sockAddrToPeerAddress sockAddr `shouldBe` Nothing
