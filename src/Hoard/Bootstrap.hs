module Hoard.Bootstrap (bootstrapCollection) where

import Data.IP (IP)
import Data.IP qualified as IP
import Data.Set qualified as S
import Effectful (Eff, IOE, (:>))
import Network.Socket (PortNumber)
import Network.Socket qualified as Socket
import Prelude hiding (State, evalState)

import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Clock qualified as Clock
import Hoard.Effects.PeerRepo (PeerRepo, upsertPeers)
import Hoard.Types.NodeIP (NodeIP (..))

import Hoard.Collector (runCollector)
import Hoard.Effects.Chan (Chan)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.NodeToNode (NodeToNode)
import Hoard.Effects.Pub (Pub)


bootstrapCollection
    :: ( Pub :> es
       , NodeToNode :> es
       , Chan :> es
       , Conc :> es
       , PeerRepo :> es
       , Clock :> es
       , IOE :> es
       )
    => Eff es ()
bootstrapCollection = do
    peer <- bootstrapPeer
    _ <- runCollector peer
    pure ()


bootstrapPeer :: (PeerRepo :> es, Clock :> es, IOE :> es) => Eff es Peer
bootstrapPeer = do
    -- Correct port from Preview testnet topology.json
    (ip, portNumber) <- liftIO $ resolvePeerAddress "preview-node.world.dev.cardano.org" 3001
    let address = PeerAddress (NodeIP ip) (fromIntegral portNumber)

    -- Upsert the peer to the database first (bootstrap: source is itself)
    -- upsertPeers returns the peer with DB-assigned ID
    timestamp <- Clock.currentTime
    upsertedPeers <-
        upsertPeers
            (S.singleton address)
            address
            timestamp

    case toList upsertedPeers of
        [p] -> pure p
        _ -> error "Expected exactly one peer from upsert"


resolvePeerAddress :: Text -> Int -> IO (IP, PortNumber)
resolvePeerAddress address port = do
    let hints = Socket.defaultHints {Socket.addrSocketType = Socket.Stream}
    addrs <- Socket.getAddrInfo (Just hints) (Just $ toString address) (Just $ show port)
    case addrs of
        (addr :| _) ->
            maybe (error "Found address of preview relay is not an IP address") pure $
                IP.fromSockAddr $
                    Socket.addrAddress addr
