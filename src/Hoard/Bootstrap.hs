module Hoard.Bootstrap (bootstrapPeers) where

import Control.Exception (try)
import Data.IP (IP)
import Data.IP qualified as IP
import Data.Set qualified as S
import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Static (Reader, asks)
import Network.Socket (HostName, PortNumber)
import Network.Socket qualified as Socket
import Prelude hiding (Reader, asks)

import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Clock qualified as Clock
import Hoard.Effects.PeerRepo (PeerRepo, upsertPeers)
import Hoard.Types.Environment
    ( BootstrapPeerDomain (..)
    , BootstrapPeerIP (..)
    , Config (..)
    , LedgerPool (..)
    , PeerSnapshotFile (..)
    )
import Hoard.Types.NodeIP (NodeIP (..))


-- | Bootstrap peers from the peer snapshot configuration.
-- Extracts all relays from bigLedgerPools, resolves their addresses,
-- and upserts them to the database.
bootstrapPeers :: (PeerRepo :> es, Clock :> es, IOE :> es, Reader Config :> es) => Eff es (Set Peer)
bootstrapPeers = do
    -- Get peer snapshot from config
    peerSnapshot <- asks (.peerSnapshot)

    -- Extract all relays from all ledger pools
    let allRelays = concatMap (.relays) peerSnapshot.bigLedgerPools

    -- Resolve all relay addresses to PeerAddress
    addresses <- fmap (S.fromList . catMaybes) $ forM allRelays $ \relay -> case relay of
        Left domain -> resolveDomain domain
        Right ipRelay -> resolveIPAddress ipRelay

    -- Use the first address as the bootstrap source for all peers
    -- (In reality, each peer is discovered from the network, but for initial bootstrap
    -- we just use one as the source)
    case S.toList addresses of
        [] -> error "No bootstrap peers found in peer snapshot"
        (firstAddr : _) -> do
            timestamp <- Clock.currentTime
            upsertPeers addresses firstAddr timestamp


resolveDomain :: (IOE :> es) => BootstrapPeerDomain -> Eff es (Maybe PeerAddress)
resolveDomain domain = do
    -- Resolve domain to IP
    parts <- liftIO $ resolvePeerAddress (toString domain.domain) domain.port
    case parts of
        Just (ip, portNumber) ->
            pure $ Just $ PeerAddress (NodeIP ip) (fromIntegral portNumber)
        Nothing -> pure Nothing


resolveIPAddress :: (IOE :> es) => BootstrapPeerIP -> Eff es (Maybe PeerAddress)
resolveIPAddress ipRelay = do
    -- Parse IP address directly, or resolve if it's actually a hostname
    case readMaybe (toString ipRelay.address) of
        Just ip ->
            pure $ Just $ PeerAddress (NodeIP ip) ipRelay.port
        Nothing -> do
            -- If parsing fails, try to resolve it as a hostname
            parts <- liftIO $ resolvePeerAddress (toString ipRelay.address) ipRelay.port
            case parts of
                Just (ip, portNumber) ->
                    pure $ Just $ PeerAddress (NodeIP ip) (fromIntegral portNumber)
                Nothing -> pure Nothing


resolvePeerAddress :: HostName -> Int -> IO (Maybe (IP, PortNumber))
resolvePeerAddress address port = do
    let hints = Socket.defaultHints {Socket.addrSocketType = Socket.Stream}
    addrs <- try $ Socket.getAddrInfo (Just hints) (Just $ toString address) (Just $ show port)
    case addrs of
        Right (addr :| _) -> pure $ IP.fromSockAddr $ Socket.addrAddress addr
        Left (_ :: SomeException) -> pure Nothing
