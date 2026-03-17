module Hoard.Bootstrap (bootstrapPeers, bootstrapPinnedPeers) where

import Control.Exception (try)
import Data.IP (IP)
import Effectful (IOE)
import Effectful.Reader.Static (Reader, ask)
import Network.Socket (HostName, PortNumber)

import Data.IP qualified as IP
import Data.Set qualified as S
import Network.Socket qualified as Socket

import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.Monitoring.Tracing (Tracing, withSpan)
import Hoard.Effects.PeerRepo (PeerRepo, pinPeers, upsertPeers)
import Hoard.Types.Environment
    ( BootstrapPeerDomain (..)
    , BootstrapPeerIP (..)
    , LedgerPool (..)
    , PeerSnapshotFile (..)
    )
import Hoard.Types.NodeIP (NodeIP (..))

import Hoard.Effects.Clock qualified as Clock


-- | Bootstrap peers from the peer snapshot configuration.
-- Extracts all relays from bigLedgerPools, resolves their addresses,
-- and upserts them to the database.
bootstrapPeers
    :: ( Clock :> es
       , IOE :> es
       , PeerRepo :> es
       , Reader PeerSnapshotFile :> es
       , Tracing :> es
       )
    => Eff es (Set Peer)
bootstrapPeers = withSpan "bootstrap.bootstrap_peers" do
    addresses <- resolveBootstrapAddresses
    case addresses of
        [] -> error "No bootstrap peers found in peer snapshot"
        (firstAddr : _) -> do
            timestamp <- Clock.currentTime
            upsertPeers (S.fromList addresses) firstAddr timestamp


-- | Bootstrap pinned peers from the peer snapshot configuration.
--
-- Used in Manual mode when selected_peers is empty on startup.
-- Upserts peers into the peers table and adds them to selected_peers.
bootstrapPinnedPeers
    :: ( Clock :> es
       , IOE :> es
       , PeerRepo :> es
       , Reader PeerSnapshotFile :> es
       , Tracing :> es
       )
    => Eff es [Peer]
bootstrapPinnedPeers = withSpan "bootstrap.bootstrap_pinned_peers" do
    addresses <- resolveBootstrapAddresses
    timestamp <- Clock.currentTime
    pinPeers timestamp [(addr, Nothing) | addr <- addresses]


-- | Resolve all relay addresses from the peer snapshot.
resolveBootstrapAddresses
    :: ( IOE :> es
       , Reader PeerSnapshotFile :> es
       , Tracing :> es
       )
    => Eff es [PeerAddress]
resolveBootstrapAddresses = do
    peerSnapshot <- ask
    let allRelays = concatMap (.relays) peerSnapshot.bigLedgerPools
    fmap catMaybes $ forM allRelays $ \relay ->
        withSpan "bootstrap.resolve_address" $ case relay of
            Left domain -> resolveDomain domain
            Right ipRelay -> resolveIPAddress ipRelay


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
