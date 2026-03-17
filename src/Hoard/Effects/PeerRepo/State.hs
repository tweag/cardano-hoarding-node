module Hoard.Effects.PeerRepo.State (runPeerRepoState) where

import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.State.Static.Shared (State, get, gets, modify)

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.UUID qualified as UUID
import Data.UUID.V5 qualified as UUID5

import Hoard.Data.ID (ID (..))
import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.Effects.Clock (Clock)
import Hoard.Effects.PeerRepo (PeerRepo (..))

import Hoard.Effects.Clock qualified as Clock


-- | Run PeerRepo using in-memory Maps — suitable for unit/server tests.
--
-- Peers are keyed by address; IDs are deterministically derived from the address
-- using UUID v5 so they are stable across upserts.
--
-- State:
--   @Map PeerAddress Peer@  — the peers table
--   @Set (ID Peer)@         — the selected_peers table (IDs only)
runPeerRepoState
    :: ( Clock :> es
       , State (Map PeerAddress Peer) :> es
       , State (Set (ID Peer)) :> es
       )
    => Eff (PeerRepo : es) a
    -> Eff es a
runPeerRepoState = interpret_ \case
    UpsertPeers peerAddrs sourcePeer timestamp -> do
        let discoveredVia = "PeerSharing:" <> show sourcePeer.host <> ":" <> show sourcePeer.port
        forM_ peerAddrs \addr -> do
            let newPeer = mkStatePeer discoveredVia timestamp addr
            modify @(Map PeerAddress Peer)
                $ Map.insertWith (\_ old -> old {lastSeen = timestamp}) addr newPeer
        gets @(Map PeerAddress Peer) (fromList . Map.elems)
    GetPeerByAddress addr ->
        gets @(Map PeerAddress Peer) (Map.lookup addr)
    GetAllPeers ->
        gets @(Map PeerAddress Peer) (fromList . Map.elems)
    HasPeers ->
        gets @(Map PeerAddress Peer) (not . Map.null)
    UpdatePeerFailure peer timestamp ->
        modify @(Map PeerAddress Peer)
            $ Map.adjust (\p -> p {lastFailureTime = Just timestamp}) peer.address
    UpdateLastConnected peerId timestamp ->
        modify @(Map PeerAddress Peer)
            $ Map.map (\p -> if p.id == peerId then p {lastConnected = Just timestamp} else p)
    GetEligiblePeers failureTimeout connectedIds limit -> do
        now <- Clock.currentTime
        peers <- gets @(Map PeerAddress Peer) Map.elems
        pure
            $ fromList
            $ take (fromIntegral limit)
            $ filter (isEligibleState now failureTimeout connectedIds) peers
    GetPinnedPeers -> do
        selectedIds <- get @(Set (ID Peer))
        peers <- gets @(Map PeerAddress Peer) Map.elems
        pure $ filter (\p -> p.id `Set.member` selectedIds) peers
    PinPeers timestamp entries -> do
        forM_ entries \(addr, _mNote) -> do
            let peer = mkStatePeer "pinned" timestamp addr
            existing <- gets @(Map PeerAddress Peer) (Map.lookup addr)
            let actualPeer = maybe peer (\old -> old {lastSeen = timestamp}) existing
            modify @(Map PeerAddress Peer) (Map.insert addr actualPeer)
            modify @(Set (ID Peer)) (Set.insert actualPeer.id)
        let addrs = Set.fromList (map fst entries)
        gets @(Map PeerAddress Peer) \peerMap ->
            [p | p <- Map.elems peerMap, p.address `Set.member` addrs]
    UnpinPeer peerAddrs -> do
        let addrSet = Set.fromList peerAddrs
        peersToRemove <- gets @(Map PeerAddress Peer) \peerMap ->
            [p.id | p <- Map.elems peerMap, p.address `Set.member` addrSet]
        forM_ peersToRemove \pid ->
            modify @(Set (ID Peer)) (Set.delete pid)
    GetEligiblePinnedPeers failureTimeout connectedIds limit -> do
        now <- Clock.currentTime
        selectedIds <- get @(Set (ID Peer))
        peers <- gets @(Map PeerAddress Peer) Map.elems
        pure
            $ fromList
            $ take (fromIntegral limit)
            $ filter (\p -> p.id `Set.member` selectedIds && isEligibleState now failureTimeout connectedIds p)
            $ peers


mkStatePeer :: Text -> UTCTime -> PeerAddress -> Peer
mkStatePeer discoveredVia timestamp addr =
    Peer
        { id = ID $ UUID5.generateNamed UUID.nil (map (fromIntegral . ord) (show addr))
        , address = addr
        , firstDiscovered = timestamp
        , lastSeen = timestamp
        , lastConnected = Nothing
        , lastFailureTime = Nothing
        , discoveredVia
        }


isEligibleState :: UTCTime -> NominalDiffTime -> Set (ID Peer) -> Peer -> Bool
isEligibleState now failureTimeout connectedIds peer =
    peer.id `Set.notMember` connectedIds
        && case peer.lastFailureTime of
            Nothing -> True
            Just ft -> diffUTCTime now ft > failureTimeout
