module Hoard.Listeners.DiscoveredNodesListener (dispatchDiscoveredNodes) where

import Data.Set qualified as S
import Data.Time (UTCTime)
import Data.Time.Clock (NominalDiffTime, diffUTCTime)
import Effectful (Eff)
import Effectful.State.Static.Shared (gets)
import Prelude hiding (State, gets)

import Hoard.Collector (runCollector)
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.Clock qualified as Clock
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.PeerRepo (upsertPeers)
import Hoard.Network.Events (PeerSharingEvent (..), PeersReceivedData (..))
import Hoard.Types.HoardState (HoardState (connectedPeers))


-- | Cooldown period after a peer failure before retrying (5 minutes)
peerFailureCooldown :: NominalDiffTime
peerFailureCooldown = 300 -- 5 minutes in seconds


-- | Dispatch discovered peer nodes for connection.
--
-- When peers are discovered via peer sharing:
-- - Upserts all discovered peer addresses to the database
-- - Filters out peers already connected
-- - Filters out peers in cooldown period (recent failures)
-- - Starts collectors for all eligible peers
dispatchDiscoveredNodes :: (_) => PeerSharingEvent -> Eff es ()
dispatchDiscoveredNodes = \case
    (PeersReceived (PeersReceivedData {peer = sourcePeer, peerAddresses})) -> do
        Log.info "Dispatch: Received peers"

        -- First, upsert all discovered peer addresses to create Peer records
        timestamp <- Clock.currentTime
        upsertedPeers <- upsertPeers peerAddresses sourcePeer.address timestamp

        currPeers <- gets @HoardState (.connectedPeers)
        let newPeers = S.difference upsertedPeers currPeers

        -- Filter out peers in cooldown period
        let eligiblePeers = S.filter (isPeerEligible timestamp) newPeers
            cooldownPeers = S.difference newPeers eligiblePeers

        unless (S.null cooldownPeers) $
            Log.info $
                "Dispatch: Skipping "
                    <> show (S.size cooldownPeers)
                    <> " peers in cooldown (from "
                    <> show sourcePeer.address
                    <> ")"

        Log.info $ "Dispatch: " <> show (S.size eligiblePeers) <> " new peers to connect to"

        forM_ eligiblePeers runCollector
    _ -> pure ()


-- | Check if a peer is eligible for connection based on failure cooldown
isPeerEligible :: UTCTime -> Peer -> Bool
isPeerEligible currentTime peer =
    case peer.lastFailureTime of
        Nothing -> True -- Never failed, eligible
        Just failureTime ->
            let timeSinceFailure = diffUTCTime currentTime failureTime
            in  timeSinceFailure >= peerFailureCooldown
