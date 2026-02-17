module Hoard.Persistence (component) where

import Effectful (Eff, (:>))
import Ouroboros.Consensus.Block (BlockNo (..))
import Ouroboros.Consensus.Block.Abstract (blockNo, blockSlot, getHeader, unSlotNo)
import Prelude hiding (Reader, State, ask, evalState, get, modify, runReader)

import Hoard.BlockFetch (BlockReceived (..))
import Hoard.ChainSync.Events (HeaderReceived (..))
import Hoard.Component (Component (..), defaultComponent)
import Hoard.Data.Block (Block (..))
import Hoard.Data.BlockHash (blockHashFromHeader)
import Hoard.Data.Header (Header (..))
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.Data.PoolID (mkPoolID)
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.HeaderRepo (HeaderRepo, upsertHeader)
import Hoard.Effects.Monitoring.Metrics (Metrics)
import Hoard.Effects.Monitoring.Metrics.Definitions (recordBlockReceived, recordHeaderReceived)
import Hoard.Effects.Monitoring.Tracing (Tracing, addAttribute, addEvent, withSpan)
import Hoard.Effects.PeerRepo (PeerRepo)
import Hoard.Effects.Publishing (Sub)
import Hoard.Effects.Quota (MessageStatus (..), Quota)
import Hoard.Effects.Verifier (Verifier, verifyBlock)
import Hoard.PeerSharing.Events (PeersReceived (..))

import Hoard.Effects.BlockRepo qualified as BlockRepo
import Hoard.Effects.PeerRepo qualified as PeerRepo
import Hoard.Effects.Publishing qualified as Sub
import Hoard.Effects.Quota qualified as Quota


component
    :: ( BlockRepo :> es
       , HeaderRepo :> es
       , Metrics :> es
       , PeerRepo :> es
       , Quota (ID Peer, Int64) :> es
       , Sub BlockReceived :> es
       , Sub HeaderReceived :> es
       , Sub PeersReceived :> es
       , Tracing :> es
       , Verifier :> es
       )
    => Component es
component =
    defaultComponent
        { name = "Persistence"
        , listeners =
            pure
                [ Sub.listen headerReceived
                , Sub.listen blockReceived
                , Sub.listen peersReceived
                ]
        }


headerReceived :: (HeaderRepo :> es, Metrics :> es, Tracing :> es) => HeaderReceived -> Eff es ()
headerReceived event = withSpan "header_received" $ do
    recordHeaderReceived
    let header = extractHeaderData event
    addAttribute "header.hash" (show header.hash)
    addAttribute "header.slot" (show header.slotNumber)
    addAttribute "header.block_number" (show header.blockNumber)
    addEvent "header_received" [("hash", show header.hash)]
    upsertHeader header event.peer event.timestamp
    addEvent "header_persisted" [("hash", show header.hash)]


blockReceived :: (BlockRepo :> es, Metrics :> es, Quota (ID Peer, Int64) :> es, Tracing :> es, Verifier :> es) => BlockReceived -> Eff es ()
blockReceived event = withSpan "block_received" $ do
    let block = extractBlockData event
        quotaKey = (event.peer.id, block.slotNumber)

    addAttribute "block.hash" (show block.hash)
    addAttribute "block.slot" (show block.slotNumber)
    addAttribute "peer.id" (show event.peer.id)
    addEvent
        "block_received"
        [ ("slot", show block.slotNumber)
        , ("hash", show block.hash)
        , ("peer_address", show event.peer.address)
        ]

    verifyBlock block >>= \case
        Left _invalidBlock ->
            addEvent "block_invalid" [("slot", show block.slotNumber), ("hash", show block.hash)]
        Right validBlock -> do
            addEvent "block_valid" [("hash", show block.hash)]
            addAttribute "peer.id" (show event.peer.id)

            Quota.withQuotaCheck quotaKey $ \count status -> do
                case status of
                    Accepted -> do
                        recordBlockReceived
                        BlockRepo.insertBlocks [validBlock]
                        addEvent "block_persisted" [("hash", show block.hash)]
                    Overflow 1 -> do
                        addAttribute "quota.exceeded" "true"
                        -- TODO: Mark the block as equivocating
                        addEvent
                            "quota_exceeded_first"
                            [ ("peer_id", show event.peer.id)
                            , ("slot", show block.slotNumber)
                            , ("count", show count)
                            ]
                    Overflow _ -> do
                        addAttribute "quota.overflow" "true"
                        addEvent
                            "quota_overflow"
                            [ ("peer_id", show event.peer.id)
                            , ("slot", show block.slotNumber)
                            , ("count", show count)
                            ]


peersReceived :: (PeerRepo :> es, Tracing :> es) => PeersReceived -> Eff es ()
peersReceived event = withSpan "peers_received" $ do
    addAttribute "peers.count" (show $ length event.peerAddresses)
    addAttribute "source.peer" (show event.peer.address)
    addEvent "persisting_peers" [("count", show $ length event.peerAddresses)]
    forM_ event.peerAddresses $ \addr ->
        addEvent "peer_address" [("host", show addr.host), ("port", show addr.port)]
    void $ PeerRepo.upsertPeers event.peerAddresses event.peer.address event.timestamp
    addEvent "peers_persisted" []


extractHeaderData :: HeaderReceived -> Header
extractHeaderData event =
    Header
        { hash = blockHashFromHeader event.header
        , slotNumber = unSlotNo $ blockSlot event.header
        , blockNumber = unBlockNo $ blockNo event.header
        , firstSeenAt = event.timestamp
        }


extractBlockData :: BlockReceived -> Block
extractBlockData BlockReceived {timestamp, block} =
    Block
        { hash = blockHashFromHeader $ getHeader block
        , slotNumber = fromIntegral $ unSlotNo $ blockSlot block
        , poolId = mkPoolID block
        , blockData = block
        , validationStatus = ""
        , validationReason = ""
        , firstSeen = timestamp
        , classification = Nothing
        , classifiedAt = Nothing
        }
