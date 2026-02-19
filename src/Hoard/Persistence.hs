module Hoard.Persistence
    ( component
    , PeerSlotKey
    ) where

import Effectful (Eff, (:>))
import Ouroboros.Consensus.Block (BlockNo (..))
import Ouroboros.Consensus.Block.Abstract (blockNo, blockSlot, getHeader, unSlotNo)
import Prelude hiding (Reader, State, ask, evalState, get, modify, runReader)

import Hoard.Component (Component (..), defaultComponent)
import Hoard.Data.Block (Block (..))
import Hoard.Data.BlockHash (blockHashFromHeader)
import Hoard.Data.Header (Header (..))
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.Data.PeerNote (NoteType (..))
import Hoard.Data.PoolID (mkPoolID)
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.HeaderRepo (HeaderRepo, upsertHeader)
import Hoard.Effects.Monitoring.Metrics (Metrics)
import Hoard.Effects.Monitoring.Metrics.Definitions (recordBlockReceived, recordHeaderReceived)
import Hoard.Effects.Monitoring.Tracing
    ( Attr (..)
    , ToAttribute
    , ToAttributeShow (..)
    , Tracing
    , addAttribute
    , addEvent
    , withSpan
    )
import Hoard.Effects.PeerNoteRepo (PeerNoteRepo)
import Hoard.Effects.PeerRepo (PeerRepo)
import Hoard.Effects.Publishing (Sub)
import Hoard.Effects.Quota (MessageStatus (..), Quota)
import Hoard.Effects.Verifier (Verifier, verifyBlock)
import Hoard.Events.BlockFetch (BlockReceived (..))
import Hoard.Events.ChainSync (HeaderReceived (..))
import Hoard.Events.PeerSharing (PeersReceived (..))
import Hoard.Sentry (AdversarialBehavior (..))

import Hoard.Effects.BlockRepo qualified as BlockRepo
import Hoard.Effects.PeerNoteRepo qualified as PeerNoteRepo
import Hoard.Effects.PeerRepo qualified as PeerRepo
import Hoard.Effects.Publishing qualified as Sub
import Hoard.Effects.Quota qualified as Quota


component
    :: ( BlockRepo :> es
       , HeaderRepo :> es
       , Metrics :> es
       , PeerNoteRepo :> es
       , PeerRepo :> es
       , Quota PeerSlotKey :> es
       , Sub AdversarialBehavior :> es
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
                , Sub.listen noteAdversarialBehavior
                ]
        }


newtype PeerSlotKey = PeerSlotKey (ID Peer, Int64)
    deriving stock (Eq, Generic, Ord, Show)
    deriving (Hashable) via (ID Peer, Int64)
    deriving (ToAttribute) via ToAttributeShow PeerSlotKey


headerReceived :: (HeaderRepo :> es, Metrics :> es, Tracing :> es) => HeaderReceived -> Eff es ()
headerReceived event = withSpan "header_received" $ do
    recordHeaderReceived
    let header = extractHeaderData event
    addAttribute "header.hash" header.hash
    addAttribute "header.slot" $ fromIntegral @_ @Int64 header.slotNumber
    addAttribute "header.block_number" $ fromIntegral @_ @Int64 header.blockNumber
    addEvent "header_received" [("hash", header.hash)]
    upsertHeader header event.peer event.timestamp
    addEvent "header_persisted" [("hash", header.hash)]


blockReceived
    :: ( BlockRepo :> es
       , Metrics :> es
       , Quota PeerSlotKey :> es
       , Tracing :> es
       , Verifier :> es
       )
    => BlockReceived -> Eff es ()
blockReceived event = withSpan "block_received" $ do
    let block = extractBlockData event
        quotaKey = PeerSlotKey (event.peer.id, block.slotNumber)

    addAttribute "block.hash" block.hash
    addAttribute "block.slot" block.slotNumber
    addAttribute "peer.id" event.peer.id
    addEvent
        "block_received"
        [ ("slot", Attr block.slotNumber)
        , ("hash", Attr block.hash)
        , ("peer_address", Attr event.peer.address)
        ]

    verifyBlock block >>= \case
        Left _invalidBlock ->
            addEvent "block_invalid" [("slot", Attr block.slotNumber), ("hash", Attr block.hash)]
        Right validBlock -> do
            addEvent "block_valid" [("hash", block.hash)]
            addAttribute "peer.id" event.peer.id

            Quota.withQuotaCheck quotaKey $ \count status -> do
                case status of
                    Accepted -> do
                        recordBlockReceived
                        BlockRepo.insertBlocks [validBlock]
                        addEvent "block_persisted" [("hash", block.hash)]
                    Overflow 1 -> do
                        addAttribute "quota.exceeded" True
                        -- TODO: Mark the block as equivocating
                        addEvent
                            "quota_exceeded_first"
                            [ ("peer_id", Attr event.peer.id)
                            , ("slot", Attr block.slotNumber)
                            , ("count", Attr count)
                            ]
                    Overflow _ -> do
                        addAttribute "quota.overflow" True
                        addEvent
                            "quota_overflow"
                            [ ("peer_id", Attr event.peer.id)
                            , ("slot", Attr block.slotNumber)
                            , ("count", Attr count)
                            ]


peersReceived :: (PeerRepo :> es, Tracing :> es) => PeersReceived -> Eff es ()
peersReceived event = withSpan "peers_received" $ do
    addAttribute "peers.count" $ length event.peerAddresses
    addAttribute "source.peer" event.peer.address
    addEvent "persisting_peers" [("count", length event.peerAddresses)]
    forM_ event.peerAddresses $ \addr ->
        addEvent "peer_address" [("host", Attr $ show @Text addr.host), ("port", Attr addr.port)]
    void $ PeerRepo.upsertPeers event.peerAddresses event.peer.address event.timestamp
    addEvent @Int "peers_persisted" []


noteAdversarialBehavior :: (PeerNoteRepo :> es, Tracing :> es) => AdversarialBehavior -> Eff es ()
noteAdversarialBehavior event = withSpan "note_adversarial_behavior" do
    _ <- PeerNoteRepo.saveNote event.peer.id Adversarial "Peer exceeded duplicate block warning threshold"
    pure ()


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
