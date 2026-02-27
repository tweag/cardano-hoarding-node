module Hoard.Persistence
    ( component
    , PeerSlotKey
    ) where

import Data.Time (UTCTime)
import Effectful.State.Static.Shared (State, gets)
import Ouroboros.Consensus.Block (BlockNo (..))
import Ouroboros.Consensus.Block.Abstract (blockNo, blockSlot, getHeader, unSlotNo)

import Hoard.Component (Component (..), defaultComponent)
import Hoard.Data.Block (Block (..))
import Hoard.Data.BlockHash (blockHashFromHeader)
import Hoard.Data.Header (Header (..))
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer (..))
import Hoard.Data.PeerNote (NoteType (..))
import Hoard.Data.PoolID (mkPoolID)
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.HeaderRepo (HeaderRepo)
import Hoard.Effects.HoardStateRepo (HoardStateRepo)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Monitoring.Metrics (Metrics)
import Hoard.Effects.Monitoring.Metrics.Definitions (recordBlockReceived, recordHeaderReceived)
import Hoard.Effects.Monitoring.Tracing
    ( ToAttribute
    , ToAttributeShow (..)
    , Tracing
    , addAttribute
    , withSpan
    )
import Hoard.Effects.PeerNoteRepo (PeerNoteRepo)
import Hoard.Effects.PeerRepo (PeerRepo)
import Hoard.Effects.Publishing (Sub)
import Hoard.Effects.Quota (MessageStatus (..), Quota)
import Hoard.Effects.Verifier (Verifier, getVerified, verifyBlock, verifyHeader)
import Hoard.Events.BlockFetch (BlockReceived (..))
import Hoard.Events.ChainSync (HeaderReceived (..))
import Hoard.Events.PeerSharing (PeersReceived (..))
import Hoard.Sentry (AdversarialBehavior (..))
import Hoard.Types.Cardano (CardanoBlock, CardanoHeader)
import Hoard.Types.HoardState (HoardState (..))

import Hoard.Data.BlockTag qualified as BlockTag
import Hoard.Data.HeaderTag qualified as HeaderTag
import Hoard.Effects.BlockRepo qualified as BlockRepo
import Hoard.Effects.HeaderRepo qualified as HeaderRepo
import Hoard.Effects.HoardStateRepo qualified as HoardStateRepo
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.PeerNoteRepo qualified as PeerNoteRepo
import Hoard.Effects.PeerRepo qualified as PeerRepo
import Hoard.Effects.Publishing qualified as Sub
import Hoard.Effects.Quota qualified as Quota
import Hoard.ImmutableTip qualified as ImmutableTip


component
    :: ( BlockRepo :> es
       , HeaderRepo :> es
       , HoardStateRepo :> es
       , Log :> es
       , Metrics :> es
       , PeerNoteRepo :> es
       , PeerRepo :> es
       , Quota PeerSlotKey :> es
       , State HoardState :> es
       , Sub AdversarialBehavior :> es
       , Sub BlockReceived :> es
       , Sub HeaderReceived :> es
       , Sub ImmutableTip.Refreshed :> es
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
                , Sub.listen_ noteAdversarialBehavior
                , Sub.listen_ persistImmutableTipOnRefresh
                ]
        }


newtype PeerSlotKey = PeerSlotKey (ID Peer, Int64)
    deriving stock (Eq, Generic, Ord, Show)
    deriving (Hashable) via (ID Peer, Int64)
    deriving (ToAttribute) via ToAttributeShow PeerSlotKey


headerReceived
    :: ( HeaderRepo :> es
       , Metrics :> es
       , Tracing :> es
       , Verifier :> es
       )
    => UTCTime -> HeaderReceived -> Eff es ()
headerReceived timestamp event = withSpan "persistence.header_received" do
    let header = extractHeaderData timestamp event.header
    verifyHeader header >>= \case
        Left invalidHeader -> do
            addAttribute "header.valid" False
            HeaderRepo.tagHeader
                ((.hash) $ getVerified invalidHeader)
                [HeaderTag.CorruptHeaderIntegrity]
        Right validHeader -> do
            addAttribute "header.valid" True
            HeaderRepo.upsertHeader validHeader event.peer timestamp
            recordHeaderReceived


blockReceived
    :: ( BlockRepo :> es
       , Log :> es
       , Metrics :> es
       , Quota PeerSlotKey :> es
       , Tracing :> es
       , Verifier :> es
       )
    => UTCTime -> BlockReceived -> Eff es ()
blockReceived timestamp event = withSpan "persistence.block_received" do
    let block = extractBlockData timestamp event.block
        quotaKey = PeerSlotKey (event.peer.id, block.slotNumber)

    res <- verifyBlock block
    case res of
        Left _invalidBlock -> do
            addAttribute "block.valid" False
            BlockRepo.tagBlock block.hash [BlockTag.CorruptBlockIntegrity]
        Right validBlock -> do
            addAttribute "block.valid" True

            Quota.withQuotaCheck quotaKey \_ -> \case
                Accepted -> do
                    recordBlockReceived
                    BlockRepo.insertBlocks [validBlock]
                Overflow overflowCount -> do
                    addAttribute "quota.exceeded" True
                    Log.warn
                        $ mconcat
                            [ "Received equivocating block from "
                            , show event.peer.address
                            , " for slot "
                            , show block.slotNumber
                            , " "
                            , show overflowCount
                            , " times"
                            ]


peersReceived :: (PeerRepo :> es, Tracing :> es) => UTCTime -> PeersReceived -> Eff es ()
peersReceived timestamp event =
    withSpan "persistence.peers_received"
        $ void
        $ PeerRepo.upsertPeers
            event.peerAddresses
            event.peer.address
            timestamp


noteAdversarialBehavior :: (PeerNoteRepo :> es, Tracing :> es) => AdversarialBehavior -> Eff es ()
noteAdversarialBehavior event =
    withSpan "persistence.note_adversarial_behavior"
        $ void
        $ PeerNoteRepo.saveNote event.peer.id Adversarial "Peer exceeded duplicate block warning threshold"


-- | Persist the updated immutable tip to the database.
persistImmutableTipOnRefresh
    :: ( HoardStateRepo :> es
       , State HoardState :> es
       , Tracing :> es
       )
    => ImmutableTip.Refreshed -> Eff es ()
persistImmutableTipOnRefresh ImmutableTip.Refreshed = withSpan "immutable_tip.persist" do
    tip <- gets (.immutableTip)
    HoardStateRepo.persistImmutableTip tip


extractHeaderData :: UTCTime -> CardanoHeader -> Header
extractHeaderData timestamp header =
    Header
        { hash = blockHashFromHeader header
        , headerData = header
        , slotNumber = unSlotNo $ blockSlot header
        , blockNumber = unBlockNo $ blockNo header
        , firstSeenAt = timestamp
        }


extractBlockData :: UTCTime -> CardanoBlock -> Block
extractBlockData timestamp block =
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
