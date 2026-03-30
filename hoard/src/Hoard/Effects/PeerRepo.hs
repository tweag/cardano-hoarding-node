module Hoard.Effects.PeerRepo
    ( PeerRepo (..)
    , upsertPeers
    , getPeerByAddress
    , getAllPeers
    , hasPeers
    , updatePeerFailure
    , updateLastConnected
    , getEligiblePeers
    , getPinnedPeers
    , pinPeers
    , unpinPeers
    , getEligiblePinnedPeers
    , runPeerRepo
    )
where

import Data.Time (NominalDiffTime, UTCTime, calendarTimeTime)
import Effectful (Effect)
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.TH (makeEffect)
import Rel8 (in_, lit, or_, (&&.), (==.), (>.))

import Data.Map.Strict qualified as Map
import Hasql.Transaction qualified as TX
import Rel8 qualified
import Rel8.Expr.Time qualified as Rel8

import Hoard.Data.ID (ID (..))
import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.Effects.DB (DBRead, DBWrite, Transaction, select, transact)

import Hoard.DB.Schemas.Peers qualified as PeersSchema
import Hoard.DB.Schemas.SelectedPeers qualified as SelectedPeersSchema


-- | Effect for peer repository operations
data PeerRepo :: Effect where
    -- | Upsert a list of peer addresses into the database
    --
    -- For each peer address:
    -- - Insert into database, or update only lastSeen if the peer already exists
    UpsertPeers
        :: Set PeerAddress
        -- ^ Set of structured peer addresses
        -> PeerAddress
        -- ^ The source peer that shared these addresses
        -> UTCTime
        -- ^ Timestamp when these peers were discovered
        -> PeerRepo m (Set Peer)
    -- | Get a peer by its address and port
    --
    -- Returns Nothing if the peer is not found in the database
    GetPeerByAddress
        :: PeerAddress
        -- ^ The address to search for
        -> PeerRepo m (Maybe Peer)
    -- | Get all peers from the database
    GetAllPeers
        :: PeerRepo m (Set Peer)
    -- | Check if any peers exist in the database
    HasPeers
        :: PeerRepo m Bool
    -- | Update a peer's last failure time
    UpdatePeerFailure
        :: Peer
        -- ^ The peer that failed
        -> UTCTime
        -- ^ The failure timestamp
        -> PeerRepo m ()
    -- | Update time for last connection to a peer.
    UpdateLastConnected
        :: ID Peer
        -- ^ Peer to update
        -> UTCTime
        -- ^ New "last connected" time
        -> PeerRepo m ()
    -- | Fetch a set of peers eligible for collectors based on the passed
    -- arguments.
    GetEligiblePeers
        :: NominalDiffTime
        -- ^ Threshold for how long ago a peer must have last failed.
        -> Set (ID Peer)
        -- ^ Peers that we are currently connected to.
        -> Word
        -- ^ The maximum number of peers we want.
        -> PeerRepo m (Set Peer)
    -- | Get all peers on the pinned peers list.
    GetPinnedPeers
        :: PeerRepo m [Peer]
    -- | Add peers to the pinned peers list in a single transaction.
    --
    -- We don't take (ID Peer) for ergonomics: operators can submit any address
    -- and peers are upserted into the peers table on the fly.
    -- Bulk-upserts addresses into peers, then bulk-inserts into selected_peers
    -- (ON CONFLICT DO NOTHING). Idempotent.
    PinPeers
        :: UTCTime
        -- ^ Current timestamp
        -> [(PeerAddress, Maybe Text)]
        -- ^ (address, optional operator note) pairs
        -> PeerRepo m [Peer]
    -- | Remove peers from the pinned peers list. Idempotent — silently
    -- succeeds if any peer is not found or was not pinned.
    UnpinPeers
        :: [PeerAddress]
        -> PeerRepo m ()
    -- | Like GetEligiblePeers but restricted to pinned peers only.
    --
    -- Used in Manual mode.
    GetEligiblePinnedPeers
        :: NominalDiffTime
        -- ^ Threshold for how long ago a peer must have last failed.
        -> Set (ID Peer)
        -- ^ Peers that we are currently connected to.
        -> Word
        -- ^ The maximum number of peers we want.
        -> PeerRepo m (Set Peer)


-- | Template Haskell to generate smart constructors
makeEffect ''PeerRepo


-- | Run the PeerRepo effect using the Rel8Read and Rel8Write effects
runPeerRepo
    :: (DBRead :> es, DBWrite :> es)
    => Eff (PeerRepo : es) a
    -> Eff es a
runPeerRepo = interpret_ \case
    UpsertPeers peerAddrs sourcePeer timestamp ->
        transact "upsert_peers"
            $ fromList <$> upsertPeersImpl (toList peerAddrs) (peerSharingDiscoveredVia sourcePeer) timestamp
    GetPeerByAddress peerAddr -> do
        rows <- select "get_peer_by_address" $ PeersSchema.selectPeerByAddress peerAddr
        pure $ case rows of
            [row] -> Just (PeersSchema.peerFromRow row)
            _ -> Nothing
    GetAllPeers ->
        fromList . map PeersSchema.peerFromRow <$> select "get_all_peers" (Rel8.each PeersSchema.schema)
    HasPeers ->
        not . null <$> select "has_peers" (Rel8.limit 1 $ Rel8.each PeersSchema.schema)
    UpdatePeerFailure peer timestamp ->
        transact "update_peer_failure" $ updatePeerFailureImpl peer timestamp
    UpdateLastConnected peerId timestamp ->
        transact "update_last_connected" $ updateLastConnectedImpl peerId timestamp
    GetEligiblePeers failureTimeout alreadyConnectedPeers limit -> do
        rows <- select "get_eligible_peers" $ Rel8.limit limit do
            peer <- Rel8.each PeersSchema.schema
            applyEligibilityFilter failureTimeout alreadyConnectedPeers peer
        pure $ fromList $ map PeersSchema.peerFromRow rows
    GetPinnedPeers ->
        map PeersSchema.peerFromRow <$> select "get_pinned_peers" do
            sp <- Rel8.each SelectedPeersSchema.schema
            peer <- Rel8.each PeersSchema.schema
            Rel8.where_ $ peer.id Rel8.==. sp.peerId
            pure peer
    PinPeers timestamp entries ->
        transact "pin_peers" $ pinPeersImpl timestamp entries
    UnpinPeers peerAddrs ->
        transact "unpin_peer" $ unpinPeersImpl peerAddrs
    GetEligiblePinnedPeers failureTimeout alreadyConnectedPeers limit -> do
        rows <- select "get_eligible_pinned_peers" $ Rel8.limit limit do
            sp <- Rel8.each SelectedPeersSchema.schema
            peer <- Rel8.each PeersSchema.schema
            Rel8.where_ $ peer.id ==. sp.peerId
            applyEligibilityFilter failureTimeout alreadyConnectedPeers peer
        pure $ fromList $ map PeersSchema.peerFromRow rows


peerSharingDiscoveredVia :: PeerAddress -> Text
peerSharingDiscoveredVia p = "PeerSharing:" <> show p.host <> ":" <> show p.port


upsertPeersImpl
    :: [PeerAddress]
    -- ^ Peer addresses to upsert
    -> Text
    -- ^ The discoveredVia value to use for new peers
    -> UTCTime
    -- ^ Timestamp when these peers were discovered
    -> Transaction [Peer]
upsertPeersImpl peerAddresses discoveredVia timestamp = do
    -- Upsert all peers in a single statement and get back the full peer records
    rows <-
        TX.statement ()
            . Rel8.run
            $ Rel8.insert
                Rel8.Insert
                    { into = PeersSchema.schema
                    , rows =
                        Rel8.values
                            $ peerAddresses <&> \peerAddr ->
                                PeersSchema.Row
                                    { PeersSchema.id = Rel8.unsafeDefault
                                    , PeersSchema.address = lit peerAddr.host
                                    , PeersSchema.port = lit (fromIntegral peerAddr.port)
                                    , PeersSchema.firstDiscovered = lit timestamp
                                    , PeersSchema.lastSeen = lit timestamp
                                    , PeersSchema.lastConnected = lit Nothing
                                    , PeersSchema.lastFailureTime = lit Nothing
                                    , PeersSchema.discoveredVia = lit discoveredVia
                                    }
                    , onConflict =
                        Rel8.DoUpdate
                            Rel8.Upsert
                                { index = \r -> (r.address, r.port)
                                , predicate = Nothing
                                , -- Only update lastSeen, keep everything else unchanged
                                  set = \_ oldRow ->
                                    oldRow
                                        { PeersSchema.lastSeen = lit timestamp
                                        }
                                , updateWhere = \_ _ -> lit True
                                }
                    , returning = Rel8.Returning Prelude.id
                    }
    pure $ map PeersSchema.peerFromRow rows


-- | Update a peer's last failure time
updatePeerFailureImpl :: Peer -> UTCTime -> Transaction ()
updatePeerFailureImpl peer timestamp = do
    TX.statement ()
        . Rel8.run_
        $ Rel8.update
            Rel8.Update
                { target = PeersSchema.schema
                , from = pure ()
                , updateWhere = \_ row -> row.id Rel8.==. lit peer.id
                , set = \_ row ->
                    row
                        { PeersSchema.lastFailureTime = lit (Just timestamp)
                        }
                , returning = Rel8.NoReturning
                }


-- | Update a peer's last failure time
updateLastConnectedImpl :: ID Peer -> UTCTime -> Transaction ()
updateLastConnectedImpl peerId timestamp = do
    TX.statement ()
        . Rel8.run_
        $ Rel8.update
            Rel8.Update
                { target = PeersSchema.schema
                , from = pure ()
                , updateWhere = \_ row -> row.id Rel8.==. lit peerId
                , set = \_ row ->
                    row
                        { PeersSchema.lastConnected = lit (Just timestamp)
                        }
                , returning = Rel8.NoReturning
                }


pinPeersImpl :: UTCTime -> [(PeerAddress, Maybe Text)] -> Transaction [Peer]
pinPeersImpl timestamp entries = do
    let noteByAddr = Map.fromList entries
    peers <- upsertPeersImpl (map fst entries) "pinned" timestamp
    -- Bulk insert into selected_peers with per-peer notes; ON CONFLICT DO NOTHING is idempotent
    TX.statement ()
        . Rel8.run_
        $ Rel8.insert
            Rel8.Insert
                { into = SelectedPeersSchema.schema
                , rows =
                    Rel8.values
                        $ peers <&> \peer ->
                            SelectedPeersSchema.Row
                                { SelectedPeersSchema.peerId = lit peer.id
                                , SelectedPeersSchema.note = lit $ Map.findWithDefault Nothing peer.address noteByAddr
                                , SelectedPeersSchema.addedAt = lit timestamp
                                }
                , onConflict = Rel8.DoNothing
                , returning = Rel8.NoReturning
                }
    pure peers


unpinPeersImpl :: [PeerAddress] -> Transaction ()
unpinPeersImpl peerAddrs =
    TX.statement ()
        . Rel8.run_
        $ Rel8.delete
            Rel8.Delete
                { from = SelectedPeersSchema.schema
                , using = Rel8.each PeersSchema.schema
                , deleteWhere = \peer sp ->
                    sp.peerId ==. peer.id
                        &&. Rel8.or_
                            [ peer.address ==. lit addr.host &&. peer.port ==. lit (fromIntegral addr.port)
                            | addr <- peerAddrs
                            ]
                , returning = Rel8.NoReturning
                }


-- | Filter a peer row by eligibility: not recently failed and not currently connected.
applyEligibilityFilter
    :: NominalDiffTime
    -> Set (ID Peer)
    -> PeersSchema.Row Rel8.Expr
    -> Rel8.Query (PeersSchema.Row Rel8.Expr)
applyEligibilityFilter failureTimeout currentPeers peer = do
    let didNotFailRecently =
            Rel8.nullable
                Rel8.true
                (\ft -> Rel8.diffTime Rel8.now ft >. lit (calendarTimeTime failureTimeout))
                peer.lastFailureTime
        isNotACurrentPeer = Rel8.not_ $ peer.id `in_` (lit <$> toList currentPeers)
    Rel8.where_ $ isNotACurrentPeer &&. didNotFailRecently
    pure peer
