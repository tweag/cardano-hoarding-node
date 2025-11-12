module Hoard.Effects.PeerRepo
    ( PeerRepo (..)
    , upsertPeers
    , getPeerByAddress
    , runPeerRepo
    )
where

import Data.Time (UTCTime)
import Effectful (Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect)

import Data.Text qualified as T
import Hasql.Statement (Statement)
import Hasql.Transaction (Transaction)
import Hasql.Transaction qualified as TX
import Hoard.DB.Schemas.Peers qualified as PeersSchema
import Hoard.Data.Peer (Peer (..), PeerAddress (..))
import Hoard.Effects.DBRead (DBRead, runQuery)
import Hoard.Effects.DBWrite (DBWrite, runTransaction)
import Rel8 (each, lit, select, where_, (&&.), (==.))
import Rel8 qualified


-- | Effect for peer repository operations
data PeerRepo :: Effect where
    -- | Upsert a list of peer addresses into the database
    --
    -- For each peer address:
    -- - Insert into database, or update only lastSeen if the peer already exists
    UpsertPeers
        :: [PeerAddress]
        -- ^ List of structured peer addresses
        -> Peer
        -- ^ The source peer that shared these addresses
        -> UTCTime
        -- ^ Timestamp when these peers were discovered
        -> PeerRepo m ()
    -- | Get a peer by its address and port
    --
    -- Returns Nothing if the peer is not found in the database
    GetPeerByAddress
        :: PeerAddress
        -- ^ The address to search for
        -> PeerRepo m (Maybe Peer)


-- | Template Haskell to generate smart constructors
makeEffect ''PeerRepo


-- | Run the PeerRepo effect using the DBRead and DBWrite effects
runPeerRepo
    :: (DBRead :> es, DBWrite :> es)
    => Eff (PeerRepo : es) a
    -> Eff es a
runPeerRepo = interpret $ \_ -> \case
    UpsertPeers peerAddrs sourcePeer timestamp ->
        runTransaction "upsert-peers" $
            upsertPeersImpl peerAddrs sourcePeer timestamp
    GetPeerByAddress peerAddr ->
        runQuery "get-peer-by-address" $
            getPeerByAddressImpl peerAddr


upsertPeersImpl
    :: [PeerAddress]
    -- ^ List of structured peer addresses
    -> Peer
    -- ^ The source peer that shared these addresses
    -> UTCTime
    -- ^ Timestamp when these peers were discovered
    -> Transaction ()
upsertPeersImpl peerAddresses sourcePeer timestamp = do
    let discoveredVia = "PeerSharing:" <> sourcePeer.address <> ":" <> T.pack (show sourcePeer.port)

    -- Upsert all peers in a single statement
    TX.statement ()
        . Rel8.run_
        $ Rel8.insert
            Rel8.Insert
                { into = PeersSchema.schema
                , rows =
                    Rel8.values $
                        peerAddresses <&> \peerAddr ->
                            PeersSchema.Row
                                { PeersSchema.id = Rel8.unsafeDefault
                                , PeersSchema.address = lit peerAddr.host
                                , PeersSchema.port = lit (fromIntegral peerAddr.port)
                                , PeersSchema.firstDiscovered = lit timestamp
                                , PeersSchema.lastSeen = lit timestamp
                                , PeersSchema.lastConnected = lit Nothing
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
                , returning = Rel8.NoReturning
                }


-- | Get a peer from the database by its address and port
getPeerByAddressImpl :: PeerAddress -> Statement () (Maybe Peer)
getPeerByAddressImpl peerAddr =
    fmap extractMaybePeer $
        Rel8.run $
            select $ do
                peer <- each PeersSchema.schema
                where_ $
                    peer.address ==. lit peerAddr.host
                        &&. peer.port ==. lit (fromIntegral peerAddr.port)
                pure peer
  where
    extractMaybePeer :: [PeersSchema.Row Rel8.Result] -> Maybe Peer
    extractMaybePeer [] = Nothing
    extractMaybePeer [row] = Just (PeersSchema.peerFromRow row)
    extractMaybePeer _ = Nothing -- Multiple matches shouldn't happen due to unique constraint
