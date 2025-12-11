module Hoard.Effects.HeaderRepo
    ( HeaderRepo (..)
    , upsertHeader
    , runHeaderRepo
    )
where

import Data.Time (UTCTime)
import Effectful (Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.TH (makeEffect)

import Hasql.Statement (Statement)
import Hasql.Transaction (Transaction)
import Hasql.Transaction qualified as TX
import Hoard.DB.Schemas.HeaderReceipts qualified as HeaderReceiptsSchema
import Hoard.DB.Schemas.Headers qualified as HeadersSchema
import Hoard.DB.Schemas.Peers qualified as PeersSchema
import Hoard.Data.Header (Header (..))
import Hoard.Data.ID (ID)
import Hoard.Data.Peer (Peer, PeerAddress (..))
import Hoard.Effects.DBWrite (DBWrite, runTransaction)
import Rel8 (lit, select)
import Rel8 qualified


-- | Effect for header repository operations
data HeaderRepo :: Effect where
    -- | Upsert a header and record receipt from a peer
    --
    -- This operation:
    -- 1. Upserts the header (inserts if new, ignores if duplicate)
    -- 2. Records that this peer sent us this header
    -- All in a single transaction.
    UpsertHeader
        :: Header
        -- ^ The header to upsert
        -> PeerAddress
        -- ^ The peer that sent us this header
        -> UTCTime
        -- ^ When we received it
        -> HeaderRepo m ()


-- | Template Haskell to generate smart constructors
makeEffect ''HeaderRepo


-- | Run the HeaderRepo effect using the DBWrite effect
runHeaderRepo
    :: (DBWrite :> es)
    => Eff (HeaderRepo : es) a
    -> Eff es a
runHeaderRepo = interpret_ \case
    UpsertHeader header peerAddr receivedAt ->
        runTransaction "upsert-header" $
            upsertHeaderImpl header peerAddr receivedAt


-- | Upsert a header and record receipt from a peer
upsertHeaderImpl :: Header -> PeerAddress -> UTCTime -> Transaction ()
upsertHeaderImpl header peerAddr receivedAt = do
    -- 1. Upsert the header (insert or ignore on conflict)
    TX.statement ()
        . Rel8.run_
        $ Rel8.insert
            Rel8.Insert
                { into = HeadersSchema.schema
                , rows =
                    Rel8.values
                        [ HeadersSchema.rowFromHeader header
                        ]
                , onConflict = Rel8.DoNothing -- Header already exists, that's fine
                , returning = Rel8.NoReturning
                }

    -- 2. Upsert the peer (create if doesn't exist, update lastSeen if exists)
    let discoveredVia = "ChainSync"
    TX.statement ()
        . Rel8.run_
        $ Rel8.insert
            Rel8.Insert
                { into = PeersSchema.schema
                , rows =
                    Rel8.values
                        [ PeersSchema.Row
                            { PeersSchema.id = Rel8.unsafeDefault
                            , PeersSchema.address = lit peerAddr.host
                            , PeersSchema.port = lit (fromIntegral peerAddr.port)
                            , PeersSchema.firstDiscovered = lit receivedAt
                            , PeersSchema.lastSeen = lit receivedAt
                            , PeersSchema.lastConnected = lit Nothing
                            , PeersSchema.discoveredVia = lit discoveredVia
                            }
                        ]
                , onConflict =
                    Rel8.DoUpdate
                        Rel8.Upsert
                            { index = \r -> (r.address, r.port)
                            , predicate = Nothing
                            , set = \_ oldRow ->
                                oldRow
                                    { PeersSchema.lastSeen = lit receivedAt
                                    }
                            , updateWhere = \_ _ -> lit True
                            }
                , returning = Rel8.NoReturning
                }

    -- 3. Look up the peer ID (guaranteed to exist after upsert)
    maybePeerId <- TX.statement () (getPeerIdByAddressStatement peerAddr)
    let peerId = case maybePeerId of
            Just pid -> pid
            Nothing -> error "Peer not found after upsert - this should never happen"

    -- 4. Record the receipt
    TX.statement ()
        . Rel8.run_
        $ Rel8.insert
            Rel8.Insert
                { into = HeaderReceiptsSchema.schema
                , rows =
                    Rel8.values
                        [ HeaderReceiptsSchema.Row
                            { HeaderReceiptsSchema.id = Rel8.unsafeDefault
                            , HeaderReceiptsSchema.hash = lit header.hash
                            , HeaderReceiptsSchema.peerId = lit peerId
                            , HeaderReceiptsSchema.receivedAt = lit receivedAt
                            }
                        ]
                , onConflict = Rel8.DoNothing -- Receipt already recorded
                , returning = Rel8.NoReturning
                }


-- | Get peer ID by address using the shared query
getPeerIdByAddressStatement :: PeerAddress -> Statement () (Maybe (ID Peer))
getPeerIdByAddressStatement peerAddr =
    fmap extractMaybePeerId $
        Rel8.run $
            select $
                (.id) <$> PeersSchema.selectPeerByAddress peerAddr
  where
    extractMaybePeerId :: [ID Peer] -> Maybe (ID Peer)
    extractMaybePeerId [] = Nothing
    extractMaybePeerId (peerId : _) = Just peerId
