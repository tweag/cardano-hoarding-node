module Hoard.Effects.HeaderRepo
    ( HeaderRepo (..)
    , upsertHeader
    , runHeaderRepo
    )
where

import Data.Time (UTCTime)
import Effectful (Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret)
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
import Rel8 (each, lit, select, where_, (&&.), (==.))
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
runHeaderRepo = interpret $ \_ -> \case
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

    -- 2. Look up the peer ID
    maybePeer <- TX.statement () (getPeerIdByAddressStatement peerAddr)

    -- 3. Record the receipt (only if peer exists)
    forM_ maybePeer $ \peerId ->
        TX.statement ()
            . Rel8.run_
            $ Rel8.insert
                Rel8.Insert
                    { into = HeaderReceiptsSchema.schema
                    , rows =
                        Rel8.values
                            [ HeaderReceiptsSchema.Row
                                { HeaderReceiptsSchema.id = Rel8.unsafeDefault
                                , HeaderReceiptsSchema.blockHash = lit header.blockHash
                                , HeaderReceiptsSchema.peerId = lit peerId
                                , HeaderReceiptsSchema.receivedAt = lit receivedAt
                                }
                            ]
                    , onConflict = Rel8.DoNothing -- Receipt already recorded
                    , returning = Rel8.NoReturning
                    }


-- | Get peer ID by address
getPeerIdByAddressStatement :: PeerAddress -> Statement () (Maybe (ID Peer))
getPeerIdByAddressStatement peerAddr =
    fmap extractMaybePeerId $
        Rel8.run $
            select $ do
                peer <- each PeersSchema.schema
                where_ $
                    peer.address ==. lit peerAddr.host
                        &&. peer.port ==. lit (fromIntegral peerAddr.port)
                pure peer.id
  where
    extractMaybePeerId :: [ID Peer] -> Maybe (ID Peer)
    extractMaybePeerId [] = Nothing
    extractMaybePeerId (peerId : _) = Just peerId
