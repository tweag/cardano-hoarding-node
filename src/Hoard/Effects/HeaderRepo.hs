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
import Hasql.Transaction (Transaction)
import Rel8 (lit)

import Hasql.Transaction qualified as TX
import Rel8 qualified

import Hoard.Data.Header (Header (..))
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.DBWrite (DBWrite, runTransaction)
import Hoard.Effects.Monitoring.Tracing (Tracing, withSpan)

import Hoard.DB.Schemas.HeaderReceipts qualified as HeaderReceiptsSchema
import Hoard.DB.Schemas.Headers qualified as HeadersSchema


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
        -> Peer
        -- ^ The peer that sent us this header
        -> UTCTime
        -- ^ When we received it
        -> HeaderRepo m ()


-- | Template Haskell to generate smart constructors
makeEffect ''HeaderRepo


-- | Run the HeaderRepo effect using the DBWrite effect
runHeaderRepo
    :: (DBWrite :> es, Tracing :> es)
    => Eff (HeaderRepo : es) a
    -> Eff es a
runHeaderRepo = interpret_ \case
    UpsertHeader header peer receivedAt ->
        withSpan "header_repo.upsert_header"
            $ runTransaction "upsert-header"
            $ upsertHeaderImpl header peer receivedAt


-- | Upsert a header and record receipt from a peer
upsertHeaderImpl :: Header -> Peer -> UTCTime -> Transaction ()
upsertHeaderImpl header peer receivedAt = do
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

    -- 2. Record the receipt
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
                            , HeaderReceiptsSchema.peerId = lit peer.id
                            , HeaderReceiptsSchema.receivedAt = lit receivedAt
                            }
                        ]
                , onConflict = Rel8.DoNothing -- Receipt already recorded
                , returning = Rel8.NoReturning
                }
