module Hoard.Effects.HeaderRepo
    ( HeaderRepo (..)
    , upsertHeader
    , tagHeader
    , runHeaderRepo
    )
where

import Data.Time (UTCTime)
import Effectful (Effect)
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.TH (makeEffect)
import Hasql.Transaction (Transaction)
import Rel8 (lit)

import Hasql.Transaction qualified as TX
import Rel8 qualified

import Hoard.Data.BlockHash (BlockHash)
import Hoard.Data.Header (Header (..))
import Hoard.Data.HeaderTag (HeaderTag)
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.DBWrite (DBWrite, runTransaction)
import Hoard.Effects.Monitoring.Tracing (Tracing, withSpan)
import Hoard.Effects.Verifier (Validity (..), Verified, getVerified)

import Hoard.DB.Schemas.HeaderReceipts qualified as HeaderReceiptsSchema
import Hoard.DB.Schemas.HeaderTags qualified as HeaderTagsSchema
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
        :: Verified 'Valid Header
        -- ^ The header to upsert
        -> Peer
        -- ^ The peer that sent us this header
        -> UTCTime
        -- ^ When we received it
        -> HeaderRepo m ()
    TagHeader :: BlockHash -> [HeaderTag] -> HeaderRepo m ()


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
            $ upsertHeaderImpl (getVerified header) peer receivedAt
    TagHeader hash tags ->
        withSpan "header_repo.tag_header"
            $ runTransaction "tag-header"
            $ tagHeaderImpl hash tags


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


tagHeaderImpl :: BlockHash -> [HeaderTag] -> Transaction ()
tagHeaderImpl hash tags =
    TX.statement ()
        . Rel8.run_
        $ Rel8.insert
            Rel8.Insert
                { into = HeaderTagsSchema.schema
                , rows = Rel8.values $ mkRow <$> tags
                , onConflict = Rel8.DoNothing
                , returning = Rel8.NoReturning
                }
  where
    mkRow tag =
        HeaderTagsSchema.Row
            { id = Rel8.unsafeDefault
            , hash = lit hash
            , tag = lit tag
            , taggedAt = Rel8.unsafeDefault
            }
