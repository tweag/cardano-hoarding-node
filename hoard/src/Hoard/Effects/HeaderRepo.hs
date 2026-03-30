module Hoard.Effects.HeaderRepo
    ( HeaderRepo (..)
    , upsertHeader
    , tagHeader
    , evictHeaders
    , getHeaders
    , runHeaderRepo
    )
where

import Data.Time (UTCTime)
import Effectful (Effect)
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.TH (makeEffect)
import Rel8 (in_, lit, where_, (&&.), (<=.), (==.), (>=.))

import Rel8 qualified

import Hoard.Data.BlockHash (BlockHash)
import Hoard.Data.Header (Header (..))
import Hoard.Data.HeaderTag (HeaderTag)
import Hoard.Data.Peer (Peer (..))
import Hoard.Effects.DB (DBRead, DBWrite, delete_, insert_, select, selectTx, transact)
import Hoard.Effects.Verifier (Validity (..), Verified, getVerified)
import Hoard.Types.SlotRange (SlotRange (..))

import Hoard.DB.Schemas.BlockTags qualified as BlockTagsSchema
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
    EvictHeaders :: HeaderRepo m Int
    GetHeaders :: SlotRange -> [HeaderTag] -> HeaderRepo m [Header]


-- | Template Haskell to generate smart constructors
makeEffect ''HeaderRepo


-- | Run the HeaderRepo effect using the Rel8Read/Rel8Write effects
runHeaderRepo
    :: (DBRead :> es, DBWrite :> es)
    => Eff (HeaderRepo : es) a
    -> Eff es a
runHeaderRepo = interpret_ \case
    UpsertHeader header peer receivedAt ->
        transact "upsert_header" $ do
            insert_
                Rel8.Insert
                    { into = HeadersSchema.schema
                    , rows = Rel8.values [HeadersSchema.rowFromHeader (getVerified header)]
                    , onConflict = Rel8.DoNothing
                    , returning = Rel8.NoReturning
                    }
            insert_
                Rel8.Insert
                    { into = HeaderReceiptsSchema.schema
                    , rows =
                        Rel8.values
                            [ HeaderReceiptsSchema.Row
                                { HeaderReceiptsSchema.id = Rel8.unsafeDefault
                                , HeaderReceiptsSchema.hash = lit (getVerified header).hash
                                , HeaderReceiptsSchema.peerId = lit peer.id
                                , HeaderReceiptsSchema.receivedAt = lit receivedAt
                                }
                            ]
                    , onConflict = Rel8.DoNothing
                    , returning = Rel8.NoReturning
                    }
    TagHeader hash tags ->
        transact "tag_header"
            $ insert_
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
    EvictHeaders -> do
        hashes <- transact "evict_headers" $ do
            hashes <- selectTx $ do
                header <- Rel8.each HeadersSchema.schema
                hasHeaderTag <- HeaderTagsSchema.hashHasTag header.hash
                where_ $ Rel8.not_ hasHeaderTag
                hasBlockTag <- BlockTagsSchema.hashHasTag header.hash
                where_ $ Rel8.not_ hasBlockTag
                pure header.hash
            unless (null hashes)
                $ delete_
                    Rel8.Delete
                        { from = HeadersSchema.schema
                        , using = pure ()
                        , deleteWhere = \_ row -> row.hash `Rel8.in_` fmap lit hashes
                        , returning = Rel8.NoReturning
                        }
            pure hashes
        pure $ length hashes
    GetHeaders (SlotRange mFromSlot mToSlot) tags -> do
        rows <- select "get_headers" $ do
            header <- Rel8.each HeadersSchema.schema
            tagsMatch <-
                if length tags > 0 then
                    Rel8.exists
                        $ Rel8.filter (\tag -> header.hash ==. tag.hash &&. tag.tag `in_` litTags)
                            =<< Rel8.each HeaderTagsSchema.schema
                else
                    pure $ lit True
            where_
                $ header.slotNumber >=. lit fromSlot
                    &&. header.slotNumber <=. lit toSlot
                    &&. tagsMatch
            pure header
        pure $ mapMaybe (rightToMaybe . HeadersSchema.headerFromRow) rows
      where
        fromSlot = fromMaybe minBound mFromSlot
        toSlot = fromMaybe maxBound mToSlot
        litTags = lit <$> tags
