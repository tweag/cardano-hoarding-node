module Hoard.Effects.HeaderRepo
    ( HeaderRepo (..)
    , insertHeader
    , runHeaderRepo
    )
where

import Effectful (Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect)

import Hasql.Transaction (Transaction)
import Hasql.Transaction qualified as TX
import Hoard.DB.Schemas.Headers qualified as HeadersSchema
import Hoard.Data.Header (Header (..))
import Hoard.Effects.DBWrite (DBWrite, runTransaction)
import Rel8 (lit)
import Rel8 qualified


-- | Effect for header repository operations
data HeaderRepo :: Effect where
    -- | Insert a header into the database
    --
    -- For step 1, this is a simple insert operation.
    -- Deduplication and upsert logic will be added in step 2.
    InsertHeader
        :: Header
        -- ^ The header to insert
        -> HeaderRepo m ()


-- | Template Haskell to generate smart constructors
makeEffect ''HeaderRepo


-- | Run the HeaderRepo effect using the DBWrite effect
runHeaderRepo
    :: (DBWrite :> es)
    => Eff (HeaderRepo : es) a
    -> Eff es a
runHeaderRepo = interpret $ \_ -> \case
    InsertHeader header ->
        runTransaction "insert-header" $
            insertHeaderImpl header


-- | Insert a header into the database
insertHeaderImpl :: Header -> Transaction ()
insertHeaderImpl header = do
    TX.statement ()
        . Rel8.run_
        $ Rel8.insert
            Rel8.Insert
                { into = HeadersSchema.schema
                , rows =
                    Rel8.values
                        [ HeadersSchema.Row
                            { HeadersSchema.id = Rel8.unsafeDefault
                            , HeadersSchema.receivedAt = lit header.receivedAt
                            , HeadersSchema.receivedFromPeerId = lit header.receivedFromPeerId
                            }
                        ]
                , onConflict = Rel8.Abort
                , returning = Rel8.NoReturning
                }
