module Atelier.Effects.DB.Rel8
    ( Rel8Read
    , Rel8Write
    , select
    , transact
    , runRel8Read
    , runRel8Write
    , runRel8
    )
where

import Effectful (Effect)
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.TH (makeEffect)
import Hasql.Transaction (Transaction)
import Rel8 (Query, Serializable)

import Rel8 qualified

import Atelier.Effects.DB (DBRead, DBWrite, runQuery, runTransaction)


-- | Effect for Rel8 SELECT queries, interpreted via 'DBRead'.
data Rel8Read :: Effect where
    Select :: (Serializable exprs results) => Text -> Query exprs -> Rel8Read m [results]


-- | Effect for Hasql transactions (typically built using Rel8 DML),
-- interpreted via 'DBWrite'.
data Rel8Write :: Effect where
    Transact :: Text -> Transaction a -> Rel8Write m a


makeEffect ''Rel8Read
makeEffect ''Rel8Write


runRel8Read :: (DBRead :> es) => Eff (Rel8Read : es) a -> Eff es a
runRel8Read = interpret_ \case
    Select name query -> runQuery name (Rel8.run $ Rel8.select query)


runRel8Write :: (DBWrite :> es) => Eff (Rel8Write : es) a -> Eff es a
runRel8Write = interpret_ \case
    Transact name tx -> runTransaction name tx


runRel8 :: (DBRead :> es, DBWrite :> es) => Eff (Rel8Read : Rel8Write : es) a -> Eff es a
runRel8 = runRel8Write . runRel8Read
