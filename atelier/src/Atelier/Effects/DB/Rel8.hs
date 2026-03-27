module Atelier.Effects.DB.Rel8
    ( Rel8Read
    , Rel8Write
    , select
    , transact
    , runRel8Read
    , runRel8Write
    , runRel8
    , insert_
    , update_
    , delete_
    , selectTx
    )
where

import Effectful (Effect)
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.TH (makeEffect)
import Hasql.Transaction (Transaction)
import Rel8 (Delete, Insert, Query, Serializable, Update)

import Hasql.Transaction qualified as TX
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


-- | Run a Rel8 INSERT inside a 'Transaction'.
insert_ :: Insert a -> Transaction ()
insert_ = TX.statement () . Rel8.run_ . Rel8.insert


-- | Run a Rel8 UPDATE inside a 'Transaction'.
update_ :: Update a -> Transaction ()
update_ = TX.statement () . Rel8.run_ . Rel8.update


-- | Run a Rel8 DELETE inside a 'Transaction'.
delete_ :: Delete a -> Transaction ()
delete_ = TX.statement () . Rel8.run_ . Rel8.delete


-- | Run a Rel8 SELECT inside a 'Transaction'.
selectTx :: (Serializable exprs results) => Query exprs -> Transaction [results]
selectTx = TX.statement () . Rel8.run . Rel8.select
