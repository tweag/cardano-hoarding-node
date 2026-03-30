module Hoard.Effects.DB
    ( DBRead
    , DBWrite
    , runQuery
    , runTransaction
    , runDB
    , runDBRead
    , runDBWrite
    , Transaction
    , select
    , select1
    , transact
    , insert_
    , update_
    , delete_
    , selectTx
    )
where

import Effectful (IOE)
import Effectful.Error.Static (Error)
import Effectful.Reader.Static (Reader)

import Atelier.Effects.DB (DBRead, DBReadMetricNames (..), DBWrite, runQuery, runTransaction)
import Atelier.Effects.DB.Config (DBPools)
import Atelier.Effects.DB.Rel8 (Transaction, delete_, insert_, select, select1, selectTx, transact, update_)
import Atelier.Effects.Monitoring.Metrics (Metrics)
import Atelier.Effects.Monitoring.Tracing (Tracing)
import Hoard.Effects.Monitoring.Metrics.Definitions (metricDBQueries, metricDBQueryDuration, metricDBQueryErrors)

import Atelier.Effects.DB qualified as Atelier


hoardDBMetricNames :: DBReadMetricNames
hoardDBMetricNames =
    DBReadMetricNames
        { queries = metricDBQueries
        , errors = metricDBQueryErrors
        , duration = metricDBQueryDuration
        }


runDB
    :: (Error Text :> es, IOE :> es, Metrics :> es, Reader DBPools :> es, Tracing :> es)
    => Eff (DBRead : DBWrite : es) a
    -> Eff es a
runDB = Atelier.runDB hoardDBMetricNames


runDBRead
    :: (Error Text :> es, IOE :> es, Metrics :> es, Reader DBPools :> es, Tracing :> es)
    => Eff (DBRead : es) a
    -> Eff es a
runDBRead = Atelier.runDBRead hoardDBMetricNames


runDBWrite
    :: (Error Text :> es, IOE :> es, Reader DBPools :> es, Tracing :> es)
    => Eff (DBWrite : es) a
    -> Eff es a
runDBWrite = Atelier.runDBWrite
