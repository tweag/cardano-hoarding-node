module Hoard.Effects.DBWrite
    ( DBWrite
    , runTransaction
    , runDBWrite
    )
where

import Effectful (Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpretWith_)
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader, asks)
import Effectful.TH
import Hasql.Transaction.Sessions (IsolationLevel (ReadCommitted), Mode (Write), transaction)
import Prelude hiding (Reader, asks)

import Hasql.Pool qualified as Pool
import Hasql.Transaction qualified as Transaction

import Hoard.Effects.Monitoring.Tracing (SpanStatus (..), Tracing, addAttribute, setStatus, withSpan)
import Hoard.Types.DBConfig (DBPools)

import Hoard.Types.DBConfig qualified as DB


-- | Effect for write database transactions
data DBWrite :: Effect where
    RunTransaction :: Text -> Transaction.Transaction a -> DBWrite m a


makeEffect ''DBWrite


-- | Run the DBWrite effect with a connection pool
runDBWrite
    :: (Error Text :> es, IOE :> es, Reader DBPools :> es, Tracing :> es)
    => Eff (DBWrite : es) a
    -> Eff es a
runDBWrite eff = do
    pool <- asks $ DB.writerPool
    interpretWith_ eff \case
        RunTransaction txName tx -> withSpan "db.transaction" do
            addAttribute @Text "db.operation" "write"
            addAttribute "db.transaction.name" txName
            addAttribute @Text "db.isolation_level" "ReadCommitted"

            result <- liftIO $ Pool.use pool (transaction ReadCommitted Write tx)
            case result of
                Left err -> do
                    setStatus $ Error $ "Transaction failed: " <> show err
                    throwError $ "Transaction failed: " <> txName <> " - " <> show err
                Right value -> do
                    setStatus Ok
                    pure value
