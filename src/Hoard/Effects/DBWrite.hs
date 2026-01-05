module Hoard.Effects.DBWrite
    ( DBWrite
    , runTransaction
    , runDBWrite
    )
where

import Effectful (Eff, Effect)
import Effectful.Dispatch.Dynamic (interpretWith_)
import Effectful.Error.Static (throwError)
import Effectful.Reader.Static (asks)
import Effectful.TH
import Hasql.Pool qualified as Pool
import Hasql.Transaction qualified as Transaction
import Hasql.Transaction.Sessions (IsolationLevel (ReadCommitted), Mode (Write), transaction)
import Prelude hiding (Reader, asks)

import Hoard.Effects.Log qualified as Log
import Hoard.Types.DBConfig qualified as DB


-- | Effect for write database transactions
data DBWrite :: Effect where
    RunTransaction :: Text -> Transaction.Transaction a -> DBWrite m a


makeEffect ''DBWrite


-- | Run the DBWrite effect with a connection pool
runDBWrite :: (_) => Eff (DBWrite : es) a -> Eff es a
runDBWrite eff = do
    pool <- asks $ DB.writerPool
    interpretWith_ eff \case
        RunTransaction txName tx -> do
            result <- liftIO $ Pool.use pool (transaction ReadCommitted Write tx)
            case result of
                Left err -> do
                    Log.debug $ "DBWrite: " <> txName <> " failed: " <> show err
                    throwError $ "Transaction failed: " <> txName <> " - " <> show err
                Right value -> pure value
