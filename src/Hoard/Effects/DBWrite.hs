module Hoard.Effects.DBWrite
    ( DBWrite
    , runTransaction
    , runDBWrite
    )
where

import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static (Error, throwError)
import Effectful.TH
import Hasql.Transaction.Sessions (IsolationLevel (ReadCommitted), Mode (Write), transaction)

import Data.Text qualified as T
import Hasql.Pool qualified as Pool
import Hasql.Transaction qualified as Transaction

import Hoard.Effects.Log (Log)

import Hoard.Effects.Log qualified as Log


-- | Effect for write database transactions
data DBWrite :: Effect where
    RunTransaction :: Text -> Transaction.Transaction a -> DBWrite m a


makeEffect ''DBWrite


-- | Run the DBWrite effect with a connection pool
runDBWrite
    :: (Error Text :> es, IOE :> es, Log :> es)
    => Pool.Pool
    -> Eff (DBWrite : es) a
    -> Eff es a
runDBWrite pool = interpret $ \_ -> \case
    RunTransaction txName tx -> do
        result <- liftIO $ Pool.use pool (transaction ReadCommitted Write tx)
        case result of
            Left err -> do
                Log.debug $ "DBWrite: " <> txName <> " failed: " <> T.pack (show err)
                throwError $ "Transaction failed: " <> txName <> " - " <> T.pack (show err)
            Right value -> pure value
