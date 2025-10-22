module Hoard.Effects.DBWrite
  ( DBWrite,
    runTransaction,
    runDBWrite,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static (Error, throwError)
import Effectful.TH
import Hasql.Pool qualified as Pool
import Hasql.Transaction qualified as Transaction
import Hasql.Transaction.Sessions (IsolationLevel (ReadCommitted), Mode (Write), transaction)

-- | Effect for write database transactions
data DBWrite :: Effect where
  RunTransaction :: Text -> Transaction.Transaction a -> DBWrite m a

makeEffect ''DBWrite

-- | Run the DBWrite effect with a connection pool
runDBWrite ::
  (IOE :> es, Error Text :> es) =>
  Pool.Pool ->
  Eff (DBWrite : es) a ->
  Eff es a
runDBWrite pool = interpret $ \_ -> \case
  RunTransaction txName tx -> do
    result <- liftIO $ Pool.use pool (transaction ReadCommitted Write tx)
    case result of
      Left err -> do
        liftIO $ putStrLn $ T.unpack $ "DBWrite: " <> txName <> " failed: " <> T.pack (show err)
        throwError $ "Transaction failed: " <> txName <> " - " <> T.pack (show err)
      Right value -> pure value
