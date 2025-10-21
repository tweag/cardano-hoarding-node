module Hoard.Effects.DBRead
  ( DBRead,
    runQuery,
    runDBRead,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static (Error, throwError)
import Effectful.TH
import Hasql.Pool qualified as Pool
import Hasql.Session qualified as Session
import Hasql.Statement (Statement)

-- | Effect for read-only database queries
data DBRead :: Effect where
  RunQuery :: Text -> Statement () b -> DBRead m b

makeEffect ''DBRead

-- | Run the DBRead effect with a connection pool
runDBRead ::
  (IOE :> es, Error Text :> es) =>
  Pool.Pool ->
  Eff (DBRead : es) a ->
  Eff es a
runDBRead pool = interpret $ \_ -> \case
  RunQuery queryName stmt -> do
    result <- liftIO $ Pool.use pool (Session.statement () stmt)
    case result of
      Left err -> throwError $ "Query failed: " <> queryName <> " - " <> T.pack (show err)
      Right value -> pure value
