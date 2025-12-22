module Hoard.Effects.DBRead
    ( DBRead
    , runQuery
    , runDBRead
    )
where

import Effectful (Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpretWith_)
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader, asks)
import Effectful.TH
import Hasql.Pool qualified as Pool
import Hasql.Session qualified as Session
import Hasql.Statement (Statement)
import Prelude hiding (Reader, asks)

import Hoard.Types.DBConfig (DBPools)
import Hoard.Types.DBConfig qualified as DB


-- | Effect for read-only database queries
data DBRead :: Effect where
    RunQuery :: Text -> Statement () b -> DBRead m b


makeEffect ''DBRead


-- | Run the DBRead effect with a connection pool
runDBRead
    :: (Error Text :> es, IOE :> es, Reader DBPools :> es)
    => Eff (DBRead : es) a
    -> Eff es a
runDBRead eff = do
    pool <- asks $ DB.readerPool
    interpretWith_ eff \case
        RunQuery queryName stmt -> do
            result <- liftIO $ Pool.use pool (Session.statement () stmt)
            case result of
                Left err -> throwError $ "Query failed: " <> queryName <> " - " <> show err
                Right value -> pure value
