module Hoard.TestHelpers
    ( withServer
    , runEffectStackTest
    , withEffectStackServer
    , client
    )
where

import Control.Monad.Trans.Except (ExceptT (..))
import Data.Default (def)
import Data.Time (UTCTime (..))
import Effectful
    ( IOE
    , Limit (..)
    , Persistence (..)
    , UnliftStrategy (..)
    , runEff
    , withEffToIO
    )
import Effectful.Exception (try)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.State.Static.Shared (State, evalState, runState)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (testWithApplication)
import Servant (hoistServer, serve)
import Servant.Client (AsClientT, BaseUrl (..), ClientM, Scheme (..), mkClientEnv, runClientM)
import Servant.Client.Core (ClientError)
import Servant.Client.Generic (genericClient)
import Servant.Server (Handler (..))

import Hoard.API (API, Routes, server)
import Hoard.Data.Block (Block)
import Hoard.Data.BlockHash (BlockHash)
import Hoard.Data.BlockTag (BlockTag)
import Hoard.Effects.BlockRepo (BlockRepo, runBlockRepoState)
import Hoard.Effects.Chan (Chan, runChan)
import Hoard.Effects.Clock (Clock, runClockConst)
import Hoard.Effects.Conc (Conc, runConc)
import Hoard.Effects.Log (Log, runLog)
import Hoard.Effects.Monitoring.Metrics (Metrics, runMetrics)
import Hoard.Effects.Monitoring.Tracing (Tracing, runTracingNoOp)
import Hoard.Types.HoardState (HoardState)

import Hoard.Effects.Log qualified as Log


withEffectStackServer
    :: (MonadIO m, es ~ TestAppEffs)
    => (Int -> (forall a. ClientM a -> Eff es (Either ClientError a)) -> Eff es b)
    -> m (b, HoardState)
withEffectStackServer action = runEffectStackTest $ withServer action


withServer
    :: forall b es
     . ( BlockRepo :> es
       , IOE :> es
       , Metrics :> es
       )
    => (Int -> (forall a. ClientM a -> Eff es (Either ClientError a)) -> Eff es b)
    -> Eff es b
withServer action = do
    withEffToIO (ConcUnlift Persistent Unlimited) $ \unlift -> do
        let servantApp =
                hoistServer
                    (Proxy @API)
                    (Handler . ExceptT . unlift . try)
                    Hoard.API.server
        let app = serve (Proxy @API) servantApp

        testWithApplication (pure app) $ \port -> do
            manager <- newManager defaultManagerSettings
            let baseUrl = BaseUrl Http "localhost" port ""
            let clientEnv = mkClientEnv manager baseUrl
            let runClient :: forall a. ClientM a -> Eff es (Either ClientError a)
                runClient = liftIO . flip runClientM clientEnv
            unlift $ action port runClient


runEffectStackTest
    :: (MonadIO m)
    => Eff TestAppEffs a
    -> m (a, HoardState)
runEffectStackTest eff = liftIO $ do
    let testTime = UTCTime (toEnum 0) 0
    runEff
        . runFileSystem
        . runChan
        . runTracingNoOp
        . runConc
        . runReader @Log.Config def
        . runLog
        . runClockConst testTime
        . runMetrics
        . evalState @[Block] []
        . evalState @(Set (BlockHash, BlockTag)) mempty
        . runBlockRepoState
        . runState @HoardState def
        $ eff


type TestAppEffs =
    [ State HoardState
    , BlockRepo
    , State (Set (BlockHash, BlockTag))
    , State [Block]
    , Metrics
    , Clock
    , Log
    , Reader Log.Config
    , Conc
    , Tracing
    , Chan
    , FileSystem
    , IOE
    ]


-- | Generate servant client from API
client :: Routes (AsClientT ClientM)
client = genericClient
