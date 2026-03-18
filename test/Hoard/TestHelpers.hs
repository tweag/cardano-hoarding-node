module Hoard.TestHelpers (withServer) where

import Control.Exception (ErrorCall (..), throwIO)
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
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Exception (try)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.State.Static.Shared (State, evalState)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (testWithApplication)
import Servant (hoistServer, serve)
import Servant.Client (BaseUrl (..), ClientM, Scheme (..), hoistClient, mkClientEnv, runClientM)
import Servant.Client.Generic (AsClientT, genericClient)
import Servant.Server (Handler (..))

import Hoard.API (API, Routes, server)
import Hoard.Effects.BlockRepo (BlockRepo, runBlockRepo)
import Hoard.Effects.Chan (Chan, runChan)
import Hoard.Effects.Clock (Clock, runClockConst)
import Hoard.Effects.Conc (Conc, runConc)
import Hoard.Effects.DBRead (DBRead, runDBRead)
import Hoard.Effects.DBWrite (DBWrite, runDBWrite)
import Hoard.Effects.Log (Log, runLog)
import Hoard.Effects.Monitoring.Metrics (Metrics, runMetrics)
import Hoard.Effects.Monitoring.Tracing (Tracing, runTracingNoOp)
import Hoard.Effects.PeerRepo (PeerRepo, runPeerRepo)
import Hoard.TestHelpers.Database (TestConfig (..))
import Hoard.Types.DBConfig (DBPools)
import Hoard.Types.HoardState (HoardState)

import Hoard.Effects.Log qualified as Log


withServer
    :: (es ~ TestAppDBEffs)
    => TestConfig
    -> (Routes (AsClientT (Eff es)) -> Eff es b)
    -> IO b
withServer config action = runEffectStackTestDB config $ withTestApp action


withTestApp
    :: forall b es
     . ( BlockRepo :> es
       , Clock :> es
       , IOE :> es
       , Metrics :> es
       , PeerRepo :> es
       )
    => (Routes (AsClientT (Eff es)) -> Eff es b)
    -> Eff es b
withTestApp action = do
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
            let liftClient :: forall a. ClientM a -> Eff es a
                liftClient cm = liftIO $ runClientM cm clientEnv >>= either throwIO pure
            unlift $ action (hoistClient (Proxy @API) liftClient genericClient)


runEffectStackTestDB
    :: TestConfig
    -> Eff TestAppDBEffs a
    -> IO a
runEffectStackTestDB config eff = do
    let testTime = UTCTime (toEnum 0) 0
    result <-
        runEff
            . runConcurrent
            . runFileSystem
            . runChan
            . runTracingNoOp
            . runConc
            . runReader @Log.Config def
            . runLog
            . runClockConst testTime
            . runMetrics
            . runReader @DBPools config.pools
            . runErrorNoCallStack @Text
            . runDBWrite
            . runDBRead
            . runBlockRepo
            . runPeerRepo
            . evalState @HoardState def
            $ eff
    either (throwIO . ErrorCall . toString) pure result


type TestAppDBEffs =
    [ State HoardState
    , PeerRepo
    , BlockRepo
    , DBRead
    , DBWrite
    , Error Text
    , Reader DBPools
    , Metrics
    , Clock
    , Log
    , Reader Log.Config
    , Conc
    , Tracing
    , Chan
    , FileSystem
    , Concurrent
    , IOE
    ]
