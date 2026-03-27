module Hoard.TestHelpers (withServer, withTestApp, TestAppDBEffs) where

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

import Atelier.Effects.Chan (Chan, runChan)
import Atelier.Effects.Clock (Clock, runClockConst)
import Atelier.Effects.Conc (Conc, runConc)
import Atelier.Effects.DB.Config (DBPools)
import Atelier.Effects.Log (Log, runLog)
import Atelier.Effects.Monitoring.Metrics (Metrics, runMetrics)
import Atelier.Effects.Monitoring.Tracing (Tracing, runTracingNoOp)
import Hoard.API (API, Routes, server)
import Hoard.Effects.BlockRepo (BlockRepo, runBlockRepo)
import Hoard.Effects.DB (DBRead, DBWrite, Rel8Read, Rel8Write, runDB, runRel8Read, runRel8Write)
import Hoard.Effects.HeaderRepo (HeaderRepo, runHeaderRepo)
import Hoard.Effects.PeerRepo (PeerRepo, runPeerRepo)
import Hoard.Types.HoardState (HoardState)

import Atelier.Effects.Log qualified as Log


withServer
    :: (es ~ TestAppDBEffs)
    => DBPools
    -> (Routes (AsClientT (Eff es)) -> Eff es b)
    -> IO b
withServer pools action = runEffectStackTestDB pools $ withTestApp action


withTestApp
    :: forall b es
     . ( BlockRepo :> es
       , Clock :> es
       , HeaderRepo :> es
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
    :: DBPools
    -> Eff TestAppDBEffs a
    -> IO a
runEffectStackTestDB pools eff = do
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
            . runReader @DBPools pools
            . runErrorNoCallStack @Text
            . runDB
            . runRel8Read
            . runRel8Write
            . runHeaderRepo
            . runBlockRepo
            . runPeerRepo
            . evalState @HoardState def
            $ eff
    either (throwIO . ErrorCall . toString) pure result


type TestAppDBEffs =
    [ State HoardState
    , PeerRepo
    , BlockRepo
    , HeaderRepo
    , Rel8Write
    , Rel8Read
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
