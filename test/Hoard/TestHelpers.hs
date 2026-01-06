module Hoard.TestHelpers
    ( withServer
    , runEffectStackTest
    , withEffectStackServer
    , client
    , filterEvents
    )
where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan.Unagi (dupChan, newChan, readChan)
import Data.Default (def)
import Data.Dynamic (Dynamic, fromDynamic)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Effectful
    ( Eff
    , IOE
    , Limit (..)
    , Persistence (..)
    , UnliftStrategy (..)
    , runEff
    , withEffToIO
    , (:>)
    )
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Exception (try)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.State.Static.Shared (State, runState)
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as Pool
import Hoard.Effects.Chan (Chan, OutChan, runChan)
import Hoard.Effects.Conc (Conc, runConcNewScope)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (testWithApplication)
import Ouroboros.Network.IOManager (withIOManager)
import Servant (hoistServer, serve)
import Servant.Client (AsClientT, BaseUrl (..), ClientM, Scheme (..), mkClientEnv, runClientM)
import Servant.Client.Core (ClientError)
import Servant.Client.Generic (genericClient)
import Servant.Server (Handler (..))
import Prelude hiding (Reader, State, atomicModifyIORef', newIORef, readIORef, runReader, runState)

import Hoard.API (API, Routes, server)
import Hoard.Effects.Chan (InChan)
import Hoard.Effects.Environment (loadNodeConfig, loadProtocolInfo)
import Hoard.Effects.Log (Log, runLog)
import Hoard.Effects.Pub (Pub, runPub)
import Hoard.Effects.Sub (Sub, runSub)
import Hoard.Types.DBConfig (DBPools (..))
import Hoard.Types.Environment
    ( Config (..)
    , Env (..)
    , Handles (..)
    , Local (MakeLocal, nodeToClientSocket, tracerSocket)
    , LogConfig
    , NodeSocketsConfig (Local)
    , PeerSnapshotFile (..)
    , ServerConfig (..)
    , Topology (..)
    , defaultLogConfig
    )
import Hoard.Types.HoardState (HoardState)


withEffectStackServer
    :: (MonadIO m, es ~ TestAppEffs)
    => (Int -> (forall a. ClientM a -> Eff es (Either ClientError a)) -> Eff es b)
    -> m (b, HoardState, [Dynamic])
withEffectStackServer action = runEffectStackTest $ \env -> withServer env action


withServer
    :: forall b es
     . (IOE :> es)
    => Env
    -> (Int -> (forall a. ClientM a -> Eff es (Either ClientError a)) -> Eff es b)
    -> Eff es b
withServer env action = do
    app <- makeApp env
    withEffToIO (ConcUnlift Persistent Unlimited) $ \unlift -> testWithApplication (pure app) $ \port -> do
        manager <- newManager defaultManagerSettings
        let baseUrl = BaseUrl Http "localhost" port ""
        let clientEnv = mkClientEnv manager baseUrl
        let runClient :: forall a. ClientM a -> Eff es (Either ClientError a)
            runClient = liftIO . flip runClientM clientEnv
        unlift $ action port runClient


makeApp :: (IOE :> es) => Env -> Eff es Application
makeApp env =
    liftIO $ do
        let servantApp =
                hoistServer
                    (Proxy @API)
                    ( Handler
                        . ExceptT
                        . runEff
                        . try
                        . runChan
                        . runReader env.handles.inChan
                        . runPub
                    )
                    Hoard.API.server
        pure $ serve (Proxy @API) servantApp


runEffectStackTest
    :: (MonadIO m)
    => (Env -> Eff TestAppEffs a)
    -> m (a, HoardState, [Dynamic])
runEffectStackTest mkEff = liftIO $ withIOManager $ \ioManager -> do
    (inChan, _) <- newChan
    wireTap <- dupChan inChan
    pool <- Pool.acquire $ Pool.settings []
    nodeConfig <- runEff $ loadNodeConfig "config/preview/config.json"
    protocolInfo <- runEff $ loadProtocolInfo nodeConfig
    let dbPools = DBPools pool pool
    let serverConfig = ServerConfig {host = "localhost", port = 3000}
    let config =
            Config
                { server = serverConfig
                , nodeConfig
                , protocolInfo
                , nodeSockets = Local $ MakeLocal {nodeToClientSocket = "preview.socket", tracerSocket = "preview_tracer.socket"}
                , logging = defaultLogConfig
                , maxFileDescriptors = Nothing
                , topology = Topology {peerSnapshotFile = "peer-snapshot.json"}
                , peerSnapshot = PeerSnapshotFile {bigLedgerPools = []}
                }
    let handles =
            Handles
                { ioManager
                , dbPools
                , inChan
                }
    let env = Env {config, handles}
    wireTapOutput <- newIORef []
    wireTapThreadID <- forkIO $ recordMessages wireTapOutput wireTap
    (a, finalState) <-
        runEff
            . runFileSystem
            . runConcurrent
            . runChan
            . runConcNewScope
            . runReader env.config.logging
            . runReader env.handles.inChan
            . runLog
            . runSub
            . runPub
            . runState @HoardState def
            $ mkEff env
    killThread wireTapThreadID
    publishes <- fmap reverse $ readIORef wireTapOutput
    pure (a, finalState, publishes)


recordMessages :: IORef [a] -> OutChan a -> IO b
recordMessages outputRef outChan = forever $ do
    event <- readChan outChan
    atomicModifyIORef' outputRef $ \xs -> (event : xs, ())


type TestAppEffs =
    [ State HoardState
    , Pub
    , Sub
    , Log
    , Reader (InChan Dynamic)
    , Reader LogConfig
    , Conc
    , Chan
    , Concurrent
    , FileSystem
    , IOE
    ]


-- | Generate servant client from API
client :: Routes (AsClientT ClientM)
client = genericClient


filterEvents :: (Typeable a) => [Dynamic] -> [a]
filterEvents = mapMaybe fromDynamic
