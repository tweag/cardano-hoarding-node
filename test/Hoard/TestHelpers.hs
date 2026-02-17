module Hoard.TestHelpers
    ( withServer
    , runEffectStackTest
    , withEffectStackServer
    , client
    )
where

import Data.Default (def)
import Data.Time (UTCTime (..))
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
import Effectful.Exception (try)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.State.Static.Shared (State, runState)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (testWithApplication)
import Ouroboros.Network.IOManager (withIOManager)
import Servant (hoistServer, serve)
import Servant.Client (AsClientT, BaseUrl (..), ClientM, Scheme (..), mkClientEnv, runClientM)
import Servant.Client.Core (ClientError)
import Servant.Client.Generic (genericClient)
import Servant.Server (Handler (..))
import Prelude hiding (Reader, State, atomicModifyIORef', newIORef, readIORef, runReader, runState)

import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as Pool

import Hoard.API (API, Routes, server)
import Hoard.ChainSync.Config ()
import Hoard.Data.Block (Block)
import Hoard.Effects.BlockRepo (BlockRepo, runBlockRepoState)
import Hoard.Effects.Chan (Chan, runChan)
import Hoard.Effects.Clock (Clock, runClockConst)
import Hoard.Effects.Conc (Conc, runConc)
import Hoard.Effects.Environment (loadNodeConfig, loadProtocolInfo)
import Hoard.Effects.Log (Log, runLog)
import Hoard.Effects.Monitoring.Metrics (Metrics, runMetrics)
import Hoard.KeepAlive.Config ()
import Hoard.PeerManager.Config ()
import Hoard.PeerSharing.Config ()
import Hoard.Types.DBConfig (DBPools (..))
import Hoard.Types.Environment
    ( CardanoNodeIntegrationConfig (..)
    , CardanoProtocolsConfig (..)
    , Config (..)
    , Env (..)
    , Handles (..)
    , Local (MakeLocal, nodeToClientSocket, tracerSocket)
    , NodeSocketsConfig (Local)
    , PeerSnapshotFile (..)
    , ServerConfig (..)
    , Topology (..)
    , TracingConfig (..)
    , TxSubmissionConfig (..)
    )
import Hoard.Types.HoardState (HoardState)

import Hoard.Effects.Log qualified as Log


withEffectStackServer
    :: (MonadIO m, es ~ TestAppEffs)
    => (Int -> (forall a. ClientM a -> Eff es (Either ClientError a)) -> Eff es b)
    -> m (b, HoardState)
withEffectStackServer action = runEffectStackTest $ \_env -> withServer action


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
    => (Env -> Eff TestAppEffs a)
    -> m (a, HoardState)
runEffectStackTest mkEff = liftIO $ withIOManager $ \ioManager -> do
    pool <- Pool.acquire $ Pool.settings []
    nodeConfig <- runEff $ loadNodeConfig "config/preview/config.json"
    protocolInfo <- runEff $ loadProtocolInfo nodeConfig
    let testTime = UTCTime (toEnum 0) 0
    let dbPools = DBPools pool pool
    let serverConfig = ServerConfig {host = "localhost", port = 3000}
    let cardanoProtocols =
            CardanoProtocolsConfig
                { peerSharing = def
                , keepAlive = def
                , blockFetch = def
                , chainSync = def
                , txSubmission = TxSubmissionConfig {maximumIngressQueue = 10}
                }
    let cardanoNodeIntegrationCfg =
            CardanoNodeIntegrationConfig
                { sshServerAliveIntervalSeconds = 60
                , immutableTipRefreshSeconds = 20
                }
    let config =
            Config
                { server = serverConfig
                , nodeConfig
                , protocolInfo
                , nodeSockets = Local $ MakeLocal {nodeToClientSocket = "preview.socket", tracerSocket = "preview_tracer.socket"}
                , logging = def
                , tracing = TracingConfig {enabled = False, serviceName = "hoard-test", otlpEndpoint = "http://localhost:4318"}
                , maxFileDescriptors = Nothing
                , topology = Topology {peerSnapshotFile = "peer-snapshot.json"}
                , peerSnapshot = PeerSnapshotFile {bigLedgerPools = []}
                , cardanoProtocols
                , cardanoNodeIntegration = cardanoNodeIntegrationCfg
                , peerManager = def
                , nodeToNode = def
                }
    let handles =
            Handles
                { ioManager
                , dbPools
                }
    let env = Env {config, handles}
    ((a, finalState), _blockState) <-
        runEff
            . runFileSystem
            . runChan
            . runConc
            . runReader env.config.logging
            . runLog
            . runClockConst testTime
            . runMetrics
            . runState @[Block] []
            . runBlockRepoState
            . runState @HoardState def
            $ mkEff env
    pure (a, finalState)


type TestAppEffs =
    [ State HoardState
    , BlockRepo
    , State [Block]
    , Metrics
    , Clock
    , Log
    , Reader Log.Config
    , Conc
    , Chan
    , FileSystem
    , IOE
    ]


-- | Generate servant client from API
client :: Routes (AsClientT ClientM)
client = genericClient
