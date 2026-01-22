module Hoard.TestHelpers
    ( withServer
    , runEffectStackTest
    , withEffectStackServer
    , client
    , filterEvents
    )
where

import Control.Concurrent.QSem (newQSem)
import Data.Default (def)
import Data.Dynamic (Dynamic, fromDynamic)
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
import Hoard.Effects.Chan (Chan, runChan)
import Hoard.Effects.Conc (Conc, runConcNewScope)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (testWithApplication)
import Ouroboros.Network.IOManager (withIOManager)
import Servant (hoistServer, serve)
import Servant.Client (AsClientT, BaseUrl (..), ClientM, Scheme (..), mkClientEnv, runClientM)
import Servant.Client.Core (ClientError)
import Servant.Client.Generic (genericClient)
import Servant.Server (Handler (..))
import Prelude hiding (Reader, State, atomicModifyIORef', newIORef, readIORef, runReader, runState)

import Effectful.Writer.Static.Shared (Writer, runWriter)
import Hoard.API (API, Routes, server)
import Hoard.BlockFetch.Config qualified as BlockFetch
import Hoard.ChainSync.Config ()
import Hoard.Collectors.Config ()
import Hoard.Effects.Environment (loadNodeConfig, loadProtocolInfo)
import Hoard.Effects.Log (Log, runLog)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.Metrics (Metrics, runMetrics)
import Hoard.Effects.Publishing (Pub, runPubWriter)
import Hoard.KeepAlive.Config ()
import Hoard.PeerSharing.Config ()
import Hoard.Types.DBConfig (DBPools (..))
import Hoard.Types.Environment
    ( CardanoNodeIntegrationConfig (..)
    , CardanoProtocolHandles (..)
    , CardanoProtocolsConfig (..)
    , Config (..)
    , Env (..)
    , Handles (..)
    , Local (MakeLocal, nodeToClientSocket, tracerSocket)
    , MonitoringConfig (..)
    , NodeSocketsConfig (Local)
    , PeerSnapshotFile (..)
    , ServerConfig (..)
    , Topology (..)
    , TxSubmissionConfig (..)
    )
import Hoard.Types.HoardState (HoardState)


withEffectStackServer
    :: (MonadIO m, es ~ TestAppEffs)
    => (Int -> (forall a. ClientM a -> Eff es (Either ClientError a)) -> Eff es b)
    -> m (b, HoardState, [Dynamic])
withEffectStackServer action = runEffectStackTest $ \_env -> withServer action


withServer
    :: forall b es
     . ( IOE :> es
       , Pub :> es
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
    -> m (a, HoardState, [Dynamic])
runEffectStackTest mkEff = liftIO $ withIOManager $ \ioManager -> do
    pool <- Pool.acquire $ Pool.settings []
    nodeConfig <- runEff $ loadNodeConfig "config/preview/config.json"
    protocolInfo <- runEff $ loadProtocolInfo nodeConfig
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
    let monitoringCfg = MonitoringConfig {pollingIntervalSeconds = 10}
    let cardanoNodeIntegrationCfg =
            CardanoNodeIntegrationConfig
                { sshServerAliveIntervalSeconds = 60
                , immutableTipRefreshSeconds = 20
                }
    cardanoProtocolHandles <- do
        qSem <- newQSem 10
        pure
            CardanoProtocolHandles
                { blockFetch = BlockFetch.Handles {qSem}
                }
    let config =
            Config
                { server = serverConfig
                , nodeConfig
                , protocolInfo
                , nodeSockets = Local $ MakeLocal {nodeToClientSocket = "preview.socket", tracerSocket = "preview_tracer.socket"}
                , logging = def
                , maxFileDescriptors = Nothing
                , topology = Topology {peerSnapshotFile = "peer-snapshot.json"}
                , peerSnapshot = PeerSnapshotFile {bigLedgerPools = []}
                , collectors = def
                , cardanoProtocols
                , monitoring = monitoringCfg
                , cardanoNodeIntegration = cardanoNodeIntegrationCfg
                }
    let handles =
            Handles
                { ioManager
                , dbPools
                , cardanoProtocols = cardanoProtocolHandles
                }
    let env = Env {config, handles}
    ((a, finalState), events) <-
        runEff
            . runFileSystem
            . runConcurrent
            . runChan
            . runConcNewScope
            . runReader env.config.logging
            . runLog
            . runMetrics
            . runWriter
            . runPubWriter
            . runState @HoardState def
            $ mkEff env
    pure (a, finalState, events)


type TestAppEffs =
    [ State HoardState
    , Pub
    , Writer [Dynamic]
    , Metrics
    , Log
    , Reader Log.Config
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
