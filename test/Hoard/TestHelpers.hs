module Hoard.TestHelpers
    ( withServer
    , runEffectStackTest
    , withEffectStackServer
    , client
    , filterEvents
    )
where

import Prelude hiding (State, atomicModifyIORef', newIORef, readIORef, runState)

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan.Unagi (OutChan, dupChan, newChan, readChan)
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
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.State.Static.Shared (State, runState)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (testWithApplication)
import Ouroboros.Network.IOManager (withIOManager)
import Servant (hoistServer, serve)
import Servant.Client (AsClientT, BaseUrl (..), ClientM, Scheme (..), mkClientEnv, runClientM)
import Servant.Client.Core (ClientError)
import Servant.Client.Generic (genericClient)

import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as Pool

import Hoard.API (API, Routes, server)
import Hoard.Effects (Config (..), ServerConfig (..), runEffectStack)
import Hoard.Effects.Log (Log, runLog)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.Pub (Pub, runPub)
import Hoard.Effects.Sub (Sub, runSub)
import Hoard.Types.DBConfig (DBPools (..))
import Hoard.Types.HoardState (HoardState)


withEffectStackServer
    :: (MonadIO m, es ~ TestAppEffs)
    => (Int -> (forall a. ClientM a -> Eff es (Either ClientError a)) -> Eff es b)
    -> m (b, HoardState, [Dynamic])
withEffectStackServer action = runEffectStackTest $ \config -> withServer config action


withServer
    :: forall b es
     . (IOE :> es)
    => Config
    -> (Int -> (forall a. ClientM a -> Eff es (Either ClientError a)) -> Eff es b)
    -> Eff es b
withServer config action = do
    app <- makeApp config
    withEffToIO (ConcUnlift Persistent Unlimited) $ \unlift -> testWithApplication (pure app) $ \port -> do
        manager <- newManager defaultManagerSettings
        let baseUrl = BaseUrl Http "localhost" port ""
        let clientEnv = mkClientEnv manager baseUrl
        let runClient :: forall a. ClientM a -> Eff es (Either ClientError a)
            runClient = liftIO . flip runClientM clientEnv
        unlift $ action port runClient


makeApp :: (IOE :> es) => Config -> Eff es Application
makeApp config =
    liftIO $ do
        let servantApp = hoistServer (Proxy @API) (runEffectStack config) Hoard.API.server
        pure $ serve (Proxy @API) servantApp


runEffectStackTest
    :: (MonadIO m)
    => (Config -> Eff TestAppEffs a)
    -> m (a, HoardState, [Dynamic])
runEffectStackTest mkEff = liftIO $ withIOManager $ \ioManager -> do
    (inChan, _) <- newChan
    wireTap <- dupChan inChan
    pool <- Pool.acquire $ Pool.settings []
    let dbPools = DBPools pool pool
    let serverConfig = ServerConfig {host = "localhost", port = 3000}
    let config =
            Config
                { ioManager
                , dbPools
                , inChan
                , server = serverConfig
                , protocolConfigPath = "config/preview/config.json"
                , localNodeSocketPath = "preview.socket"
                , logging = Log.defaultConfig
                }
    wireTapOutput <- newIORef []
    wireTapThreadID <- forkIO $ recordMessages wireTapOutput wireTap
    (a, finalState) <-
        runEff
            . runLog config.logging
            . runFileSystem
            . runConcurrent
            . runSub config.inChan
            . runPub config.inChan
            . runState def
            $ mkEff config
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
    , Concurrent
    , FileSystem
    , Log
    , IOE
    ]


-- | Generate servant client from API
client :: Routes (AsClientT ClientM)
client = genericClient


filterEvents :: (Typeable a) => [Dynamic] -> [a]
filterEvents = mapMaybe fromDynamic
