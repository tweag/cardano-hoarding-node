module Hoard.Server
  ( runServer,
    ServerConfig (..),
  )
where

import Control.Concurrent.STM (TQueue)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString.Char8 qualified as BS
import Data.String (fromString)
import Effectful (Eff)
import Effectful.Console.ByteString (putStrLn)
import Hoard.API (API, server)
import Hoard.Effects (AppEff, Config (..), runEffectStack)
import Hoard.Events (SomeEvent)
import Hoard.Types.DBConfig (DBPools)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import Servant
import Prelude hiding (putStrLn)

-- | Configuration for the HTTP server
data ServerConfig = ServerConfig
  { port :: Int,
    host :: String,
    eventQueue :: TQueue SomeEvent,
    dbPools :: DBPools
  }

-- | Run the Servant server with the provided configuration
runServer :: (AppEff es) => ServerConfig -> Eff es ()
runServer config = do
  -- Log startup messages
  putStrLn $ BS.pack $ "Starting Hoard server on " <> config.host <> ":" <> show config.port
  putStrLn $ BS.pack "Waiting for data..."

  -- Run Warp server (needs liftIO since Warp's runSettings is in IO)
  let settings = setPort config.port $ setHost (fromString config.host) defaultSettings
  let servantApp = hoistServer (Proxy @API) (Handler . runEffectStack (Config config.eventQueue config.dbPools)) server
  liftIO $ runSettings settings (serve (Proxy @API) servantApp)
