module Hoard.Server
    ( runServer
    , ServerConfig (..)
    )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.String (fromString)
import Effectful (Eff)
import Effectful.Console.ByteString (putStrLn)
import Network.Wai.Handler.Warp (Port, defaultSettings, runSettings, setHost, setPort)
import Servant
import Prelude hiding (putStrLn)

import Data.ByteString.Char8 qualified as BS

import Hoard.API (API, server)
import Hoard.Effects (AppEff, Config (..), runEffectStack)


-- | Configuration for the HTTP server
data ServerConfig = ServerConfig
    { port :: Port
    , host :: String
    , config :: Config
    }


-- | Run the Servant server with the provided configuration
runServer :: (AppEff es) => ServerConfig -> Eff es ()
runServer config = do
    -- Log startup messages
    putStrLn $ BS.pack $ "Starting Hoard server on " <> config.host <> ":" <> show config.port
    putStrLn $ BS.pack "Waiting for data..."

    -- Run Warp server (needs liftIO since Warp's runSettings is in IO)
    let settings = setPort config.port $ setHost (fromString config.host) defaultSettings
        servantApp = hoistServer (Proxy @API) (Handler . runEffectStack config.config) server
    liftIO $ runSettings settings (serve (Proxy @API) servantApp)
