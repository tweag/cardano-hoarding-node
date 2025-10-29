module Hoard.Server
    ( runServer
    )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.String (fromString)
import Effectful (Eff)
import Effectful.Console.ByteString (putStrLn)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import Servant
import Prelude hiding (putStrLn)

import Data.ByteString.Char8 qualified as BS
import Data.Text qualified as T

import Hoard.API (API, server)
import Hoard.Effects (AppEff, Config (..), ServerConfig (..), runEffectStack)


-- | Run the Servant server with the provided configuration
runServer :: (AppEff es) => Config -> Eff es ()
runServer config = do
    -- Log startup messages
    let ServerConfig {host = serverHost, port = serverPort} = config.server
        host = T.unpack serverHost
        port = fromIntegral serverPort
    putStrLn $ BS.pack $ "Starting Hoard server on " <> host <> ":" <> show port
    putStrLn $ BS.pack "Waiting for data..."

    -- Run Warp server (needs liftIO since Warp's runSettings is in IO)
    let settings = setPort port $ setHost (fromString host) defaultSettings
        servantApp = hoistServer (Proxy @API) (Handler . runEffectStack config) Hoard.API.server
    liftIO $ runSettings settings (serve (Proxy @API) servantApp)
