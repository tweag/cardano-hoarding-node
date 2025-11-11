module Hoard.Server
    ( runServer
    )
where

import Effectful (Eff)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import Servant

import Hoard.API (API, server)
import Hoard.Effects (AppEff, Config (..), ServerConfig (..), runEffectStack)

import Hoard.Effects.Log qualified as Log


-- | Run the Servant server with the provided configuration
runServer :: (AppEff es) => Config -> Eff es ()
runServer config = do
    -- Log startup messages
    let ServerConfig {host = serverHost, port = serverPort} = config.server
        host = toString serverHost
        port = fromIntegral serverPort
    Log.debug $ "Starting Hoard server on " <> toText host <> ":" <> show port
    Log.debug "Waiting for data..."

    -- Run Warp server (needs liftIO since Warp's runSettings is in IO)
    let settings = setPort port $ setHost (fromString host) defaultSettings
        servantApp = hoistServer (Proxy @API) (Handler . runEffectStack config) Hoard.API.server
    liftIO $ runSettings settings (serve (Proxy @API) servantApp)
