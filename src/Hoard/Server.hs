module Hoard.Server
    ( runServer
    )
where

import Effectful (Eff)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import Servant

import Hoard.API (API, server)
import Hoard.Effects (AppEff, runEffectStack)
import Hoard.Types.Environment (Config (..), Env (..), ServerConfig (..))

import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Log qualified as Log


runServer :: (AppEff es) => Env -> Eff es ()
runServer env = do
    _ <- Conc.fork $ do
        -- Log startup messages
        let host = toString env.config.server.host
            port = fromIntegral env.config.server.port

        Log.debug $ "Starting Hoard server on " <> toText host <> ":" <> show port

        -- Run Warp server (needs liftIO since Warp's runSettings is in IO)
        let settings = setPort port $ setHost (fromString host) defaultSettings
            servantApp =
                hoistServer
                    (Proxy @API)
                    (Handler . runEffectStack env)
                    Hoard.API.server

        liftIO $ runSettings settings (serve (Proxy @API) servantApp)

    pure ()
