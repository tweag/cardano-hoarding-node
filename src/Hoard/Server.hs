module Hoard.Server
    ( runServer
    )
where

import Effectful (Eff, IOE, withEffToIO, (:>))
import Effectful.Exception (try)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import Servant (Handler (..), hoistServer, serve)

import Hoard.API (API, server)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.Pub (Pub)
import Hoard.Types.Environment (Config (..), Env (..), ServerConfig (..))


runServer
    :: ( Conc :> es
       , IOE :> es
       , Log :> es
       , Pub :> es
       )
    => Env
    -> Eff es ()
runServer env = do
    _ <- Conc.fork $ do
        -- Log startup messages
        let host = toString env.config.server.host
            port = fromIntegral env.config.server.port

        Log.debug $ "Starting Hoard server on " <> toText host <> ":" <> show port

        -- Run Warp server (needs liftIO since Warp's runSettings is in IO)
        let settings = setPort port $ setHost (fromString host) defaultSettings
        servantApp <- withEffToIO Conc.concStrat \unlift -> do
            pure $
                hoistServer
                    (Proxy @API)
                    (Handler . ExceptT . unlift . try)
                    Hoard.API.server

        liftIO $ runSettings settings (serve (Proxy @API) servantApp)

    pure ()
