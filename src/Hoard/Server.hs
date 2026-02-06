module Hoard.Server
    ( runServer
    )
where

import Effectful (Eff, IOE, withEffToIO, (:>))
import Effectful.Exception (try)
import Effectful.Reader.Static (Reader, ask)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import Servant (Handler (..), hoistServer, serve)
import Prelude hiding (Reader, ask)

import Hoard.API (API, server)
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.Metrics (Metrics)
import Hoard.Effects.Publishing (Pub)
import Hoard.Types.Environment (Config (..), Env (..), ServerConfig (..))


runServer
    :: ( BlockRepo :> es
       , Conc :> es
       , IOE :> es
       , Log :> es
       , Metrics :> es
       , Pub :> es
       , Reader Env :> es
       )
    => Eff es ()
runServer = do
    env <- ask
    _ <- Conc.fork $ do
        -- Log startup messages
        let host = toString env.config.server.host
            port = fromIntegral env.config.server.port

        Log.debug $ "Starting Hoard server on " <> toText host <> ":" <> show port

        -- Run Warp server (needs liftIO since Warp's runSettings is in IO)
        let settings = setPort port $ setHost (fromString host) defaultSettings
        servantApp <- withEffToIO Conc.concStrat \unlift ->
            pure $
                hoistServer
                    (Proxy @API)
                    (Handler . ExceptT . unlift . try)
                    Hoard.API.server

        liftIO $ runSettings settings (serve (Proxy @API) servantApp)

    pure ()
