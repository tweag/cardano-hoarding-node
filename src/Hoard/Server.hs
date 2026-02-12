module Hoard.Server
    ( Server (..)
    )
where

import Effectful (IOE, withSeqEffToIO, (:>))
import Effectful.Exception (try)
import Effectful.Reader.Static (Reader, ask)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import Servant (Handler (..), hoistServer, serve)
import Prelude hiding (Reader, ask)

import Hoard.API (API, server)
import Hoard.Component (Component (..))
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Monitoring.Metrics (Metrics)
import Hoard.Effects.Monitoring.Tracing (Tracing)
import Hoard.Types.Environment (Config (..), Env (..), ServerConfig (..))

import Hoard.Effects.Log qualified as Log


data Server = Server


instance Component Server es where
    type
        Effects Server es =
            ( BlockRepo :> es
            , Conc :> es
            , IOE :> es
            , Log :> es
            , Metrics :> es
            , Reader Env :> es
            , Tracing :> es
            )


    start = do
        env <- ask
        -- Log startup messages
        let host = toString env.config.server.host
            port = fromIntegral env.config.server.port

        Log.debug $ "Starting Hoard server on " <> toText host <> ":" <> show port

        -- Run Warp server (blocking call, but runSystem auto-forks start phases)
        let settings = setPort port $ setHost (fromString host) defaultSettings
        servantApp <- withSeqEffToIO \unlift ->
            pure
                $ hoistServer
                    (Proxy @API)
                    (Handler . ExceptT . unlift . try)
                    Hoard.API.server

        liftIO $ runSettings settings (serve (Proxy @API) servantApp)
