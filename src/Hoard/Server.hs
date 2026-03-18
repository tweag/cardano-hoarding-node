module Hoard.Server
    ( component
    , Config (..)
    )
where

import Control.Monad.Trans.Except (ExceptT (..))
import Effectful (IOE, Limit (..), Persistence (..), UnliftStrategy (..), withEffToIO)
import Effectful.Exception (try)
import Effectful.Reader.Static (Reader, ask)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import Servant (Handler (..), hoistServer, serve)

import Atelier.Component (Component (..), defaultComponent)
import Atelier.Effects.Clock (Clock)
import Atelier.Effects.Log (Log)
import Atelier.Effects.Monitoring.Metrics (Metrics)
import Hoard.API (API, server)
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.PeerRepo (PeerRepo)
import Hoard.Server.Config (Config (..))

import Atelier.Effects.Log qualified as Log


component
    :: ( BlockRepo :> es
       , Clock :> es
       , IOE :> es
       , Log :> es
       , Metrics :> es
       , PeerRepo :> es
       , Reader Config :> es
       )
    => Component es
component =
    defaultComponent
        { name = "Server"
        , start = do
            cfg <- ask
            -- Log startup messages
            let host = toString cfg.host
                port = fromIntegral cfg.port

            Log.debug $ "Starting Hoard server on " <> toText host <> ":" <> show port

            -- Run Warp server (blocking call, but runSystem auto-forks start phases)
            let settings = setPort port $ setHost (fromString host) defaultSettings
            servantApp <- withEffToIO (ConcUnlift Persistent Unlimited) \unlift ->
                pure
                    $ hoistServer
                        (Proxy @API)
                        (Handler . ExceptT . unlift . try)
                        Hoard.API.server

            liftIO $ runSettings settings (serve (Proxy @API) servantApp)
        }
