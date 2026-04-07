module Hoard.API
    ( API
    , Routes (..)
    , server
    )
where

import Servant.Server.Generic (AsServerT)

import Atelier.Effects.Clock (Clock)
import Atelier.Effects.Monitoring.Metrics (Metrics, exportMetrics)
import Hoard.API.OpenAPI (spec)
import Hoard.API.Peers (peersHandler)
import Hoard.API.Routes (API, Routes (..))
import Hoard.API.Util ((::>))
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.HeaderRepo (HeaderRepo)
import Hoard.Effects.PeerRepo (PeerRepo)

import Hoard.API.Blocks qualified as Blocks
import Hoard.API.Headers qualified as Headers


-- | Server implementation, handlers run in Eff monad
server
    :: ( BlockRepo ::> es
       , Clock ::> es
       , HeaderRepo ::> es
       , Metrics ::> es
       , PeerRepo ::> es
       )
    => Routes (AsServerT (Eff es))
server =
    Routes
        { metrics = exportMetrics
        , peers = peersHandler
        , blocks = Blocks.handler
        , headers = Headers.handler
        , openapi = pure spec
        }
