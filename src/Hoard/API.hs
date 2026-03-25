module Hoard.API
    ( API
    , Routes (..)
    , server
    )
where

import Servant
import Servant.Server.Generic (AsServerT)

import Atelier.Effects.Clock (Clock)
import Atelier.Effects.Monitoring.Metrics (Metrics, exportMetrics)
import Hoard.API.Peers (PeersAPI, peersHandler)
import Hoard.API.Util ((::>))
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.HeaderRepo (HeaderRepo)
import Hoard.Effects.PeerRepo (PeerRepo)
import Prelude hiding ((:>))

import Hoard.API.Blocks qualified as Blocks
import Hoard.API.Headers qualified as Headers


-- | Named routes for the API
data Routes mode = Routes
    { metrics :: mode :- "metrics" :> Get '[PlainText] Text
    , peers :: PeersAPI mode
    , blocks :: mode :- "blocks" :> Blocks.API
    , headers :: mode :- "headers" :> Headers.API
    }
    deriving stock (Generic)


-- | API using named routes
type API = NamedRoutes Routes


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
        }
