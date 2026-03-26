module Hoard.API.Routes
    ( API
    , Routes (..)
    )
where

import Data.OpenApi (OpenApi)
import Servant (Get, JSON, NamedRoutes, PlainText, (:-), (:>))

import Hoard.API.Peers (PeersAPI)
import Prelude hiding ((:>))

import Hoard.API.Blocks qualified as Blocks
import Hoard.API.Headers qualified as Headers


-- | Named routes for the API
data Routes mode = Routes
    { metrics :: mode :- "metrics" :> Get '[PlainText] Text
    , peers :: PeersAPI mode
    , blocks :: mode :- "blocks" :> Blocks.API
    , headers :: mode :- "headers" :> Headers.API
    , openapi :: mode :- "openapi.json" :> Get '[JSON] OpenApi
    }
    deriving stock (Generic)


-- | API using named routes
type API = NamedRoutes Routes
