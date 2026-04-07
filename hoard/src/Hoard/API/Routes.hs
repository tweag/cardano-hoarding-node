module Hoard.API.Routes
    ( API
    , Routes (..)
    )
where

import Data.OpenApi (OpenApi)
import Servant (Get, JSON, NamedRoutes, PlainText, (:-), (:>))

import Prelude hiding ((:>))

import Hoard.API.Blocks qualified as Blocks
import Hoard.API.Headers qualified as Headers
import Hoard.API.Peers qualified as Peers


-- | Named routes for the API
data Routes mode = Routes
    { metrics :: mode :- "metrics" :> Get '[PlainText] Text
    , peers :: mode :- "peers" :> Peers.API
    , blocks :: mode :- "blocks" :> Blocks.API
    , headers :: mode :- "headers" :> Headers.API
    , openapi :: mode :- "openapi.json" :> Get '[JSON] OpenApi
    }
    deriving stock (Generic)


-- | API using named routes
type API = NamedRoutes Routes
