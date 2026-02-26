module Hoard.API
    ( API
    , Routes (..)
    , server
    )
where

import Servant
import Servant.Server.Generic (AsServerT)

import Hoard.API.Violations (ViolationsAPI, violationsHandler)
import Hoard.Effects ((::>))
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.Monitoring.Metrics (Metrics, exportMetrics)
import Prelude hiding ((:>))


-- | Named routes for the API
data Routes mode = Routes
    { metrics :: mode :- "metrics" :> Get '[PlainText] Text
    , violations :: ViolationsAPI mode
    }
    deriving (Generic)


-- | API using named routes
type API = NamedRoutes Routes


-- | Server implementation, handlers run in Eff monad
server :: (BlockRepo ::> es, Metrics ::> es) => Routes (AsServerT (Eff es))
server =
    Routes
        { metrics = exportMetrics
        , violations = violationsHandler
        }
