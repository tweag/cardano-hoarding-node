module Hoard.API
    ( API
    , Routes (..)
    , server
    )
where

import Effectful (Eff)
import Servant hiding (Header)
import Servant.Server.Generic (AsServerT)
import Prelude hiding (appendFile, readFile)

import Hoard.API.Violations (ViolationsAPI, violationsHandler)
import Hoard.Effects ((::>))
import Hoard.Effects.BlockRepo (BlockRepo)
import Hoard.Effects.Monitoring.Metrics (Metrics, exportMetrics)
import Hoard.Effects.Publishing (Pub, publish)
import Hoard.Events.HeaderReceived (Header, HeaderReceived (..))


-- | Named routes for the API
data Routes mode = Routes
    { receiveHeader :: mode :- "header" :> ReqBody '[JSON] Header :> Post '[JSON] NoContent
    , metrics :: mode :- "metrics" :> Get '[PlainText] Text
    , violations :: ViolationsAPI mode
    }
    deriving (Generic)


-- | API using named routes
type API = NamedRoutes Routes


-- | Server implementation, handlers run in Eff monad
server :: (Pub ::> es, Metrics ::> es, BlockRepo ::> es) => Routes (AsServerT (Eff es))
server =
    Routes
        { receiveHeader = headerHandler
        , metrics = exportMetrics
        , violations = violationsHandler
        }


-- | Handler for header data
headerHandler :: (Pub ::> es) => Header -> Eff es NoContent
headerHandler headerData = do
    -- Publish event via Publisher effect
    publish $ HeaderReceived headerData

    pure NoContent
