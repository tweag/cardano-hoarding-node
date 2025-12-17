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

import Hoard.Effects ((::>))
import Hoard.Effects.Pub (Pub, publish)
import Hoard.Events.HeaderReceived (Header, HeaderReceived (..))


-- | Named routes for the API
data Routes mode = Routes
    { receiveHeader :: mode :- "header" :> ReqBody '[JSON] Header :> Post '[JSON] NoContent
    }
    deriving (Generic)


-- | API using named routes
type API = NamedRoutes Routes


-- | Server implementation, handlers run in Eff monad
server :: (Pub ::> es) => Routes (AsServerT (Eff es))
server =
    Routes
        { receiveHeader = headerHandler
        }


-- | Handler for header data
headerHandler :: (Pub ::> es) => Header -> Eff es NoContent
headerHandler headerData = do
    -- Publish event via Publisher effect
    publish $ HeaderReceived headerData

    pure NoContent
