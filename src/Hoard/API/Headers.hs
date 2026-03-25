module Hoard.API.Headers
    ( Routes (..)
    , API
    , handler
    ) where

import Servant (Get, JSON, NamedRoutes, QueryParam, QueryParams, (:-), (:>))
import Servant.Server.Generic (AsServerT)

import Hoard.API.Data.Header (Header)
import Hoard.API.Util ((::>))
import Hoard.Data.HeaderTag (HeaderTag)
import Hoard.Effects.HeaderRepo (HeaderRepo)
import Hoard.Types.SlotRange (SlotRange (..))
import Prelude hiding ((:>))

import Hoard.API.Data.Header qualified as Header
import Hoard.Effects.HeaderRepo qualified as HeaderRepo


data Routes mode = Routes
    { get
        :: mode
            :- QueryParam "minSlot" Int64
                :> QueryParam "maxSlot" Int64
                :> QueryParams "tags" HeaderTag
                :> Get '[JSON] [Header]
    }
    deriving stock (Generic)


type API = NamedRoutes Routes


handler :: (HeaderRepo ::> es) => Routes (AsServerT (Eff es))
handler =
    Routes
        { get = \minSlot maxSlot tags ->
            fmap Header.fromHeader <$> HeaderRepo.getHeaders (SlotRange minSlot maxSlot) tags
        }
