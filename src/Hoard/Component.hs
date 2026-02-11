{-# LANGUAGE AllowAmbiguousTypes #-}

module Hoard.Component
    ( Component (..)
    , Listener
    , Trigger
    , runComponent
    , defaultComponentName
    ) where

import Data.Text qualified as Text
import Data.Typeable (typeRep)
import Effectful (Eff, (:>))
import Text.Casing (fromHumps, toSnake)

import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Monitoring.Tracing (Tracing, withSpan)


-- | Type aliases for semantic clarity
type Listener es = Eff es Void


type Trigger es = Eff es Void


-- | Component interface for modular application structure
class (Typeable c) => Component c es where
    -- | Additional effect constraints this component needs
    type Effects c es :: Constraint


    type Effects c es = ()


    -- | Component name for tracing (auto-derived from type name)
    componentName :: Proxy c -> Text
    componentName = defaultComponentName


    -- | Event listeners (react to events)
    listeners :: (Effects c es) => Eff es [Listener es]
    listeners = pure []


    -- | Triggers (initiate periodic/scheduled work)
    triggers :: (Effects c es) => Eff es [Trigger es]
    triggers = pure []


-- | Run a component by forking its listeners and triggers
runComponent
    :: forall c es
     . ( Component c es
       , Effects c es
       , Conc :> es
       , Tracing :> es
       )
    => Eff es ()
runComponent = withSpan (componentName @c @es Proxy) $ do
    ls <- listeners @c
    ts <- triggers @c
    traverse_ Conc.fork_ ls
    traverse_ Conc.fork_ ts


-- | Auto-derive component name from type name
-- Examples: PeerManager -> "peer_manager", ChainSync -> "chain_sync"
defaultComponentName :: forall c. (Typeable c) => Proxy c -> Text
defaultComponentName _ =
    let typeName = show $ typeRep (Proxy @c)
    in  Text.pack $ toSnake $ fromHumps typeName
