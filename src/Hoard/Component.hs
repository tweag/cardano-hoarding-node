{-# LANGUAGE AllowAmbiguousTypes #-}

module Hoard.Component
    ( Component (..)
    , Listener
    , Trigger
    , runComponent
    , SomeComponent (..)
    , component
    , runSystem
    , defaultComponentName
    ) where

import Data.Text qualified as Text
import Data.Typeable (typeRep)
import Effectful (Eff, (:>))
import Text.Casing (fromHumps, toSnake)

import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
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


    -- | Setup component (runs before listeners/triggers start)
    setup :: (Effects c es) => Eff es ()
    setup = pure ()


    -- | Event listeners (react to events)
    listeners :: (Effects c es) => Eff es [Listener es]
    listeners = pure []


    -- | Triggers (initiate periodic/scheduled work)
    triggers :: (Effects c es) => Eff es [Trigger es]
    triggers = pure []


    -- | Post-start actions (runs after all components have started)
    start :: (Effects c es) => Eff es ()
    start = pure ()


-- | Existential wrapper for components to allow heterogeneous lists
data SomeComponent es where
    SomeComponent :: forall c es. (Component c es, Effects c es) => Proxy c -> SomeComponent es


-- | Helper to create a component with type application syntax
component :: forall c es. (Component c es, Effects c es) => SomeComponent es
component = SomeComponent (Proxy @c)


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


-- | Run multiple components with structured lifecycle coordination
--
-- Executes components in three sequential phases:
--   1. Run @setup for all components (initialization before activation)
--   2. Fork all listeners and triggers (components become active)
--   3. Fork @start for all components (background startup actions)
--
-- This separation allows components to prepare resources during setup, then perform
-- work during start that depends on listeners already running (e.g., publishing events).
-- The start phase is forked to allow parallel execution across components.
runSystem
    :: forall es
     . (Conc :> es, Log :> es, Tracing :> es)
    => [SomeComponent es]
    -> Eff es ()
runSystem components = do
    -- Phase 1: Setup all components
    traverse_ setupComponent components

    -- Phase 2: Start all components (fork listeners/triggers)
    traverse_ startComponent components

    -- Phase 3: Fork post-start actions
    traverse_ postStartComponent components
  where
    setupComponent :: SomeComponent es -> Eff es ()
    setupComponent (SomeComponent (p :: Proxy c)) = do
        let name = componentName @c @es p
        Log.debug $ "Setting up component: " <> name
        withSpan (name <> ":setup") $
            setup @c @es

    startComponent :: SomeComponent es -> Eff es ()
    startComponent (SomeComponent (p :: Proxy c)) = do
        let name = componentName @c @es p
        Log.debug $ "Starting listeners/triggers for component: " <> name
        runComponent @c

    postStartComponent :: SomeComponent es -> Eff es ()
    postStartComponent (SomeComponent (p :: Proxy c)) = do
        let name = componentName @c @es p
        Log.debug $ "Forking start phase for component: " <> name
        void . Conc.fork $
            withSpan (name <> ":start") $
                start @c @es


-- | Auto-derive component name from type name
-- Examples: PeerManager -> "peer_manager", ChainSync -> "chain_sync"
defaultComponentName :: forall c. (Typeable c) => Proxy c -> Text
defaultComponentName _ =
    let typeName = show $ typeRep (Proxy @c)
    in  Text.pack $ toSnake $ fromHumps typeName
