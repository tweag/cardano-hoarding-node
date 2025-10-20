module Hoard.App
  ( App,
    startEventSystem,
    addListener,
  )
where

import Control.Concurrent.STM (TQueue, atomically, readTQueue)
import Control.Monad (forever)
import Data.Typeable (Typeable)
import Effectful (Eff, IOE, (:>))
import Eve (AppState, AppT, EventDispatcher, asyncEventProvider)
import Eve qualified
import Hoard.Events (SomeEvent (..))

-- | Event system's application monad - Eve's AppT with Effectful base
-- Game state is accessed via lenses (e.g., heroPositions .= newMap)
type App es a = AppT AppState (Eff es) a

-- | Start the event system with the given initialization block
startEventSystem :: (IOE :> es, Typeable es) => TQueue SomeEvent -> App es () -> Eff es AppState
startEventSystem eventQueue initBlock = Eve.eve $ do
  asyncEventProvider $ eventProvider eventQueue
  initBlock

-- | Add a listener for a specific event type
-- Listeners run in the App monad and use lift to access Eff effects
addListener :: (Eve.HasEvents base, Monad m, Monoid result, Typeable m, Typeable eventType, Typeable result) => (eventType -> AppT base m result) -> Eve.ActionT base zoomed m ()
addListener = Eve.addListener_

-- | Event provider that reads from queue and dispatches events
eventProvider :: TQueue SomeEvent -> EventDispatcher -> IO ()
eventProvider queue dispatch = forever $ do
  -- Blocking read - waits for an event
  SomeEvent event <- atomically $ readTQueue queue
  dispatch event
