module Hoard.Effects.Publisher
  ( Publisher (..),
    publish,
    runPublisher,
  )
where

import Control.Concurrent.STM (TQueue, atomically, writeTQueue)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (Typeable)
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, type (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import Hoard.Events (SomeEvent (..))

-- | Effect for publishing events to the event system
data Publisher :: Effect where
  Publish :: (Typeable e, Show e) => e -> Publisher m ()

type instance DispatchOf Publisher = Dynamic

-- | Publish an event to the event system.
-- We write it manually to add the Typeable constraint.
publish :: (Publisher :> es, Typeable e, Show e) => e -> Eff es ()
publish = send . Publish

-- | Run the Publisher effect by writing to a TQueue
runPublisher :: (IOE :> es) => TQueue SomeEvent -> Eff (Publisher : es) a -> Eff es a
runPublisher queue = interpret $ \_ -> \case
  Publish event -> liftIO $ atomically $ writeTQueue queue (SomeEvent event)
