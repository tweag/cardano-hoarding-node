module Hoard.Events (SomeEvent (..)) where


-- | Existential wrapper for any event type
-- Allows heterogeneous events to be stored in a single queue
data SomeEvent where
    SomeEvent :: (Show e, Typeable e) => e -> SomeEvent
