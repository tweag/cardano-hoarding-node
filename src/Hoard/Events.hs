module Hoard.Events (SomeEvent (..)) where

import Text.Show qualified


-- | Existential wrapper for any event type
-- Allows heterogeneous events to be stored in a single queue
data SomeEvent where
    SomeEvent :: (Text.Show.Show e, Typeable e) => e -> SomeEvent


instance Text.Show.Show SomeEvent where
    show (SomeEvent e) = Text.Show.show e
