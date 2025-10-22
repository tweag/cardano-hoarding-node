module Hoard.Events (SomeEvent (..)) where

import Data.Typeable (Typeable)


-- | Existential wrapper for any event type
-- Allows heterogeneous events to be stored in a single queue
data SomeEvent where
    SomeEvent :: (Show e, Typeable e) => e -> SomeEvent


instance Show SomeEvent where
    show (SomeEvent e) = show e
