module Hoard.Events.ImmutableTipRefreshTriggered (ImmutableTipRefreshTriggered (..)) where


-- | Trigger event to refresh the immutable tip from the node.
data ImmutableTipRefreshTriggered = ImmutableTipRefreshTriggered
    deriving (Show, Typeable)
