module Hoard.Triggers (every) where

import Effectful.Concurrent (Concurrent, threadDelay)


-- | Runs an action repeatedly every @delay@ seconds, starting immediately.
-- Returns Void since it runs forever. Caller is responsible for forking.
every :: (Concurrent :> es) => Int -> Eff es () -> Eff es Void
every delay action = forever $ do
    threadDelay (delay * 1000000)
    action
