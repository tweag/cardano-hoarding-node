-- | Module: Hoard.Effects.Chan
-- Description: Effect for creating and operating on bidirectional channels
module Hoard.Effects.Chan
    ( -- * Effect
      Chan
    , newChan
    , readChan
    , writeChan
    , dupChan
    , runChan

      -- * Channel Types

      -- | Re-exported from the underlying channel implementation.
      -- Import these from this module rather than directly from Unagi
      -- to maintain abstraction boundaries.
    , InChan
    , OutChan
    ) where

import Control.Concurrent.Chan.Unagi (InChan, OutChan)
import Control.Concurrent.Chan.Unagi qualified as Unagi
import Effectful (Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect)


-- | Effect for creating and operating on bidirectional channels.
data Chan :: Effect where
    NewChan :: Chan m (Unagi.InChan a, Unagi.OutChan a)
    ReadChan :: Unagi.OutChan a -> Chan m a
    WriteChan :: Unagi.InChan a -> a -> Chan m ()
    DupChan :: Unagi.InChan a -> Chan m (Unagi.OutChan a)


makeEffect ''Chan


-- | Run the Chan effect using real Unagi channels.
--
-- This is an interpreter that performs actual channel operations via IO.
--
-- = Usage
--
-- @
-- runChan $ do
--     (inChan, outChan) <- newChan
--     outChan2 <- dupChan inChan  -- Create another reader
--     writeChan inChan value
--     value1 <- readChan outChan
--     value2 <- readChan outChan2  -- Both readers get the same value
--     ...
-- @
runChan :: (IOE :> es) => Eff (Chan : es) a -> Eff es a
runChan = interpret $ \_ -> \case
    NewChan -> liftIO Unagi.newChan
    ReadChan outChan -> liftIO $ Unagi.readChan outChan
    WriteChan inChan val -> liftIO $ Unagi.writeChan inChan val
    DupChan inChan -> liftIO $ Unagi.dupChan inChan
