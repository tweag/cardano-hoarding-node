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
    , readChanBatched
    ) where

import Control.Concurrent.Chan.Unagi (InChan, OutChan)
import Control.Concurrent.Chan.Unagi qualified as Unagi
import Effectful (Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (evalState, get, modify)
import Effectful.TH (makeEffect)
import Effectful.Timeout (Timeout, timeout)
import Prelude hiding (evalState, get, modify)


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


-- | Read a batch of items from a channel.
--
-- Blocks until at least one item is available, then attempts to read
-- up to @batchSize@ items total within the given timeout. Always returns
-- at least one item.
readChanBatched
    :: forall a es
     . ( Chan :> es
       , Timeout :> es
       )
    => Int
    -- ^ Timeout in microseconds for reading additional items after the first
    -> Int
    -- ^ Maximum number of items to read (batch size)
    -> OutChan a
    -- ^ Channel to read from
    -> Eff es (NonEmpty a)
    -- ^ Non-empty batch of items (at least one, up to batch size)
readChanBatched timeoutMicroseconds batchSize outChan = do
    evalState @[a] [] $ do
        h <- readChan outChan -- blocking, get first item
        _ <- timeout timeoutMicroseconds $
            replicateM_ (batchSize - 1) $ do
                x <- readChan outChan
                modify (x :)
        rest <- get
        pure $ h :| reverse rest
