module Hoard.BlockFetch.FlowControl
    ( FlowControlState (..)
    , BytesInFlight (..)
    , mkFlowControlState
    , waitUntilReadyToFetch
    , registerRequest
    , unregisterRequest
    ) where

import Effectful (Eff, (:>))
import Effectful.Concurrent.MVar (Concurrent, newMVar, putMVar, readMVar, takeMVar)
import Effectful.State.Static.Shared (State, gets, stateM)
import Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common (estimateHfcBlockSize)
import Prelude hiding (State, gets, newMVar, putMVar, readMVar, takeMVar)

import Hoard.BlockFetch.Config (Config (..))
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Types.Cardano (CardanoHeader)


data FlowControlState = FlowControlState
    { readySignal :: MVar ()
    -- ^ Signals whether this collector is ready to issue block fetch requests
    -- to its connected peer. Full = Ready, Empty = Busy (backpressure active).
    , bytesInFlight :: BytesInFlight
    }


mkFlowControlState :: (Concurrent :> es) => Eff es FlowControlState
mkFlowControlState = do
    readySignal <- newMVar ()
    pure
        FlowControlState
            { readySignal
            , bytesInFlight = 0
            }


newtype BytesInFlight = BytesInFlight Int
    deriving (Eq, Ord, Show)
    deriving (Num) via Int


waitUntilReadyToFetch :: (Concurrent :> es, State FlowControlState :> es, Log :> es) => Eff es ()
waitUntilReadyToFetch = do
    Log.info "Waiting until ready to issue block fetch request..."
    _ <- readMVar =<< gets (.readySignal)
    Log.info "Ready to issue block fetch request!"
    pure ()


-- | Registers a new batch of blocks being fetched with BlockFetch. Calculates
-- the estimated total size of the fetched blocks and adds it to the total of
-- bytes in flight. If the new total of bytes in flight exceed the high
-- watermark, sets the status to Busy.
registerRequest :: (Concurrent :> es, State FlowControlState :> es) => Config -> [CardanoHeader] -> Eff es ()
registerRequest config headers = stateM \s -> do
    let newBytes = fromIntegral . sum $ estimateHfcBlockSize <$> toList headers
        bytesInFlight = s.bytesInFlight + newBytes

    when (coerce bytesInFlight > config.highWatermark) $ markBusy s.readySignal
    pure ((), s {bytesInFlight})


-- | Removes the passed batch of blocks being fetched from the total count of
-- bytes in flight. Calculates the estimated total size of the fetched blocks
-- and removes it from the total of bytes in flight. If the new total of bytes
-- in flight is lower than the low watermark, sets the status to Ready.
unregisterRequest :: (Concurrent :> es, State FlowControlState :> es) => Config -> [CardanoHeader] -> Eff es ()
unregisterRequest config headers = stateM \s -> do
    let newBytes = fromIntegral . sum $ estimateHfcBlockSize <$> toList headers
        bytesInFlight = s.bytesInFlight - newBytes

    when (coerce bytesInFlight < config.lowWatermark) $ markReady s.readySignal
    pure ((), s {bytesInFlight})


-- | Empties the ready signal, setting status to Busy.
-- Causes threads waiting in 'waitUntilReadyToFetch' to block.
markBusy :: (Concurrent :> es) => MVar () -> Eff es ()
markBusy = takeMVar


-- | Fills the ready signal, setting status to Ready.
-- Unblocks threads waiting in 'waitUntilReadyToFetch'.
markReady :: (Concurrent :> es) => MVar () -> Eff es ()
markReady signal = putMVar signal ()
