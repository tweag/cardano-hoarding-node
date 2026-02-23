module Hoard.ImmutableTip
    ( component
    , Refreshed (..)
    , Config
    ) where

import Data.Aeson (FromJSON)
import Data.Default (Default (..))
import Effectful.Concurrent (Concurrent)
import Effectful.Reader.Static (Reader, asks)
import Effectful.State.Static.Shared (State, modify, modifyM)

import Hoard.Component (Component (..), defaultComponent)
import Hoard.Effects.Monitoring.Tracing (SpanStatus (..), Tracing, setStatus, withSpan)
import Hoard.Effects.NodeToClient (NodeToClient)
import Hoard.Effects.Publishing (Pub, Sub, publish)
import Hoard.Events.ImmutableTipRefreshTriggered (ImmutableTipRefreshTriggered (..))
import Hoard.Triggers (every)
import Hoard.Types.HoardState (HoardState (..))
import Hoard.Types.QuietSnake (QuietSnake (..))

import Hoard.Effects.HoardStateRepo qualified as HoardStateRepo
import Hoard.Effects.NodeToClient qualified as NodeToClient
import Hoard.Effects.Publishing qualified as Sub


component
    :: ( Concurrent :> es
       , HoardStateRepo.HoardStateRepo :> es
       , NodeToClient :> es
       , Pub ImmutableTipRefreshTriggered :> es
       , Pub Refreshed :> es
       , Reader Config :> es
       , State HoardState :> es
       , Sub ImmutableTipRefreshTriggered :> es
       , Tracing :> es
       )
    => Component es
component =
    defaultComponent
        { name = "ImmutableTip"
        , setup = do
            -- Load immutable tip from DB and set in state
            immutableTip <- HoardStateRepo.getImmutableTip
            modify (\hoardState -> hoardState {immutableTip = immutableTip})
        , triggers = do
            refreshInterval <- asks @Config $ (.immutableTipRefreshSeconds)
            pure
                [ every refreshInterval $ publish ImmutableTipRefreshTriggered
                ]
        , listeners =
            pure
                [ Sub.listen_ immutableTipRefreshTriggeredListener
                ]
        , start =
            publish ImmutableTipRefreshTriggered
        }


-- | Fetch the immutable tip from the cardano-node and store it in HoardState.
--
-- This is called regularly and during application setup to initialize the immutable tip
-- before we start connecting to peers.
-- If fetching the tip fails due to a connection error, it retries once to reconnect and fetch.
immutableTipRefreshTriggeredListener
    :: ( NodeToClient :> es
       , Pub Refreshed :> es
       , State HoardState :> es
       , Tracing :> es
       )
    => ImmutableTipRefreshTriggered -> Eff es ()
immutableTipRefreshTriggeredListener ImmutableTipRefreshTriggered = withSpan "immutable_tip.refresh" do
    NodeToClient.immutableTip >>= \case
        Left _ ->
            setStatus $ Error "Fetch failed: connection may be down"
        Right tip -> do
            setStatus Ok
            modifyM $ \hoardState ->
                if hoardState.immutableTip < tip then
                    publish Refreshed $> hoardState {immutableTip = tip}
                else
                    pure hoardState


data Refreshed = Refreshed


data Config = Config
    { immutableTipRefreshSeconds :: Int
    -- ^ Interval between immutable tip refresh
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake Config


instance Default Config where
    def =
        Config
            { immutableTipRefreshSeconds = 30
            }
