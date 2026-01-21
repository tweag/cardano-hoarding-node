module Hoard.Monitoring
    ( run
    , runListeners
    , listener
    , runTriggers
    , Poll (..)
    ) where

import Data.Set qualified as S
import Effectful (Eff, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Reader.Static (Reader, asks)
import Effectful.State.Static.Shared (State, gets)
import Prelude hiding (Reader, State, asks, gets)

import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.Publishing (Pub, Sub, publish)
import Hoard.Effects.Publishing qualified as Sub
import Hoard.Triggers (every)
import Hoard.Types.Environment (Config (..), MonitoringConfig (..))
import Hoard.Types.HoardState (HoardState (..))


run
    :: ( Concurrent :> es
       , Pub :> es
       , Reader Config :> es
       , State HoardState :> es
       , Conc :> es
       , Sub :> es
       , Log :> es
       )
    => Eff es ()
run = do
    runListeners
    runTriggers


runListeners
    :: ( Conc :> es
       , Sub :> es
       , Log :> es
       , State HoardState :> es
       )
    => Eff es ()
runListeners = do
    Conc.fork_ $ Sub.listen listener


listener :: (Log :> es, State HoardState :> es) => Poll -> Eff es ()
listener Poll = do
    numPeers <- gets (S.size . (.connectedPeers))
    Log.info $ "Currently connected to " <> show numPeers <> " peers"


runTriggers :: (Conc :> es, Concurrent :> es, Pub :> es, Reader Config :> es) => Eff es ()
runTriggers = do
    pollingInterval <- asks $ (.monitoring.pollingIntervalSeconds)
    every pollingInterval $ publish Poll


data Poll = Poll
    deriving stock (Show, Typeable)
