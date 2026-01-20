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
import Effectful.State.Static.Shared (State, gets)
import Prelude hiding (State, gets)

import Hoard.Effects.Conc (Conc)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.Publishing (Pub, Sub, publish)
import Hoard.Effects.Publishing qualified as Sub
import Hoard.Triggers (every)
import Hoard.Types.HoardState (HoardState (..))


run
    :: ( Concurrent :> es
       , Pub :> es
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


runTriggers :: (Conc :> es, Concurrent :> es, Pub :> es) => Eff es ()
runTriggers = do
    every 10 $ publish Poll


data Poll = Poll
    deriving stock (Show, Typeable)
