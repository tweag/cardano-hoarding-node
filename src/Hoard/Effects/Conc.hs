module Hoard.Effects.Conc
    ( -- * Effect
      Conc
    , Thread
    , fork
    , fork_
    , await
    , awaitAll

      -- * Scope
    , Scope
    , scoped

      -- * Interpreters
    , runConcWithKi
    )
where

import Data.Void (Void)
import Effectful (Eff, Effect, IOE, Limit (..), Persistence (..), UnliftStrategy (..), liftIO, withEffToIO, (:>))
import Effectful.Concurrent.STM (atomically, runConcurrent)
import Effectful.Dispatch.Dynamic (interpret, localUnliftIO)
import Effectful.TH (makeEffect)

import Ki qualified


data Conc :: Effect where
    Fork :: m a -> Conc m (Thread a)
    Fork_ :: m Void -> Conc m ()
    Await :: Thread a -> Conc m a
    AwaitAll :: Conc m ()


newtype Scope = Scope Ki.Scope


newtype Thread a = Thread (Ki.Thread a)


makeEffect ''Conc


scoped :: (IOE :> es) => (Scope -> Eff es a) -> Eff es a
scoped action = withEffToIO concStrat $ \unlift ->
    Ki.scoped $ \scope ->
        unlift
            . action
            $ Scope scope


runConcWithKi :: (IOE :> es) => Scope -> Eff (Conc : es) a -> Eff es a
runConcWithKi (Scope scope) = interpret $ \env -> \case
    Fork action ->
        localUnliftIO env concStrat $ \unlift ->
            fmap Thread
                . liftIO
                . Ki.fork scope
                $ unlift action
    Fork_ action ->
        localUnliftIO env concStrat $ \unlift ->
            liftIO
                . Ki.fork_ scope
                $ unlift action
    Await (Thread thread) ->
        runConcurrent
            . atomically
            $ Ki.await thread
    AwaitAll ->
        runConcurrent
            . atomically
            $ Ki.awaitAll scope


concStrat :: UnliftStrategy
concStrat = ConcUnlift Persistent Unlimited
