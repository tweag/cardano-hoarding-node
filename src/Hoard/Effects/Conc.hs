module Hoard.Effects.Conc
    ( -- * Effect
      Conc
    , Thread
    , fork
    , fork_
    , await
    , awaitAll
    , forkTry

      -- * Scope
    , Scope
    , scoped

      -- * Interpreters
    , runConcWithKi
    , runConcNewScope

      -- * Unlift Strategy
    , concStrat
    )
where

import Prelude hiding (atomically)

import Effectful (Eff, Effect, Limit (..), Persistence (..), UnliftStrategy (..), withEffToIO)
import Effectful.Concurrent.STM (atomically, runConcurrent)
import Effectful.Dispatch.Dynamic (interpret, localUnliftIO)
import Effectful.TH (makeEffect)

import Ki qualified


data Conc :: Effect where
    Fork :: m a -> Conc m (Thread a)
    Fork_ :: m Void -> Conc m ()
    Await :: Thread a -> Conc m a
    AwaitAll :: Conc m ()
    ForkTry :: (Exception e) => m a -> Conc m (Thread (Either e a))


newtype Scope = Scope Ki.Scope


newtype Thread a = Thread (Ki.Thread a)


makeEffect ''Conc


scoped :: (_) => (Scope -> Eff es a) -> Eff es a
scoped action = withEffToIO concStrat $ \unlift ->
    Ki.scoped $ \scope ->
        unlift
            . action
            $ Scope scope


runConcWithKi :: (_) => Scope -> Eff (Conc : es) a -> Eff es a
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
    ForkTry action ->
        localUnliftIO env concStrat $ \unlift ->
            fmap Thread
                . liftIO
                . Ki.forkTry scope
                $ unlift action


runConcNewScope :: (_) => Eff (Conc : es) a -> Eff es a
runConcNewScope eff = withEffToIO concStrat $ \unlift ->
    Ki.scoped $ \scope ->
        unlift $ runConcWithKi (Scope scope) eff


concStrat :: UnliftStrategy
concStrat = ConcUnlift Persistent Unlimited
