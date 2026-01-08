module Hoard.Effects.Conc
    ( -- * Effect
      Conc
    , Thread
    , fork
    , fork_
    , await
    , awaitAll
    , forkTry
    , subScoped

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

import Effectful
    ( Eff
    , Effect
    , IOE
    , Limit (..)
    , Persistence (..)
    , UnliftStrategy (..)
    , raise
    , withEffToIO
    , (:>)
    )
import Effectful.Concurrent.STM (atomically, runConcurrent)
import Effectful.Dispatch.Dynamic
    ( EffectHandler
    , interpose
    , interpret
    , localLend
    , localUnlift
    , localUnliftIO
    )
import Effectful.TH (makeEffect)

import Ki qualified


data Conc :: Effect where
    Fork :: m a -> Conc m (Thread a)
    Fork_ :: m Void -> Conc m ()
    Await :: Thread a -> Conc m a
    AwaitAll :: Conc m ()
    ForkTry :: (Exception e) => m a -> Conc m (Thread (Either e a))
    SubScoped :: m a -> Conc m a


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
runConcWithKi (Scope scope0) = interpret $ handler scope0
  where
    handler :: (IOE :> es) => Ki.Scope -> EffectHandler Conc es
    handler scope env = \case
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
        SubScoped m ->
            -- Unlift the contained `m` action (of type `Eff localEs a`) to run it in `Eff es a`.
            localUnlift env concStrat \unliftEff ->
                -- Lend `es`' `IOE` effect to the `localEs` effect stack.
                localLend @'[IOE] env concStrat \lend ->
                    -- Unlift `Eff` into `IO` because that's what `Ki` expects.
                    withEffToIO concStrat \unliftIO ->
                        Ki.scoped \subScope ->
                            -- Temporarily step into `IO` for `Ki.scoped`'s sake.
                            unliftIO
                                -- Resolve `localEs` to `es`.
                                . unliftEff
                                -- Lend the `IOE` effect to the inner (recursive) handler.
                                . lend
                                -- Resolve `m`'s `Conc` effect using this handler.
                                . interpose (handler subScope)
                                -- Make `m` use the lended `IOE` effect.
                                . raise @IOE
                                $ m


runConcNewScope :: (IOE :> es) => Eff (Conc : es) a -> Eff es a
runConcNewScope eff = withEffToIO concStrat $ \unlift ->
    Ki.scoped $ \scope ->
        unlift $ runConcWithKi (Scope scope) eff


concStrat :: UnliftStrategy
concStrat = ConcUnlift Persistent Unlimited
