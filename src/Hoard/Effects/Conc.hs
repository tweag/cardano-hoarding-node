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
    , runConc

      -- * Unlift Strategy
    , concStrat
    )
where

import Effectful (Effect, IOE, Limit (..), Persistence (..), UnliftStrategy (..), raise, withEffToIO)
import Effectful.Concurrent.STM (atomically, runConcurrent)
import Effectful.Dispatch.Dynamic (EffectHandler, interpose, interpret, localLend, localUnlift, localUnliftIO)
import Effectful.TH (makeEffect)

import Ki qualified
import OpenTelemetry.Context qualified as Context
import OpenTelemetry.Context.ThreadLocal qualified as ThreadLocal
import OpenTelemetry.Trace.Core qualified as OT

import Hoard.Effects.Monitoring.Tracing (Tracing)

import Hoard.Effects.Monitoring.Tracing qualified as Tracing


data Conc :: Effect where
    Fork :: m a -> Conc m (Thread a)
    Fork_ :: m Void -> Conc m ()
    Await :: Thread a -> Conc m a
    AwaitAll :: Conc m ()
    ForkTry :: (Exception e) => m a -> Conc m (Thread (Either e a))
    Scoped :: m a -> Conc m a


newtype Scope = Scope Ki.Scope


newtype Thread a = Thread (Ki.Thread a)


makeEffect ''Conc


-- | Run Conc effect with automatic trace context propagation in a new scope.
--
-- Combines the convenience of `runConcNewScope` with automatic trace context
-- propagation from `runConcTraced`.
runConc :: (IOE :> es, Tracing :> es) => Eff (Conc : es) a -> Eff es a
runConc eff = withEffToIO concStrat $ \unlift ->
    Ki.scoped $ \scope ->
        unlift $ runConcTraced (Scope scope) eff


-- | Run Conc effect with automatic trace context propagation.
--
-- When forking threads, span context is automatically captured from the parent
-- thread and propagated to the forked thread via OpenTelemetry's thread-local
-- context storage. This means any `withSpan` calls in forked threads will
-- automatically become children of the parent thread's current span.
--
-- == Example
--
-- @
-- runConcTraced $ do
--     withSpan "parent" $ do
--         fork_ $ do
--             -- This span will automatically be a child of "parent"
--             withSpan "child" $ doWork
-- @
runConcTraced :: forall es a. (IOE :> es, Tracing :> es) => Scope -> Eff (Conc : es) a -> Eff es a
runConcTraced (Scope scope0) = interpret $ handler @es scope0
  where
    handler :: forall es'. (IOE :> es', Tracing :> es') => Ki.Scope -> EffectHandler Conc es'
    handler scope env = \case
        Fork action -> do
            -- Capture parent span context
            parentCtx <- Tracing.getSpanContext
            localUnliftIO env concStrat $ \unlift ->
                fmap Thread
                    . liftIO
                    . Ki.fork scope
                    $ propagateContext parentCtx (unlift action)
        Fork_ action -> do
            -- Capture parent span context
            parentCtx <- Tracing.getSpanContext
            localUnliftIO env concStrat $ \unlift ->
                liftIO
                    . Ki.fork_ scope
                    $ propagateContext parentCtx (unlift action)
        Await (Thread thread) ->
            runConcurrent
                . atomically
                $ Ki.await thread
        AwaitAll ->
            runConcurrent
                . atomically
                $ Ki.awaitAll scope
        ForkTry action -> do
            -- Capture parent span context
            parentCtx <- Tracing.getSpanContext
            localUnliftIO env concStrat $ \unlift ->
                fmap Thread
                    . liftIO
                    . Ki.forkTry scope
                    $ propagateContext parentCtx (unlift action)
        Scoped m ->
            -- Unlift the contained `m` action (of type `Eff localEs a`) to run it in `Eff es a`.
            localUnlift env concStrat \unliftEff ->
                -- Lend `es`' `IOE` and `Tracing` effects to the `localEs` effect stack.
                localLend @'[Tracing, IOE] env concStrat \lend ->
                    -- Unlift `Eff` into `IO` because that's what `Ki` expects.
                    withEffToIO concStrat \unliftIO ->
                        Ki.scoped \subScope ->
                            -- Temporarily step into `IO` for `Ki.scoped`'s sake.
                            unliftIO
                                -- Resolve `localEs` to `es`.
                                . unliftEff
                                -- Lend the `IOE` and `Tracing` effects to the inner (recursive) handler.
                                . lend
                                -- Resolve `m`'s `Conc` effect using this handler.
                                . interpose (handler subScope)
                                -- Make `m` use the lended `IOE` and `Tracing` effects.
                                . raise @Tracing
                                . raise @IOE
                                $ m


-- | Helper to propagate span context to a forked thread
propagateContext :: Maybe OT.SpanContext -> IO a -> IO a
propagateContext Nothing action = action
propagateContext (Just spanCtx) action = do
    -- Create context with parent span
    let parentSpan = OT.wrapSpanContext spanCtx
    let ctx = Context.insertSpan parentSpan Context.empty

    -- Attach context to thread-local storage
    oldCtx <- ThreadLocal.attachContext ctx

    -- Run action
    result <- action

    -- Restore old context
    void $ case oldCtx of
        Just old -> ThreadLocal.attachContext old
        Nothing -> ThreadLocal.detachContext

    pure result


concStrat :: UnliftStrategy
concStrat = ConcUnlift Persistent Unlimited
