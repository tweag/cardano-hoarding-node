-- | Tracing integration for the 'Conc' effect.
--
-- Provides interpreters and interposers that automatically propagate
-- OpenTelemetry trace context across thread boundaries using span links.
module Hoard.Effects.Conc.Traced
    ( -- * Interpreters
      runConc
    , runConcTraced

      -- * Interposers
    , withConcTracingLinks
    )
where

import Effectful (IOE, raise, withEffToIO)
import Effectful.Dispatch.Dynamic (interpose, localLend, localUnlift, passthrough)

import Ki qualified

import Hoard.Effects.Conc (Conc (..), Scope (..), concStrat, fork, forkTry, fork_, runConcBase)
import Hoard.Effects.Monitoring.Tracing (Tracing)

import Hoard.Effects.Monitoring.Tracing qualified as Tracing


-- | Run 'Conc' effect with automatic trace context propagation in a new scope.
runConc :: (IOE :> es, Tracing :> es) => Eff (Conc : es) a -> Eff es a
runConc eff = withEffToIO concStrat $ \unlift ->
    Ki.scoped $ \scope ->
        unlift $ runConcTraced (Scope scope) eff


-- | Run 'Conc' effect with automatic trace context propagation.
--
-- All fork variants ('fork', 'fork_', 'forkTry') use span links, i.e. forked
-- threads start a fresh trace whose root spans carry a link back to the
-- originating span.
runConcTraced :: (IOE :> es, Tracing :> es) => Scope -> Eff (Conc : es) a -> Eff es a
runConcTraced scope = runConcBase scope . withConcTracingLinks


-- | Intercept fork operations and wrap the forked action with span link propagation.
--
-- Re-dispatches to the underlying 'Conc' handler via 'fork', 'fork_', 'forkTry'.
withConcTracingLinks :: (Conc :> es, Tracing :> es) => Eff es a -> Eff es a
withConcTracingLinks = interpose \env -> \case
    Fork action -> tracedFork env action fork
    Fork_ action -> tracedFork env action fork_
    ForkTry action -> tracedFork env action forkTry
    other -> passthrough env other
  where
    tracedFork env action kiOp = do
        parentCtx <- Tracing.getSpanContext
        localUnlift env concStrat \unliftEff ->
            localLend @'[Tracing] env concStrat \lend ->
                kiOp
                    $ unliftEff
                        . lend
                        . Tracing.withLinkPropagation parentCtx
                        . raise @Tracing
                    $ action
