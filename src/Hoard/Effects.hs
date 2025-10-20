module Hoard.Effects
  ( -- * Effect Stack
    runEffectStack,
    runEffectStackReturningState,
    AppEff,
    -- Config
    Config (..),

    -- * Type Aliases
    type (::>),
  )
where

import Control.Concurrent.STM (TQueue)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default (def)
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Console.ByteString (Console, runConsole)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.State.Static.Local (State, evalState, runState)
import Hoard.Effects.Publisher (Publisher, runPublisher)
import Hoard.Events (SomeEvent)
import Hoard.Types.HoardState (HoardState)
import Prelude hiding (appendFile, putStrLn, readFile)

data Config = Config
  { eventQueue :: TQueue SomeEvent
  }

-- | Constraint alias for application effects
type AppEff es =
  ( IOE :> es,
    Console :> es,
    FileSystem :> es,
    Concurrent :> es,
    Publisher :> es,
    State HoardState :> es
  )

-- | Alias to avoid typing Effectful.:> in servant modules.
type a ::> b = a Effectful.:> b

-- | Full effect stack for the application
type AppEffects = '[State HoardState, Publisher, Concurrent, FileSystem, Console, IOE]

-- | Run the full effect stack for the application
runEffectStack :: (MonadIO m) => Config -> Eff AppEffects a -> m a
runEffectStack config =
  liftIO
    . runEff
    . runConsole
    . runFileSystem
    . runConcurrent
    . runPublisher config.eventQueue
    . evalState def

-- | Run the full effect stack and return both the result and final HoardState
runEffectStackReturningState :: (MonadIO m) => Config -> Eff AppEffects a -> m (a, HoardState)
runEffectStackReturningState config =
  liftIO
    . runEff
    . runConsole
    . runFileSystem
    . runConcurrent
    . runPublisher config.eventQueue
    . runState def
