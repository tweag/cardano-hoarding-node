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
import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default (def)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Console.ByteString (Console, runConsole)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.State.Static.Local (State, evalState, runState)
import Hoard.Effects.DBRead (DBRead, runDBRead)
import Hoard.Effects.DBWrite (DBWrite, runDBWrite)
import Hoard.Effects.Publisher (Publisher, runPublisher)
import Hoard.Events (SomeEvent)
import Hoard.Types.DBConfig (DBPools (..))
import Hoard.Types.HoardState (HoardState)

data Config = Config
  { eventQueue :: TQueue SomeEvent,
    dbPools :: DBPools
  }

-- | Constraint alias for application effects
type AppEff es =
  ( IOE :> es,
    Console :> es,
    FileSystem :> es,
    Concurrent :> es,
    Publisher :> es,
    DBRead :> es,
    DBWrite :> es,
    Error Text :> es,
    State HoardState :> es
  )

-- | Alias to avoid typing Effectful.:> in servant modules.
type a ::> b = a Effectful.:> b

-- | Full effect stack for the application
type AppEffects = '[State HoardState, DBWrite, DBRead, Error Text, Publisher, Concurrent, FileSystem, Console, IOE]

-- | Run the full effect stack for the application
runEffectStack :: (MonadIO m) => Config -> Eff AppEffects a -> m a
runEffectStack config action = liftIO $ do
  result <-
    runEff
      . runConsole
      . runFileSystem
      . runConcurrent
      . runPublisher config.eventQueue
      . runErrorNoCallStack @Text
      . runDBRead config.dbPools.readerPool
      . runDBWrite config.dbPools.writerPool
      . evalState def
      $ action
  case result of
    Left err -> throwIO $ userError $ T.unpack err
    Right value -> pure value

-- | Run the full effect stack and return both the result and final HoardState
runEffectStackReturningState :: (MonadIO m) => Config -> Eff AppEffects a -> m (a, HoardState)
runEffectStackReturningState config action = liftIO $ do
  result <-
    runEff
      . runConsole
      . runFileSystem
      . runConcurrent
      . runPublisher config.eventQueue
      . runErrorNoCallStack @Text
      . runDBRead config.dbPools.readerPool
      . runDBWrite config.dbPools.writerPool
      . runState def
      $ action
  case result of
    Left err -> throwIO $ userError $ T.unpack err
    Right value -> pure value
