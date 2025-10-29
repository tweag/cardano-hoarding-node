module Hoard.Effects
    ( -- * Effect Stack
      runEffectStack
    , runEffectStackReturningState
    , AppEff
    , AppEffects
    -- Config
    , Config (..)
    , ServerConfig (..)

      -- * Type Aliases
    , type (::>)
    )
where

import Control.Concurrent.Chan.Unagi (InChan)
import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON)
import Data.Default (def)
import Data.Dynamic (Dynamic)
import Data.Text (Text)
import Data.Word (Word16)
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Console.ByteString (Console, runConsole)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.State.Static.Shared (State, evalState, runState)
import GHC.Generics (Generic)

import Data.Text qualified as T

import Hoard.Effects.Conc (Conc, runConcWithKi, scoped)
import Hoard.Effects.DBRead (DBRead, runDBRead)
import Hoard.Effects.DBWrite (DBWrite, runDBWrite)
import Hoard.Effects.Network (Network, runNetwork)
import Hoard.Effects.Pub (Pub, runPub)
import Hoard.Effects.Sub (Sub, runSub)
import Hoard.Network.Config (defaultNetworkConfig)
import Hoard.Types.DBConfig (DBPools (..))
import Hoard.Types.HoardState (HoardState)
import Hoard.Types.QuietSnake (QuietSnake (..))


-- | HTTP server configuration
data ServerConfig = ServerConfig
    { host :: Text
    , port :: Word16
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake ServerConfig


data Config = Config
    { dbPools :: DBPools
    , inChan :: InChan Dynamic
    , server :: ServerConfig
    }


-- | Constraint alias for application effects
type AppEff es =
    ( IOE :> es
    , Console :> es
    , FileSystem :> es
    , Concurrent :> es
    , Conc :> es
    , Sub :> es
    , Pub :> es
    , Network :> es
    , DBRead :> es
    , DBWrite :> es
    , Error Text :> es
    , State HoardState :> es
    )


-- | Alias to avoid typing Effectful.:> in servant modules.
type a ::> b = a Effectful.:> b


-- | Full effect stack for the application
type AppEffects = '[State HoardState, DBWrite, DBRead, Network, Error Text, Pub, Sub, Conc, Concurrent, FileSystem, Console, IOE]


-- | Run the full effect stack for the application
runEffectStack :: (MonadIO m) => Config -> Eff AppEffects a -> m a
runEffectStack config action = liftIO $ do
    result <-
        runEff
            . runConsole
            . runFileSystem
            . runConcurrent
            . scoped
            $ \scope ->
                runConcWithKi scope
                    . runSub config.inChan
                    . runPub config.inChan
                    . runErrorNoCallStack @Text
                    . runNetwork defaultNetworkConfig
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
            . scoped
            $ \scope ->
                runConcWithKi scope
                    . runSub config.inChan
                    . runPub config.inChan
                    . runErrorNoCallStack @Text
                    . runNetwork defaultNetworkConfig
                    . runDBRead config.dbPools.readerPool
                    . runDBWrite config.dbPools.writerPool
                    . runState def
                    $ action
    case result of
        Left err -> throwIO $ userError $ T.unpack err
        Right value -> pure value
