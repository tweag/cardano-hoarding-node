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

import Prelude hiding (State, evalState, runState)

import Control.Exception (throwIO)
import Data.Aeson (FromJSON)
import Data.Default (def)
import Data.Dynamic (Dynamic)
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.State.Static.Shared (State, evalState, runState)
import Ouroboros.Network.IOManager (IOManager)
import System.IO.Error (userError)

import Hoard.Effects.Chan (Chan, InChan, runChan)
import Hoard.Effects.Clock (Clock, runClock)
import Hoard.Effects.Conc (Conc, runConcWithKi, scoped)
import Hoard.Effects.DBRead (DBRead, runDBRead)
import Hoard.Effects.DBWrite (DBWrite, runDBWrite)
import Hoard.Effects.HeaderRepo (HeaderRepo, runHeaderRepo)
import Hoard.Effects.Log (Log, runLog)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.NodeToClient (NodeToClient, runNodeToClient)
import Hoard.Effects.PeerRepo (PeerRepo, runPeerRepo)
import Hoard.Effects.Pub (Pub, runPub)
import Hoard.Effects.Sub (Sub, runSub)
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
    { ioManager :: IOManager
    , dbPools :: DBPools
    , inChan :: InChan Dynamic
    , server :: ServerConfig
    , protocolConfigPath :: FilePath
    , localNodeSocketPath :: FilePath
    , logging :: Log.Config
    }


-- | Constraint alias for application effects
type AppEff es =
    ( IOE :> es
    , Log :> es
    , Clock :> es
    , FileSystem :> es
    , Concurrent :> es
    , Conc :> es
    , NodeToClient :> es
    , Sub :> es
    , Pub :> es
    , Chan :> es
    , DBRead :> es
    , DBWrite :> es
    , PeerRepo :> es
    , HeaderRepo :> es
    , Error Text :> es
    , State HoardState :> es
    )


-- | Alias to avoid typing Effectful.:> in servant modules.
type a ::> b = a Effectful.:> b


-- | Full effect stack for the application
type AppEffects =
    '[ State HoardState
     , PeerRepo
     , HeaderRepo
     , DBWrite
     , DBRead
     , Error Text
     , Pub
     , Sub
     , Chan
     , NodeToClient
     , Conc
     , Concurrent
     , FileSystem
     , Clock
     , Log
     , IOE
     ]


-- | Run the full effect stack for the application
runEffectStack :: (MonadIO m) => Config -> Eff AppEffects a -> m a
runEffectStack config action = liftIO $ do
    result <-
        runEff
            . runLog config.logging
            . runClock
            . runFileSystem
            . runConcurrent
            . scoped
            $ \scope ->
                runConcWithKi scope
                    . runNodeToClient config
                    . runChan
                    . runSub config.inChan
                    . runPub config.inChan
                    . runErrorNoCallStack @Text
                    . runDBRead config.dbPools.readerPool
                    . runDBWrite config.dbPools.writerPool
                    . runHeaderRepo
                    . runPeerRepo
                    . evalState def
                    $ action
    case result of
        Left err -> throwIO $ userError $ toString err
        Right value -> pure value


-- | Run the full effect stack and return both the result and final HoardState
runEffectStackReturningState :: (MonadIO m) => Config -> Eff AppEffects a -> m (a, HoardState)
runEffectStackReturningState config action = liftIO $ do
    result <-
        runEff
            . runLog config.logging
            . runClock
            . runFileSystem
            . runConcurrent
            . scoped
            $ \scope ->
                runConcWithKi scope
                    . runNodeToClient config
                    . runChan
                    . runSub config.inChan
                    . runPub config.inChan
                    . runErrorNoCallStack @Text
                    . runDBRead config.dbPools.readerPool
                    . runDBWrite config.dbPools.writerPool
                    . runHeaderRepo
                    . runPeerRepo
                    . runState def
                    $ action
    case result of
        Left err -> throwIO $ userError $ toString err
        Right value -> pure value
