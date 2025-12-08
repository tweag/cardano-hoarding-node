module Hoard.Effects
    ( -- * Effect Stack
      runEffectStack
    , AppEff
    , AppEffects
    -- Config
    , Config (..)
    , Handles (..)
    , Env (..)
    , ServerConfig (..)

      -- * Type Aliases
    , type (::>)
    )
where

import Prelude hiding (State, evalState)

import Control.Exception (throwIO)
import Data.Aeson (FromJSON)
import Data.Default (def)
import Data.Dynamic (Dynamic)
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.State.Static.Shared (State, evalState)
import Ouroboros.Network.IOManager (IOManager)
import System.IO.Error (userError)

import Hoard.Effects.Chan (Chan, InChan, runChan)
import Hoard.Effects.Clock (Clock, runClock)
import Hoard.Effects.Conc (Conc, runConcNewScope)
import Hoard.Effects.DBRead (DBRead, runDBRead)
import Hoard.Effects.DBWrite (DBWrite, runDBWrite)
import Hoard.Effects.HeaderRepo (HeaderRepo, runHeaderRepo)
import Hoard.Effects.Log (Log, runLog)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.NodeToClient (NodeToClient, runNodeToClient)
import Hoard.Effects.NodeToNode (NodeToNode, runNodeToNode)
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


-- | Pure configuration data loaded from config files
data Config = Config
    { server :: ServerConfig
    , protocolConfigPath :: FilePath
    , localNodeSocketPath :: FilePath
    , logging :: Log.Config
    }


-- | Runtime handles and resources
data Handles = Handles
    { ioManager :: IOManager
    , dbPools :: DBPools
    , inChan :: InChan Dynamic
    }


-- | Application environment combining config and handles
data Env = Env
    { config :: Config
    , handles :: Handles
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
    , NodeToNode :> es
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
     , NodeToNode
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
runEffectStack :: (MonadIO m) => Env -> Eff AppEffects a -> m a
runEffectStack env action = liftIO $ do
    result <-
        runEff
            . runLog env.config.logging
            . runClock
            . runFileSystem
            . runConcurrent
            . runConcNewScope
            . runNodeToClient env.config
            . runChan
            . runSub env.handles.inChan
            . runPub env.handles.inChan
            . runErrorNoCallStack @Text
            . runNodeToNode env.handles.ioManager env.config.protocolConfigPath
            . runDBRead env.handles.dbPools.readerPool
            . runDBWrite env.handles.dbPools.writerPool
            . runHeaderRepo
            . runPeerRepo
            . evalState def
            $ action
    case result of
        Left err -> throwIO $ userError $ toString err
        Right value -> pure value
