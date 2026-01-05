module Hoard.Effects
    ( -- * Effect Stack
      runEffectStack
    , AppEff
    , AppEffects

      -- * Type Aliases
    , type (::>)
    )
where

import Control.Exception (throwIO)
import Data.Default (def)
import Data.Dynamic (Dynamic)
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Shared (State, evalState)
import System.IO.Error (userError)
import Prelude hiding (Reader, State, evalState)

import Hoard.Effects.BlockRepo (BlockRepo, runBlockRepo)
import Effectful.Labeled (Labeled)
import Effectful.Temporary (Temporary, runTemporary)
import Hoard.Effects.Chan (Chan, InChan, runChan)
import Hoard.Effects.Clock (Clock, runClock)
import Hoard.Effects.Conc (Conc, runConcNewScope)
import Hoard.Effects.DBRead (DBRead, runDBRead)
import Hoard.Effects.DBWrite (DBWrite, runDBWrite)
import Hoard.Effects.Environment (loadEnv, runConfigReader, runHandlesReader)
import Hoard.Effects.HeaderRepo (HeaderRepo, runHeaderRepo)
import Hoard.Effects.Log (Log, runLog)
import Hoard.Effects.NodeToClient (NodeToClient, runNodeToClient)
import Hoard.Effects.NodeToNode (NodeToNode, runNodeToNode)
import Hoard.Effects.Options (Options, loadOptions)
import Hoard.Effects.PeerRepo (PeerRepo, runPeerRepo)
import Hoard.Effects.Pub (Pub, runPub)
import Hoard.Effects.Sub (Sub, runSub)
import Hoard.Effects.WithSocket (WithSocket, withNodeSockets)
import Hoard.Types.DBConfig (DBPools)
import Hoard.Types.Environment (Config, Env, LogConfig)
import Hoard.Types.HoardState (HoardState)


-- | Constraint alias for application effects
type AppEff es =
    ( IOE :> es
    , Log :> es
    , Clock :> es
    , FileSystem :> es
    , Reader Options :> es
    , Reader DBPools :> es
    , Reader Config :> es
    , Reader LogConfig :> es
    , Reader (InChan Dynamic) :> es
    , Reader Env :> es
    , Concurrent :> es
    , Conc :> es
    , NodeToClient :> es
    , Sub :> es
    , Pub :> es
    , Chan :> es
    , NodeToNode :> es
    , DBRead :> es
    , DBWrite :> es
    , BlockRepo :> es
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
     , BlockRepo
     , HeaderRepo
     , DBWrite
     , DBRead
     , NodeToNode
     , Error Text
     , Pub
     , Sub
     , NodeToClient
     , Labeled "nodeToClient" WithSocket
     , Labeled "tracer" WithSocket
     , Temporary
     , Conc
     , Concurrent
     , FileSystem
     , Clock
     , Log
     , Reader (InChan Dynamic)
     , Reader DBPools
     , Reader LogConfig
     , Reader Config
     , Reader Env
     , Reader Options
     , Chan
     , IOE
     ]


-- | Run the full effect stack for the application
runEffectStack :: (MonadIO m) => Eff AppEffects a -> m a
runEffectStack action = liftIO $ do
    result <-
        runEff
            . runChan
            . loadOptions
            . loadEnv
            . runConfigReader
            . runHandlesReader
            . runLog
            . runClock
            . runFileSystem
            . runConcurrent
            . runConcNewScope
            . runTemporary
            . withNodeSockets
            . runNodeToClient
            . runSub
            . runPub
            . runErrorNoCallStack @Text
            . runNodeToNode
            . runDBRead
            . runDBWrite
            . runHeaderRepo
            . runBlockRepo
            . runPeerRepo
            . evalState def
            $ action
    case result of
        Left err -> throwIO $ userError $ toString err
        Right value -> pure value
