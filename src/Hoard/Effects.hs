module Hoard.Effects
    ( -- * Effect Stack
      runEffectStack

      -- * Type Aliases
    , type (::>)
    )
where

import Control.Exception (throwIO)
import Data.Default (def)
import Effectful (Eff, runEff, (:>))
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.FileSystem (runFileSystem)
import Effectful.State.Static.Shared (evalState)
import System.IO.Error (userError)
import Prelude hiding (Reader, State, evalState)

import Effectful.Temporary (runTemporary)
import Hoard.Effects.BlockRepo (runBlockRepo)
import Hoard.Effects.Chan (runChan)
import Hoard.Effects.Clock (runClock)
import Hoard.Effects.Conc (runConcNewScope)
import Hoard.Effects.DBRead (runDBRead)
import Hoard.Effects.DBWrite (runDBWrite)
import Hoard.Effects.Environment (loadEnv, runConfigReader, runHandlesReader)
import Hoard.Effects.HeaderRepo (runHeaderRepo)
import Hoard.Effects.Log (runLog)
import Hoard.Effects.NodeToClient (runNodeToClient)
import Hoard.Effects.NodeToNode (runNodeToNode)
import Hoard.Effects.Options (loadOptions)
import Hoard.Effects.PeerRepo (runPeerRepo)
import Hoard.Effects.Pub (runPub)
import Hoard.Effects.Sub (runSub)
import Hoard.Effects.WithSocket (withNodeSockets)


-- | Alias to avoid typing Effectful.:> in servant modules.
type a ::> b = a Effectful.:> b


-- | Run the full effect stack for the application
runEffectStack :: (_) => Eff _ a -> m a
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
