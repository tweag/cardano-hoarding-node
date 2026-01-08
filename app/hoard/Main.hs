module Main (main) where

import Data.Default (def)
import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.FileSystem (runFileSystem)
import Effectful.State.Static.Shared (evalState)
import Effectful.Temporary (runTemporary)
import Prelude hiding (evalState)

import Hoard.Collector (runCollectors)
import Hoard.Control.Exception (runErrorThrowing)
import Hoard.Effects.BlockRepo (runBlockRepo)
import Hoard.Effects.Chan (runChan)
import Hoard.Effects.Clock (runClock)
import Hoard.Effects.Conc (runConcNewScope)
import Hoard.Effects.Conc qualified as Conc
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
import Hoard.Listeners (runListeners)
import Hoard.Monitoring qualified as Monitoring
import Hoard.Server (runServer)
import Hoard.Setup (setup)
import Hoard.Triggers (runTriggers)
import Hoard.Types.HoardState (HoardState)


main :: IO ()
main =
    runEff
        . runChan
        . runConcNewScope
        . loadOptions
        . loadEnv
        . runConfigReader
        . runHandlesReader
        . runLog
        . runClock
        . runFileSystem
        . runConcurrent
        . runTemporary
        . withNodeSockets
        . runNodeToClient
        . runSub
        . runPub
        . runErrorThrowing
        . evalState @HoardState def
        . runNodeToNode
        . runDBRead
        . runDBWrite
        . runHeaderRepo
        . runPeerRepo
        . runBlockRepo
        $ do
            setup
            runServer
            runListeners
            runCollectors
            runTriggers
            Monitoring.run
            Conc.awaitAll
