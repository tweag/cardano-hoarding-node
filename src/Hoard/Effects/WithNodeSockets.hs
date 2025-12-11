{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Hoard.Effects.WithNodeSockets
    ( -- * Effect
      WithSocket (..)

      -- * Handlers
    , withNodeSockets
    , NodeSocketsConfig (..)
    , sshTunnelSocket
    , localSocket

      -- * Operations
    , getSocket
    ) where

import Effectful (Eff, Effect, IOE, type (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.Labeled (Labeled, runLabeled)
import Effectful.TH (makeEffect)
import Effectful.Temporary (Temporary, withSystemTempFile)
import System.Process.Typed (proc, withProcessTerm)


data WithSocket :: Effect where
    GetSocket :: WithSocket m FilePath


makeEffect ''WithSocket


data NodeSocketsConfig
    = SshTunnel
        { nodeToClientSocket :: FilePath
        , tracerSocket :: FilePath
        , remoteHost :: Text
        , sshKey :: Maybe FilePath
        }
    | Local
        { nodeToClientSocket :: FilePath
        , tracerSocket :: FilePath
        }


withNodeSockets
    :: (Temporary :> es, IOE :> es)
    => NodeSocketsConfig
    -> Eff (Labeled "nodeToClient" WithSocket : Labeled "tracer" WithSocket : es) a
    -> Eff es a
withNodeSockets (SshTunnel {nodeToClientSocket, tracerSocket, remoteHost, sshKey}) =
    runLabeled @"tracer" (sshTunnelSocket remoteHost tracerSocket sshKey)
        . runLabeled @"nodeToClient" (sshTunnelSocket remoteHost nodeToClientSocket sshKey)
withNodeSockets (Local {nodeToClientSocket, tracerSocket}) =
    runLabeled @"tracer" (localSocket tracerSocket)
        . runLabeled @"nodeToClient" (localSocket nodeToClientSocket)


sshTunnelSocket
    :: (Temporary :> es, IOE :> es)
    => Text
    -- ^ remote host
    -> FilePath
    -- ^ remote socket path
    -> Maybe FilePath
    -- ^ ssh key path
    -> Eff (WithSocket : es) a
    -> Eff es a
sshTunnelSocket remoteHost remoteSocket sshKey action =
    withSystemTempFile ".socket" $ \localPath _ ->
        withProcessTerm
            (proc "ssh" $ maybe [] (\k -> ["-i", k]) sshKey <> ["-N", "-L", localPath <> ":" <> remoteSocket, toString remoteHost])
            (\_process -> interpret_ (\case GetSocket -> pure localPath) action)


localSocket
    :: FilePath
    -- ^ local socket path
    -> Eff (WithSocket : es) a
    -> Eff es a
localSocket localPath = interpret_ (\case GetSocket -> pure localPath)
