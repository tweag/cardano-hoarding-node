module Hoard.Effects.WithSocket
    ( -- * Effect
      WithSocket (..)

      -- * Handlers
    , withNodeSockets
    , sshTunnelSocket
    , localSocket

      -- * Operations
    , getSocket
    ) where

import Cardano.Api (File (File), SocketPath)
import Effectful (Eff, Effect)
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.Labeled (Labeled, runLabeled)
import Effectful.Reader.Static (asks)
import Effectful.TH (makeEffect)
import Effectful.Temporary (withSystemTempFile)
import Hoard.Types.Environment
import System.Process.Typed (proc, withProcessTerm)
import Prelude hiding (Reader, asks)


data WithSocket :: Effect where
    GetSocket :: WithSocket m SocketPath


makeEffect ''WithSocket


withNodeSockets
    :: (_)
    => Eff (Labeled "nodeToClient" WithSocket : Labeled "tracer" WithSocket : es) a
    -> Eff es a
withNodeSockets action =
    asks @Config (.nodeSockets) >>= \case
        SshTunnel (MakeSshTunnel {nodeToClientSocket, tracerSocket, user, remoteHost, sshKey}) ->
            runLabeled @"tracer" (sshTunnelSocket user remoteHost tracerSocket sshKey)
                . runLabeled @"nodeToClient" (sshTunnelSocket user remoteHost nodeToClientSocket sshKey)
                $ action
        Local (MakeLocal {nodeToClientSocket, tracerSocket}) ->
            runLabeled @"tracer" (localSocket $ File tracerSocket)
                . runLabeled @"nodeToClient" (localSocket $ File nodeToClientSocket)
                $ action


sshTunnelSocket
    :: (_)
    => Text
    -- ^ user
    -> Text
    -- ^ host
    -> FilePath
    -- ^ remote socket path
    -> Maybe FilePath
    -- ^ ssh key path
    -> Eff (WithSocket : es) a
    -> Eff es a
sshTunnelSocket user remoteHost remoteSocket sshKey action =
    withSystemTempFile ".socket" $ \localPath _ ->
        withProcessTerm -- do not wait for `ssh` to exit. it will not.
            ( proc "ssh" $
                maybe [] (\k -> ["-i", k]) sshKey
                    <> [ "-N"
                       , "-o"
                       , "ExitOnForwardFailure=yes"
                       , "-o"
                       , "ServerAliveInterval=60"
                       , "-L"
                       , localPath <> ":" <> remoteSocket
                       , toString $ user <> "@" <> remoteHost
                       ]
            )
            (\_process -> interpret_ (\case GetSocket -> pure $ File localPath) action)


localSocket
    :: SocketPath
    -- ^ local socket path
    -> Eff (WithSocket : es) a
    -> Eff es a
localSocket localPath = interpret_ (\case GetSocket -> pure localPath)
