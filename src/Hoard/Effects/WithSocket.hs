module Hoard.Effects.WithSocket
    ( -- * Effect
      WithSocket (..)

      -- * Handlers
    , withNodeSockets
    , sshTunnelSocket
    , localSocket

      -- * Operations
    , getSocket

      -- * Configuration
    , NodeSocketsConfig (..)
    , SshTunnel (..)
    , Local (..)
    ) where

import Cardano.Api (File (File), SocketPath)
import Data.Aeson (FromJSON)
import Effectful (Effect, IOE)
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.Labeled (Labeled, runLabeled)
import Effectful.Reader.Static (Reader, ask)
import Effectful.TH (makeEffect)
import Effectful.Temporary (Temporary, withSystemTempFile)
import System.Process.Typed (proc, withProcessTerm)

import Hoard.CardanoNode.Config (Config (..))
import Hoard.Types.QuietSnake (QuietSnake (..))


data NodeSocketsConfig
    = SshTunnel SshTunnel
    | Local Local
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake NodeSocketsConfig


data SshTunnel = MakeSshTunnel
    { nodeToClientSocket :: FilePath
    , tracerSocket :: FilePath
    , user :: Text
    , remoteHost :: Text
    , sshKey :: Maybe FilePath
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake SshTunnel


data Local = MakeLocal
    { nodeToClientSocket :: FilePath
    , tracerSocket :: FilePath
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON) via QuietSnake Local


data WithSocket :: Effect where
    GetSocket :: WithSocket m SocketPath


makeEffect ''WithSocket


withNodeSockets
    :: (IOE :> es, Reader Config :> es, Reader NodeSocketsConfig :> es, Temporary :> es)
    => Eff (Labeled "nodeToClient" WithSocket : Labeled "tracer" WithSocket : es) a
    -> Eff es a
withNodeSockets action = do
    nodeIntegrationCfg <- ask @Config
    ask @NodeSocketsConfig >>= \case
        SshTunnel (MakeSshTunnel {nodeToClientSocket, tracerSocket, user, remoteHost, sshKey}) ->
            runLabeled @"tracer" (sshTunnelSocket nodeIntegrationCfg user remoteHost tracerSocket sshKey)
                . runLabeled @"nodeToClient" (sshTunnelSocket nodeIntegrationCfg user remoteHost nodeToClientSocket sshKey)
                $ action
        Local (MakeLocal {nodeToClientSocket, tracerSocket}) ->
            runLabeled @"tracer" (localSocket $ File tracerSocket)
                . runLabeled @"nodeToClient" (localSocket $ File nodeToClientSocket)
                $ action


sshTunnelSocket
    :: (IOE :> es, Temporary :> es)
    => Config
    -- ^ Cardano node integration configuration
    -> Text
    -- ^ user
    -> Text
    -- ^ host
    -> FilePath
    -- ^ remote socket path
    -> Maybe FilePath
    -- ^ ssh key path
    -> Eff (WithSocket : es) a
    -> Eff es a
sshTunnelSocket nodeIntegrationCfg user remoteHost remoteSocket sshKey action =
    withSystemTempFile ".socket" $ \localPath _ ->
        withProcessTerm -- do not wait for `ssh` to exit. it will not.
            ( proc "ssh"
                $ maybe [] (\k -> ["-i", k]) sshKey
                    <> [ "-N"
                       , "-o"
                       , "ExitOnForwardFailure=yes"
                       , "-o"
                       , "ServerAliveInterval=" <> show nodeIntegrationCfg.sshServerAliveIntervalSeconds
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
