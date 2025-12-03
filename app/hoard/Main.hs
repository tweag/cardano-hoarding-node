module Main (main) where

import Ouroboros.Network.IOManager (withIOManager)

import Options.Applicative qualified as Opt

import Hoard.CLI.Options (Options (..), optsParser)
import Hoard.Config.Loader (loadConfig)
import Hoard.Effects (Config (..), ServerConfig (..), runEffectStack)
import Hoard.Effects.Sub (listen)
import Hoard.Listeners.ChainSyncEventListener (chainSyncEventListener)
import Hoard.Listeners.CollectorEventListener (collectorEventListener)
import Hoard.Listeners.HeaderReceivedListener (headerReceivedListener)
import Hoard.Listeners.NetworkEventListener (networkEventListener)
import Hoard.Listeners.PeerSharingEventListener (peerSharingEventListener)
import Hoard.Listeners.PeersReceivedListener (peersReceivedListener)
import Hoard.Types.Environment (Environment (..))

import Effectful (Eff)
import Hoard.Collector (dispatchDiscoveredNodes)
import Hoard.Effects (AppEff)
import Hoard.Effects.Conc qualified as Conc
import Hoard.Server qualified as Server


main :: IO ()
main = withIOManager $ \ioManager -> do
    options <- Opt.execParser optsParser

    -- Determine environment: CLI flag > default to Dev
    let env = fromMaybe Dev options.environment

    putTextLn $ "Loading configuration for environment: " <> show env
    config <- loadConfig ioManager env

    -- Start background threads in the effect stack
    putTextLn $ "Starting Hoard on " <> config.server.host <> ":" <> show config.server.port <> "..."
    runEffectStack config $ do
        runServer config
        runListeners
        Conc.awaitAll


runServer :: (AppEff es) => Config -> Eff es ()
runServer config = do
    _ <- Conc.fork $ Server.runServer config
    pure ()


runListeners :: (AppEff es) => Eff es ()
runListeners = do
    _ <- Conc.fork $ listen headerReceivedListener
    _ <- Conc.fork $ listen peersReceivedListener
    _ <- Conc.fork $ listen dispatchDiscoveredNodes
    _ <- Conc.fork $ listen networkEventListener
    _ <- Conc.fork $ listen peerSharingEventListener
    _ <- Conc.fork $ listen chainSyncEventListener
    _ <- Conc.fork $ listen collectorEventListener
    pure ()
