{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Hoard.Effects.NodeToClient
    ( NodeToClient
    , runNodeToClient
    , immutableTip
    , isOnChain
    ) where

import Cardano.Api
    ( ConsensusModeParams (CardanoModeParams)
    , EpochSize
    , LocalChainSyncClient (LocalChainSyncClient)
    , LocalNodeClientProtocols
        ( LocalNodeClientProtocols
        , localChainSyncClient
        , localStateQueryClient
        , localTxMonitoringClient
        , localTxSubmissionClient
        )
    , LocalNodeConnectInfo (LocalNodeConnectInfo)
    , NetworkId (Mainnet, Testnet)
    , QueryInMode (QueryChainPoint)
    , ShelleyConfig (ShelleyConfig)
    , ShelleyGenesis (ShelleyGenesis)
    , Target
    , connectToLocalNode
    , readShelleyGenesisConfig
    )
import Control.Concurrent.Chan.Unagi
    ( InChan
    , OutChan
    , newChan
    , readChan
    , writeChan
    )
import Effectful
    ( Eff
    , Effect
    , IOE
    , inject
    , (:>)
    )
import Effectful.Concurrent.Async (race)
import Effectful.Concurrent.MVar
    ( Concurrent
    , newEmptyMVar
    , putMVar
    , readMVar
    )
import Effectful.Dispatch.Dynamic (interpretWith_)
import Effectful.Exception (handleSync)
import Effectful.Labeled (Labeled, labeled)
import Effectful.Reader.Static (Reader, ask)
import Effectful.State.Static.Shared (State, evalState, put, stateM)
import Effectful.TH (makeEffect)
import Ouroboros.Consensus.Config (configBlock)
import Ouroboros.Consensus.Config.SupportsNode (ConfigSupportsNode (getNetworkMagic))
import Ouroboros.Consensus.Node (ProtocolInfo (pInfoConfig))
import Prelude hiding (Reader, State, ask, evalState, get, newEmptyMVar, put, putMVar, readMVar)

import Cardano.Api qualified as C
import Ouroboros.Network.Protocol.ChainSync.Client qualified as S
import Ouroboros.Network.Protocol.LocalStateQuery.Client qualified as Q
import Prelude qualified as P

import Hoard.Effects.Conc (Conc, Thread, fork)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Monitoring.Tracing (Tracing, withSpan)
import Hoard.Effects.WithSocket (WithSocket, getSocket)
import Hoard.Types.Cardano (ChainPoint (ChainPoint))
import Hoard.Types.Environment (Config (..))

import Hoard.Effects.Log qualified as Log


data NodeToClient :: Effect where
    ImmutableTip :: NodeToClient m (Maybe ChainPoint)
    IsOnChain :: ChainPoint -> NodeToClient m (Maybe Bool)


makeEffect ''NodeToClient


data Connection
    = Connection
    { localStateQueries :: InChan (Target C.ChainPoint, MVar ChainPoint)
    , isOnChainQueries :: InChan (ChainPoint, MVar Bool)
    , dead :: MVar SomeException
    -- ^ used to signal that the connection died so queries can stop waiting for responses
    }


-- to do. use `Hoard.Effects.Chan`,...
runNodeToClient
    :: ( Conc :> es
       , Concurrent :> es
       , IOE :> es
       , Labeled "nodeToClient" WithSocket :> es
       , Log :> es
       , Reader Config :> es
       , Tracing :> es
       )
    => Eff (NodeToClient : es) a
    -> Eff es a
runNodeToClient nodeToClient = do
    (connection, newConnectionHandles) <- newConnection
    evalState (Right connection) $ do
        _ <- initializeConnection newConnectionHandles
        interpretWith_
            (inject nodeToClient)
            ( \case
                ImmutableTip -> withSpan "node_query.immutable_tip" $ do
                    Connection localStateQueries _ dead <- ensureConnection
                    resultVar <- newEmptyMVar
                    liftIO $ writeChan localStateQueries (C.ImmutableTip, resultVar)
                    rightToMaybe <$> race (readMVar dead) (readMVar resultVar)
                IsOnChain point -> withSpan "node_query.is_on_chain" $ do
                    Connection _ isOnChainQueries dead <- ensureConnection
                    resultVar <- newEmptyMVar
                    liftIO $ writeChan isOnChainQueries (point, resultVar)
                    rightToMaybe <$> race (readMVar dead) (readMVar resultVar)
            )


newConnection
    :: (Concurrent :> es, IOE :> es)
    => Eff es (Connection, (OutChan (Target C.ChainPoint, MVar ChainPoint), OutChan (ChainPoint, MVar Bool), MVar SomeException))
newConnection =
    do
        (localStateQueriesIn, localStateQueriesOut) <- liftIO newChan
        (isOnChainQueriesIn, isOnChainQueriesOut) <- liftIO newChan
        dead <- newEmptyMVar
        pure (Connection localStateQueriesIn isOnChainQueriesIn dead, (localStateQueriesOut, isOnChainQueriesOut, dead))


initializeConnection
    :: ( Conc :> es
       , Concurrent :> es
       , IOE :> es
       , Labeled "nodeToClient" WithSocket :> es
       , Log :> es
       , Reader Config :> es
       , State (Either SomeException Connection) :> es
       )
    => (OutChan (Target C.ChainPoint, MVar ChainPoint), OutChan (ChainPoint, MVar Bool), MVar SomeException)
    -> Eff es (Thread ())
initializeConnection (localStateQueriesOut, isOnChainQueriesOut, dead) = do
    config <- ask
    epochSize <- loadEpochSize config
    nodeToClientSocket <- labeled @"nodeToClient" getSocket
    fork
        . handleSync
            ( \e -> do
                put (Left e)
                putMVar dead e
                Log.warn $ "`connectToLocalNode` error. " <> toText (displayException e)
            )
        . liftIO
        $ localNodeClient
            ( LocalNodeConnectInfo
                { localConsensusModeParams = CardanoModeParams $ coerce $ epochSize
                , localNodeNetworkId =
                    toNetworkId $ getNetworkMagic $ configBlock $ pInfoConfig $ config.protocolInfo
                , localNodeSocketPath = nodeToClientSocket
                }
            )
            localStateQueriesOut
            isOnChainQueriesOut


ensureConnection
    :: ( Conc :> es
       , Concurrent :> es
       , IOE :> es
       , Labeled "nodeToClient" WithSocket :> es
       , Log :> es
       , Reader Config :> es
       , State (Either SomeException Connection) :> es
       )
    => Eff es Connection
ensureConnection = do
    (connection, newConnectionHandlesMaybe) <-
        stateM
            ( \case
                s@(Right c) -> pure ((c, Nothing), s)
                Left _ -> (\(c, hs) -> ((c, Just hs), Right c)) <$> newConnection
            )
    traverse_
        initializeConnection
        newConnectionHandlesMaybe
    pure connection


localNodeClient
    :: LocalNodeConnectInfo
    -> OutChan (Target C.ChainPoint, MVar ChainPoint)
    -> OutChan (ChainPoint, MVar Bool)
    -> IO ()
localNodeClient connectionInfo localStateQueries isOnChainQueries =
    connectToLocalNode
        connectionInfo
        LocalNodeClientProtocols
            { localChainSyncClient = LocalChainSyncClient (S.ChainSyncClient queryIsOnChain)
            , localStateQueryClient = Just (Q.LocalStateQueryClient queryImmutableTip)
            , localTxSubmissionClient = Nothing
            , localTxMonitoringClient = Nothing
            }
  where
    queryImmutableTip :: IO (Q.ClientStIdle block C.ChainPoint QueryInMode IO void)
    queryImmutableTip = do
        (target, resultVar) <- readChan localStateQueries
        pure
            . Q.SendMsgAcquire target
            $ Q.ClientStAcquiring
                { recvMsgAcquired =
                    pure
                        . Q.SendMsgQuery QueryChainPoint
                        . Q.ClientStQuerying
                        $ \result -> P.putMVar resultVar (ChainPoint result) $> Q.SendMsgRelease queryImmutableTip
                , recvMsgFailure = error "`ImmutableTip` should never fail to be acquired."
                }
    queryIsOnChain :: IO (S.ClientStIdle header C.ChainPoint tip IO void)
    queryIsOnChain = do
        (point, resultVar) <- readChan isOnChainQueries
        pure
            $ S.SendMsgFindIntersect [coerce point]
            $ S.ClientStIntersect
                { recvMsgIntersectFound = \_ _ -> S.ChainSyncClient $ P.putMVar resultVar True *> queryIsOnChain
                , recvMsgIntersectNotFound = \_ -> S.ChainSyncClient $ P.putMVar resultVar False *> queryIsOnChain
                }


loadEpochSize :: (IOE :> es) => Config -> Eff es EpochSize
loadEpochSize Config {nodeConfig} = do
    genesisConfigResult <- runExceptT $ readShelleyGenesisConfig nodeConfig
    ShelleyConfig {scConfig = ShelleyGenesis {sgEpochLength}} <- case genesisConfigResult of
        Left err -> error $ "Failed to read shelley genesis config: " <> show err
        Right cfg -> pure cfg
    pure sgEpochLength


-- | Convert a NetworkMagic to a NetworkId.
-- Mainnet has magic 764824073, everything else is treated as Testnet.
toNetworkId :: C.NetworkMagic -> NetworkId
toNetworkId magic@(C.NetworkMagic m)
    | m == 764824073 = Mainnet
    | otherwise = Testnet magic
