module Hoard.Effects.NodeToClient
    ( NodeToClient
    , ConnectionError (..)
    , runNodeToClient
    , immutableTipWith1Retry
    , isOnChainWith1Retry
    , ensureConnection
    , immutableTip
    , isOnChain
    , retry1
    ) where

import Cardano.Api
    ( ChainPoint
    , ConsensusModeParams (CardanoModeParams)
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
    , connectToLocalNode
    , readShelleyGenesisConfig
    )
import Cardano.Api qualified as C
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
    , (:>)
    )
import Effectful.Concurrent.Async (race)
import Effectful.Concurrent.MVar
    ( Concurrent
    , newEmptyMVar
    , putMVar
    , readMVar
    )
import Effectful.Dispatch.Dynamic (reinterpret_)
import Effectful.Exception (handleSync)
import Effectful.Labeled (Labeled, labeled)
import Effectful.Reader.Static (Reader, ask)
import Effectful.State.Static.Shared (evalState, get, put, stateM)
import Effectful.TH (makeEffect)
import Hoard.Effects.Conc (Conc, fork)
import Hoard.Effects.Log (Log, withNamespace)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.WithSocket (WithSocket, getSocket)
import Hoard.Types.Environment (Config (..))
import Ouroboros.Network.Protocol.ChainSync.Client qualified as S
import Ouroboros.Network.Protocol.LocalStateQuery.Client qualified as Q
import Prelude hiding (Reader, ask, evalState, get, newEmptyMVar, put, putMVar, readMVar)
import Prelude qualified as P
import Ouroboros.Consensus.Config.SupportsNode (ConfigSupportsNode(getNetworkMagic))
import Ouroboros.Consensus.Config (configBlock)
import Ouroboros.Consensus.Node (ProtocolInfo(pInfoConfig))


data NodeToClient :: Effect where
    ImmutableTip :: NodeToClient m (Either ConnectionError ChainPoint)
    IsOnChain :: ChainPoint -> NodeToClient m (Either ConnectionError Bool)
    EnsureConnection :: NodeToClient m (Maybe ())


data ConnectionError
    = UninitializedConnection
    | ConnectionException
    deriving (Show)


makeEffect ''NodeToClient


data Connection
    = Connection
        (InChan (MVar ChainPoint))
        -- ^ immutableTipQueries
        (InChan (ChainPoint, MVar Bool))
        -- ^ isOnChainQueries
        (MVar SomeException)
        -- ^ used to signal that the connection died so queries can stop waiting for responses
    | Uninitialized
    | Error SomeException


runNodeToClient
    :: ( Labeled "nodeToClient" WithSocket :> es
       , Conc :> es
       , Log :> es
       , IOE :> es
       , Reader Config :> es
       , Concurrent :> es
       )
    => Eff (NodeToClient : es) a
    -> Eff es a
runNodeToClient =
    -- to do. use `Hoard.Effects.Chan`,...
    reinterpret_
        (evalState Uninitialized)
        ( \case
            ImmutableTip ->
                get >>= \case
                    Connection immutableTipQueries _ dead -> do
                        resultVar <- newEmptyMVar
                        liftIO $ writeChan immutableTipQueries resultVar
                        race (ConnectionException <$ readMVar dead) (readMVar resultVar)
                    Uninitialized -> pure (Left UninitializedConnection)
                    Error _ -> pure (Left ConnectionException)
            IsOnChain point ->
                get >>= \case
                    Connection _ isOnChainQueries dead -> do
                        resultVar <- newEmptyMVar
                        liftIO $ writeChan isOnChainQueries (point, resultVar)
                        race (ConnectionException <$ readMVar dead) (readMVar resultVar)
                    Uninitialized -> pure (Left UninitializedConnection)
                    Error _ -> pure (Left ConnectionException)
            EnsureConnection ->
                stateM
                    ( \case
                        s@(Connection {}) -> pure (Nothing, s)
                        _ -> do
                            (immutableTipQueriesIn, immutableTipQueriesOut) <- liftIO newChan
                            (isOnChainQueriesIn, isOnChainQueriesOut) <- liftIO newChan
                            dead <- newEmptyMVar
                            pure
                                ( Just (immutableTipQueriesOut, isOnChainQueriesOut, dead)
                                , Connection immutableTipQueriesIn isOnChainQueriesIn dead
                                )
                    )
                    >>= ( \case
                            Nothing -> pure Nothing
                            Just (immutableTipQueriesOut, isOnChainQueriesOut, dead) -> do
                                config <- ask
                                epochSize <- loadEpochSize config
                                nodeToClientSocket <- labeled @"nodeToClient" getSocket
                                let networkMagic = getNetworkMagic (configBlock (pInfoConfig config.protocolInfo))
                                    networkId = toNetworkId networkMagic
                                ($> Just ())
                                    . fork
                                    . handleSync
                                        ( \e -> do
                                            put (Error e)
                                            putMVar dead e
                                            Log.warn $ "`connectToLocalNode` error. " <> toText (displayException e)
                                        )
                                    . liftIO
                                    $ localNodeClient
                                        ( LocalNodeConnectInfo
                                            { localConsensusModeParams = CardanoModeParams $ coerce $ epochSize
                                            , localNodeNetworkId = networkId
                                            , localNodeSocketPath = nodeToClientSocket
                                            }
                                        )
                                        immutableTipQueriesOut
                                        isOnChainQueriesOut
                        )
        )


-- | Queries the immutable tip. If there is no connection the local node,
-- it tried reconnecting and querying again once.
immutableTipWith1Retry :: (NodeToClient :> es, Log :> es) => Eff es (Either ConnectionError ChainPoint)
immutableTipWith1Retry = withNamespace "immutableTipWith1Retry" $ retry1 immutableTip


-- | Queries whether the given `ChainPoint` is on the chain. If there is no connection to the local node,
-- -- it tries reconnecting and querying again once.
isOnChainWith1Retry :: (NodeToClient :> es, Log :> es) => ChainPoint -> Eff es (Either ConnectionError Bool)
isOnChainWith1Retry = withNamespace "isOnChainWith1Retry" . retry1 . isOnChain


retry1 :: (NodeToClient :> es, Log :> es) => Eff es (Either ConnectionError a) -> Eff es (Either ConnectionError a)
retry1 operation =
    operation >>= \case
        Left e -> Log.warn ("local node connection failed with `" <> show e <> "`. reconnecting...") *> ensureConnection *> operation
        Right tip -> pure (Right tip)


localNodeClient :: LocalNodeConnectInfo -> OutChan (MVar ChainPoint) -> OutChan (ChainPoint, MVar Bool) -> IO ()
localNodeClient connectionInfo immutableTipQueries isOnChainQueries =
    connectToLocalNode
        connectionInfo
        LocalNodeClientProtocols
            { localChainSyncClient = LocalChainSyncClient (S.ChainSyncClient queryIsOnChain)
            , localStateQueryClient = Just (Q.LocalStateQueryClient queryImmutableTip)
            , localTxSubmissionClient = Nothing
            , localTxMonitoringClient = Nothing
            }
  where
    queryImmutableTip :: IO (Q.ClientStIdle block point QueryInMode IO void)
    queryImmutableTip = do
        resultVar <- readChan immutableTipQueries
        pure
            . Q.SendMsgAcquire C.ImmutableTip
            $ Q.ClientStAcquiring
                { recvMsgAcquired =
                    pure
                        . Q.SendMsgQuery QueryChainPoint
                        . Q.ClientStQuerying
                        $ \result -> P.putMVar resultVar result $> Q.SendMsgRelease queryImmutableTip
                , recvMsgFailure = error "`ImmutableTip` should never fail to be acquired."
                }
    queryIsOnChain :: IO (S.ClientStIdle header ChainPoint tip IO void)
    queryIsOnChain = do
        (point, resultVar) <- readChan isOnChainQueries
        pure $
            S.SendMsgFindIntersect [point] $
                S.ClientStIntersect
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
