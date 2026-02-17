{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Hoard.Effects.NodeToClient
    ( NodeToClient
    , runNodeToClient
    , immutableTip
    , isOnChain
    , validateVrfSignature
    ) where

import Cardano.Api
    ( AnyCardanoEra (AnyCardanoEra)
    , ConsensusModeParams (CardanoModeParams)
    , Convert (convert)
    , EpochSize
    , Hash (StakePoolKeyHash)
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
    , PoolDistribution (unPoolDistr)
    , QueryInMode (QueryChainPoint)
    , ShelleyConfig (ShelleyConfig)
    , ShelleyGenesis (ShelleyGenesis)
    , Target
    , connectToLocalNode
    , decodePoolDistribution
    , executeLocalStateQueryExpr
    , forEraMaybeEon
    , queryCurrentEra
    , queryPoolDistribution
    , readShelleyGenesisConfig
    )
import Cardano.Api.Experimental (Era)
import Cardano.Ledger.Keys (HasKeyRole (coerceKeyRole), hashKey)
import Control.Concurrent.Chan.Unagi
    ( InChan
    , OutChan
    , newChan
    , readChan
    , writeChan
    )
import Control.Monad.Trans.Except (runExcept)
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
import Ouroboros.Consensus.Cardano.Block (Header (HeaderBabbage, HeaderConway, HeaderDijkstra))
import Ouroboros.Consensus.Config (configBlock)
import Ouroboros.Consensus.Config.SupportsNode (ConfigSupportsNode (getNetworkMagic))
import Ouroboros.Consensus.Node (ProtocolInfo (pInfoConfig))
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
import Ouroboros.Consensus.Protocol.Praos (doValidateVRFSignature)
import Ouroboros.Consensus.Protocol.Praos.Views (HeaderView (hvVK))
import Ouroboros.Consensus.Shelley.Ledger (Header (ShelleyHeader, shelleyHeaderRaw))
import Ouroboros.Consensus.Shelley.Protocol.Abstract (ProtocolHeaderSupportsProtocol (protocolHeaderView))
import Prelude hiding (Reader, State, ask, evalState, get, newEmptyMVar, put, putMVar, readMVar)

import Cardano.Api qualified as C
import Cardano.Ledger.PoolDistr qualified as SL
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Ouroboros.Network.Protocol.ChainSync.Client qualified as S
import Ouroboros.Network.Protocol.LocalStateQuery.Client qualified as Q
import Prelude qualified as P

import Hoard.Effects.Conc (Conc, Thread, fork)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Monitoring.Tracing (Tracing, withSpan)
import Hoard.Effects.WithSocket (WithSocket, getSocket)
import Hoard.Types.Cardano (CardanoHeader, ChainPoint (ChainPoint))
import Hoard.Types.Environment (Config (..))

import Hoard.Effects.Log qualified as Log


data NodeToClient :: Effect where
    ImmutableTip :: NodeToClient m (Maybe ChainPoint)
    IsOnChain :: ChainPoint -> NodeToClient m (Maybe Bool)
    ValidateVrfSignature :: CardanoHeader -> NodeToClient m Bool


makeEffect ''NodeToClient


data Connection
    = Connection
    { localStateQueries :: InChan (Target C.ChainPoint, LocalStateQueryWithResultMVar)
    , isOnChainQueries :: InChan (ChainPoint, MVar Bool)
    , dead :: MVar SomeException
    -- ^ used to signal that the connection died so queries can stop waiting for responses
    }


data LocalStateQueryWithResultMVar where
    LocalStateQueryWithResultMVar :: QueryInMode result -> MVar (Either AcquireFailure result) -> LocalStateQueryWithResultMVar


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
                    liftIO $ writeChan localStateQueries (C.ImmutableTip, LocalStateQueryWithResultMVar QueryChainPoint resultVar)
                    rightToMaybe
                        <$> race
                            (readMVar dead)
                            ( fmap ChainPoint
                                $ fmap (fromRight $ error "`C.ImmutableTip` should never fail to be acquired.")
                                $ readMVar
                                $ resultVar
                            )
                IsOnChain point -> withSpan "node_query.is_on_chain" $ do
                    Connection _ isOnChainQueries dead <- ensureConnection
                    resultVar <- newEmptyMVar
                    liftIO $ writeChan isOnChainQueries (point, resultVar)
                    rightToMaybe <$> race (readMVar dead) (readMVar resultVar)
                ValidateVrfSignature header -> withSpan "node_query.validate_vrf_signature" $ do
                    let headerView = case header of
                            HeaderBabbage (ShelleyHeader {shelleyHeaderRaw}) -> protocolHeaderView shelleyHeaderRaw
                            HeaderConway (ShelleyHeader {shelleyHeaderRaw}) -> protocolHeaderView shelleyHeaderRaw
                            HeaderDijkstra (ShelleyHeader {shelleyHeaderRaw}) -> protocolHeaderView shelleyHeaderRaw
                            _ -> error "to do"
                        blockIssuer = coerceKeyRole $ hashKey $ hvVK $ headerView
                    poolDistribution <- liftIO $ fmap (fromRight $ error $ "to do") $ executeLocalStateQueryExpr undefined undefined $ do
                        AnyCardanoEra e <- fromRight (error "to do") <$> queryCurrentEra
                        let era = fromMaybe (error "to do") $ forEraMaybeEon @Era e
                        serPoolDistr <-
                            fmap (fromRight $ error $ "to do")
                                $ fmap (fromRight $ error $ "to do")
                                $ queryPoolDistribution (convert era)
                                $ Just
                                $ S.singleton
                                $ StakePoolKeyHash
                                $ blockIssuer
                        pure
                            $ fromRight (error "to do")
                            $
                            -- copied from https://cardano-api.cardano.intersectmbo.org/cardano-api/src/Cardano.Api.LedgerState.html#line-2213
                            fmap (SL.unPoolDistr . unPoolDistr)
                            $ decodePoolDistribution (convert era) serPoolDistr
                    let activeSlotsCoeff = undefined 0.05 -- to do. get from config
                    pure
                        $ isRight
                        $ runExcept
                        $ doValidateVRFSignature
                            undefined -- to do. maybe `slotToNonce` or `mkNonceFromOutputVRF $ certifiedOutput $ hvVrfRes $ headerView`? however, in `doValidateVRFSignature`, this is not used for calling `checkLeaderNatValue`. so we could inline the definition of `doValidateVRFSignature`. we have to inline a variation of the definition of `doValidateVRFSignature` anyway for `TPraos`.
                            poolDistribution
                            activeSlotsCoeff
                            headerView
            )


newConnection
    :: (Concurrent :> es, IOE :> es)
    => Eff es (Connection, (OutChan (Target C.ChainPoint, LocalStateQueryWithResultMVar), OutChan (ChainPoint, MVar Bool), MVar SomeException))
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
    => (OutChan (Target C.ChainPoint, LocalStateQueryWithResultMVar), OutChan (ChainPoint, MVar Bool), MVar SomeException)
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
    -> OutChan (Target C.ChainPoint, LocalStateQueryWithResultMVar)
    -> OutChan (ChainPoint, MVar Bool)
    -> IO ()
localNodeClient connectionInfo localStateQueries isOnChainQueries =
    connectToLocalNode
        connectionInfo
        LocalNodeClientProtocols
            { localChainSyncClient = LocalChainSyncClient (S.ChainSyncClient queryIsOnChain)
            , localStateQueryClient = Just (Q.LocalStateQueryClient localStateQuery)
            , localTxSubmissionClient = Nothing
            , localTxMonitoringClient = Nothing
            }
  where
    localStateQuery :: IO (Q.ClientStIdle block C.ChainPoint QueryInMode IO void)
    localStateQuery = do
        (target, LocalStateQueryWithResultMVar query resultVar) <- readChan localStateQueries
        pure
            . Q.SendMsgAcquire target
            $ Q.ClientStAcquiring
                { recvMsgAcquired =
                    pure
                        . Q.SendMsgQuery query
                        . Q.ClientStQuerying
                        $ \r -> P.putMVar resultVar (Right r) $> Q.SendMsgRelease localStateQuery
                , recvMsgFailure = \e -> P.putMVar resultVar (Left e) *> localStateQuery
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
