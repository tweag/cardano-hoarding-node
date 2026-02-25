{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Hoard.Effects.NodeToClient
    ( NodeToClient
    , NodeConnectionError
    , runNodeToClient
    , immutableTip
    , isOnChain
    , validateVrfSignature
    ) where

import Cardano.Api
    ( ConsensusModeParams (CardanoModeParams)
    , Convert (convert)
    , EraMismatch
    , Hash (HeaderHash, StakePoolKeyHash)
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
    , QueryInEra (QueryInShelleyBasedEra)
    , QueryInMode (QueryChainPoint, QueryInEra)
    , QueryInShelleyBasedEra (QueryPoolDistribution)
    , ShelleyBasedEra (ShelleyBasedEraBabbage, ShelleyBasedEraConway, ShelleyBasedEraDijkstra)
    , ShelleyConfig
    , Target (SpecificPoint)
    , connectToLocalNode
    , decodePoolDistribution
    )
import Cardano.Binary (DecoderError)
import Cardano.Crypto.VRF (CertifiedVRF (certifiedOutput))
import Cardano.Ledger.BaseTypes (mkActiveSlotCoeff, mkNonceFromOutputVRF)
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
import Effectful.Error.Static (Error, throwError)
import Effectful.Exception (handleSync, throwIO)
import Effectful.Labeled (Labeled, labeled)
import Effectful.Reader.Static (Reader, ask, asks)
import Effectful.State.Static.Shared (State, evalState, put, stateM)
import Effectful.TH (makeEffect)
import Ouroboros.Consensus.Block (ConvertRawHash (toShortRawHash), blockSlot, headerHash)
import Ouroboros.Consensus.Cardano.Block
    ( Header
        ( HeaderAllegra
        , HeaderAlonzo
        , HeaderBabbage
        , HeaderByron
        , HeaderConway
        , HeaderDijkstra
        , HeaderMary
        , HeaderShelley
        )
    )
import Ouroboros.Consensus.Config (configBlock)
import Ouroboros.Consensus.Config.SupportsNode (ConfigSupportsNode (getNetworkMagic))
import Ouroboros.Consensus.Node (ProtocolInfo (pInfoConfig))
import Ouroboros.Consensus.Protocol.Praos (PraosValidationErr, doValidateVRFSignature)
import Ouroboros.Consensus.Protocol.Praos.Views (HeaderView (hvVK, hvVrfRes))
import Ouroboros.Consensus.Shelley.Ledger (Header (ShelleyHeader, shelleyHeaderRaw))
import Ouroboros.Consensus.Shelley.Protocol.Abstract (ProtocolHeaderSupportsProtocol (protocolHeaderView))
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
import Prelude hiding (Reader, State, ask, asks, evalState, get, newEmptyMVar, put, putMVar, readMVar)

import Cardano.Api qualified as C
import Cardano.Ledger.State qualified as SL
import Data.Set qualified as S
import Ouroboros.Network.Protocol.ChainSync.Client qualified as S
import Ouroboros.Network.Protocol.LocalStateQuery.Client qualified as Q
import Prelude qualified as P

import Hoard.Effects.Conc (Conc, Thread, fork)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Monitoring.Tracing (Tracing, withSpan)
import Hoard.Effects.WithSocket (WithSocket, getSocket)
import Hoard.Types.Cardano (CardanoBlock, CardanoHeader, ChainPoint (ChainPoint), Crypto)

import Hoard.Effects.Log qualified as Log


data NodeToClient :: Effect where
    ImmutableTip :: NodeToClient m (Either NodeConnectionError ChainPoint)
    IsOnChain :: ChainPoint -> NodeToClient m (Either NodeConnectionError Bool)
    ValidateVrfSignature_ :: CardanoHeader -> NodeToClient m (NodeConnectionError :+ AcquireFailure :+ DecoderError :+ ElectionValidationError :+ ())


data NodeConnectionError
    = NodeConnectionError SomeException
    deriving (Show)


data ElectionValidationError
    = PraosValidationErr (PraosValidationErr Crypto)
    | EraMismatch EraMismatch


infixr 6 :+
type (:+) = Either


makeEffect ''NodeToClient


validateVrfSignature
    :: ( Error AcquireFailure :> es
       , Error NodeConnectionError :> es
       , NodeToClient :> es
       )
    => CardanoHeader -> Eff es (Either ElectionValidationError ())
validateVrfSignature =
    (=<<) (either throwIO pure) -- `decodePoolDistribution` should never fail to decode the pool distribution, should it?
        . (=<<) (either throwError pure)
        . (=<<) (either throwError pure)
        . validateVrfSignature_


data Connection
    = Connection
    { localStateQueries :: InChan LocalStateQueryWithResultMVar
    , isOnChainQueries :: InChan (ChainPoint, MVar Bool)
    , dead :: MVar SomeException
    -- ^ used to signal that the connection died so queries can stop waiting for responses
    }


data LocalStateQueryWithResultMVar where
    LocalStateQueryWithResultMVar :: MVar (Either AcquireFailure result) -> Target C.ChainPoint -> QueryInMode result -> LocalStateQueryWithResultMVar


executeLocalStateQuery
    :: ( Conc :> es
       , Concurrent :> es
       , IOE :> es
       , Labeled "nodeToClient" WithSocket :> es
       , Log :> es
       , Reader (ProtocolInfo CardanoBlock) :> es
       , Reader ShelleyConfig :> es
       , State (Either SomeException Connection) :> es
       )
    => Target C.ChainPoint
    -> QueryInMode result
    -> Eff es (Either NodeConnectionError (Either AcquireFailure result))
executeLocalStateQuery target localStateQuery =
    do
        Connection localStateQueries _ dead <- ensureConnection
        resultVar <- newEmptyMVar
        liftIO $ writeChan localStateQueries $ LocalStateQueryWithResultMVar resultVar target localStateQuery
        race (NodeConnectionError <$> readMVar dead) (readMVar resultVar)


-- to do. use `Hoard.Effects.Chan`,...
runNodeToClient
    :: ( Conc :> es
       , Concurrent :> es
       , IOE :> es
       , Labeled "nodeToClient" WithSocket :> es
       , Log :> es
       , Reader (ProtocolInfo CardanoBlock) :> es
       , Reader ShelleyConfig :> es
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
                ImmutableTip ->
                    withSpan "node_query.immutable_tip"
                        $ (fmap . fmap) ChainPoint
                        $ (fmap . fmap) (fromRight $ error "`C.ImmutableTip` should never fail to be acquired.")
                        $ executeLocalStateQuery C.ImmutableTip
                        $ QueryChainPoint
                IsOnChain point -> withSpan "node_query.is_on_chain" $ do
                    Connection _ isOnChainQueries dead <- ensureConnection
                    resultVar <- newEmptyMVar
                    liftIO $ writeChan isOnChainQueries (point, resultVar)
                    race (NodeConnectionError <$> readMVar dead) $ readMVar $ resultVar
                ValidateVrfSignature_ header -> withSpan "node_query.validate_vrf_signature"
                    $ case header of
                        HeaderByron _ -> error "to do"
                        HeaderShelley _ -> error "to do"
                        HeaderAllegra _ -> error "to do"
                        HeaderMary _ -> error "to do"
                        HeaderAlonzo _ -> error "to do"
                        HeaderBabbage (ShelleyHeader {shelleyHeaderRaw}) ->
                            validateVrfSignaturePraos
                                ShelleyBasedEraBabbage
                                header
                                (protocolHeaderView shelleyHeaderRaw)
                        HeaderConway (ShelleyHeader {shelleyHeaderRaw}) ->
                            validateVrfSignaturePraos
                                ShelleyBasedEraConway
                                header
                                (protocolHeaderView shelleyHeaderRaw)
                        HeaderDijkstra (ShelleyHeader {shelleyHeaderRaw}) ->
                            validateVrfSignaturePraos
                                ShelleyBasedEraDijkstra
                                header
                                (protocolHeaderView shelleyHeaderRaw)
            )


validateVrfSignaturePraos
    :: ( Conc :> es
       , Concurrent :> es
       , IOE :> es
       , Labeled "nodeToClient" WithSocket :> es
       , Log :> es
       , Reader (ProtocolInfo CardanoBlock) :> es
       , Reader ShelleyConfig :> es
       , State (Either SomeException Connection) :> es
       )
    => ShelleyBasedEra era
    -> CardanoHeader
    -> HeaderView Crypto
    -> Eff es (NodeConnectionError :+ AcquireFailure :+ DecoderError :+ ElectionValidationError :+ ())
validateVrfSignaturePraos era header headerView = do
    activeSlotsCoeff <- mkActiveSlotCoeff <$> asks @ShelleyConfig (.scConfig.sgActiveSlotsCoeff)
    (fmap . fmap . fmap . fmap . (=<<))
        ( \poolDistribution ->
            first PraosValidationErr -- wrap `PraosValidationErr` into an `ElectionValidationError`
                $ runExcept
                $ doValidateVRFSignature
                    (mkNonceFromOutputVRF $ certifiedOutput $ hvVrfRes $ headerView) -- to do. or `slotToNonce`? however, in `doValidateVRFSignature`, this is not used for calling `checkLeaderNatValue`. so we could inline the definition of `doValidateVRFSignature`. we have to inline a variation of the definition of `doValidateVRFSignature` anyway for `TPraos`.
                    poolDistribution
                    activeSlotsCoeff
                    headerView
        )
        $ (fmap . fmap . fmap . fmap . first) EraMismatch -- wrap `EraMismatch` into an `ElectionValidationError`
        $ (fmap . fmap . fmap . fmap . fmap) (SL.unPoolDistr . unPoolDistr)
        $ (fmap . fmap . fmap . traverse) (decodePoolDistribution era)
        $ executeLocalStateQuery
            ( SpecificPoint
                $ C.ChainPoint (blockSlot header)
                $ HeaderHash
                $ toShortRawHash (Proxy @CardanoBlock)
                $ headerHash
                $ header
            )
        $ QueryInEra
        $ QueryInShelleyBasedEra (convert era)
        $ QueryPoolDistribution
        $ Just
        $ S.singleton
        -- block issuer
        $ StakePoolKeyHash
        $ coerceKeyRole
        $ hashKey
        $ hvVK
        $ headerView


newConnection
    :: (Concurrent :> es, IOE :> es)
    => Eff es (Connection, (OutChan LocalStateQueryWithResultMVar, OutChan (ChainPoint, MVar Bool), MVar SomeException))
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
       , Reader (ProtocolInfo CardanoBlock) :> es
       , Reader ShelleyConfig :> es
       , State (Either SomeException Connection) :> es
       )
    => (OutChan LocalStateQueryWithResultMVar, OutChan (ChainPoint, MVar Bool), MVar SomeException)
    -> Eff es (Thread ())
initializeConnection (localStateQueriesOut, isOnChainQueriesOut, dead) = do
    protocolInfo <- ask @(ProtocolInfo CardanoBlock)
    epochSize <- asks @ShelleyConfig (.scConfig.sgEpochLength)
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
                    toNetworkId $ getNetworkMagic $ configBlock $ pInfoConfig $ protocolInfo
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
       , Reader (ProtocolInfo CardanoBlock) :> es
       , Reader ShelleyConfig :> es
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
    -> OutChan LocalStateQueryWithResultMVar
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
        LocalStateQueryWithResultMVar resultVar target query <- readChan localStateQueries
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


-- | Convert a NetworkMagic to a NetworkId.
-- Mainnet has magic 764824073, everything else is treated as Testnet.
toNetworkId :: C.NetworkMagic -> NetworkId
toNetworkId magic@(C.NetworkMagic m)
    | m == 764824073 = Mainnet
    | otherwise = Testnet magic
