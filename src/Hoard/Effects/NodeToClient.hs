{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Hoard.Effects.NodeToClient
    ( NodeToClient
    , NodeConnectionError
    , runNodeToClient
    , immutableTip
    , isOnChain
    , validateVrfSignature
    , validateVrfSignature_
    ) where

import Cardano.Api
    ( ByronBlock
    , ChainDepState
    , ConsensusModeParams (CardanoModeParams)
    , ConsensusProtocol
    , Convert (convert)
    , EraMismatch
    , FromCBOR
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
    , PastHorizonException
    , PoolDistribution (unPoolDistr)
    , QueryInEra (QueryInShelleyBasedEra)
    , QueryInMode (QueryChainPoint, QueryEraHistory, QueryInEra)
    , QueryInShelleyBasedEra (QueryPoolDistribution, QueryProtocolState)
    , ShelleyBasedEra
        ( ShelleyBasedEraAllegra
        , ShelleyBasedEraAlonzo
        , ShelleyBasedEraBabbage
        , ShelleyBasedEraConway
        , ShelleyBasedEraDijkstra
        , ShelleyBasedEraMary
        , ShelleyBasedEraShelley
        )
    , ShelleyConfig
    , Target (SpecificPoint)
    , chainPointToSlotNo
    , connectToLocalNode
    , decodePoolDistribution
    , decodeProtocolState
    , slotToEpoch
    )
import Cardano.Crypto.VRF (CertifiedVRF (CertifiedVRF, certifiedOutput), VRFAlgorithm)
import Cardano.Ledger.BaseTypes (mkActiveSlotCoeff)
import Cardano.Ledger.Keys (HasKeyRole (coerceKeyRole), hashKey)
import Cardano.Ledger.State (IndividualPoolStake (individualPoolStake))
import Cardano.Protocol.Crypto (VRF)
import Cardano.Protocol.TPraos.BHeader
    ( BHBody (BHBody, bheaderL, bheaderVk)
    , BHeader (BHeader)
    , BoundedNatural (bvValue)
    , assertBoundedNatural
    , checkLeaderValue
    )
import Control.Concurrent.Chan.Unagi
    ( InChan
    , OutChan
    , newChan
    , readChan
    , writeChan
    )
import Control.Monad.Trans.Except (runExcept)
import Effectful
    ( Effect
    , IOE
    , inject
    )
import Effectful.Concurrent.Async (race)
import Effectful.Concurrent.MVar
    ( Concurrent
    , MVar
    , newEmptyMVar
    , putMVar
    , readMVar
    )
import Effectful.Dispatch.Dynamic (interpretWith_)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
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
import Ouroboros.Consensus.Protocol.Praos (PraosValidationErr (VRFKeyUnknown, VRFLeaderValueTooBig), doValidateVRFSignature)
import Ouroboros.Consensus.Protocol.Praos.Views (HeaderView (hvVK))
import Ouroboros.Consensus.Shelley.Ledger (Header (ShelleyHeader))
import Ouroboros.Consensus.Shelley.Protocol.Abstract (ProtocolHeaderSupportsProtocol (protocolHeaderView))
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)

import Cardano.Api qualified as C
import Cardano.Crypto.VRF qualified as VRF
import Cardano.Ledger.State qualified as SL
import Control.Concurrent.MVar qualified as MVar
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Ouroboros.Consensus.Protocol.Praos.Common qualified as Consensus
import Ouroboros.Network.Protocol.ChainSync.Client qualified as S
import Ouroboros.Network.Protocol.LocalStateQuery.Client qualified as Q

import Hoard.Effects.Conc (Conc, Thread, fork)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Monitoring.Tracing (Tracing, withSpan)
import Hoard.Effects.WithSocket (WithSocket, getSocket)
import Hoard.Types.Cardano (CardanoBlock, CardanoHeader, ChainPoint (ChainPoint), Crypto)

import Hoard.Effects.Log qualified as Log


data NodeToClient :: Effect where
    ImmutableTip :: NodeToClient m (Either NodeConnectionError ChainPoint)
    IsOnChain :: ChainPoint -> NodeToClient m (Either NodeConnectionError Bool)
    ValidateVrfSignature_ :: CardanoHeader -> NodeToClient m (HeaderAtGenesis :+ PastHorizonException :+ NodeConnectionError :+ IntersectNotFound :+ PointPastEpochHorizon :+ NoByronSupport :+ AcquireFailure :+ ElectionValidationError :+ ())


data NodeConnectionError = NodeConnectionError SomeException deriving (Show)


data IntersectNotFound = IntersectNotFound (Header CardanoBlock) ChainPoint deriving (Show)


data PointPastEpochHorizon = PointPastEpochHorizon (Header CardanoBlock) ChainPoint ChainPoint deriving (Show)


data NoByronSupport = NoByronSupport (Header ByronBlock) deriving (Show)
data HeaderAtGenesis = HeaderAtGenesis (Header CardanoBlock) deriving (Show)


data ElectionValidationError
    = PraosValidationErr (PraosValidationErr Crypto)
    | EraMismatch EraMismatch
    deriving (Show)


infixr 6 :+
type (:+) = Either


makeEffect ''NodeToClient


validateVrfSignature
    :: ( Error AcquireFailure :> es
       , Error HeaderAtGenesis :> es
       , Error IntersectNotFound :> es
       , Error NoByronSupport :> es
       , Error NodeConnectionError :> es
       , Error PastHorizonException :> es
       , Error PointPastEpochHorizon :> es
       , NodeToClient :> es
       )
    => CardanoHeader -> Eff es (Either ElectionValidationError ())
validateVrfSignature =
    (=<<) leftToError
        . (=<<) leftToError
        . (=<<) leftToError
        . (=<<) leftToError
        . (=<<) leftToError
        . (=<<) leftToError
        . (=<<) leftToError
        . validateVrfSignature_


data Connection
    = Connection
    { localStateQueries :: InChan LocalStateQueryWithResultMVar
    , intersectRequests :: InChan (ChainPoint, MVar (Maybe ChainPoint))
    , dead :: MVar SomeException
    -- ^ used to signal that the connection died so queries can stop waiting for responses
    }


data LocalStateQueryWithResultMVar where
    LocalStateQueryWithResultMVar :: MVar (Either AcquireFailure result) -> Target C.ChainPoint -> QueryInMode result -> LocalStateQueryWithResultMVar


executeLocalStateQuery
    :: (Concurrent :> es, IOE :> es)
    => Connection
    -> Target C.ChainPoint
    -> QueryInMode result
    -> Eff es (Either NodeConnectionError (Either AcquireFailure result))
executeLocalStateQuery (Connection localStateQueries _ dead) target localStateQuery =
    do
        resultVar <- newEmptyMVar
        liftIO $ writeChan localStateQueries $ LocalStateQueryWithResultMVar resultVar target localStateQuery
        race (NodeConnectionError <$> readMVar dead) (readMVar resultVar)


makeIntersectRequest
    :: (Concurrent :> es, IOE :> es)
    => Connection
    -> ChainPoint
    -> Eff es (Either NodeConnectionError (Maybe ChainPoint))
makeIntersectRequest (Connection _ intersectRequests dead) chainPoint =
    do
        resultVar <- newEmptyMVar
        liftIO $ writeChan intersectRequests (chainPoint, resultVar)
        race (NodeConnectionError <$> readMVar dead) $ readMVar $ resultVar


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
    (c, newConnectionHandles) <- newConnection
    evalState (Right c) $ do
        _ <- initializeConnection newConnectionHandles
        interpretWith_
            (inject nodeToClient)
            ( \case
                ImmutableTip -> withSpan "node_to_client.immutable_tip" $ do
                    connection <- ensureConnection
                    (fmap . fmap) ChainPoint
                        $ (fmap . fmap) (fromRight $ error "`C.ImmutableTip` should never fail to be acquired.")
                        $ executeLocalStateQuery connection C.ImmutableTip
                        $ QueryChainPoint
                IsOnChain point -> withSpan "node_to_client.is_on_chain" $ do
                    connection <- ensureConnection
                    (fmap . fmap) isJust
                        $ makeIntersectRequest connection
                        $ point
                ValidateVrfSignature_ header -> withSpan "node_to_client.validate_vrf_signature"
                    $ runErrorNoCallStack
                    $ runErrorNoCallStack
                    $ runErrorNoCallStack
                    $ runErrorNoCallStack
                    $ runErrorNoCallStack
                    $ runErrorNoCallStack
                    $ runErrorNoCallStack
                    $ runErrorNoCallStack
                    $ do
                        connection <- ensureConnection
                        let chainPoint =
                                ChainPoint
                                    $ C.ChainPoint (blockSlot header)
                                    $ HeaderHash
                                    $ toShortRawHash (Proxy @CardanoBlock)
                                    $ headerHash
                                    $ header
                        intersect <-
                            (=<<) (maybe (throwError $ IntersectNotFound header $ chainPoint) pure)
                                $ (=<<) (leftToError @NodeConnectionError)
                                $ makeIntersectRequest connection
                                $ chainPoint
                        let target = SpecificPoint $ coerce $ intersect
                        if intersect == chainPoint then
                            pure () -- the block is already part of our cardano node's chain. so our cardano node considers it valid.
                        else do
                            eraHistory <- -- to do. cache
                                fmap (fromRight @_ @AcquireFailure $ error "`C.VolatileTip` should never fail to be acquired.")
                                    $ (=<<) (leftToError @NodeConnectionError)
                                    $ executeLocalStateQuery connection C.VolatileTip
                                    $ QueryEraHistory
                            let
                                toEpoch (ChainPoint p) =
                                    (fmap . fmap) (\(a, _, _) -> a)
                                        $ fmap (flip slotToEpoch eraHistory)
                                        $ maybe (throwError $ HeaderAtGenesis header) pure -- to do. can we just use slot number 0 for `ChainPointAtGenesis`?
                                        $ chainPointToSlotNo
                                        $ p
                            chainPointEpoch <- leftToError @PastHorizonException =<< toEpoch chainPoint -- to do. refresh cached `EraHistory`, that is `toEpoch`, on `PastHorizonException`s
                            intersectEpoch <- either (throwIO @PastHorizonException) pure =<< toEpoch intersect -- intersect should never be past the horizon because it is less than `VolatileTip`.
                            if intersectEpoch < chainPointEpoch then
                                throwError (PointPastEpochHorizon header intersect chainPoint) -- to do. attach the distance between intersect and the volatile tip which encodes the probability of the epoch horizon reaching the chain point, so the caller can decide if and when to retry.
                            else
                                if chainPointEpoch < intersectEpoch then
                                    error "a chain point should never be less than its intersect." -- to do. remove?
                                else case header of
                                    HeaderByron h -> throwError (NoByronSupport h)
                                    HeaderShelley (ShelleyHeader (BHeader bhbody _signed) _) ->
                                        validateVrfSignatureTPraos
                                            ShelleyBasedEraShelley
                                            connection
                                            target
                                            bhbody
                                    HeaderAllegra (ShelleyHeader (BHeader bhbody _signed) _) ->
                                        validateVrfSignatureTPraos
                                            ShelleyBasedEraAllegra
                                            connection
                                            target
                                            bhbody
                                    HeaderMary (ShelleyHeader (BHeader bhbody _signed) _) ->
                                        validateVrfSignatureTPraos
                                            ShelleyBasedEraMary
                                            connection
                                            target
                                            bhbody
                                    HeaderAlonzo (ShelleyHeader (BHeader bhbody _signed) _) ->
                                        validateVrfSignatureTPraos
                                            ShelleyBasedEraAlonzo
                                            connection
                                            target
                                            bhbody
                                    HeaderBabbage (ShelleyHeader shelleyHeaderRaw _) ->
                                        validateVrfSignaturePraos
                                            ShelleyBasedEraBabbage
                                            connection
                                            target
                                            (protocolHeaderView shelleyHeaderRaw)
                                    HeaderConway (ShelleyHeader shelleyHeaderRaw _) ->
                                        validateVrfSignaturePraos
                                            ShelleyBasedEraConway
                                            connection
                                            target
                                            (protocolHeaderView shelleyHeaderRaw)
                                    HeaderDijkstra (ShelleyHeader shelleyHeaderRaw _) ->
                                        validateVrfSignaturePraos
                                            ShelleyBasedEraDijkstra
                                            connection
                                            target
                                            (protocolHeaderView shelleyHeaderRaw)
            )


validateVrfSignaturePraos
    :: forall era es
     . ( Concurrent :> es
       , Consensus.PraosProtocolSupportsNode (ConsensusProtocol era)
       , Error AcquireFailure :> es
       , Error ElectionValidationError :> es
       , Error NodeConnectionError :> es
       , FromCBOR (ChainDepState (ConsensusProtocol era))
       , IOE :> es
       , Reader ShelleyConfig :> es
       )
    => ShelleyBasedEra era
    -> Connection
    -> Target C.ChainPoint
    -> HeaderView Crypto
    -> Eff es ()
validateVrfSignaturePraos era connection target headerView = do
    activeSlotsCoeff <- mkActiveSlotCoeff <$> asks @ShelleyConfig (.scConfig.sgActiveSlotsCoeff)
    epochNonce <-
        fmap (Consensus.epochNonce . Consensus.getPraosNonces (Proxy @(ConsensusProtocol era)))
            $ (=<<) (either (throwIO . snd) pure) -- `decodeProtocolState` should never fail to decode the protocol state, should it?
            $ fmap decodeProtocolState
            $ (=<<) leftToError
            $ (=<<) leftToError
            $ (=<<) leftToError
            $ (fmap . fmap . fmap . first) EraMismatch -- wrap `EraMismatch` into an `ElectionValidationError`
            $ executeLocalStateQuery connection target
            $ QueryInEra
            $ QueryInShelleyBasedEra (convert era)
            $ QueryProtocolState
    (=<<)
        ( \poolDistribution ->
            either throwError pure
                $ first PraosValidationErr -- wrap `PraosValidationErr` into an `ElectionValidationError`
                $ runExcept
                $ doValidateVRFSignature
                    epochNonce -- or `mkNonceFromOutputVRF $ certifiedOutput $ hvVrfRes $ headerView` or `slotToNonce`? however, in `doValidateVRFSignature`, this is not used for calling `checkLeaderNatValue`. so we could inline the definition of `doValidateVRFSignature`. we have to inline a variation of the definition of `doValidateVRFSignature` anyway for `TPraos`.
                    poolDistribution
                    activeSlotsCoeff
                    headerView
        )
        $ fmap (SL.unPoolDistr . unPoolDistr)
        $ (=<<) (either throwIO pure) -- `decodePoolDistribution` should never fail to decode the pool distribution, should it?
        $ fmap (decodePoolDistribution era)
        $ (=<<) leftToError
        $ (=<<) leftToError
        $ (=<<) leftToError
        $ (fmap . fmap . fmap . first) EraMismatch -- wrap `EraMismatch` into an `ElectionValidationError`
        $ executeLocalStateQuery connection target
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


validateVrfSignatureTPraos
    :: ( Concurrent :> es
       , Error AcquireFailure :> es
       , Error ElectionValidationError :> es
       , Error NodeConnectionError :> es
       , IOE :> es
       , Reader ShelleyConfig :> es
       , VRFAlgorithm (VRF c)
       )
    => ShelleyBasedEra era
    -> Connection
    -> Target C.ChainPoint
    -> BHBody c
    -> Eff es ()
validateVrfSignatureTPraos era connection target (BHBody {bheaderVk, bheaderL = CertifiedVRF {certifiedOutput = certVRF}}) = do
    let blockIssuer = coerceKeyRole (hashKey bheaderVk)
    activeSlotsCoeff <- mkActiveSlotCoeff <$> asks @ShelleyConfig (.scConfig.sgActiveSlotsCoeff)
    (=<<) leftToError
        $ (fmap . first)
            ( \sigma ->
                -- copied from https://github.com/IntersectMBO/cardano-ledger/blob/f3c4a2cacd829d002530b6884dfa42d0cc642dcb/libs/cardano-protocol-tpraos/src/Cardano/Protocol/TPraos/BHeader.hs#L335-L338
                let vrfLeaderVal = assertBoundedNatural certNatMax (VRF.getOutputVRFNatural certVRF)
                    certNatMax :: Natural
                    certNatMax = (2 :: Natural) ^ (8 * VRF.sizeOutputVRF certVRF)
                in  -- copied from https://github.com/IntersectMBO/ouroboros-consensus/blob/7908694b77ed52d51c098bec1ac39134a6c28c0c/ouroboros-consensus-protocol/src/ouroboros-consensus-protocol/Ouroboros/Consensus/Protocol/Praos.hs#L595
                    PraosValidationErr $ VRFLeaderValueTooBig (bvValue vrfLeaderVal) sigma activeSlotsCoeff
            )
        $ fmap
            ( \individualPoolStake ->
                if checkLeaderValue certVRF individualPoolStake activeSlotsCoeff then
                    Right ()
                else
                    Left individualPoolStake
            )
        $ fmap individualPoolStake
        $ (=<<) (maybe (throwError $ PraosValidationErr $ VRFKeyUnknown $ blockIssuer) pure)
        $ fmap (M.lookup blockIssuer)
        $ fmap (SL.unPoolDistr . unPoolDistr)
        $ (=<<) (either throwIO pure) -- `decodePoolDistribution` should never fail to decode the pool distribution, should it?
        $ fmap (decodePoolDistribution era)
        $ (=<<) leftToError
        $ (fmap . first) EraMismatch -- wrap `EraMismatch` into an `ElectionValidationError`
        $ (=<<) leftToError
        $ (=<<) leftToError
        $ executeLocalStateQuery connection target
        $ QueryInEra
        $ QueryInShelleyBasedEra (convert era)
        $ QueryPoolDistribution
        $ Just
        $ S.singleton
        $ StakePoolKeyHash
        $ blockIssuer


leftToError :: (Error e :> es, Show e) => Either e a -> Eff es a
leftToError = either throwError pure


newConnection
    :: (Concurrent :> es, IOE :> es)
    => Eff es (Connection, (OutChan LocalStateQueryWithResultMVar, OutChan (ChainPoint, MVar (Maybe ChainPoint)), MVar SomeException))
newConnection =
    do
        (localStateQueriesIn, localStateQueriesOut) <- liftIO newChan
        (intersectRequestsIn, intersectRequestsOut) <- liftIO newChan
        dead <- newEmptyMVar
        pure (Connection localStateQueriesIn intersectRequestsIn dead, (localStateQueriesOut, intersectRequestsOut, dead))


initializeConnection
    :: ( Conc :> es
       , Concurrent :> es
       , IOE :> es
       , Labeled "nodeToClient" WithSocket :> es
       , Log :> es
       , Reader (ProtocolInfo CardanoBlock) :> es
       , Reader ShelleyConfig :> es
       , State (Either SomeException Connection) :> es
       , Tracing :> es
       )
    => (OutChan LocalStateQueryWithResultMVar, OutChan (ChainPoint, MVar (Maybe ChainPoint)), MVar SomeException)
    -> Eff es (Thread ())
initializeConnection (localStateQueriesOut, intersectRequestsOut, dead) = withSpan "node_to_client.initialize_connection" $ do
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
            intersectRequestsOut


ensureConnection
    :: ( Conc :> es
       , Concurrent :> es
       , IOE :> es
       , Labeled "nodeToClient" WithSocket :> es
       , Log :> es
       , Reader (ProtocolInfo CardanoBlock) :> es
       , Reader ShelleyConfig :> es
       , State (Either SomeException Connection) :> es
       , Tracing :> es
       )
    => Eff es Connection
ensureConnection = withSpan "node_to_client.ensure_connection" $ do
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
    -> OutChan (ChainPoint, MVar (Maybe ChainPoint))
    -> IO ()
localNodeClient connectionInfo localStateQueries intersectRequests =
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
                        $ \r -> MVar.putMVar resultVar (Right r) $> Q.SendMsgRelease localStateQuery
                , recvMsgFailure = \e -> MVar.putMVar resultVar (Left e) *> localStateQuery
                }
    queryIsOnChain :: IO (S.ClientStIdle header C.ChainPoint tip IO void)
    queryIsOnChain = do
        (chainPoint, resultVar) <- readChan intersectRequests
        pure
            $ S.SendMsgFindIntersect [coerce chainPoint]
            $ S.ClientStIntersect
                { recvMsgIntersectFound =
                    \point _ ->
                        S.ChainSyncClient $ (MVar.putMVar resultVar $ Just $ ChainPoint $ point) *> queryIsOnChain
                , recvMsgIntersectNotFound = \_ -> S.ChainSyncClient $ MVar.putMVar resultVar Nothing *> queryIsOnChain
                }


-- | Convert a NetworkMagic to a NetworkId.
-- Mainnet has magic 764824073, everything else is treated as Testnet.
toNetworkId :: C.NetworkMagic -> NetworkId
toNetworkId magic@(C.NetworkMagic m)
    | m == 764824073 = Mainnet
    | otherwise = Testnet magic
