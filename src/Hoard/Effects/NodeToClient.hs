{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-redundant-constraints #-}

module Hoard.Effects.NodeToClient
    ( NodeToClient
    , runNodeToClient
    , immutableTip
    , isOnChain
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
    ( OutChan
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
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.Reader.Static (Reader, ask)
import Effectful.TH (makeEffect)
import Ouroboros.Consensus.Config (configBlock)
import Ouroboros.Consensus.Config.SupportsNode (getNetworkMagic)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import Ouroboros.Network.Protocol.ChainSync.Client qualified as S
import Ouroboros.Network.Protocol.LocalStateQuery.Client qualified as Q
import Prelude hiding (Reader, ask)

import Effectful.Labeled (Labeled, labeled)
import Hoard.Control.Exception (withExceptionLogging)
import Hoard.Effects.Conc (Conc, fork_)
import Hoard.Effects.Log (Log)
import Hoard.Effects.WithSocket (WithSocket, getSocket)
import Hoard.Types.Environment (Config (..))


data NodeToClient :: Effect where
    ImmutableTip :: NodeToClient m ChainPoint
    IsOnChain :: ChainPoint -> NodeToClient m Bool


makeEffect ''NodeToClient


runNodeToClient
    :: ( Labeled "nodeToClient" WithSocket :> es
       , Conc :> es
       , Log :> es
       , IOE :> es
       , Reader Config :> es
       )
    => Eff (NodeToClient : es) a
    -> Eff es a
runNodeToClient nodeToClient = do
    config <- ask
    (immutableTipQueriesIn, immutableTipQueriesOut) <- liftIO newChan
    (isOnChainQueriesIn, isOnChainQueriesOut) <- liftIO newChan
    epochSize <- loadEpochSize config
    nodeToClientSocket <- labeled @"nodeToClient" getSocket
    let networkMagic = getNetworkMagic (configBlock (pInfoConfig config.protocolInfo))
        networkId = toNetworkId networkMagic
    _ <-
        withExceptionLogging "NodeToClient"
            . fork_
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
    interpret_
        ( \case
            ImmutableTip -> liftIO $ do
                resultVar <- newEmptyMVar
                writeChan immutableTipQueriesIn resultVar
                readMVar resultVar
            IsOnChain point -> liftIO $ do
                resultVar <- newEmptyMVar
                writeChan isOnChainQueriesIn (point, resultVar)
                readMVar resultVar
        )
        nodeToClient


localNodeClient :: LocalNodeConnectInfo -> OutChan (MVar ChainPoint) -> OutChan (ChainPoint, MVar Bool) -> IO Void
localNodeClient connectionInfo immutableTipQueries isOnChainQueries =
    error "localNodeClient should never return"
        <$> connectToLocalNode
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
                        $ \result -> putMVar resultVar result $> Q.SendMsgRelease queryImmutableTip
                , recvMsgFailure = error "`ImmutableTip` should never fail to be acquired."
                }
    queryIsOnChain :: IO (S.ClientStIdle header ChainPoint tip IO void)
    queryIsOnChain = do
        (point, resultVar) <- readChan isOnChainQueries
        pure $
            S.SendMsgFindIntersect [point] $
                S.ClientStIntersect
                    { recvMsgIntersectFound = \_ _ -> S.ChainSyncClient $ putMVar resultVar True *> queryIsOnChain
                    , recvMsgIntersectNotFound = \_ -> S.ChainSyncClient $ putMVar resultVar False *> queryIsOnChain
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
