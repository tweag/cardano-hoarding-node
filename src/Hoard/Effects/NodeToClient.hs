{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-redundant-constraints #-}

module Hoard.Effects.NodeToClient
    ( NodeToClient
    , ConnectionError (..)
    , runNodeToClient
    , immutableTip
    , connect
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
    , NetworkId (Testnet)
    , NetworkMagic (NetworkMagic)
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
import Effectful.Dispatch.Dynamic (reinterpret_)
import Effectful.Reader.Static (Reader, ask)
import Effectful.TH (makeEffect)
import Ouroboros.Network.Protocol.ChainSync.Client qualified as S
import Ouroboros.Network.Protocol.LocalStateQuery.Client qualified as Q
import Prelude hiding (Reader, ask, evalState, get, put)

import Effectful.Exception (handleSync)
import Effectful.Labeled (Labeled, labeled)
import Effectful.State.Static.Shared (evalState, get, put)
import Hoard.Effects.Conc (Conc, fork)
import Hoard.Effects.Log (Log)
import Hoard.Effects.Log qualified as Log
import Hoard.Effects.WithSocket (WithSocket, getSocket)
import Hoard.Types.Environment (Config (..))


data NodeToClient connectionError :: Effect where -- to do. add operation to query state
    ImmutableTip :: NodeToClient connectionError m (Either connectionError ChainPoint)
    IsOnChain :: ChainPoint -> NodeToClient connectionError m (Either connectionError Bool)
    Connect :: NodeToClient connectionError m ()


makeEffect ''NodeToClient


data Connection = Connection (InChan (MVar ChainPoint)) (InChan (ChainPoint, MVar Bool))


data ConnectionError = UninitializedConnection | ConnectionException SomeException


runNodeToClient
    :: ( Labeled "nodeToClient" WithSocket :> es
       , Conc :> es
       , Log :> es
       , IOE :> es
       , Reader Config :> es
       )
    => Eff (NodeToClient ConnectionError : es) a
    -> Eff es a
runNodeToClient = do
    -- to do. use `Effectful.Concurrent.MVar`, `Hoard.Effects.Chan`,...
    reinterpret_
        (evalState @(Either ConnectionError Connection) (Left UninitializedConnection))
        ( \case
            ImmutableTip ->
                get >>= \case
                    Left e -> pure (Left e)
                    Right (Connection immutableTipQueries _) -> liftIO $ do
                        resultVar <- newEmptyMVar
                        writeChan immutableTipQueries resultVar
                        Right <$> readMVar resultVar -- to do. prevent dead lock
            IsOnChain point ->
                get >>= \case
                    Left e -> pure (Left e)
                    Right (Connection _ isOnChainQueries) -> liftIO $ do
                        resultVar <- newEmptyMVar
                        writeChan isOnChainQueries (point, resultVar)
                        Right <$> readMVar resultVar -- to do. prevent dead lock
            Connect -> do
                config <- ask
                (immutableTipQueriesIn, immutableTipQueriesOut) <- liftIO newChan
                (isOnChainQueriesIn, isOnChainQueriesOut) <- liftIO newChan
                epochSize <- loadEpochSize config
                nodeToClientSocket <- labeled @"nodeToClient" getSocket
                put (Right (Connection immutableTipQueriesIn isOnChainQueriesIn))
                void
                    . fork
                    . handleSync
                        ( \e -> do
                            Log.warn ("`connectToLocalNode` error. " <> toText (displayException e))
                            put $ Left $ ConnectionException $ e
                        )
                    . liftIO
                    $ localNodeClient
                        ( LocalNodeConnectInfo
                            { localConsensusModeParams = CardanoModeParams $ coerce $ epochSize
                            , localNodeNetworkId = Testnet $ NetworkMagic $ 1
                            , localNodeSocketPath = nodeToClientSocket
                            }
                        )
                        immutableTipQueriesOut
                        isOnChainQueriesOut
        )


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
