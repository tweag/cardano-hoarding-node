module Hoard.Effects.NodeToClient
    ( runNodeToClient
    , immutableTip
    , isOnChain
    ) where

import Cardano.Api
    ( ChainPoint
    , LocalChainSyncClient (LocalChainSyncClient)
    , LocalNodeClientProtocols
        ( LocalNodeClientProtocols
        , localChainSyncClient
        , localStateQueryClient
        , localTxMonitoringClient
        , localTxSubmissionClient
        )
    , LocalNodeConnectInfo
    , QueryInMode (QueryChainPoint)
    , connectToLocalNode
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
import Effectful.TH (makeEffect)
import Hoard.Effects.Conc (Conc, fork_)
import Ouroboros.Network.Protocol.ChainSync.Client qualified as S
import Ouroboros.Network.Protocol.LocalStateQuery.Client qualified as Q


data NodeToClient :: Effect where
    ImmutableTip :: NodeToClient m ChainPoint
    IsOnChain :: ChainPoint -> NodeToClient m Bool


makeEffect ''NodeToClient


runNodeToClient :: (Conc :> es, IOE :> es) => LocalNodeConnectInfo -> Eff (NodeToClient : es) a -> Eff es a
runNodeToClient connectionInfo nodeToClient = do
    (immutableTipQueriesIn, immutableTipQueriesOut) <- liftIO newChan
    (isOnChainQueriesIn, isOnChainQueriesOut) <- liftIO newChan
    _ <- fork_ $ liftIO $ localNodeClient connectionInfo immutableTipQueriesOut isOnChainQueriesOut
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
    error "`connectToLocalNode` should run indefinitely because `localStateQueryClient` and `queryIsOnChain` return `void`."
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
