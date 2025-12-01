module Hoard.Effects.NodeToClient
    ( runNodeToClient
    , immutableTip
    ) where

import Cardano.Api
    ( ChainPoint
    , LocalChainSyncClient (NoLocalChainSyncClient)
    , LocalNodeClientProtocols
        ( LocalNodeClientProtocols
        , localChainSyncClient
        , localStateQueryClient
        , localTxMonitoringClient
        , localTxSubmissionClient
        )
    , LocalNodeConnectInfo
    , LocalStateQueryClient (LocalStateQueryClient)
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
import Ouroboros.Network.Protocol.LocalStateQuery.Client qualified as Q


data NodeToClient :: Effect where
    ImmutableTip :: NodeToClient m ChainPoint


makeEffect ''NodeToClient


runNodeToClient :: (Conc :> es, IOE :> es) => LocalNodeConnectInfo -> Eff (NodeToClient : es) a -> Eff es a
runNodeToClient connectionInfo nodeToClient = do
    (immutableTipQueriesIn, immutableTipQueriesOut) <- liftIO newChan
    _ <- fork_ $ liftIO $ localNodeClient connectionInfo immutableTipQueriesOut
    interpret_
        ( \case
            ImmutableTip -> liftIO $ do
                resultVar <- newEmptyMVar
                writeChan immutableTipQueriesIn resultVar
                readMVar resultVar
        )
        nodeToClient


localNodeClient :: LocalNodeConnectInfo -> OutChan (MVar ChainPoint) -> IO Void
localNodeClient connectionInfo immutableTipQueries =
    error "`connectToLocalNode` should run indefinitely because `localStateQueryClient` returns `void`."
        <$> connectToLocalNode
            connectionInfo
            LocalNodeClientProtocols
                { localChainSyncClient = NoLocalChainSyncClient
                , localStateQueryClient = Just (LocalStateQueryClient queryImmutableTip)
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
