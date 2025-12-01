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
    , withEffToIO
    , (:>)
    )
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.TH (makeEffect)
import Hoard.Effects.Conc (concStrat)
import Ki qualified as Ki
import Ouroboros.Network.Protocol.LocalStateQuery.Client qualified as Q


data NodeToClient :: Effect where
    ImmutableTip :: NodeToClient m ChainPoint


makeEffect ''NodeToClient


runNodeToClient :: (IOE :> es) => LocalNodeConnectInfo -> Eff (NodeToClient : es) a -> Eff es a
runNodeToClient connectionInfo nodeToClient = do
    (immutableTipQueriesIn, immutableTipQueriesOut) <- liftIO newChan
    withEffToIO concStrat $ \unlift ->
        liftIO $ Ki.scoped $ \scope -> do
            _ <- Ki.fork scope $ localNodeClient connectionInfo immutableTipQueriesOut
            unlift $
                interpret_
                    ( \case
                        ImmutableTip -> liftIO $ do
                            resultVar <- newEmptyMVar
                            writeChan immutableTipQueriesIn resultVar
                            readMVar resultVar
                    )
                    nodeToClient


localNodeClient :: LocalNodeConnectInfo -> OutChan (MVar ChainPoint) -> IO ()
localNodeClient connectionInfo immutableTipQueries = do
    connectToLocalNode
        connectionInfo
        LocalNodeClientProtocols
            { localChainSyncClient = NoLocalChainSyncClient
            , localStateQueryClient = Just (LocalStateQueryClient queryImmutableTip)
            , localTxSubmissionClient = Nothing
            , localTxMonitoringClient = Nothing
            }
  where
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
