module Hoard.Effects.NodeToClient
    ( runNodeToClient
    , queryImmutableTip
    , NodeToClient (QueryImmutableTip)
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
    , Target (ImmutableTip)
    , connectToLocalNode
    )
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
    QueryImmutableTip :: NodeToClient m ChainPoint


makeEffect ''NodeToClient


runNodeToClient :: (IOE :> es) => LocalNodeConnectInfo -> Eff (NodeToClient : es) a -> Eff es a
runNodeToClient connectionInfo nodeToClient = do
    (queriesIn, queriesOut) <- liftIO newChan
    withEffToIO concStrat $ \unlift ->
        liftIO $ Ki.scoped $ \scope -> do
            _ <- Ki.fork scope $ localNodeClient connectionInfo queriesOut
            unlift $
                interpret_
                    ( \case
                        QueryImmutableTip -> liftIO $ do
                            resultVar <- newEmptyMVar
                            writeChan queriesIn resultVar
                            readMVar resultVar
                    )
                    nodeToClient


localNodeClient :: LocalNodeConnectInfo -> OutChan (MVar ChainPoint) -> IO ()
localNodeClient connectionInfo queries = do
    connectToLocalNode
        connectionInfo
        LocalNodeClientProtocols
            { localChainSyncClient = NoLocalChainSyncClient
            , localStateQueryClient = Just (LocalStateQueryClient singleQuery)
            , localTxSubmissionClient = Nothing
            , localTxMonitoringClient = Nothing
            }
  where
    singleQuery = do
        resultVar <- readChan queries
        pure
            . Q.SendMsgAcquire ImmutableTip
            $ Q.ClientStAcquiring
                { recvMsgAcquired =
                    pure
                        . Q.SendMsgQuery QueryChainPoint
                        . Q.ClientStQuerying
                        $ \result -> putMVar resultVar result $> Q.SendMsgRelease singleQuery
                , recvMsgFailure = error "`ImmutableTip` should never fail to be acquired."
                }
