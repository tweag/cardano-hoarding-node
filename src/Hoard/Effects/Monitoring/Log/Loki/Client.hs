-- | HTTP client for Loki push API
module Hoard.Effects.Monitoring.Log.Loki.Client
    ( sendToLoki
    ) where

import Control.Exception (handle)
import Data.Aeson (encode)
import Network.HTTP.Client (Manager, Request, RequestBody (..), httpNoBody, method, parseRequest, requestBody, requestHeaders, responseStatus)
import Network.HTTP.Types.Status (statusCode)
import System.IO (hPutStrLn)

import Hoard.Effects.Monitoring.Log.Loki.Types (LokiPushRequest)


-- | Send a push request to Loki
--
-- This is a fire-and-forget operation that never throws exceptions
-- to the caller. Any errors are logged to stderr.
sendToLoki :: Manager -> Text -> LokiPushRequest -> IO ()
sendToLoki manager endpoint pushRequest =
    handle @SomeException logError $ do
        req <- buildRequest endpoint pushRequest
        resp <- httpNoBody req manager
        let status = statusCode $ responseStatus resp
        when (status >= 400) $
            hPutStrLn stderr $
                "[ERROR] Loki push failed with status: " <> show status
  where
    logError :: SomeException -> IO ()
    logError ex =
        hPutStrLn stderr $
            "[ERROR] Loki push error: " <> displayException ex


-- | Build HTTP request for Loki push API
buildRequest :: Text -> LokiPushRequest -> IO Request
buildRequest endpoint pushRequest = do
    baseReq <- parseRequest $ toString $ endpoint <> "/loki/api/v1/push"
    pure
        baseReq
            { method = "POST"
            , requestHeaders =
                [ ("Content-Type", "application/json")
                ]
            , requestBody = RequestBodyLBS $ encode pushRequest
            }
