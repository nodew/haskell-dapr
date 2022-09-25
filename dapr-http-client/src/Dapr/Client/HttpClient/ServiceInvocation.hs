-- |
-- Module      : Dapr.Client.HttpClient.ServiceInvocation
-- Description : Lets you perform service invocations
-- Copyright   : (c)
-- License     : Apache-2.0
-- This module lets you perform service invocations
module Dapr.Client.HttpClient.ServiceInvocation where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Internal
import Dapr.Client.HttpClient.Req
import Dapr.Core.Types
import Data.Aeson (ToJSON)
import Data.Bifunctor (bimap)
import Data.CaseInsensitive (CI (original))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Req
import Network.HTTP.Types (hContentType)

-- | Invoke a method on a remote dapr app
invokeServiceMethod ::
  ( HttpBodyAllowed
      (AllowsBody method)
      (ProvidesBody payload),
    MonadIO m,
    HttpMethod method,
    HttpBody payload
  ) =>
  DaprConfig ->
  InvokeServiceRequest method payload ->
  m (Either DaprClientError InvokeResponse)
invokeServiceMethod config InvokeServiceRequest {..} = do
  let url = ["invoke", getRemoteAppId remoteApp, "method"] <> requestEndpoint
      options = maybe mempty (header (original hContentType) . encodeUtf8) requestContentType <> mapQueryToParam requestQueryString
  response <- makeHttpRequest config httpMethod url reqeustData lbsResponse options
  return $ bimap DaprHttpException getInvokeResponse response
  where
    getInvokeResponse :: LbsResponse -> InvokeResponse
    getInvokeResponse response =
      let content = responseBody response
          contentType = responseHeader response (original hContentType)
       in InvokeResponse content (maybe "text/plain" decodeUtf8 contentType)

-- | Invoke a method on a remote dapr app with JSON payload
invokeServiceMethodWithJsonPayload ::
  ( HttpBodyAllowed
      (AllowsBody method)
      'CanHaveBody,
    MonadIO m,
    HttpMethod method,
    ToJSON payload
  ) =>
  DaprConfig ->
  InvokeServiceRequest method payload ->
  m (Either DaprClientError InvokeResponse)
invokeServiceMethodWithJsonPayload config (InvokeServiceRequest {..}) = do
  let updatedRequest = InvokeServiceRequest remoteApp httpMethod requestEndpoint (ReqBodyJson reqeustData) requestContentType requestQueryString
  invokeServiceMethod config updatedRequest
