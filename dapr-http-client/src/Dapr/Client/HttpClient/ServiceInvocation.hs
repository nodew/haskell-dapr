-- |
-- Module      : ServiceInvocation
-- Description : Lets you perform service invocations
-- Copyright   : (c)
-- License     : Apache-2.0
-- This module lets you perform service invocations
module Dapr.Client.HttpClient.ServiceInvocation where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Req
import Dapr.Core.Types
import Data.Aeson (FromJSON, eitherDecode)
import Data.Bifunctor (bimap)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Req

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
  let url = ["invoke", getRemoteAppId remoteApp, "method"] <> requestMethod
      options = maybe mempty (header "ContentType" . encodeUtf8) requestContentType
  response <- makeHttpRequest config httpMethod url reqeustData lbsResponse options
  return $ bimap DaprHttpException getInvokeResponse response
  where
    getInvokeResponse :: LbsResponse -> InvokeResponse
    getInvokeResponse response =
      let content = responseBody response
          contentType = responseHeader response "Content-Type"
       in InvokeResponse content (maybe "text/plain" decodeUtf8 contentType)

-- | Invoke a method on a remote dapr app, and it will try to decode the response if it's JSON
invokeServiceMethod' ::
  ( HttpBodyAllowed
      (AllowsBody method)
      (ProvidesBody payload),
    MonadIO m,
    HttpMethod method,
    HttpBody payload,
    FromJSON response
  ) =>
  DaprConfig ->
  InvokeServiceRequest method payload ->
  m (Either DaprClientError response)
invokeServiceMethod' config request = do
  response <- invokeServiceMethod config request
  return $ case response of
    Left err -> Left err
    Right (InvokeResponse content contentType) ->
      if contentType == "application/json"
        then case eitherDecode content of
          Left err' -> Left $ JsonDecodeError (T.pack err')
          Right result -> Right result
        else Left $ JsonDecodeError "Content-Type is not json"
