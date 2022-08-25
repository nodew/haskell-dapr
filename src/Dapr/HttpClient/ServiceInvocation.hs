{-# LANGUAGE DataKinds #-}

module Dapr.HttpClient.ServiceInvocation where

import Dapr.HttpClient.Core
import Data.Aeson
import Network.HTTP.Req
import RIO
import qualified Data.Text as T

invokeMethod ::
  ( HttpBodyAllowed (AllowsBody method) (ProvidesBody payload),
    MonadIO m,
    HttpMethod method,
    HttpBody payload
  ) =>
  DaprClientConfig ->
  Text ->
  method ->
  [Text] ->
  payload ->
  Option 'Http ->
  m (Either DaprClientError LbsResponse)
invokeMethod config appId httpMethod path payload options = do
  let url = ["invoke", appId, "method"] <> path
  response <- makeRequest config httpMethod url payload lbsResponse options
  return $ first DaprHttpException response

invokeMethodWithJsonPayload ::
  ( HttpBodyAllowed (AllowsBody method) 'CanHaveBody,
    MonadIO m,
    HttpMethod method,
    ToJSON a
  ) =>
  DaprClientConfig ->
  Text ->
  method ->
  [Text] ->
  a ->
  Option 'Http ->
  m (Either DaprClientError LbsResponse)
invokeMethodWithJsonPayload config appId httpMethod path payload options =
  invokeMethod
    config
    appId
    httpMethod
    path
    (ReqBodyJson payload)
    (options <> header "Content-Type" "application/json")

invokeMethodWithJsonPayload' ::
  ( HttpBodyAllowed (AllowsBody method) 'CanHaveBody,
    MonadIO m,
    HttpMethod method,
    ToJSON a,
    FromJSON b
  ) =>
  DaprClientConfig ->
  Text ->
  method ->
  [Text] ->
  a ->
  Option 'Http ->
  m (Either DaprClientError b)
invokeMethodWithJsonPayload' config appId httpMethod path payload options = do
  response <- invokeMethodWithJsonPayload
    config
    appId
    httpMethod
    path
    payload
    (options <> header "Accept" "application/json")
  case response of
    Left err -> return $ Left err
    Right body -> do
      case eitherDecode (responseBody body) of
        Left err -> return $ Left $ AesonDecodeError (T.pack err)
        Right result -> return $ Right result
