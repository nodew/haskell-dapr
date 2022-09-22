-- |
-- Module      : ServiceInvocation
-- Description : Lets you perform service invocations
-- Copyright   : (c)
-- License     : Apache-2.0
-- This module lets you perform service invocations
module Dapr.Client.HttpClient.ServiceInvocation where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Req
import Dapr.Client.HttpClient.Types
import Data.Aeson
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Req

-- | Perform service invocation using the request provided by payload with specified `HttpMethod`. The response will be returned without performing any validation on status code.
invokeMethod ::
  ( HttpBodyAllowed (AllowsBody method) (ProvidesBody payload),
    MonadIO m,
    HttpMethod method,
    HttpBody payload
  ) =>
  DaprConfig ->
  DaprHostApp ->
  method ->
  [Text] ->
  payload ->
  Option 'Http ->
  m (Either DaprClientError LbsResponse)
invokeMethod config app httpMethod path payload options = do
  let url = ["invoke", getId app, "method"] <> path
  response <- makeHttpRequest config httpMethod url payload lbsResponse options
  return $ first DaprHttpException response

-- |  Perform service invocation using the provided request payload with specified `HttpMethod`. If the response has a success status code the JSON body will be validated.
invokeMethod' ::
  ( HttpBodyAllowed (AllowsBody method) 'CanHaveBody,
    MonadIO m,
    HttpMethod method,
    ToJSON a,
    FromJSON b
  ) =>
  DaprConfig ->
  DaprHostApp ->
  method ->
  [Text] ->
  a ->
  Option 'Http ->
  m (Either DaprClientError b)
invokeMethod' config app httpMethod path payload options = do
  response <-
    invokeMethod
      config
      app
      httpMethod
      path
      (ReqBodyJson payload)
      (options <> header "Accept" "application/json")
  case response of
    Left err -> return $ Left err
    Right body -> do
      case eitherDecode (responseBody body) of
        Left err -> return $ Left $ JsonDecodeError (T.pack err)
        Right result -> return $ Right result
