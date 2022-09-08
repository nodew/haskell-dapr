module Dapr.Client.HttpClient.ServiceInvocation where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Req
import Dapr.Client.HttpClient.Types
import Data.Aeson
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Req

invokeMethod ::
  ( HttpBodyAllowed (AllowsBody method) (ProvidesBody payload),
    MonadIO m,
    HttpMethod method,
    HttpBody payload
  ) =>
  DaprConfig ->
  Text ->
  method ->
  [Text] ->
  payload ->
  Option 'Http ->
  m (Either DaprClientError LbsResponse)
invokeMethod config appId httpMethod path payload options = do
  let url = ["invoke", appId, "method"] <> path
  response <- makeHttpRequest config httpMethod url payload lbsResponse options
  return $ first DaprHttpException response

invokeMethod' ::
  ( HttpBodyAllowed (AllowsBody method) 'CanHaveBody,
    MonadIO m,
    HttpMethod method,
    ToJSON a,
    FromJSON b
  ) =>
  DaprConfig ->
  Text ->
  method ->
  [Text] ->
  a ->
  Option 'Http ->
  m (Either DaprClientError b)
invokeMethod' config appId httpMethod path payload options = do
  response <-
    invokeMethod
      config
      appId
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
