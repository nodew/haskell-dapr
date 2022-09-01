module Dapr.Client.HttpClient.Req where

import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO)
import Dapr.Common
import Data.Data (Proxy)
import Data.Text (Text)
import Network.HTTP.Req

data DaprClientError
  = DaprHttpException HttpException
  | JsonDecodeError Text
  | NotFound
  | UnknownError
  deriving (Show)

makeHttpRequest ::
  ( HttpBodyAllowed (AllowsBody method) (ProvidesBody body),
    MonadIO m,
    HttpMethod method,
    HttpBody body,
    HttpResponse response
  ) =>
  DaprConfig ->
  method ->
  [Text] ->
  body ->
  Proxy response ->
  Option 'Http ->
  m (Either HttpException response)
makeHttpRequest config method subUrl reqBody responseHandler options = runReq defaultHttpConfig $ do
  let host = daprHost config
      apiVersion = daprApiVersion config
      defaultOptions = port $ daprPort config
      updatedOptions = defaultOptions <> options
      completeUrl = appendUrl (http host /: apiVersion) subUrl
  try $ req method completeUrl reqBody responseHandler updatedOptions
  where
    appendUrl :: Url scheme -> [Text] -> Url scheme
    appendUrl prefix [] = prefix
    appendUrl prefix (x : xs) = appendUrl (prefix /: x) xs