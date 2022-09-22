-- |
-- Module      : Req
-- Copyright   : (c)
-- License     : Apache-2.0
module Dapr.Client.HttpClient.Req where

import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Types
import Data.Data (Proxy (Proxy))
import Data.Text (Text)
import Network.HTTP.Req

-- | Method to make http calls to public Dapr APIs
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
      defaultContentType = case httpMethodName (proxy method) of
        "POST" -> header "Content-Type" "application/json"
        "PUT" -> header "Content-Type" "application/json"
        _ -> mempty
      defaultOptions = port $ daprPort config
      updatedOptions = defaultContentType <> defaultOptions <> options
      completeUrl = appendUrl (http host /: apiVersion) subUrl
  try $ req method completeUrl reqBody responseHandler updatedOptions
  where
    appendUrl :: Url scheme -> [Text] -> Url scheme
    appendUrl prefix [] = prefix
    appendUrl prefix (x : xs) = appendUrl (prefix /: x) xs

    proxy :: a -> Proxy a
    proxy _ = Proxy
