-- |
-- Module      : Dapr.Client.HttpClient.Req
-- Description : Make http calls to public Dapr APIs
-- Copyright   : (c)
-- License     : Apache-2.0
-- This module provides base method to call public Dapr APIs
module Dapr.Client.HttpClient.Req where

import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Internal
import Dapr.Core.Types
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
        "POST" -> headerContentTypeJSON
        "PUT" -> headerContentTypeJSON
        _ -> mempty
      defaultOptions = defaultContentType <> port (daprPort config)
      updatedOptions = defaultOptions <> options
      completeUrl = appendUrl (http host /: apiVersion) subUrl
  try $ req method completeUrl reqBody responseHandler updatedOptions
  where
    appendUrl :: Url scheme -> [Text] -> Url scheme
    appendUrl prefix [] = prefix
    appendUrl prefix (x : xs) = appendUrl (prefix /: x) xs

    proxy :: a -> Proxy a
    proxy _ = Proxy
