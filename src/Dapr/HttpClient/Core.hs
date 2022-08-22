{-# LANGUAGE DataKinds #-}

module Dapr.HttpClient.Core where

import Network.HTTP.Req
import RIO
import Web.HttpApiData

data DaprClientConfig = DaprClientConfig
  { daprHost :: Text,
    daprPort :: Int,
    daprApiVersion :: Text
  }
  deriving (Show)

defaultDaprClientConfig :: DaprClientConfig
defaultDaprClientConfig =
  DaprClientConfig
    { daprHost = "localhost",
      daprPort = 3500,
      daprApiVersion = "v1.0"
    }

makeRequest ::
  ( HttpBodyAllowed (AllowsBody method) (ProvidesBody reqBody),
    MonadIO m,
    HttpMethod method,
    HttpBody reqBody,
    HttpResponse response,
    ToHttpApiData url
  ) =>
  DaprClientConfig ->
  method ->
  url ->
  reqBody ->
  Proxy response ->
  Option 'Http ->
  m response
makeRequest config method url reqBody responseHandler options = runReq defaultHttpConfig $ do
  let host = daprHost config
      apiVersion = daprApiVersion config
      defaultOptions = port $ daprPort config
      updatedOptions = defaultOptions <> options
      completeUrl = http host /: apiVersion /~ url
  req method completeUrl reqBody responseHandler updatedOptions
