{-# LANGUAGE DataKinds #-}

module Dapr.HttpClient.Core where

import Network.HTTP.Req
import RIO

data DaprClientConfig = DaprClientConfig
  { daprHost :: Text,
    daprPort :: Int,
    daprApiVersion :: Text
  }
  deriving (Show)

data DaprClientErrorType = HttpException Int | AesonDecodeError | UnknownError
  deriving (Show, Eq)

data DaprClientError = DaprClientError
  { daprClientErrorType :: DaprClientErrorType,
    daprClientErrorMessage :: Text
  }
  deriving (Show, Eq)

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
    HttpResponse response
  ) =>
  DaprClientConfig ->
  method ->
  [Text] ->
  reqBody ->
  Proxy response ->
  Option 'Http ->
  m response
makeRequest config method subUrl reqBody responseHandler options = runReq defaultHttpConfig $ do
  let host = daprHost config
      apiVersion = daprApiVersion config
      defaultOptions = port $ daprPort config
      updatedOptions = defaultOptions <> options
      completeUrl = appendUrl (http host /: apiVersion) subUrl
  req method completeUrl reqBody responseHandler updatedOptions

  where
    appendUrl :: Url scheme -> [Text] -> Url scheme
    appendUrl prefix [] = prefix
    appendUrl prefix (x:xs) = appendUrl (prefix /: x) xs
