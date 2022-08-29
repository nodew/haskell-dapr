module Dapr.Client.HttpClient.Core where

import Network.HTTP.Req
import RIO

data DaprClientConfig = DaprClientConfig
  { daprHost :: Text,
    daprPort :: Int,
    daprApiVersion :: Text
  }
  deriving (Show)

data DaprClientError
  = DaprHttpException HttpException
  | AesonDecodeError Text
  | NotFound
  | UnknownError
  deriving (Show)

type RequestMetadata = Map Text Text

defaultDaprClientConfig :: DaprClientConfig
defaultDaprClientConfig =
  DaprClientConfig
    { daprHost = "localhost",
      daprPort = 3500,
      daprApiVersion = "v1.0"
    }

makeRequest ::
  ( HttpBodyAllowed (AllowsBody method) (ProvidesBody body),
    MonadIO m,
    HttpMethod method,
    HttpBody body,
    HttpResponse a
  ) =>
  DaprClientConfig ->
  method ->
  [Text] ->
  body ->
  Proxy a ->
  Option 'Http ->
  m (Either HttpException a)
makeRequest config method subUrl reqBody responseHandler options = runReq defaultHttpConfig $ do
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
