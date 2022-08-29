module Dapr.Client.HttpClient.Secrets where

import Dapr.Client.HttpClient.Core
import Dapr.Client.HttpClient.Internal
import Network.HTTP.Req
import RIO

type Secrets = Map Text Text

type BulkSecrets = Map Text Secrets

getSecrets :: MonadIO m => DaprClientConfig -> Text -> Text -> Maybe RequestMetadata -> m (Either DaprClientError Secrets)
getSecrets config store name metadata = do
  let url = ["secrets", store, name]
      options = mapMetadataToQueryParam metadata
  response <- makeRequest config GET url NoReqBody jsonResponse options
  return $ bimap DaprHttpException responseBody response

getBulkSecrets :: MonadIO m => DaprClientConfig -> Text -> Maybe RequestMetadata -> m (Either DaprClientError Secrets)
getBulkSecrets config store metadata = do
  let url = ["secrets", store, "bulk"]
      options = mapMetadataToQueryParam metadata
  response <- makeRequest config GET url NoReqBody jsonResponse options
  return $ bimap DaprHttpException responseBody response
