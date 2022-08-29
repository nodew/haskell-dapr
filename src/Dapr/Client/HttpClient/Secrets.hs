module Dapr.Client.HttpClient.Secrets where

import Dapr.Common
import Dapr.Client.HttpClient.Req
import Dapr.Client.HttpClient.Internal
import Network.HTTP.Req
import RIO

getSecrets :: MonadIO m => DaprConfig -> Text -> Text -> Maybe RequestMetadata -> m (Either DaprClientError Secrets)
getSecrets config store name metadata = do
  let url = ["secrets", store, name]
      options = mapMetadataToQueryParam metadata
  response <- makeHttpRequest config GET url NoReqBody jsonResponse options
  return $ bimap DaprHttpException responseBody response

getBulkSecrets :: MonadIO m => DaprConfig -> Text -> Maybe RequestMetadata -> m (Either DaprClientError Secrets)
getBulkSecrets config store metadata = do
  let url = ["secrets", store, "bulk"]
      options = mapMetadataToQueryParam metadata
  response <- makeHttpRequest config GET url NoReqBody jsonResponse options
  return $ bimap DaprHttpException responseBody response
