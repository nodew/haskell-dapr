module Dapr.Client.HttpClient.Secrets where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Internal
import Dapr.Client.HttpClient.Req
import Dapr.Common
import Data.Bifunctor (bimap)
import Data.Text (Text)
import Network.HTTP.Req

getSecrets :: MonadIO m => DaprConfig -> Text -> Text -> Maybe RequestMetadata -> m (Either DaprClientError Secrets)
getSecrets config store name metadata' = do
  let url = ["secrets", store, name]
      options = mapMetadataToQueryParam metadata'
  response <- makeHttpRequest config GET url NoReqBody jsonResponse options
  return $ bimap DaprHttpException responseBody response

getBulkSecrets :: MonadIO m => DaprConfig -> Text -> Maybe RequestMetadata -> m (Either DaprClientError Secrets)
getBulkSecrets config store metadata' = do
  let url = ["secrets", store, "bulk"]
      options = mapMetadataToQueryParam metadata'
  response <- makeHttpRequest config GET url NoReqBody jsonResponse options
  return $ bimap DaprHttpException responseBody response