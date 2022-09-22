-- |
-- Module      : Secrets
-- Description : Lets you retrieve secrets from a configured secrets store using Dapr secrets API.
-- Copyright   : (c)
-- License     : Apache-2.0
-- This module lets you retrieve secrets from a configured secrets store using Dapr secrets API.
module Dapr.Client.HttpClient.Secrets where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Internal
import Dapr.Client.HttpClient.Req
import Dapr.Core.Types
import Data.Bifunctor (bimap)
import Data.Map
import Data.Text
import Network.HTTP.Req

-- | Get the secret values for a given `GetSecretRequest` from the secret store.
getSecrets :: MonadIO m => DaprConfig -> GetSecretRequest -> m (Either DaprClientError Secrets)
getSecrets config GetSecretRequest {..} = do
  let url = ["secrets", getSecretStoreName secretStore, getSecretKey secretKey]
      options = mapMetadataToQueryParam secretMetadata
  response <- makeHttpRequest config GET url NoReqBody jsonResponse options
  return $ bimap DaprHttpException responseBody response

-- | Gets all secret values that the application is allowed to access from the secret store.
getBulkSecrets :: MonadIO m => DaprConfig -> GetBulkSecretRequest -> m (Either DaprClientError (Map Text Secrets))
getBulkSecrets config GetBulkSecretRequest {..} = do
  let url = ["secrets", getSecretStoreName secretStore, "bulk"]
      options = mapMetadataToQueryParam secretMetadata
  response <- makeHttpRequest config GET url NoReqBody jsonResponse options
  return $ bimap DaprHttpException responseBody response
