-- |
-- Module      : Dapr.Client.HttpClient.Secrets
-- Description : Lets you retrieve secrets from a configured secrets store using Dapr secrets API.
-- Copyright   : (c)
-- License     : Apache-2.0
-- This module lets you retrieve secrets from a configured secrets store using Dapr secrets API.
module Dapr.Client.HttpClient.Secrets where

import Dapr.Client.HttpClient.Internal
import Dapr.Client.HttpClient.Req
import Dapr.Core.Types
import Data.Bifunctor (bimap)
import Network.HTTP.Req

-- | Get the secret values for a given `GetSecretRequest` from the secret store.
getSecrets :: GetSecretRequest -> DaprHttpClient (Either DaprClientError GetSecretResponse)
getSecrets GetSecretRequest {..} = do
  let url = ["secrets", getSecretStoreName secretStore, getSecretKey secretKey]
      options = mapMetadataToQueryParam secretMetadata
  response <- makeHttpRequest GET url NoReqBody jsonResponse options
  return $ bimap DaprHttpException (GetSecretResponse . responseBody) response

-- | Gets all secret values that the application is allowed to access from the secret store.
getBulkSecrets :: GetBulkSecretRequest -> DaprHttpClient (Either DaprClientError GetBulkSecretResponse)
getBulkSecrets GetBulkSecretRequest {..} = do
  let url = ["secrets", getSecretStoreName secretStore, "bulk"]
      options = mapMetadataToQueryParam secretMetadata
  response <- makeHttpRequest GET url NoReqBody jsonResponse options
  return $ bimap DaprHttpException (GetBulkSecretResponse . responseBody) response
