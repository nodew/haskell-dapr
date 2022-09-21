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
import Dapr.Client.HttpClient.Types
import Data.Bifunctor (bimap)
import Network.HTTP.Req

-- | Get the secret values for a given `SecretName` from the secret store.
getSecrets :: MonadIO m => DaprConfig -> SecretStore -> SecretName -> Maybe RequestMetadata -> m (Either DaprClientError Secrets)
getSecrets config store name metadata' = do
  let url = ["secrets", getSecretStoreName store, name]
      options = mapMetadataToQueryParam metadata'
  response <- makeHttpRequest config GET url NoReqBody jsonResponse options
  return $ bimap DaprHttpException responseBody response

-- | Gets all secret values that the application is allowed to access from the secret store.
getBulkSecrets :: MonadIO m => DaprConfig -> SecretStore -> Maybe RequestMetadata -> m (Either DaprClientError Secrets)
getBulkSecrets config store metadata' = do
  let url = ["secrets", getSecretStoreName store, "bulk"]
      options = mapMetadataToQueryParam metadata'
  response <- makeHttpRequest config GET url NoReqBody jsonResponse options
  return $ bimap DaprHttpException responseBody response
