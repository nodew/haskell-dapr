-- |
-- Module      : Configuration
-- Description : Manage Configuration stores
-- Copyright   : (c)
-- License     : Apache-2.0
-- This module manages Configuration stores which can then be read by application instances on startup or notified of when changes occur. This allows for dynamic configuration.
module Dapr.Client.HttpClient.Configuration where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Req
import Dapr.Core.Types
import Data.Bifunctor (bimap)
import Data.Text (Text)
import Network.HTTP.Req

-- | Get a list of configuration items based on keys from the given statestore
getConfiguration :: MonadIO m => DaprConfig -> ConfigurationStore -> [Text] -> m (Either DaprClientError GetConfigurationResponse)
getConfiguration config store keys = do
  let url = ["configuration", getConfigStoreName store]
      params = keysToParams keys
  response <- makeHttpRequest config GET url NoReqBody jsonResponse params
  return $ bimap DaprHttpException responseBody response
  where
    keysToParams [] = mempty
    keysToParams (x : xs) = queryParam "key" (Just x) <> keysToParams xs

-- | Subscribe to a configuration store for the specified keys and receive an updated value whenever the key is updated in the store
subscribeConfiguration :: MonadIO m => DaprConfig -> ConfigurationStore -> [Text] -> m (Either DaprClientError SubscribeConfigurationResponse)
subscribeConfiguration config store keys = do
  let url = ["configuration", getConfigStoreName store, "subscribe"]
      params = keysToParams keys
  response <- makeHttpRequest config GET url NoReqBody jsonResponse params
  return $ bimap DaprHttpException responseBody response
  where
    keysToParams [] = mempty
    keysToParams (x : xs) = queryParam "key" (Just x) <> keysToParams xs

-- | Unsubscribe from a configuration store using the specified Id
unsubscribeConfiguration :: MonadIO m => DaprConfig -> Text -> m (Either DaprClientError ())
unsubscribeConfiguration config subscriptionId = do
  let url = ["configuration", subscriptionId, "unsubscribe"]
  response <- makeHttpRequest config GET url NoReqBody ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response
