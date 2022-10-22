-- |
-- Module      : Dapr.Client.HttpClient.Configuration
-- Description : Manage Configuration stores
-- Copyright   : (c)
-- License     : Apache-2.0
-- This module manages Configuration stores which can then be read by application instances on startup or notified of when changes occur. This allows for dynamic configuration.
module Dapr.Client.HttpClient.Configuration where

import Dapr.Client.HttpClient.Internal
import Dapr.Client.HttpClient.Req
import Dapr.Core.Types
import Dapr.Core.Types.Internal
import Data.Aeson (FromJSON (parseJSON))
import Data.Bifunctor (bimap)
import Data.Map (fromList)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Req

data ConfigurationItem' = ConfigurationItem'
  { configurationItemKey :: ConfigurationKey,
    configurationItemValue :: Text,
    configurationItemVersion :: Text,
    configurationItemMetadata :: ExtendedMetadata
  }
  deriving (Generic)

instance FromJSON ConfigurationItem' where
  parseJSON = customParseJSON 17

-- | Get a list of configuration items based on keys from the given statestore
getConfiguration :: GetConfigurationRequest -> DaprHttpClient (Either DaprClientError GetConfigurationResponse)
getConfiguration GetConfigurationRequest {..} = do
  let url = ["configuration", getConfigStoreName storeName]
      params = mapKeysToParam "key" keys
  response <- makeHttpRequest GET url NoReqBody jsonResponse params
  return $ bimap DaprHttpException (toGetConfigurationResponse . responseBody) response
  where
    toGetConfigurationResponse :: [ConfigurationItem'] -> GetConfigurationResponse
    toGetConfigurationResponse = GetConfigurationResponse . fromList . map (\ConfigurationItem' {..} -> (configurationItemKey, ConfigurationItem {..}))

-- | Subscribe to a configuration store for the specified keys and receive an updated value whenever the key is updated in the store
subscribeConfiguration :: SubscribeConfigurationRequest -> DaprHttpClient (Either DaprClientError SubscribeConfigurationResponse)
subscribeConfiguration SubscribeConfigurationRequest {..} = do
  let url = ["configuration", getConfigStoreName storeName, "subscribe"]
      params = mapKeysToParam "key" keys
  response <- makeHttpRequest GET url NoReqBody jsonResponse params
  return $ bimap DaprHttpException responseBody response

-- | Unsubscribe from a configuration store using given subscription Id
unsubscribeConfiguration :: SubscriptionId -> DaprHttpClient (Either DaprClientError ())
unsubscribeConfiguration subscriptionId = do
  let url = ["configuration", getSubscriptionId subscriptionId, "unsubscribe"]
  response <- makeHttpRequest GET url NoReqBody ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response
