{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dapr.HttpClient.Configuration where

import Dapr.HttpClient.Core
import Dapr.HttpClient.Internal
import Data.Aeson
import Network.HTTP.Req
import RIO

data Configuration = Configuration
  { configKey :: Text,
    configValue :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Configuration where
  parseJSON = customParseJSON 5

newtype SubscribeConfigurationResponse = ConfigurationResponse
  { subscriptionId :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON SubscribeConfigurationResponse where
  parseJSON = customParseJSON 12

getConfiguration :: MonadIO m => DaprClientConfig -> Text -> [Text] -> m (Either DaprClientError [Configuration])
getConfiguration config store keys = do
  let url = ["configuration", store]
      params = keysToParams keys
  response <- makeRequest config GET url NoReqBody jsonResponse params
  return $ bimap DaprHttpException responseBody response
  where
    keysToParams [] = mempty
    keysToParams (x : xs) = queryParam "key" (Just x) <> keysToParams xs

subscribeConfiguration :: MonadIO m => DaprClientConfig -> Text -> [Text] -> m (Either DaprClientError SubscribeConfigurationResponse)
subscribeConfiguration config store keys = do
  let url = ["configuration", store, "subscribe"]
      params = keysToParams keys
  response <- makeRequest config GET url NoReqBody jsonResponse params
  return $ bimap DaprHttpException responseBody response
  where
    keysToParams [] = mempty
    keysToParams (x : xs) = queryParam "key" (Just x) <> keysToParams xs

unsubscribeConfiguration :: MonadIO m => DaprClientConfig -> Text -> m (Either DaprClientError ())
unsubscribeConfiguration config subscriptionId = do
  let url = ["configuration", subscriptionId, "unsubscribe"]
  response <- makeRequest config GET url NoReqBody ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response
