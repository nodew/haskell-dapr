{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dapr.Client.HttpClient.Configuration where

import Dapr.Client.HttpClient.Req
import Dapr.Common
import Network.HTTP.Req
import RIO

getConfiguration :: MonadIO m => DaprConfig -> Text -> [Text] -> m (Either DaprClientError [Configuration])
getConfiguration config store keys = do
  let url = ["configuration", store]
      params = keysToParams keys
  response <- makeHttpRequest config GET url NoReqBody jsonResponse params
  return $ bimap DaprHttpException responseBody response
  where
    keysToParams [] = mempty
    keysToParams (x : xs) = queryParam "key" (Just x) <> keysToParams xs

subscribeConfiguration :: MonadIO m => DaprConfig -> Text -> [Text] -> m (Either DaprClientError SubscribeConfigurationResponse)
subscribeConfiguration config store keys = do
  let url = ["configuration", store, "subscribe"]
      params = keysToParams keys
  response <- makeHttpRequest config GET url NoReqBody jsonResponse params
  return $ bimap DaprHttpException responseBody response
  where
    keysToParams [] = mempty
    keysToParams (x : xs) = queryParam "key" (Just x) <> keysToParams xs

unsubscribeConfiguration :: MonadIO m => DaprConfig -> Text -> m (Either DaprClientError ())
unsubscribeConfiguration config subscriptionId = do
  let url = ["configuration", subscriptionId, "unsubscribe"]
  response <- makeHttpRequest config GET url NoReqBody ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response
