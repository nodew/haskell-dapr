{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dapr.Client.HttpClient.Configuration where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Req
import Dapr.Client.HttpClient.Types
import Data.Bifunctor (bimap)
import Data.Text (Text)
import Network.HTTP.Req

getConfiguration :: MonadIO m => DaprConfig -> ConfigurationStore -> [Text] -> m (Either DaprClientError [Configuration])
getConfiguration config store keys = do
  let url = ["configuration", getConfigStoreName store]
      params = keysToParams keys
  response <- makeHttpRequest config GET url NoReqBody jsonResponse params
  return $ bimap DaprHttpException responseBody response
  where
    keysToParams [] = mempty
    keysToParams (x : xs) = queryParam "key" (Just x) <> keysToParams xs

subscribeConfiguration :: MonadIO m => DaprConfig -> ConfigurationStore -> [Text] -> m (Either DaprClientError SubscribeConfigurationResponse)
subscribeConfiguration config store keys = do
  let url = ["configuration", getConfigStoreName store, "subscribe"]
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
