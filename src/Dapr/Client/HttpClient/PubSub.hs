{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dapr.Client.HttpClient.PubSub where

import Dapr.Client.HttpClient.Internal
import Dapr.Client.HttpClient.Req
import Dapr.Common
import Data.Aeson
import qualified Data.Text.Encoding as T
import Network.HTTP.Req
import RIO

publishMessage ::
  (MonadIO m, HttpBody body) =>
  DaprConfig ->
  Text ->
  Text ->
  body ->
  Option 'Http ->
  Maybe RequestMetadata ->
  m (Either DaprClientError ())
publishMessage config pubsubname topic message optionalHeader metadata = do
  let url = ["pubsubname", pubsubname, topic]
      metadataParam = mapMetadataToQueryParam metadata
      options = metadataParam <> optionalHeader
  response <- makeHttpRequest config POST url message ignoreResponse options
  return $ bimap DaprHttpException (const ()) response

publishJsonMessage ::
  (MonadIO m, ToJSON a) =>
  DaprConfig ->
  Text ->
  Text ->
  a ->
  Maybe RequestMetadata ->
  m (Either DaprClientError ())
publishJsonMessage config pubsubname topic message =
  publishMessage config pubsubname topic (ReqBodyJson message) (header "Content-Type" "application/json")

publishTextMessage ::
  MonadIO m =>
  DaprConfig ->
  Text ->
  Text ->
  Text ->
  Maybe RequestMetadata ->
  m (Either DaprClientError ())
publishTextMessage config pubsubname topic message =
  publishMessage config pubsubname topic (ReqBodyBs (T.encodeUtf8 message)) (header "Content-Type" "text/plain")

publishCloudEvent ::
  (MonadIO m, ToJSON a) =>
  DaprConfig ->
  Text ->
  Text ->
  a ->
  Maybe RequestMetadata ->
  m (Either DaprClientError ())
publishCloudEvent config pubsubname topic message =
  publishMessage config pubsubname topic (ReqBodyJson message) (header "Content-Type" "application/cloudevents+json")

subscribeMessage :: MonadIO m => DaprConfig -> [SubscriptionInfo] -> m (Either DaprClientError ())
subscribeMessage config subscriptions = do
  let url = ["dapr", "subscribe"]
      options = header "Content-Type" "application/json"
  response <- makeHttpRequest config POST url (ReqBodyJson subscriptions) ignoreResponse options
  return $ bimap DaprHttpException (const ()) response
