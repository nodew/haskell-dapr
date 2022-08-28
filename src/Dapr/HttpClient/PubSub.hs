{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dapr.HttpClient.PubSub where

import Dapr.HttpClient.Core
import Dapr.HttpClient.Internal
import Data.Aeson
import qualified Data.Text.Encoding as T
import Network.HTTP.Req
import RIO

data Subscrption = Subscrption
  { pubsubname :: Text,
    topic :: Text,
    route :: Text,
    metadata :: Metadata
  }
  deriving (Eq, Show, Generic)

instance ToJSON Subscrption

publishMessage ::
  (MonadIO m, HttpBody body) =>
  DaprClientConfig ->
  Text ->
  Text ->
  body ->
  Option 'Http ->
  Maybe Metadata ->
  m (Either DaprClientError ())
publishMessage config pubsubname topic message optionalHeader metadata = do
  let url = ["pubsubname", pubsubname, topic]
      metadataParam = mapMetadataToQueryParam metadata
      options = metadataParam <> optionalHeader
  response <- makeRequest config POST url message ignoreResponse options
  return $ bimap DaprHttpException (const ()) response

publishJsonMessage ::
  (MonadIO m, ToJSON a) =>
  DaprClientConfig ->
  Text ->
  Text ->
  a ->
  Maybe Metadata ->
  m (Either DaprClientError ())
publishJsonMessage config pubsubname topic message =
  publishMessage config pubsubname topic (ReqBodyJson message) (header "Content-Type" "application/json")

publishTextMessage ::
  MonadIO m =>
  DaprClientConfig ->
  Text ->
  Text ->
  Text ->
  Maybe Metadata ->
  m (Either DaprClientError ())
publishTextMessage config pubsubname topic message =
  publishMessage config pubsubname topic (ReqBodyBs (T.encodeUtf8 message)) (header "Content-Type" "text/plain")

publishCloudEvent ::
  (MonadIO m, ToJSON a) =>
  DaprClientConfig ->
  Text ->
  Text ->
  a ->
  Maybe Metadata ->
  m (Either DaprClientError ())
publishCloudEvent config pubsubname topic message =
  publishMessage config pubsubname topic (ReqBodyJson message) (header "Content-Type" "application/cloudevents+json")

subscribeMessage :: MonadIO m => DaprClientConfig -> [Subscrption] -> m (Either DaprClientError ())
subscribeMessage config subscrptions = do
  let url = ["darp", "subscribe"]
      options = header "Content-Type" "application/json"
  response <- makeRequest config POST url (ReqBodyJson subscrptions) ignoreResponse options
  return $ bimap DaprHttpException (const ()) response
