{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- |
-- Module      : PubSub
-- Description : Manages publishing of events to Dapr Publish subscribe service
-- Copyright   : (c)
-- License     : Apache-2.0
-- This module manages publishing of events to Dapr Publish subscribe service
module Dapr.Client.HttpClient.PubSub where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Internal
import Dapr.Client.HttpClient.Req
import Dapr.Client.HttpClient.Types
import Data.Aeson
import Data.Bifunctor (bimap)
import qualified Data.Text.Encoding as T
import Network.HTTP.Req

-- | Publishes an event to specified topic
publishMessage ::
  (MonadIO m, HttpBody body) =>
  DaprConfig ->
  PubSub ->
  Topic ->
  body ->
  Option 'Http ->
  Maybe RequestMetadata ->
  m (Either DaprClientError ())
publishMessage config pubsub topic message optionalHeader metadata = do
  let url = ["publish", getPubSubName pubsub, topicName topic]
      metadataParam = mapMetadataToQueryParam metadata
      options = metadataParam <> optionalHeader
  response <- makeHttpRequest config POST url message ignoreResponse options
  return $ bimap DaprHttpException (const ()) response

-- | Publishes an event to a specified topic
publishJsonMessage ::
  (MonadIO m, ToJSON a) =>
  DaprConfig ->
  PubSub ->
  Topic ->
  a ->
  Maybe RequestMetadata ->
  m (Either DaprClientError ())
publishJsonMessage config pubsub topic message =
  publishMessage config pubsub topic (ReqBodyJson message) mempty

-- | Publishes an event to a specified topic
publishTextMessage ::
  MonadIO m =>
  DaprConfig ->
  PubSub ->
  Topic ->
  TextMessage ->
  Maybe RequestMetadata ->
  m (Either DaprClientError ())
publishTextMessage config pubsub topic message =
  publishMessage config pubsub topic (ReqBodyBs (T.encodeUtf8 message)) (header "Content-Type" "text/plain")

-- | Publishes an event to a specified topic
publishCloudEvent ::
  (MonadIO m, ToJSON a) =>
  DaprConfig ->
  PubSub ->
  Topic ->
  a ->
  Maybe RequestMetadata ->
  m (Either DaprClientError ())
publishCloudEvent config pubsub topic message =
  publishMessage config pubsub topic (ReqBodyJson message) (header "Content-Type" "application/cloudevents+json")
