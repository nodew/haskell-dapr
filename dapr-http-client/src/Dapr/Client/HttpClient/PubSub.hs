-- |
-- Module      : Dapr.Client.HttpClient.PubSub
-- Description : Manages publishing of events to Dapr Publish subscribe service
-- Copyright   : (c)
-- License     : Apache-2.0
-- This module manages publishing of events to Dapr Publish subscribe service
module Dapr.Client.HttpClient.PubSub where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Internal
import Dapr.Client.HttpClient.Req
import Dapr.Core.Types
import Data.Aeson (ToJSON)
import Data.Bifunctor (bimap)
import Data.CaseInsensitive (CI (original))
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Req
import Network.HTTP.Types (hContentType)

-- | Publishes an event to specified topic
publishMessage ::
  ( MonadIO m,
    HttpBody payload
  ) =>
  DaprConfig ->
  PublishEventRequest payload ->
  m (Either DaprClientError ())
publishMessage config PublishEventRequest {..} = do
  let url = ["publish", getPubsubName pubsubName, getPubsubTopic pubsubTopic]
      metadataParam = mapMetadataToQueryParam pubsubMetadata
      options = metadataParam <> maybe mempty (header (original hContentType) . encodeUtf8) pubsubDataContentType
  response <- makeHttpRequest config POST url pubsubData ignoreResponse options
  return $ bimap DaprHttpException (const ()) response

publishMessageWithJsonPayload ::
  (MonadIO m, ToJSON a) =>
  DaprConfig ->
  PublishEventRequest a ->
  m (Either DaprClientError ())
publishMessageWithJsonPayload config PublishEventRequest {..} = do
  let updatedRequest = PublishEventRequest pubsubName pubsubTopic (ReqBodyJson pubsubData) (Just "application/json") pubsubMetadata
  publishMessage config updatedRequest
