-- |
-- Module      : Dapr.Client.HttpClient.PubSub
-- Description : Manages publishing of events to Dapr Publish subscribe service
-- Copyright   : (c)
-- License     : Apache-2.0
-- This module manages publishing of events to Dapr Publish subscribe service
module Dapr.Client.HttpClient.PubSub where

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
publishMessage :: (HttpBody payload) => PublishEventRequest payload -> DaprHttpClient (Either DaprClientError ())
publishMessage PublishEventRequest {..} = do
  let url = ["publish", getPubsubName pubsubName, getPubsubTopic pubsubTopic]
      metadataParam = mapMetadataToQueryParam pubsubMetadata
      options = metadataParam <> maybe mempty (header (original hContentType) . encodeUtf8) pubsubDataContentType
  response <- makeHttpRequest POST url pubsubData ignoreResponse options
  return $ bimap DaprHttpException (const ()) response

publishMessageWithJsonPayload :: (ToJSON a) => PublishEventRequest a -> DaprHttpClient (Either DaprClientError ())
publishMessageWithJsonPayload PublishEventRequest {..} = do
  let updatedRequest = PublishEventRequest pubsubName pubsubTopic (ReqBodyJson pubsubData) (Just "application/json") pubsubMetadata
  publishMessage updatedRequest
