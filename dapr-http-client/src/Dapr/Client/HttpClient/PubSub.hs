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
import Dapr.Core.Types
import Data.Aeson (ToJSON)
import Data.Bifunctor (bimap)
import Network.HTTP.Req

-- | Publishes an event to specified topic
publishMessage ::
  (MonadIO m, ToJSON message) =>
  DaprConfig ->
  PublishEventRequest message ->
  m (Either DaprClientError ())
publishMessage config PublishEventRequest {..} = do
  let url = ["publish", getPubsubName pubsubName, getPubsubTopic pubsubTopic]
      metadataParam = mapMetadataToQueryParam pubsubMetadata
      options = metadataParam
  response <- makeHttpRequest config POST url (ReqBodyJson pubsubData) ignoreResponse options
  return $ bimap DaprHttpException (const ()) response
