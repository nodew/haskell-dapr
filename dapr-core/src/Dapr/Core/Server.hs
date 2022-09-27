-- |
-- Module      : Dapr.Core.Server
-- Description : Core type definitions
-- Copyright   : (c)
-- License     : Apache-2.0
-- This module export
module Dapr.Core.Server where

import Dapr.Core.Types.Common
import Dapr.Core.Types.Pubsub
import qualified Data.ByteString.Lazy as L
import Data.Text

-- | TopicEvent is the content of the inbound topic message.
data TopicEvent = TopicEvent
  { -- | ID identifies the event.
    topicEventId :: Text,
    -- | The version of the CloudEvents specification.
    topicEventSpecVersion :: Text,
    -- | The type of event related to the originating occurrence.
    topicEventType :: Text,
    -- | Source identifies the context in which an event happened.
    topicEventSource :: Text,
    -- | The content type of data value.
    topicEventDataContentType :: Text,
    -- | The content of the event.
    topicEventData :: L.ByteString,
    -- | The base64 encoding content of the event.
    -- | Note, this is processing rawPayload and binary content types.
    -- | This field is deprecated and will be removed in the future.
    topicEventDataBase64 :: Text,
    -- | The pubsub topic which publisher sent to.
    topicEventTopic :: PubsubTopic,
    -- | The name of the pubsub the publisher sent to.
    topicEventPubsubName :: PubsubName
  }

-- | Subscription represents single topic subscription.
data Subscription = Subscription
  { -- | The name of the pub/sub this message came from
    subscriptionName :: PubsubName,
    -- | The name of the topic
    subscriptionTopic :: PubsubTopic,
    -- | The subscription metadata
    subscriptionMetadata :: ExtendedMetadata,
    -- | Route is the route of the handler where HTTP topic events should be published (passed as Path in gRPC)
    subscriptionRoute :: Text,
    -- | The CEL expression to match on the CloudEvent envelope.
    subscriptionMatch :: Text,
    -- | The priority in which to evaluate the match (lower to higher).
    subscriptionPriority :: Int,
    -- | DisableTopicValidation allows to receive events from publisher topics that differ from the subscribed topic.
    subscriptionDisableTopicValidation :: Bool
  }

-- | BindingEvent represents the binding event handler input.
data BindingEvent = BindingEvent
  { -- | Data from the input bindings
    bindingEventData :: L.ByteString,
    -- | The input binding metadata
    bindingEventMetadata :: ExtendedMetadata
  }
