-- |
-- Module      : Dapr.Core.Server
-- Description : Core core type definitions for server module
-- Copyright   : (c)
-- License     : Apache-2.0
-- This module export
module Dapr.Core.Server where

import Dapr.Core.Types.Common
import Dapr.Core.Types.Internal
import Dapr.Core.Types.Pubsub
import Data.Aeson
import qualified Data.ByteString.Lazy as L
import Data.Text
import qualified Data.Text as T
import GHC.Generics (Generic)

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
    -- | The pubsub topic which publisher sent to.
    topicEventTopic :: PubsubTopic,
    -- | The name of the pubsub the publisher sent to.
    topicEventPubsubName :: PubsubName
  }

-- | Subscription represents single topic subscription.
data Subscription = Subscription
  { -- | The name of the pub/sub this message came from
    subscriptionPubsubname :: PubsubName,
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
  deriving (Show, Eq, Generic)

instance ToJSON Subscription where
  toJSON = customToJSON 12

newtype EventHandler m event result = EventHandler {
  handle :: event -> m result
}

-- | BindingEvent represents the binding event handler input.
data BindingEvent = BindingEvent
  { -- | Data from the input bindings
    bindingEventData :: L.ByteString,
    -- | The input binding metadata
    bindingEventMetadata :: ExtendedMetadata
  }

-- | Handler for input binding
type BindingEventHandler m = EventHandler m BindingEvent Bool

data TopicEventResult = TopicEventSuccess | TopicEventRetry | TopicEventDrop
  deriving (Eq)

instance Show TopicEventResult where
  show TopicEventSuccess = "SUCCESS"
  show TopicEventRetry = "RETRY"
  show TopicEventDrop = "DROP"

instance ToJSON TopicEventResult where
  toJSON = Data.Aeson.String . T.pack . show

-- | Handler for subscribed topic
type TopicEventHandler m = EventHandler m TopicEvent TopicEventResult

data DaprServerConfig m = DaprServerConfig
  { subscriptions :: [(Subscription, TopicEventHandler m)],
    inputBindings :: [(Text, BindingEventHandler m)]
  }
