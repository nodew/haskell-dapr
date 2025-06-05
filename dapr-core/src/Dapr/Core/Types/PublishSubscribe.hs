{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Dapr.Core.Types.PublishSubscribe
-- Description : Defines the types used by PubSub module
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines the types used by PubSub module.
module Dapr.Core.Types.PublishSubscribe where

import Dapr.Core.Types.Common (ExtendedMetadata, PubsubSubscriptionType)
import Dapr.Core.Types.Internal (customParseJSON, customToJSON)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

-- | 'PubsubName' represents the name of pubsub component.
newtype PubsubName = PubsubName {getPubsubName :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | 'PubsubTopic' represents the pubsub topic
newtype PubsubTopic = PubsubTopic {getPubsubTopic :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | 'EntryId' represents the request scoped unique ID for bulk publish entries
newtype EntryId = EntryId {getEntryId :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | 'PublishEventRequest' is the message to publish event data to pubsub topic
data PublishEventRequest a = PublishEventRequest
  { -- | The name of the pubsub component
    pubsubName :: PubsubName,
    -- | The pubsub topic
    pubsubTopic :: PubsubTopic,
    -- | The data which will be published to topic
    pubsubData :: a,
    -- | The content type for the data (optional)
    pubsubDataContentType :: Maybe Text,
    -- | The metadata passing to pubsub components
    pubsubMetadata :: ExtendedMetadata
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (PublishEventRequest a) where
  toJSON = customToJSON 6

-- | 'BulkPublishRequestEntry' is the message containing the event to be bulk published
data BulkPublishRequestEntry a = BulkPublishRequestEntry
  { -- | The request scoped unique ID referring to this message
    bulkPublishEntryId :: EntryId,
    -- | The event which will be published to the topic
    bulkPublishEvent :: a,
    -- | The content type for the event
    bulkPublishContentType :: Maybe Text,
    -- | The event level metadata passing to the pubsub component
    bulkPublishEntryMetadata :: ExtendedMetadata
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (BulkPublishRequestEntry a) where
  toJSON = customToJSON 11

-- | 'BulkPublishRequest' is the message to bulk publish events to pubsub topic
data BulkPublishRequest a = BulkPublishRequest
  { -- | The name of the pubsub component
    bulkPublishPubsubName :: PubsubName,
    -- | The pubsub topic
    bulkPublishTopic :: PubsubTopic,
    -- | The entries which contain the individual events and associated details to be published
    bulkPublishEntries :: [BulkPublishRequestEntry a],
    -- | The request level metadata passing to the pubsub components
    bulkPublishMetadata :: ExtendedMetadata
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (BulkPublishRequest a) where
  toJSON = customToJSON 11

-- | 'BulkPublishResponseFailedEntry' is the message containing the entryID and error of a failed event
data BulkPublishResponseFailedEntry = BulkPublishResponseFailedEntry
  { -- | The response scoped unique ID referring to this message
    failedEntryId :: EntryId,
    -- | The error message if any on failure
    failedEntryError :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON BulkPublishResponseFailedEntry where
  parseJSON = customParseJSON 11

-- | 'BulkPublishResponse' is the message returned from a BulkPublishEvent call
data BulkPublishResponse = BulkPublishResponse
  { -- | The entries for different events that failed publish in the BulkPublishEvent call
    bulkPublishFailedEntries :: [BulkPublishResponseFailedEntry]
  }
  deriving (Eq, Show, Generic)

instance FromJSON BulkPublishResponse where
  parseJSON = customParseJSON 11

-- | 'TopicEventResponse' represents the status of processing a topic event
data TopicEventResponse
  = TopicEventSuccess
  | TopicEventRetry
  | TopicEventDrop
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | 'SubscribeTopicEventsRequestInitial' is the initial message for subscribing to a topic via streaming
data SubscribeTopicEventsRequestInitial = SubscribeTopicEventsRequestInitial
  { -- | The name of the pubsub component
    subscribeInitialPubsubName :: PubsubName,
    -- | The pubsub topic
    subscribeInitialTopic :: PubsubTopic,
    -- | The metadata passing to pub components
    subscribeInitialMetadata :: ExtendedMetadata,
    -- | Dead letter topic is the topic to which messages that fail to be processed are sent
    subscribeInitialDeadLetterTopic :: Maybe PubsubTopic
  }
  deriving (Eq, Show, Generic)

instance ToJSON SubscribeTopicEventsRequestInitial where
  toJSON = customToJSON 16

-- | 'SubscribeTopicEventsRequestProcessed' is the message containing the subscription to a topic
data SubscribeTopicEventsRequestProcessed = SubscribeTopicEventsRequestProcessed
  { -- | ID is the unique identifier for the subscription request
    subscribeProcessedId :: Text,
    -- | Status is the result of the subscription request
    subscribeProcessedStatus :: TopicEventResponse
  }
  deriving (Eq, Show, Generic)

instance ToJSON SubscribeTopicEventsRequestProcessed where
  toJSON = customToJSON 17
