-- |
-- Module      : Dapr.Core.Types.PublishSubscribe
-- Description : Defines the types used by PubSub module
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines the types used by PubSub module.
module Dapr.Core.Types.Pubsub where

import Dapr.Core.Types.Common (ExtendedMetadata)
import Data.Text (Text)

-- | 'PubsubName' represents the name of pubsub component.
newtype PubsubName = PubsubName {getPubsubName :: Text}

-- | 'PubsubTopic' represents the pubsub topic
newtype PubsubTopic = PubsubTopic {getPubsubTopic :: Text}

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

