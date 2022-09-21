module Dapr.Core.Types.PublishSubscribe where

import Data.Text (Text)
import Dapr.Core.Types.Common

newtype PubsubName = PubsubName {getPubsubName :: Text}

newtype PubsubTopic = PubSubTopic {getPubsubTopic :: Text}

data PublishEventRequest a = PublishEventRequest
  { pubsubName :: PubsubName,
    pubsubTopic :: PubsubTopic,
    pubsubData :: a,
    pubsubDataContentType :: Text,
    pubsubMetadata :: ExtendedMetadata
  }
