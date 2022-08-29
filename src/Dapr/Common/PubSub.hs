module Dapr.Common.PubSub where

import RIO
import Data.Aeson
import Dapr.Common.Core

data SubscriptionInfo = SubscriptionInfo
  { pubsubname :: Text,
    topic :: Text,
    route :: Text,
    metadata :: RequestMetadata
  }
  deriving (Eq, Show, Generic)

instance ToJSON SubscriptionInfo
