module Dapr.Common.PubSub where

import Data.Aeson
import Dapr.Common.Core
import Data.Text (Text)
import GHC.Generics (Generic)

data SubscriptionInfo = SubscriptionInfo
  { pubsubname :: Text,
    topic :: Text,
    route :: Text,
    metadata :: RequestMetadata
  }
  deriving (Eq, Show, Generic)

instance ToJSON SubscriptionInfo
