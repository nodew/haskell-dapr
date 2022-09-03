module Dapr.Common.PubSub where

import Dapr.Common.Core
import Data.Aeson
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
