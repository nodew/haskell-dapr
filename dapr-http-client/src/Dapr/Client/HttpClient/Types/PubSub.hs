module Dapr.Client.HttpClient.Types.PubSub where

import Dapr.Client.HttpClient.Types.Core
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
