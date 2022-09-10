module Dapr.Server.HttpServer.Types where

import Data.Aeson
import Data.Map
import Data.Text
import GHC.Generics

data SubscriptionInfo = SubscriptionInfo
  { pubsubname :: Text,
    topic :: Text,
    route :: Text,
    metadata :: Map Text Text
  }
  deriving (Eq, Show, Generic, ToJSON)

data SubscribedConfigurationItem = ConfigurationItem
  { key :: Text,
    value :: Text,
    version :: Text
  }
  deriving (Eq, Show, Generic, FromJSON)

data SubscribedConfiguration = Configuration
  { id :: Text,
    items :: [SubscribedConfigurationItem]
  }
  deriving (Eq, Show, Generic, FromJSON)
