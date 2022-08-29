module Dapr.Common.Configuration where

import Dapr.Common.Internal
import Data.Aeson
import RIO

data Configuration = Configuration
  { configKey :: Text,
    configValue :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Configuration where
  parseJSON = customParseJSON 5

newtype SubscribeConfigurationResponse = ConfigurationResponse
  { subscriptionId :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON SubscribeConfigurationResponse where
  parseJSON = customParseJSON 12
