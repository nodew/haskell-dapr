module Dapr.Client.HttpClient.Types.Configuration where

import Dapr.Client.HttpClient.Types.Internal
import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

newtype ConfigurationStore = ConfigurationStore {getConfigStoreName :: Text}

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
