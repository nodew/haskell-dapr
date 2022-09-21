module Dapr.Core.Types.Configuration where

import Dapr.Core.Types.Internal
import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Dapr.Core.Types.Common

newtype ConfigurationStore = ConfigurationStore { getConfigStoreName :: Text }

data GetConfigurationRequest = GetConfigurationRequest
  {
    storeName :: ConfigurationStore,
    keys :: [Text],
    metadata :: ExtendedMetadata
  }

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
