module Dapr.Core.Types.Configuration where

import Dapr.Core.Types.Common
import Dapr.Core.Types.Internal
import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

newtype ConfigurationStore = ConfigurationStore {getConfigStoreName :: Text}

data GetConfigurationRequest = GetConfigurationRequest
  { storeName :: ConfigurationStore,
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

data SubscribeConfigurationRequest = SubscribeConfigurationRequest
  { storeName :: ConfigurationStore,
    keys :: [Text],
    metadata :: ExtendedMetadata
  }

newtype SubscribeConfigurationResponse = ConfigurationResponse
  { subscriptionId :: SubscriptionId
  }
  deriving (Eq, Show, Generic)

instance FromJSON SubscribeConfigurationResponse where
  parseJSON = customParseJSON 12

data UnsubscribeConfigurationRequest = UnsubscribeConfigurationRequest
  { storeName :: ConfigurationStore,
    subscriptionId :: SubscriptionId
  }

data UnsubscribeConfigurationResponse = UnsubscribeConfigurationResponse
  { unsubscribeOk :: Bool,
    unsubscribeMessage :: Text
  }
