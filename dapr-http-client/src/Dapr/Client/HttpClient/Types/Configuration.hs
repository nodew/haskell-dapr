-- |
-- Module      : Types.Configuraiton
-- Description : Defines the types used in Configuration
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines the types used in Configuration.
module Dapr.Client.HttpClient.Types.Configuration where

import Dapr.Client.HttpClient.Types.Internal
import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Represents the name of the configuration store
newtype ConfigurationStore = ConfigurationStore {getConfigStoreName :: Text}

-- | Represents a configuration
data Configuration = Configuration
  { -- | The Key of configuraiton
    configKey :: Text,
    -- | The value of configuration
    configValue :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Configuration where
  parseJSON = customParseJSON 5

-- | Represents response for the Subscribe Configuration request
newtype SubscribeConfigurationResponse = ConfigurationResponse
  { -- | The Id of the subscription
    subscriptionId :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON SubscribeConfigurationResponse where
  parseJSON = customParseJSON 12
