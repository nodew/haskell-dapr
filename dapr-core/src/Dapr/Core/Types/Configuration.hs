-- |
-- Module      : Dapr.Core.Types.Configuration
-- Description : Defines the types used by Configuration module
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines the types used by Configuration module.
module Dapr.Core.Types.Configuration where

import Dapr.Core.Types.Common
  ( ConfigurationItem,
    ConfigurationKey,
    ExtendedMetadata,
    SubscriptionId,
  )
import Dapr.Core.Types.Internal (customParseJSON)
import Data.Aeson (FromJSON (parseJSON))
import Data.Map
import Data.Text (Text)
import GHC.Generics (Generic)

-- | 'ConfigurationStore' is the name of configuration store.
newtype ConfigurationStore = ConfigurationStore {getConfigStoreName :: Text}

-- | 'GetConfigurationRequest' is the message to get a list of key-value configuration from specified configuration store.
data GetConfigurationRequest = GetConfigurationRequest
  { -- | The name of configuration store.
    storeName :: ConfigurationStore,
    -- | Optional. The key of the configuration item to fetch. If set, only query for the specified configuration items. Empty list means fetch all.
    keys :: [Text],
    -- | Optional. The metadata which will be sent to configuration store components.
    metadata :: ExtendedMetadata
  }

-- | 'SubscribeConfigurationRequest' is the message to get a list of key-value configuration from specified configuration store.
data SubscribeConfigurationRequest = SubscribeConfigurationRequest
  { -- | The name of configuration store.
    storeName :: ConfigurationStore,
    -- | Optional. The key of the configuration item to fetch. If set, only query for the specified configuration items. Empty list means fetch all.
    keys :: [Text],
    -- | Optional. The metadata which will be sent to configuration store components.
    metadata :: ExtendedMetadata
  }

-- | 'SubscribeConfigurationResponse' is the response of subscribe configuration call
data SubscribeConfigurationResponse = SubscribeConfigurationResponse
  { -- | Subscribe id, used to stop subscription.
    subscriptionId :: SubscriptionId,
    -- | The list of items containing configuration values
    subscriptionItems :: Map ConfigurationKey ConfigurationItem
  }
  deriving (Eq, Show, Generic)

instance FromJSON SubscribeConfigurationResponse where
  parseJSON = customParseJSON 12

-- | 'UnsubscribeConfigurationRequest' is the message to stop watching the key-value configuration.
data UnsubscribeConfigurationRequest = UnsubscribeConfigurationRequest
  { -- | The name of configuration store.
    storeName :: ConfigurationStore,
    -- | Subscribe id.
    subscriptionId :: SubscriptionId
  }

-- | 'UnsubscribeConfigurationResponse' is the response of unsubscribe configuration call
data UnsubscribeConfigurationResponse = UnsubscribeConfigurationResponse
  { unsubscribeOk :: Bool,
    unsubscribeMessage :: Text
  }
