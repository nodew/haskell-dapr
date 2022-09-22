-- |
-- Module      : Dapr.Core.Types.Common
-- Description : Defines the types used by multiple modules
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines the types used by multiple modules
module Dapr.Core.Types.Common where

import Control.Exception (Exception)
import Dapr.Core.Types.Internal (customParseJSON, customToJSON)
import Data.Aeson (FromJSON(parseJSON), ToJSON (toJSON), Value (String), FromJSONKey)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Req (HttpException)

-- | 'DaprConfig' daprConfig
data DaprConfig = DaprConfig
  { -- | Dapr host
    daprHost :: Text,
    -- | Dapr port
    daprPort :: Int,
    -- | Dapr API version
    daprApiVersion :: Text
  }
  deriving (Show)

-- | 'defaultDaprConfig' is the default value of 'DaprConfig'
defaultDaprConfig :: DaprConfig
defaultDaprConfig =
  DaprConfig
    { daprHost = "localhost",
      daprPort = 3500,
      daprApiVersion = "v1.0"
    }

-- | 'DaprClientError' is the exception for client request
data DaprClientError
  = -- | HttpException
    DaprHttpException HttpException
  | -- | JSON decode error
    JsonDecodeError Text
  | -- | Not found expected data
    NotFound
  | -- | Unknown error
    UnknownError
  deriving (Show)

instance Exception DaprClientError

-- | 'RemoteAppId' is the App ID associated with the remote app
newtype RemoteAppId = RemoteAppId {getRemoteAppId :: Text}

-- | 'ExtendedMetadata' is a list of key-value pairs
type ExtendedMetadata = Map Text Text

-- | 'Etag' represents the specific version of data.
newtype Etag = Etag {getEtagValue :: Text}
  deriving (Show, Eq, Generic, ToJSON)

-- | Concurrency mode for Dapr operations
data ConcurrencyMode
  = -- | Unspecified concurrency
    ConcurrencyUnspecified
  | -- | First-write-wins fashion
    ConcurrencyFirstWrite
  | -- | Last-write-wins fashion
    ConcurrencyLastWrite
  deriving (Eq)

instance Show ConcurrencyMode where
  show ConcurrencyUnspecified = "unspecified"
  show ConcurrencyFirstWrite = "first-write"
  show ConcurrencyLastWrite = "last-write"

instance ToJSON ConcurrencyMode where
  toJSON = Data.Aeson.String . T.pack . show

-- | Consistency Mode for Dapr operations
data ConsistencyMode
  = -- | Unspecified consistency
    ConsistencyUnspecified
  | -- | Strong consistency
    ConsistencyStrong
  | -- | Eventual consistency
    ConsistencyEventual
  deriving (Eq)

instance Show ConsistencyMode where
  show ConsistencyUnspecified = "unspecified"
  show ConsistencyStrong = "strong"
  show ConsistencyEventual = "eventual"

instance ToJSON ConsistencyMode where
  toJSON = Data.Aeson.String . T.pack . show

-- | 'DaprHealthStatus' indicates the healthy status of dapr side-car
data DaprHealthStatus = DaprHealthy | DaprUnhealthy deriving (Eq, Show)

-- | 'StateKey' is the name of state key.
newtype StateKey = StateKey {getStateKey :: Text}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | 'StateItem' represents state key, value, and additional options to save state.
data StateItem a = StateItem
  { -- | Required. The state key
    stateItemKey :: StateKey,
    -- | Required. The state data for key
    stateItemValue :: a,
    -- | The entity tag which represents the specific version of data.
    stateItemEtag :: Maybe Etag,
    -- | The metadata which will be passed to state store component.
    stateItemMetadata :: ExtendedMetadata,
    -- | Options for concurrency and consistency to save the state.
    stateItemOption :: Maybe StateOption
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (StateItem a) where
  toJSON = customToJSON 9

-- | 'StateOption' configures concurrency and consistency for state operations
data StateOption = StateOperation
  { concurrency :: ConcurrencyMode,
    consistency :: ConsistencyMode
  }
  deriving (Eq, Show, Generic, ToJSON)

data TransactionOperation
  = TransactionOperationUpsert
  | TransactionOperationDelete
  deriving (Eq)

instance Show TransactionOperation where
  show TransactionOperationUpsert = "upsert"
  show TransactionOperationDelete = "delete"

instance ToJSON TransactionOperation where
  toJSON = Data.Aeson.String . T.pack . show

newtype ConfigurationKey = ConfigurationKey {getConfigurationKey :: Text}
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, FromJSONKey)

-- | 'ConfigurationItem' represents all the configuration with its name(key).
data ConfigurationItem = ConfigurationItem
  { -- | Required. The value of configuration item.
    configurationItemValue :: Text,
    -- | Version is response only and cannot be fetched. Store is not expected to keep all versions available
    configurationItemVersion :: Text,
    -- | The metadata which will be passed to/from configuration store component
    configurationItemMetadata :: ExtendedMetadata
  }
  deriving (Eq, Show, Generic)

instance ToJSON ConfigurationItem where
  toJSON = customToJSON 17

instance FromJSON ConfigurationItem where
  parseJSON = customParseJSON 17

newtype SubscriptionId = SubscriptionId {getSubscriptionId :: Text}
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Type of HTTP 1.1 Methods
data HttpVerb
  = HttpVerbGet
  | HttpVerbHead
  | HttpVerbPost
  | HttpVerbPut
  | HttpVerbDelete
  | HttpVerbConnect
  | HttpVerbOptions
  | HttpVerbTrace
  | HttpVerbPatch

instance Show HttpVerb where
  show HttpVerbGet = "GET"
  show HttpVerbHead = "HEAD"
  show HttpVerbPost = "POST"
  show HttpVerbPut = "PUT"
  show HttpVerbDelete = "DELETE"
  show HttpVerbConnect = "CONNECT"
  show HttpVerbOptions = "OPTIOTNS"
  show HttpVerbTrace = "TRACE"
  show HttpVerbPatch = "PATCH"

-- | Dapr runtime will parse POST as a verb and extract querystring to quersytring map.
data HttpExtension = HttpExtension
  { httpVerb :: HttpVerb,
    queryString :: Text
  }
