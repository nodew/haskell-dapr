module Dapr.Core.Types.Common where

import Control.Exception (Exception)
import Dapr.Core.Types.Internal
import Data.Aeson
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Req (HttpException)

-- | 'DaprConfig' daprConfig
data DaprConfig = DaprConfig
  { daprHost :: Text, -- ^Dapr host
    daprPort :: Int, -- ^Dapr port
    daprApiVersion :: Text -- ^Dapr API version
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
  = DaprHttpException HttpException -- ^ HttpException
  | JsonDecodeError Text -- ^JSON decode error
  | NotFound -- ^ Not found expected data
  | UnknownError -- ^ Unknown error
  deriving (Show)

instance Exception DaprClientError

-- | 'RemoteAppId' is the App ID associated with the remote app
newtype RemoteAppId = RemoteAppId {getRemoteAppId :: Text}

-- | 'ExtendedMetadata' is a list of key-value pairs
type ExtendedMetadata = Map Text Text

-- | 'Etag' represents the specific version of data.
newtype Etag = Etag {getEtagValue :: Text}
  deriving (Show, Eq, Generic, ToJSON)

-- | Supported concurrency mode
data ConcurrencyMode
  = ConcurrencyUnspecified
  | ConcurrencyFirstWrite
  | ConcurrencyLastWrite
  deriving (Eq)

instance Show ConcurrencyMode where
  show ConcurrencyUnspecified = "unspecified"
  show ConcurrencyFirstWrite = "first-write"
  show ConcurrencyLastWrite = "last-write"

instance ToJSON ConcurrencyMode where
  toJSON = Data.Aeson.String . T.pack . show

-- | Supported consistency mode
data ConsistencyMode
  = ConsistencyUnspecified
  | ConsistencyStrong
  | ConsistencyEventual
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
  { stateItemKey :: StateKey,
    stateItemValue :: a,
    stateItemEtag :: Maybe Etag,
    stateItemMetadata :: ExtendedMetadata,
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

newtype ConfigurationKey = ConfigurationKey { getConfigurationKey :: Text}
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | 'ConfigurationItem' represents all the configuration with its name(key).
data ConfigurationItem = ConfigurationItem
  { configurationItemKey :: ConfigurationKey,
    configurationItemValue :: Text,
    configurationItemVersion :: Text,
    configurationItemMetadata :: ExtendedMetadata
  }
  deriving (Eq, Show, Generic)

instance ToJSON ConfigurationItem where
  toJSON = customToJSON 17

newtype SubscriptionId = SubscriptionId { getSubscriptionId :: Text }
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
