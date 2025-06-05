{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Dapr.Core.Types.Common
-- Description : Defines the types used by multiple modules
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines the types used by multiple modules
module Dapr.Core.Types.Common where

import Control.Exception (Exception)
import Control.Monad.Reader
import Dapr.Core.Types.Internal (customParseJSON, customToJSON)
import Data.Aeson (FromJSON (parseJSON), FromJSONKey, ToJSON (toJSON), Value (String))
import qualified Data.ByteString as BS
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
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

-- | 'DaprClient' dapr client
newtype DaprClient m a = DaprClient {runDaprClient :: ReaderT DaprConfig m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadTrans,
      MonadReader DaprConfig
    )

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

-- | 'RemoteApp' represents the remote app
newtype RemoteApp = RemoteApp {getRemoteAppId :: Text}

-- | 'ExtendedMetadata' is a list of key-value pairs
type ExtendedMetadata = Map Text Text

-- | 'Etag' represents the specific version of data.
newtype Etag = Etag {getEtagValue :: Text}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Concurrency mode for Dapr operations
data ConcurrencyMode
  = -- | Unspecified concurrency
    ConcurrencyUnspecified
  | -- | First-write-wins fashion
    ConcurrencyFirstWrite
  | -- | Last-write-wins fashion
    ConcurrencyLastWrite
  deriving (Eq, Bounded, Enum)

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
  deriving (Eq, Bounded, Enum)

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
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

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
data StateOption = StateOption
  { concurrency :: ConcurrencyMode,
    consistency :: ConsistencyMode
  }
  deriving (Eq, Show, Generic)

instance ToJSON StateOption

data TransactionOperation
  = TransactionOperationUpsert
  | TransactionOperationDelete
  deriving (Eq, Bounded, Enum)

instance Show TransactionOperation where
  show TransactionOperationUpsert = "upsert"
  show TransactionOperationDelete = "delete"

instance ToJSON TransactionOperation where
  toJSON = Data.Aeson.String . T.pack . show

newtype ConfigurationKey = ConfigurationKey {getConfigurationKey :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, FromJSONKey)

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
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | 'StreamPayload' represents a chunk of data sent in a streaming request or response
data StreamPayload = StreamPayload
  { -- | Data sent in the chunk (base64 encoded when serialized)
    streamPayloadData :: Text,
    -- | Sequence number (counter that starts from 0 and increments by 1 on each chunk)
    streamPayloadSeq :: Word64
  }
  deriving (Eq, Show, Generic)

instance ToJSON StreamPayload where
  toJSON = customToJSON 13

instance FromJSON StreamPayload where
  parseJSON = customParseJSON 13

-- | 'HTTPVerb' represents HTTP methods
data HTTPVerb
  = HTTPVerbNone
  | HTTPVerbGET
  | HTTPVerbHEAD
  | HTTPVerbPOST
  | HTTPVerbPUT
  | HTTPVerbDELETE
  | HTTPVerbCONNECT
  | HTTPVerbOPTIONS
  | HTTPVerbTRACE
  | HTTPVerbPATCH
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | 'HTTPExtension' includes HTTP verb and querystring when Dapr runtime delivers HTTP content
data HTTPExtension = HTTPExtension
  { -- | HTTP verb
    httpExtensionVerb :: HTTPVerb,
    -- | Querystring represents an encoded HTTP url query string
    httpExtensionQuerystring :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON HTTPExtension where
  toJSON = customToJSON 13

instance FromJSON HTTPExtension where
  parseJSON = customParseJSON 13

-- | 'InvokeRequest' is the message to invoke a method with data
data InvokeRequest a = InvokeRequest
  { -- | Method name which will be invoked by caller
    invokeRequestMethod :: Text,
    -- | Bytes value or data which caller sent
    invokeRequestData :: a,
    -- | The type of data content
    invokeRequestContentType :: Maybe Text,
    -- | HTTP specific fields if request conveys http-compatible request
    invokeRequestHttpExtension :: Maybe HTTPExtension
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (InvokeRequest a) where
  toJSON = customToJSON 13

instance FromJSON a => FromJSON (InvokeRequest a) where
  parseJSON = customParseJSON 13

-- | 'ShutdownRequest' is the request for shutting down the sidecar
data ShutdownRequest = ShutdownRequest
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | 'PubsubSubscriptionType' indicates the type of subscription
data PubsubSubscriptionType
  = PubsubSubscriptionUnknown      -- ^ UNKNOWN is the default value
  | PubsubSubscriptionDeclarative  -- ^ Declarative subscription (k8s CRD)
  | PubsubSubscriptionProgrammatic -- ^ Programmatically created subscription
  | PubsubSubscriptionStreaming    -- ^ Bidirectional Streaming subscription
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
