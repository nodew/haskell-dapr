module Dapr.Core.Types.Core where

import Control.Exception (Exception)
import Dapr.Core.Types.Internal
import Data.Aeson
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Req (HttpException)

data DaprConfig = DaprConfig
  { daprHost :: Text,
    daprPort :: Int,
    daprApiVersion :: Text
  }
  deriving (Show)

newtype DaprHostedService = DaprHostedService {getServiceId :: Text}

defaultDaprConfig :: DaprConfig
defaultDaprConfig =
  DaprConfig
    { daprHost = "localhost",
      daprPort = 3500,
      daprApiVersion = "v1.0"
    }

data DaprClientError
  = DaprHttpException HttpException
  | JsonDecodeError Text
  | NotFound
  | UnknownError
  deriving (Show)

instance Exception DaprClientError

type ExtendedMetadata = Map Text Text

newtype Etag = Etag {getEtagValue :: Text}
  deriving (Show, Eq, Generic, ToJSON)

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

data DaprHealthStatus = DaprHealthy | DaprUnhealthy deriving (Eq, Show)

data StateItem a = StateItem
  { stateItemKey :: Text,
    stateItemValue :: a,
    stateItemEtag :: Etag,
    stateItemMetadata :: ExtendedMetadata,
    stateItemOption :: StateOption
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (StateItem a) where
  toJSON = customToJSON 9

data StateOption = StateOperation
  { concurrency :: ConcurrencyMode,
    consistency :: ConsistencyMode
  }
  deriving (Eq, Show, Generic, ToJSON)

data ConfigurationItem = ConfigurationItem
  { configurationItemKey :: Text,
    configurationItemValue :: Text,
    configurationItemVersion :: Text,
    configurationItemMetadata :: ExtendedMetadata
  }
  deriving (Eq, Show, Generic)

instance ToJSON ConfigurationItem where
  toJSON = customToJSON 17

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

data HttpExtension = HttpExtension
  { httpVerb :: HttpVerb,
    queryString :: Text
  }
