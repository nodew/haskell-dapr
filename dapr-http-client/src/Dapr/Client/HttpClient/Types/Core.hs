-- |
-- Module      : Types.Core
-- Description : Defines the core types used acorss the library
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines the core types used acorss the library.
module Dapr.Client.HttpClient.Types.Core where

import Data.Aeson
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Req (HttpException)

-- | Represents the configuration of the Dapr service
data DaprConfig = DaprConfig
  { -- | The hostname of the Dapr service
    daprHost :: Text,
    -- | Dapr service HTTP port
    daprPort :: Int,
    -- | Dapr API version
    daprApiVersion :: Text
  }
  deriving (Show)

-- | Represents the name of the Dapr host app
newtype DaprHostApp = DaprHostApp {getId :: Text}

-- | Creates a default Dapr config for localhost
defaultDaprConfig :: DaprConfig
defaultDaprConfig =
  DaprConfig
    { daprHost = "localhost",
      daprPort = 3500,
      daprApiVersion = "v1.0"
    }

-- | Represents the Dapr client errors
data DaprClientError
  = DaprHttpException HttpException
  | JsonDecodeError Text
  | NotFound
  | UnknownError
  deriving (Show)

-- | A collection of key-value pairs that will be provided to the Dapr APIs
type RequestMetadata = Map Text Text

-- | Concurrency mode for Dapr operations
data ConcurrencyMode
  = -- | State operations will be handled in the first-write-wins fashion
    FirstWrite
  | -- | State operations will be handled in a last-write-wins fashion
    LastWrite
  deriving (Eq)

instance Show ConcurrencyMode where
  show FirstWrite = "first-write"
  show LastWrite = "last-write"

instance ToJSON ConcurrencyMode where
  toJSON = Data.Aeson.String . T.pack . show

-- | Consistency Mode for Dapr operations
data ConsistencyMode
  = -- | Strong Consistency
    Strong
  | -- | Eventual Consistency
    Eventual
  deriving (Eq)

instance Show ConsistencyMode where
  show Strong = "strong"
  show Eventual = "eventual"

instance ToJSON ConsistencyMode where
  toJSON = Data.Aeson.String . T.pack . show

-- | Represents the Health status of Dapr sidecar
data DaprHealthStatus
  = -- | Represents that Dapr sidecar status is Healthy
    Healthy
  | -- | Represents that Dapr sidecar status is Unhealthy
    Unhealthy
  deriving (Eq, Show)
