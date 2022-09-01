module Dapr.Common.Core where

import Data.Aeson
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T

data DaprConfig = DaprConfig
  { daprHost :: Text,
    daprPort :: Int,
    daprApiVersion :: Text
  }
  deriving (Show)

defaultDaprConfig :: DaprConfig
defaultDaprConfig =
  DaprConfig
    { daprHost = "localhost",
      daprPort = 3500,
      daprApiVersion = "v1.0"
    }

type RequestMetadata = Map Text Text

data ConcurrencyMode = FirstWrite | LastWrite deriving (Eq)

instance Show ConcurrencyMode where
  show FirstWrite = "first-write"
  show LastWrite = "last-write"

instance ToJSON ConcurrencyMode where
  toJSON = Data.Aeson.String . T.pack . show

data ConsistencyMode = Strong | Eventual deriving (Eq)

instance Show ConsistencyMode where
  show Strong = "strong"
  show Eventual = "eventual"

instance ToJSON ConsistencyMode where
  toJSON = Data.Aeson.String . T.pack . show

data DaprHealthStatus = Healthy | Unhealthy deriving (Eq, Show)
