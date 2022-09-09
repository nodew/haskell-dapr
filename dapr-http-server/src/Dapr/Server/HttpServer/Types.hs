module Dapr.Server.HttpServer.Types where

import Data.Aeson
import Data.Map
import Data.Text
import GHC.Generics

data SubscriptionInfo = SubscriptionInfo
  { pubsubname :: Text,
    topic :: Text,
    route :: Text,
    metadata :: Map Text Text
  }
  deriving (Eq, Show, Generic, ToJSON)
