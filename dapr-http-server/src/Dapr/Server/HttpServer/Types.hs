{-# LANGUAGE RecordWildCards #-}

module Dapr.Server.HttpServer.Types where

import Data.Aeson
import Data.Map
import Data.Text
import qualified Data.Text as T
import GHC.Generics

data SubscriptionInfo = SubscriptionInfo
  { pubsubname :: Text,
    topic :: Text,
    route :: Text,
    metadata :: Map Text Text
  }
  deriving (Eq, Show, Generic, ToJSON)

data SubscribedConfigurationItem = ConfigurationItem
  { key :: Text,
    value :: Text,
    version :: Text
  }
  deriving (Eq, Show, Generic, FromJSON)

data SubscribedConfiguration = Configuration
  { id :: Text,
    items :: [SubscribedConfigurationItem]
  }
  deriving (Eq, Show, Generic, FromJSON)

data SubscriptionHandleStatusType = StatusSuccess | StatusRetry | StatusDrop
  deriving (Eq)

instance Show SubscriptionHandleStatusType where
  show StatusSuccess = "SUCCESS"
  show StatusRetry = "RETRY"
  show StatusDrop = "DROP"

instance ToJSON SubscriptionHandleStatusType where
  toJSON = Data.Aeson.String . T.pack . show

newtype SubscriptionHandleStatus = SubscriptionHandleStatus
  { status :: SubscriptionHandleStatusType
  }
  deriving (Eq, Show, Generic, ToJSON)

data SubscribedMessage a = SubscribedMessage
  { msgData :: a,
    msgPubsubName :: Text,
    msgTopic :: Text,
    msgDataContentType :: Text,
    msgId :: Text,
    msgSpecVersion :: Text,
    msgType :: Text,
    msgSource :: Text,
    msgTraceId :: Text,
    msgTraceParent :: Text,
    msgTraceState :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON a => FromJSON (SubscribedMessage a) where
  parseJSON = withObject "SubscribedMessage" $ \o -> do
    msgData <- o .: "data"
    msgPubsubName <- o .: "pubsubname"
    msgTopic <- o .: "topic"
    msgDataContentType <- o .: "datacontenttype"
    msgId <- o .: "id"
    msgSpecVersion <- o .: "specversion"
    msgType <- o .: "type"
    msgSource <- o .: "source"
    msgTraceId <- o .: "traceid"
    msgTraceParent <- o .: "traceparent"
    msgTraceState <- o .: "tracestate"
    pure SubscribedMessage {..}
