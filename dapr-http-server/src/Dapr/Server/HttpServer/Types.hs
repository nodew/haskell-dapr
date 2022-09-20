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

data SubScribeStatus = SubscribeSuccess | SubscribeRetry | SubscribeDrop
  deriving (Eq)

instance Show SubScribeStatus where
  show SubscribeSuccess = "SUCCESS"
  show SubscribeRetry = "RETRY"
  show SubscribeDrop = "DROP"

instance ToJSON SubScribeStatus where
  toJSON = Data.Aeson.String . T.pack . show

newtype SubscribedMessageHttpResponse = SubscribedMessageHttpResponse
  { status :: SubScribeStatus
  }
  deriving (Eq, Show, Generic, ToJSON)

data DaprSubMessage a = DaprSubMessage
  { subMsgData :: a,
    subMsgPubsubName :: Text,
    subMsgTopic :: Text,
    subMsgDataContentType :: Text,
    subMsgId :: Text,
    subMsgSpecVersion :: Text,
    subMsgType :: Text,
    subMsgSource :: Text,
    subMsgTraceId :: Text,
    subMsgTraceParent :: Text,
    subMsgTraceState :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON a => FromJSON (DaprSubMessage a) where
  parseJSON = withObject "DaprSubMessage" $ \o -> do
    subMsgData <- o .: "data"
    subMsgPubsubName <- o .: "pubsubname"
    subMsgTopic <- o .: "topic"
    subMsgDataContentType <- o .: "datacontenttype"
    subMsgId <- o .: "id"
    subMsgSpecVersion <- o .: "specversion"
    subMsgType <- o .: "type"
    subMsgSource <- o .: "source"
    subMsgTraceId <- o .: "traceid"
    subMsgTraceParent <- o .: "traceparent"
    subMsgTraceState <- o .: "tracestate"
    pure DaprSubMessage {..}
