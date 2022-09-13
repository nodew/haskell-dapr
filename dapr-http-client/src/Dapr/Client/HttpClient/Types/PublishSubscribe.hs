module Dapr.Client.HttpClient.Types.PublishSubscribe where

import Data.Text (Text)

newtype PubSub = PubSub {getPubSubName :: Text}

newtype Topic = Topic {topicName :: Text}

type TextMessage = Text
