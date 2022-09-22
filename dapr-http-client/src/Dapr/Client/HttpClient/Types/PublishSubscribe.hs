-- |
-- Module      : Types.PublishSubscribe
-- Description : Defines the types used by PubSub module
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines the types used by PubSub module
module Dapr.Client.HttpClient.Types.PublishSubscribe where

import Data.Text (Text)

-- | Represents name of the Dapr Pub sub service
newtype PubSub = PubSub {getPubSubName :: Text}

-- | Name of the Topic used in pub sub service
newtype Topic = Topic {topicName :: Text}

-- | Message to publish in plain text format
type TextMessage = Text
