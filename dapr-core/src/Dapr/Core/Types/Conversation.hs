{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Dapr.Core.Types.Conversation
-- Description : Defines the types used by Conversation module
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines the types used by Conversation module.
module Dapr.Core.Types.Conversation where

import Dapr.Core.Types.Common (ExtendedMetadata)
import Dapr.Core.Types.Internal (customParseJSON, customToJSON)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | 'ConversationComponent' represents the name of conversation component
newtype ConversationComponent = ConversationComponent {getConversationComponent :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | 'ContextId' represents the ID of an existing chat (like in ChatGPT)
newtype ContextId = ContextId {getContextId :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | 'ConversationInput' represents input for the conversation
data ConversationInput = ConversationInput
  { -- | The content to send to the LLM
    conversationInputContent :: Text,
    -- | The role to set for the message
    conversationInputRole :: Maybe Text,
    -- | Scrub PII data that goes into the LLM
    conversationInputScrubPII :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON ConversationInput where
  toJSON = customToJSON 18

instance FromJSON ConversationInput where
  parseJSON = customParseJSON 18

-- | 'ConversationRequest' is the request object for Conversation
data ConversationRequest = ConversationRequest
  { -- | The name of Conversation component
    conversationRequestName :: ConversationComponent,
    -- | The ID of an existing chat (like in ChatGPT)
    conversationRequestContextId :: Maybe ContextId,
    -- | Inputs for the conversation, support multiple input in one time
    conversationRequestInputs :: [ConversationInput],
    -- | Parameters for all custom fields
    conversationRequestParameters :: Map Text Text,
    -- | The metadata passing to conversation components
    conversationRequestMetadata :: ExtendedMetadata,
    -- | Scrub PII data that comes back from the LLM
    conversationRequestScrubPII :: Maybe Bool,
    -- | Temperature for the LLM to optimize for creativity or predictability
    conversationRequestTemperature :: Maybe Double
  }
  deriving (Eq, Show, Generic)

instance ToJSON ConversationRequest where
  toJSON = customToJSON 19

-- | 'ConversationResult' is the result for one input
data ConversationResult = ConversationResult
  { -- | Result for the one conversation input
    conversationResultResult :: Text,
    -- | Parameters for all custom fields
    conversationResultParameters :: Map Text Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON ConversationResult where
  parseJSON = customParseJSON 18

-- | 'ConversationResponse' is the response for Conversation
data ConversationResponse = ConversationResponse
  { -- | The ID of an existing chat (like in ChatGPT)
    conversationResponseContextId :: Maybe ContextId,
    -- | An array of results
    conversationResponseOutputs :: [ConversationResult]
  }
  deriving (Eq, Show, Generic)

instance FromJSON ConversationResponse where
  parseJSON = customParseJSON 20
