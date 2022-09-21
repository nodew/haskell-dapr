-- |
-- Module      : Types.Actor
-- Description : Defines the types used by Actor module
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines the types used by Actor module.
module Dapr.Client.HttpClient.Types.Actor where

import Dapr.Client.HttpClient.Types.Internal
import Dapr.Client.HttpClient.Types.State
import Data.Aeson
import Data.Text
import GHC.Generics (Generic)

-- | Represents a Dapr virtual actor
data Actor = Actor
  { -- | The type of Actor
    actorType :: Text,
    -- | The ID of the Actor
    actorId :: Text
  }

-- | Represents the name of the Actor method to invoke.
newtype ActorMethod = ActorMethod {getMethodName :: Text}

-- | Represents Key of the operation
type OperationKey = Text

-- | Represents the name of the reminder for an actor
type ReminderName = Text

-- | Creates an Actor Operation request
data ActorOperationRequest a = ActorOperationRequest
  { -- | Key of the operation
    key :: OperationKey,
    -- | Value that corresponds to the key
    value :: a
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (ActorOperationRequest a) where
  toJSON = genericToJSON defaultOptions

-- | Represents an action state transaction request
data ActorStateTransactionItem a = ActorStateTransactionItem
  { -- | Operation type for state operations with Dapr
    operation :: StateOperationType,
    -- | Actor Opertation request
    request :: ActorOperationRequest a
  }
  deriving (Eq, Show, Generic, ToJSON)

-- | Represents an actor reminder request
data ActorReminderRequest = ActorReminderRequest
  { -- | Specifies the time after which the reminder is invoked. Its format should be ISO 8601 duration format with optional recurrence. Example: 0h0m3s0ms
    reminderDueTime :: Text,
    -- | Specifies the period between different invocations. Its format should be ISO 8601 duration format with optional recurrence. Example: 0h0m3s0ms
    reminderPeriod :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON ActorReminderRequest where
  toJSON = customToJSON 8

-- | Represents an actor reminder request's response
data ActorReminderResponse = ActorReminderResponse
  { -- | Specifies the time after which the reminder is invoked. Its format should be ISO 8601 duration format with optional recurrence. Example: 0h0m3s0ms
    reminderDueTime :: Text,
    -- | Specifies the period between different invocations. Its format should be ISO 8601 duration format with optional recurrence. Example: 0h0m3s0ms
    reminderPeriod :: Text,
    -- | Actor reminder data
    reminderData :: Text
  }
  deriving (Eq, Show, Generic, ToJSON)

instance FromJSON ActorReminderResponse where
  parseJSON = customParseJSON 8

-- | Represents name of the timer
type TimerName = Text

-- | Creates an Actor timer creation request
data ActorTimerRequest = ActorTimerRequest
  { -- | Specifies the time after which the reminder is invoked. Its format should be ISO 8601 duration format with optional recurrence. Example: 0h0m3s0ms
    reminderDueTime :: Text,
    -- | Specifies the period between different invocations. Its format should be ISO 8601 duration format with optional recurrence. Example: 0h0m3s0ms
    reminderPeriod :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON ActorTimerRequest where
  toJSON = customToJSON 8
