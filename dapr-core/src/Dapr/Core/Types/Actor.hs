-- |
-- Module      : Dapr.Core.Types.Actor
-- Description : Defines the types used by Actor module
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines the types used by Actor module.
module Dapr.Core.Types.Actor where

import Dapr.Core.Types.Common (ExtendedMetadata, StateKey, TransactionOperation)
import Dapr.Core.Types.Internal (customParseJSON)
import Data.Aeson (FromJSON (parseJSON))
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import Data.UUID (UUID, toText)
import GHC.Generics (Generic)

-- | Represents a Dapr virtual actor type
newtype ActorType = ActorType {getActorType :: Text}

-- | Represents a Dapr virtual actor ID
newtype ActorId = ActorId {getActorId :: UUID}

getActorIdText :: ActorId -> Text
getActorIdText = Data.UUID.toText . getActorId

-- | Represents a Dapr virtual actor
data Actor = Actor {actorType :: ActorType, actorId :: ActorId}

-- | 'RegisterActorTimerRequest' is the message to register a timer for an actor of a given type and id.
data RegisterActorTimerRequest a = RegisterActorTimerRequest
  { -- | Actor
    timerActor :: Actor,
    -- | Name of actor timer
    timerName :: Text,
    -- | Specifies the time after which the timer is invoked. Its format should be ISO 8601 duration format with optional recurrence. Example: 0h0m3s0ms
    timerDueTime :: Text,
    -- | Specifies the time after which the timer is invoked. Its format should be ISO 8601 duration format with optional recurrence. Example: 0h0m3s0ms
    timerPeriod :: Text,
    -- | Event handler
    timerCallback :: Maybe Text,
    -- | Extra data
    timerData :: a,
    -- | Time to live
    timerTtl :: Maybe Text
  }

-- | 'UnregisterActorTimerRequest' is the message to unregister an actor timer
data UnregisterActorTimerRequest = UnregisterActorTimerRequest
  { -- | Actor
    timerActor :: Actor,
    -- | The name of actor timer
    timerName :: Text
  }

-- | 'RegisterActorReminderRequest' is the message to register a reminder for an actor of a given type and id.
data RegisterActorReminderRequest a = RegisterActorReminderRequest
  { -- | Actor
    reminderActor :: Actor,
    -- | The name of actor reminder
    reminderName :: Text,
    -- | Specifies the time after which the reminder is invoked. Its format should be ISO 8601 duration format with optional recurrence. Example: 0h0m3s0ms
    reminderDueTime :: Text,
    -- | Specifies the time after which the reminder is invoked. Its format should be ISO 8601 duration format with optional recurrence. Example: 0h0m3s0ms
    reminderPeriod :: Text,
    -- | Extra data
    reminderData :: a,
    -- | Time to live
    reminderTtl :: Text
  }

-- | 'GetActorReminderRequest' is the message to unregister an actor reminder.
data GetActorReminderRequest = GetActorReminderRequest
  { -- | Actor
    reminderActor :: Actor,
    -- | The name of actor reminder
    reminderName :: Text
  }

data GetActorReminderResponse a = GetActorReminderResponse
  { -- | Specifies the time after which the reminder is invoked.
    reminderDueTime :: Text,
    -- | Specifies the time after which the reminder is invoked. Its format should be ISO 8601 duration format with optional recurrence. Example: 0h0m3s0ms
    reminderPeriod :: Text,
    -- | Extra data
    reminderData :: Maybe a
  }
  deriving (Eq, Show, Generic)

instance FromJSON a => FromJSON (GetActorReminderResponse a) where
  parseJSON = customParseJSON 8

-- | 'UnregisterActorReminderRequest' is the message to unregister an actor reminder.
data UnregisterActorReminderRequest = UnregisterActorReminderRequest
  { -- | Actor
    reminderActor :: Actor,
    -- | The name of actor reminder
    reminderName :: Text
  }

-- | 'RenameActorReminderRequest' is the message to rename an actor reminder.
data RenameActorReminderRequest = RenameActorReminderRequest
  { -- | Actor
    reminderActor :: Actor,
    -- | The new name of actor reminder
    reminderNewName :: Text,
    -- | The old name of actor reminder
    reminderOldName :: Text
  }

-- | 'GetActorStateRequest' is the message to get key-value states from specific actor.
data GetActorStateRequest = GetActorStateRequest
  { -- | Actor
    actor :: Actor,
    -- | State key
    key :: StateKey
  }

-- | 'GetActorStateResponse' is the response conveying the actor's state value.
newtype GetActorStateResponse a = GetActorStateResponse
  { -- | Result of get actor state call
    result :: a
  }

-- | 'ExecuteActorStateTransactionRequest' is the message to execute multiple operations on a specified actor.
data ExecuteActorStateTransactionRequest a = ExecuteActorStateTransactionRequest
  { -- | Actor
    actor :: Actor,
    -- | Bulk operations on a specified actor
    operations :: [TransactionalActorStateOperation a]
  }

-- | 'TransactionalActorStateOperation' is the message to execute a specified operation with a key-value pair.
data TransactionalActorStateOperation a = TransactionalActorStateOperation
  { -- | Operation
    operationType :: TransactionOperation,
    -- | State key
    key :: StateKey,
    -- | State value
    value :: Maybe a
  }
  deriving (Eq, Show, Generic)

-- | 'InvokeActorRequest' is the message to call an actor.
data InvokeActorRequest method a = InvokeActorRequest
  { -- | Actor
    actor :: Actor,
    -- | Http method, PUT, POST, GET, DELETE, etc
    httpMethod :: method,
    -- | Actor method
    actorMethod :: Text,
    -- | Data pass to the actor method
    actorData :: a,
    -- | Content type of request data
    actorContentType :: Maybe Text,
    -- | The metadata
    actorMetadata :: ExtendedMetadata
  }

-- | 'InvokeActorResponse' is the method that returns an actor invocation response.
newtype InvokeActorResponse = InvokeActorResponse
  { result :: L.ByteString
  }
