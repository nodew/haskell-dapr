-- |
-- Module      : Dapr.Core.Types.Actor
-- Description : Defines the types used by Actor module
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines the types used by Actor module.
module Dapr.Core.Types.Actor where

import Dapr.Core.Types.Common (ExtendedMetadata, StateKey, TransactionOperation)
import Dapr.Core.Types.Internal ( customParseJSON )
import Data.Text (Text)
import Data.UUID (UUID, toText)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON (parseJSON), ToJSON)

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
  { timerActor :: Actor, -- ^Actor
    timerName :: Text -- ^The name of actor timer
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
  { reminderActor :: Actor, -- ^Actor
    reminderName :: Text -- ^ The name of actor reminder
  }

data GetActorReminderResponse a = GetActorReminderResponse
  { -- | Specifies the time after which the reminder is invoked.
    reminderDueTime :: Text,
    -- | Specifies the time after which the reminder is invoked. Its format should be ISO 8601 duration format with optional recurrence. Example: 0h0m3s0ms
    reminderPeriod :: Text,
    -- | Extra data
    reminderData :: Maybe a
  } deriving (Eq, Show, Generic)

instance FromJSON a => FromJSON (GetActorReminderResponse a) where
  parseJSON = customParseJSON 8

-- | 'UnregisterActorReminderRequest' is the message to unregister an actor reminder.
data UnregisterActorReminderRequest = UnregisterActorReminderRequest
  { reminderActor :: Actor, -- ^Actor
    reminderName :: Text -- ^ The name of actor reminder
  }

-- | 'RenameActorReminderRequest' is the message to rename an actor reminder.
data RenameActorReminderRequest = RenameActorReminderRequest
  { reminderActor :: Actor, -- ^Actor
    reminderNewName :: Text, -- ^The new name of actor reminder
    reminderOldName :: Text -- ^ The old name of actor reminder
  }

-- | 'GetActorStateRequest' is the message to get key-value states from specific actor.
data GetActorStateRequest = GetActorStateRequest
  { actor :: Actor, -- ^Actor
    key :: StateKey -- ^State key
  }

-- | 'GetActorStateResponse' is the response conveying the actor's state value.
newtype GetActorStateResponse a = GetActorStateResponse
  { result :: a -- ^Result of get actor state call
  }

-- | 'ExecuteActorStateTransactionRequest' is the message to execute multiple operations on a specified actor.
data ExecuteActorStateTransactionRequest a = ExecuteActorStateTransactionRequest
  { actor :: Actor, -- ^Actor
    operations :: [TransactionalActorStateOperation a] -- ^Bulk operations on a specified actor
  }

-- | 'TransactionalActorStateOperation' is the message to execute a specified operation with a key-value pair.
data TransactionalActorStateOperation a = TransactionalActorStateOperation
  { operationType :: TransactionOperation, -- ^Operation
    key :: StateKey, -- ^State key
    value :: Maybe a -- ^State value
  } deriving (Eq, Show, Generic)

-- | 'InvokeActorRequest' is the message to call an actor.
data InvokeActorRequest a = InvokeActorRequest
  { actor :: Actor, -- ^Actor
    actorMethod :: Text, -- ^Actor method
    actorData :: a, -- ^Data pass to the actor method
    actorMetadata :: ExtendedMetadata -- ^The metadata
  }

-- | 'InvokeActorResponse' is the method that returns an actor invocation response.
newtype InvokeActorResponse a = InvokeActorResponse
  { result :: a
  }
