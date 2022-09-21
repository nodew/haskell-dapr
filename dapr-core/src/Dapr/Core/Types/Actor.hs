module Dapr.Core.Types.Actor where

import Dapr.Core.Types.Common (ExtendedMetadata, StateKey, TransactionOperation)
import Data.Text
import Data.UUID (UUID)

newtype ActorType = ActorType {getActorType :: Text}

newtype ActorId = ActorId {getActorId :: UUID}

data Actor = Actor {actorType :: ActorType, actorId :: ActorId}

data RegisterActorTimerRequest a = RegisterActorTimerRequest
  { timerActorType :: ActorType,
    timerActorId :: ActorId,
    timerName :: Text,
    timerDueTime :: Text,
    timerPeriod :: Text,
    timerCallback :: Text,
    timerData :: a,
    timerTtl :: Text
  }

data UnregisterActorTimerRequest = UnregisterActorTimerRequest
  { timerActorType :: ActorType,
    timerActorId :: ActorId,
    timerName :: Text
  }

data RegisterActorReminderRequest a = RegisterActorReminderRequest
  { reminderActorType :: ActorType,
    reminderActorId :: ActorId,
    reminderName :: Text,
    reminderDueTime :: Text,
    reminderPeriod :: Text,
    reminderData :: a,
    reminderTtl :: Text
  }

data UnregisterActorReminderRequest = UnregisterActorReminderRequest
  { reminderActorType :: ActorType,
    reminderActorId :: ActorId,
    reminderName :: Text
  }

data RenameActorReminderRequest = RenameActorReminderRequest
  { reminderActorType :: ActorType,
    reminderActorId :: ActorId,
    reminderNewName :: Text,
    reminderOldName :: Text
  }

data GetActorStateRequest = GetActorStateRequest
  { actorType :: ActorType,
    actorId :: ActorId,
    key :: StateKey
  }

newtype GetActorStateResponse a = GetActorStateResponse
  { result :: a
  }

data ExecuteActorStateTransactionRequest a = ExecuteActorStateTransactionRequest
  { actorType :: ActorType,
    actorId :: ActorId,
    operations :: [TransactionalActorStateOperation a]
  }

data TransactionalActorStateOperation a = TransactionalActorStateOperation
  { operationType :: TransactionOperation,
    key :: StateKey,
    value :: a
  }

data InvokeActorRequest a = InvokeActorRequest
  { actorType :: ActorType,
    actorId :: ActorId,
    actorMethod :: Text,
    actorData :: a,
    actorMetadata :: ExtendedMetadata
  }

newtype InvokeActorResponse a = InvokeActorResponse
  { result :: a
  }
