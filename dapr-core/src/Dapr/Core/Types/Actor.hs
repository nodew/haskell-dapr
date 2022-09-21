module Dapr.Core.Types.Actor where

import Dapr.Core.Types.Internal
import Dapr.Core.Types.State
import Data.Aeson
import Data.Text
import GHC.Generics (Generic)

data Actor = Actor {actorType :: Text, actorId :: Text}

newtype ActorMethod = ActorMethod {getMethodName :: Text}

type OperationKey = Text

type ReminderName = Text

data ActorOperationRequest a = ActorOperationRequest
  { key :: OperationKey,
    value :: a
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (ActorOperationRequest a) where
  toJSON = genericToJSON defaultOptions

data ActorStateTransactionItem a = ActorStateTransactionItem
  { operation :: StateOperationType,
    request :: ActorOperationRequest a
  }
  deriving (Eq, Show, Generic, ToJSON)

data ActorReminderRequest = ActorReminderRequest
  { reminderDueTime :: Text,
    reminderPeriod :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON ActorReminderRequest where
  toJSON = customToJSON 8

data ActorReminderResponse = ActorReminderResponse
  { reminderDueTime :: Text,
    reminderPeriod :: Text,
    reminderData :: Text
  }
  deriving (Eq, Show, Generic, ToJSON)

instance FromJSON ActorReminderResponse where
  parseJSON = customParseJSON 8

type TimerName = Text

data ActorTimerRequest = ActorTimerRequest
  { reminderDueTime :: Text,
    reminderPeriod :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON ActorTimerRequest where
  toJSON = customToJSON 8
