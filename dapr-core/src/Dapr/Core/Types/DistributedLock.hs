module Dapr.Core.Types.DistributedLock where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Dapr.Core.Types.Internal

newtype LockStore = LockStore {getLockStoreName :: Text}

data TryLockRequest = TryLockRequest
  { lockStore :: LockStore,
    resourceId :: Text,
    lockOwner :: Text,
    expiryInSeconds :: Int
  }

newtype TryLockResponse = TryLockResponse
  { success :: Bool
  }
  deriving (Eq, Show, Generic, FromJSON)

data UnlockRequest = UnlockRequest
  { lockStore :: LockStore,
    resourceId :: Text,
    lockOwner :: Text
  }

data UnlockStatus
  = UnlockSUCCESS
  | LockDoesNotExist
  | LockBelongsToOthers
  | InternalError
  deriving (Generic, FromJSON)

instance Show UnlockStatus where
  show UnlockSUCCESS = "Success"
  show LockDoesNotExist = "LockDoesNotExist"
  show LockBelongsToOthers = "LockBelongsToOthers"
  show InternalError = "InternalError"


newtype UnlockResponse = UnlockResponse
  { unlockStatus :: UnlockStatus
  } deriving (Show, Generic)

instance FromJSON UnlockResponse where
  parseJSON = customParseJSON 6
