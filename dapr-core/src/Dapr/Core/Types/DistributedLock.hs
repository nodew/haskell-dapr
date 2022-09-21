module Dapr.Core.Types.DistributedLock where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

newtype LockStore = LockStore {getLockStoreName :: Text}

data LockRequest = LockRequest
  { resourceId :: Text,
    lockOwner :: Text,
    expiryInSeconds :: Int
  }
  deriving (Eq, Show, Generic, ToJSON)

newtype LockResponseBody = LockResponseBody
  { success :: Text
  }
  deriving (Eq, Show, Generic, FromJSON)

data UnlockRequest = UnlockRequest
  { resourceId :: Text,
    lockOwner :: Text
  }
  deriving (Eq, Show, Generic, ToJSON)

newtype UnlockResponseBody = UnlockResponseBody
  { status :: Int
  }
  deriving (Eq, Show, Generic, FromJSON)
