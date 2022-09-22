-- |
-- Module      : Types.DistributedLock
-- Description : Defines the types used by DistributedLock module
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines the types used by DistributedLock module.
module Dapr.Client.HttpClient.Types.DistributedLock where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Name of the Distributed Lock Store
newtype LockStore = LockStore {getLockStoreName :: Text}

-- | Creates a request to lock the given resource
data LockRequest = LockRequest
  { -- | Lock key that stands for which resource to protect
    resourceId :: Text,
    -- | Indicates the identifier of the lock owner
    lockOwner :: Text,
    -- | The time after which the lock gets expired
    expiryInSeconds :: Int
  }
  deriving (Eq, Show, Generic, ToJSON)

-- | Represents the response to the lock request indicating success.
newtype LockResponseBody = LockResponseBody
  { success :: Text
  }
  deriving (Eq, Show, Generic, FromJSON)

-- | Creates a request to unlock the given resource
data UnlockRequest = UnlockRequest
  { -- | Lock key that stands for which resource to protect
    resourceId :: Text,
    -- | Indicates the identifier of the lock owner
    lockOwner :: Text
  }
  deriving (Eq, Show, Generic, ToJSON)

-- | Represents the response to the lock request indicating unlock request status.
newtype UnlockResponseBody = UnlockResponseBody
  { status :: Int
  }
  deriving (Eq, Show, Generic, FromJSON)
