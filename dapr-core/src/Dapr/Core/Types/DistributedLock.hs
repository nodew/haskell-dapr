{-# LANGUAGE DeriveAnyClass #-}
-- |
-- Module      : Dapr.Core.Types.DistributedLock
-- Description : Defines the types used by DistributedLock module
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines the types used by DistributedLock module.
module Dapr.Core.Types.DistributedLock where

import Dapr.Core.Types.Internal (customParseJSON)
import Data.Aeson (FromJSON (parseJSON))
import Data.Text (Text)
import GHC.Generics (Generic)

-- | 'LockStore' is the name of lock store.
newtype LockStore = LockStore {getLockStoreName :: Text}

-- | 'TryLockRequest' is the message to try to lock specific resource
data TryLockRequest = TryLockRequest
  { -- | Required. The lock store name, e.g. `redis`.
    lockStore :: LockStore,
    -- | Required. 'resourceId' is the lock key. e.g. `order_id_111`. It stands for "which resource I want to protect".
    resourceId :: Text,
    -- | Required. 'lockOwner' indicate the identifier of lock owner.
    lockOwner :: Text,
    -- | Required. The time before expiry.The time unit is second.
    expiryInSeconds :: Int
  }

-- | 'TryLockResponse' is the response of try lock request
newtype TryLockResponse = TryLockResponse
  { success :: Bool
  }
  deriving (Eq, Show, Generic, FromJSON)

-- | 'UnlockRequest' is the message to unlock specific resource
data UnlockRequest = UnlockRequest
  { -- | Required. The lock store name.
    lockStore :: LockStore,
    -- | 'resourceId' is the lock key
    resourceId :: Text,
    -- | 'lockOwner' indicate the identifier of lock owner.
    lockOwner :: Text
  }

data UnlockStatus
  = UnlockSUCCESS
  | LockDoesNotExist
  | LockBelongsToOthers
  | InternalError
  deriving (Generic, Bounded, Enum, FromJSON)

instance Show UnlockStatus where
  show UnlockSUCCESS = "Success"
  show LockDoesNotExist = "LockDoesNotExist"
  show LockBelongsToOthers = "LockBelongsToOthers"
  show InternalError = "InternalError"

newtype UnlockResponse = UnlockResponse
  { unlockStatus :: UnlockStatus
  }
  deriving (Show, Generic)

instance FromJSON UnlockResponse where
  parseJSON = customParseJSON 6
