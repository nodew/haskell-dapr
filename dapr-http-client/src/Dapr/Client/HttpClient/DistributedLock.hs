-- |
-- Module      : Dapr.Client.HttpClient.DistributedLock
-- Description : Manages Distrubuted locks
-- Copyright   : (c)
-- License     : Apache-2.0
-- This module manages Distrubuted locks
module Dapr.Client.HttpClient.DistributedLock
  ( tryLock,
    unlock,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Req
import Dapr.Core.Types
import Data.Aeson
import Data.Bifunctor (bimap)
import Data.Text
import GHC.Generics
import Network.HTTP.Req

data TryLockRequestPayload = TryLockRequestPayload
  { resourceId :: Text,
    lockOwner :: Text,
    expiryInSeconds :: Int
  }
  deriving (Show, Generic, ToJSON)

mapTryLockRequestToPayload :: TryLockRequest -> TryLockRequestPayload
mapTryLockRequestToPayload TryLockRequest {..} = TryLockRequestPayload {..}

data UnlockRequestPayload = UnlockRequestPayload
  { resourceId :: Text,
    lockOwner :: Text
  }
  deriving (Show, Generic, ToJSON)

mapUnlockRequestToPayload :: UnlockRequest -> UnlockRequestPayload
mapUnlockRequestToPayload UnlockRequest {..} = UnlockRequestPayload {..}

-- | Attempt to lock the given resourceId with response indicating success
tryLock :: MonadIO m => DaprConfig -> TryLockRequest -> m (Either DaprClientError TryLockResponse)
tryLock config request@(TryLockRequest {..}) = do
  let url = ["lock", getLockStoreName lockStore]
      payload = mapTryLockRequestToPayload request
  response <- makeHttpRequest config POST url (ReqBodyJson payload) jsonResponse mempty
  return $ bimap DaprHttpException responseBody response

-- | Attempt to unlock the given resourceId with response indicating success
unlock :: MonadIO m => DaprConfig -> UnlockRequest -> m (Either DaprClientError UnlockResponse)
unlock config request@(UnlockRequest {..}) = do
  let url = ["unlock", getLockStoreName lockStore]
      payload = mapUnlockRequestToPayload request
  response <- makeHttpRequest config POST url (ReqBodyJson payload) jsonResponse mempty
  return $ bimap DaprHttpException responseBody response
