-- |
-- Module      : DistributedLock
-- Description : Manages Distrubuted locks
-- Copyright   : (c)
-- License     : Apache-2.0
-- This module manages Distrubuted locks
module Dapr.Client.HttpClient.DistributedLock where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Req
import Dapr.Client.HttpClient.Types
import Data.Bifunctor (bimap)
import Network.HTTP.Req

-- | Attempt to lock the given resourceId with response indicating success
lock :: MonadIO m => DaprConfig -> LockStore -> LockRequest -> m (Either DaprClientError LockResponseBody)
lock config store body = do
  let url = ["lock", getLockStoreName store]
  response <- makeHttpRequest config POST url (ReqBodyJson body) jsonResponse mempty
  return $ bimap DaprHttpException responseBody response

-- | Attempt to unlock the given resourceId with response indicating success
unlock :: MonadIO m => DaprConfig -> LockStore -> UnlockRequest -> m (Either DaprClientError UnlockResponseBody)
unlock config store body = do
  let url = ["unlock", getLockStoreName store]
  response <- makeHttpRequest config POST url (ReqBodyJson body) jsonResponse mempty
  return $ bimap DaprHttpException responseBody response
