module Dapr.Client.HttpClient.DistributedLock where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Req
import Dapr.Core.Types
import Data.Bifunctor (bimap)
import Network.HTTP.Req
import Data.Text
import Data.Aeson
import GHC.Generics

data TryLockRequestPayload = TryLockRequestPayload
  { resourceId :: Text,
    lockOwner :: Text,
    expiryInSeconds :: Int
  } deriving (Show, Generic, ToJSON)

mapTryLockRequestToPayload :: TryLockRequest -> TryLockRequestPayload
mapTryLockRequestToPayload TryLockRequest{..} = TryLockRequestPayload{..}

data UnlockRequestPayload = UnlockRequestPayload
  { resourceId :: Text,
    lockOwner :: Text
  } deriving (Show, Generic, ToJSON)

mapUnlockRequestToPayload :: UnlockRequest -> UnlockRequestPayload
mapUnlockRequestToPayload UnlockRequest{..} = UnlockRequestPayload{..}

lock :: MonadIO m => DaprConfig -> TryLockRequest -> m (Either DaprClientError TryLockResponse)
lock config TryLockRequest{..} = do
  let url = ["lock", getLockStoreName lockStore]
      payload = mapTryLockRequestToPayload $ TryLockRequest{..}
  response <- makeHttpRequest config POST url (ReqBodyJson payload) jsonResponse mempty
  return $ bimap DaprHttpException responseBody response

unlock :: MonadIO m => DaprConfig -> UnlockRequest -> m (Either DaprClientError UnlockResponse)
unlock config UnlockRequest{..} = do
  let url = ["unlock", getLockStoreName lockStore]
      payload = mapUnlockRequestToPayload $ UnlockRequest{..}
  response <- makeHttpRequest config POST url (ReqBodyJson payload) jsonResponse mempty
  return $ bimap DaprHttpException responseBody response
