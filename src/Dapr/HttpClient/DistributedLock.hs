module Dapr.HttpClient.DistributedLock where

import Dapr.HttpClient.Core (DaprClientConfig, DaprClientError (DaprHttpException), makeRequest)
import Data.Aeson
import Network.HTTP.Req
import RIO

data LockRequestBody = LockRequestBody
  { resourceId :: Text,
    lockOwner :: Text,
    expiryInSeconds :: Int
  }
  deriving (Eq, Show, Generic, ToJSON)

newtype LockResponseBody = LockResponseBody
  { success :: Text
  }
  deriving (Eq, Show, Generic, FromJSON)

data UnlockRequestBody = UnlockRequestBody
  { resourceId :: Text,
    lockOwner :: Text
  }
  deriving (Eq, Show, Generic, ToJSON)

newtype UnlockResponseBody = UnlockResponseBody
  { status :: Int
  }
  deriving (Eq, Show, Generic, FromJSON)

lock :: MonadIO m => DaprClientConfig -> Text -> LockRequestBody -> m (Either DaprClientError LockResponseBody)
lock config store body = do
  let url = ["lock", store]
      options = header "Content-Type" "application/json"
  response <- makeRequest config POST url (ReqBodyJson body) jsonResponse options
  return $ bimap DaprHttpException responseBody response

unlock :: MonadIO m => DaprClientConfig -> Text -> UnlockRequestBody -> m (Either DaprClientError UnlockResponseBody)
unlock config store body = do
  let url = ["unlock", store]
      options = header "Content-Type" "application/json"
  response <- makeRequest config POST url (ReqBodyJson body) jsonResponse options
  return $ bimap DaprHttpException responseBody response
