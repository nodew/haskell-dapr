module Dapr.Client.HttpClient.DistributedLock where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Req
import Dapr.Client.HttpClient.Types
import Data.Bifunctor (bimap)
import Data.Text (Text)
import Network.HTTP.Req

lock :: MonadIO m => DaprConfig -> Text -> LockRequest -> m (Either DaprClientError LockResponseBody)
lock config store body = do
  let url = ["lock", store]
  response <- makeHttpRequest config POST url (ReqBodyJson body) jsonResponse mempty
  return $ bimap DaprHttpException responseBody response

unlock :: MonadIO m => DaprConfig -> Text -> UnlockRequest -> m (Either DaprClientError UnlockResponseBody)
unlock config store body = do
  let url = ["unlock", store]
  response <- makeHttpRequest config POST url (ReqBodyJson body) jsonResponse mempty
  return $ bimap DaprHttpException responseBody response
