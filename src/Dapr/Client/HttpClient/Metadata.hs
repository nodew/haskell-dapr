module Dapr.Client.HttpClient.Metadata where

import Dapr.Client.HttpClient.Req
import Dapr.Common
import Network.HTTP.Req
import RIO

getMetadata :: MonadIO m => DaprConfig -> m (Either DaprClientError DaprMetadata)
getMetadata config = do
  response <- makeHttpRequest config GET ["metadata"] NoReqBody jsonResponse mempty
  return $ bimap DaprHttpException responseBody response
