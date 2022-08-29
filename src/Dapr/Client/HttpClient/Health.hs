module Dapr.Client.HttpClient.Health where

import Dapr.Common
import Dapr.Client.HttpClient.Req
import Network.HTTP.Req
import RIO

checkHealth :: (MonadIO m) => DaprConfig -> m DaprHealthStatus
checkHealth config = do
  response <- makeHttpRequest config GET ["healthz"] NoReqBody ignoreResponse mempty
  return $ case response of
    Right _ -> Healthy
    Left _ -> Unhealthy
