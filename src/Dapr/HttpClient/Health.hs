module Dapr.HttpClient.Health where

import Dapr.HttpClient.Core
import Dapr.HttpClient.Internal
import Network.HTTP.Req
import RIO

data DaprHealthStatus = Healthy | Unhealthy deriving (Eq, Show)

checkHealth :: (MonadIO m) => DaprClientConfig -> m DaprHealthStatus
checkHealth config = do
  response <- makeRequest config GET ["healthz"] NoReqBody ignoreResponse mempty
  return $ if isSucceededResponse response
    then Healthy
    else Unhealthy
