module Dapr.Client.HttpClient.Health where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Req
import Dapr.Client.HttpClient.Types
import Network.HTTP.Req

checkHealth :: (MonadIO m) => DaprConfig -> m DaprHealthStatus
checkHealth config = do
  response <- makeHttpRequest config GET ["healthz"] NoReqBody ignoreResponse mempty
  return $ case response of
    Right _ -> Healthy
    Left _ -> Unhealthy
