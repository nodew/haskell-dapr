-- |
-- Module      : Dapr.Client.HttpClient.Health
-- Description : To check the Health of Dapr sidecar
-- Copyright   : (c)
-- License     : Apache-2.0
-- This module allows to check the Health of Dapr sidecar
module Dapr.Client.HttpClient.Health where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Req (makeHttpRequest)
import Dapr.Core.Types (DaprConfig, DaprHealthStatus (..))
import Network.HTTP.Req
  ( GET (GET),
    NoReqBody (NoReqBody),
    ignoreResponse,
  )

-- | Perform health-check of Dapr sidecar. Returns `Healthy` if sidecar is healthy. Otherwise 'Unhealthy'.
checkHealth :: (MonadIO m) => DaprConfig -> m DaprHealthStatus
checkHealth config = do
  response <- makeHttpRequest config GET ["healthz"] NoReqBody ignoreResponse mempty
  return $ case response of
    Right _ -> DaprHealthy
    Left _ -> DaprUnhealthy
