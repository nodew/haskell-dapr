-- |
-- Module      : Dapr.Client.HttpClient.Health
-- Description : To check the Health of Dapr sidecar
-- Copyright   : (c)
-- License     : Apache-2.0
-- This module allows to check the Health of Dapr sidecar
module Dapr.Client.HttpClient.Health where

import Dapr.Client.HttpClient.Req (makeHttpRequest, DaprHttpClient)
import Dapr.Core.Types (DaprHealthStatus (..))
import Network.HTTP.Req
  ( GET (GET),
    NoReqBody (NoReqBody),
    ignoreResponse,
  )

-- | Perform health-check of Dapr sidecar. Returns `Healthy` if sidecar is healthy. Otherwise 'Unhealthy'.
checkHealth :: DaprHttpClient DaprHealthStatus
checkHealth = do
  response <- makeHttpRequest GET ["healthz"] NoReqBody ignoreResponse mempty
  return $ case response of
    Right _ -> DaprHealthy
    Left _ -> DaprUnhealthy
