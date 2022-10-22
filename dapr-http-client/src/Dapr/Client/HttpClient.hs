-- |
-- Module      : Dapr.Client.HttpClient
-- Description :
-- Copyright   : (c)
-- License     : Apache-2.0
module Dapr.Client.HttpClient
  ( module Req,
    module Configuration,
    module DistributedLock,
    module Health,
    module Metadata,
    module PubSub,
    module Secrets,
    module ServiceInvocation,
    module StateManagement,
    module OutputBinding,
    module Types,
    module Actor,

    runDaprHttpClient
  )
where

import Dapr.Client.HttpClient.Actor as Actor
import Dapr.Client.HttpClient.Configuration as Configuration
import Dapr.Client.HttpClient.DistributedLock as DistributedLock
import Dapr.Client.HttpClient.Health as Health
import Dapr.Client.HttpClient.Metadata as Metadata
import Dapr.Client.HttpClient.OutputBinding as OutputBinding
import Dapr.Client.HttpClient.PubSub as PubSub
import Dapr.Client.HttpClient.Req as Req
import Dapr.Client.HttpClient.Secrets as Secrets
import Dapr.Client.HttpClient.ServiceInvocation as ServiceInvocation
import Dapr.Client.HttpClient.StateManagement as StateManagement
import Dapr.Core.Types as Types
import Network.HTTP.Req (defaultHttpConfig, runReq)
import Control.Monad.Cont (MonadIO)
import Control.Monad.Reader ( ReaderT (runReaderT))

-- | Run a computation in the 'DaprHttpClient' monad with the given 'DaprConfig'.
runDaprHttpClient :: MonadIO m => DaprConfig -> DaprHttpClient a -> m a
runDaprHttpClient config client = runReq defaultHttpConfig $ runReaderT (runDaprClient client) config
