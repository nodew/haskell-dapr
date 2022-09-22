-- |
-- Module      : ServiceInvocation
-- Description : Lets you perform service invocations
-- Copyright   : (c)
-- License     : Apache-2.0
-- This module lets you perform service invocations
module Dapr.Client.HttpClient.ServiceInvocation where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Req
import Dapr.Core.Types
import Data.Aeson
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Req
import GHC.Generics

invokeServiceMethod ::
  ( MonadIO m,
    HttpBody payload
  ) =>
  DaprConfig ->
  InvokeServiceRequest payload ->
  m (Either DaprClientError LbsResponse)
invokeServiceMethod config request = do
  let appId = getServiceId . serviceId app
  let url = ["invoke", appId, "method"] <> path
  response <- makeHttpRequest config httpMethod url payload lbsResponse options
  return $ first DaprHttpException response
