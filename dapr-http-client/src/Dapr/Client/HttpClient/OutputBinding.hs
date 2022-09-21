module Dapr.Client.HttpClient.OutputBinding where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Req
import Dapr.Core.Types
import Data.Aeson
import Data.Bifunctor (bimap)
import Network.HTTP.Req

-- | 'invokeOutputBinding' lets you invoke a Dapr output binding. Dapr bindings support various operations.
invokeOutputBinding ::
  (MonadIO m, ToJSON a) =>
  DaprConfig ->
  InvokeBindingRequest a ->
  m (Either DaprClientError ())
invokeOutputBinding config bindingRequest = do
  let url = ["bindings", bindingName bindingRequest]
  response <- makeHttpRequest config POST url (ReqBodyJson $ mapInvokeBindingRequestToPayload bindingRequest) ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response
