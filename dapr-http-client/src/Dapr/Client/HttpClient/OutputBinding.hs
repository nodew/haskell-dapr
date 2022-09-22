-- |
-- Module      : OutputBinding
-- Description : Invokes an output binding
-- Copyright   : (c)
-- License     : Apache-2.0
-- This module lets you invoke output bindings
module Dapr.Client.HttpClient.OutputBinding (invokeOutputBinding) where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Req
import Dapr.Core.Types
import Dapr.Core.Types.Internal
import Data.Aeson
import Data.Bifunctor (bimap)
import Data.Text
import GHC.Generics
import Network.HTTP.Req

data InvokeBindingRequestPayload a = InvokeBindingRequestPayload
  { bindingMetadata :: ExtendedMetadata,
    bindingData :: a,
    bindingOperation :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (InvokeBindingRequestPayload a) where
  toJSON = customToJSON 7

mapInvokeBindingRequestToPayload :: InvokeBindingRequest a -> InvokeBindingRequestPayload a
mapInvokeBindingRequestToPayload InvokeBindingRequest {..} = InvokeBindingRequestPayload {..}

-- | 'invokeOutputBinding' lets you invoke a Dapr output binding. Dapr bindings support various operations.
invokeOutputBinding ::
  (MonadIO m, ToJSON a) =>
  DaprConfig ->
  InvokeBindingRequest a ->
  m (Either DaprClientError ())
invokeOutputBinding config request@(InvokeBindingRequest {..})  = do
  let url = ["bindings", getBindingName bindingName]
      payload = mapInvokeBindingRequestToPayload request
  response <- makeHttpRequest config POST url (ReqBodyJson payload) ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response
