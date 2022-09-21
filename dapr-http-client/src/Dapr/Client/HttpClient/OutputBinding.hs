module Dapr.Client.HttpClient.OutputBinding
  ( invokeOutputBinding,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Internal
import Dapr.Client.HttpClient.Req
import Dapr.Core.Types
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
invokeOutputBinding config bindingRequest = do
  let url = ["bindings", bindingName bindingRequest]
  response <- makeHttpRequest config POST url (ReqBodyJson $ mapInvokeBindingRequestToPayload bindingRequest) ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response
