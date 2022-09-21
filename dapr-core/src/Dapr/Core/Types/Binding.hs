module Dapr.Core.Types.Binding where

import Dapr.Core.Types.Common
import Dapr.Core.Types.Internal
import Data.Aeson
import Data.Text
import GHC.Generics (Generic)

newtype Binding = Binding {getBindingName :: Text}

data InvokeBindingRequest a = InvokeBindingRequest
  { bindingName :: Text,
    bindingMetadata :: ExtendedMetadata,
    bindingData :: a,
    bindingOperation :: Text
  }
  deriving (Eq, Show, Generic)

data InvokeBindingRequestPayload a = InvokeBindingRequestPayload
  { bindingMetadata :: ExtendedMetadata,
    bindingData :: a,
    bindingOperation :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (InvokeBindingRequestPayload a) where
  toJSON = customToJSON 7

mapInvokeBindingRequestToPayload :: InvokeBindingRequest a -> InvokeBindingRequestPayload a
mapInvokeBindingRequestToPayload InvokeBindingRequest{..} = InvokeBindingRequestPayload{..}
