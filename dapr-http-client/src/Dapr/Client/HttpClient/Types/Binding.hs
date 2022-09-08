module Dapr.Client.HttpClient.Types.Binding where

import Dapr.Client.HttpClient.Types.Core
import Dapr.Client.HttpClient.Types.Internal
import Data.Aeson
import GHC.Generics (Generic)

data BindingRequest a = BindingRequest
  { bindingMetadata :: RequestMetadata,
    bindingData :: a,
    bindingOperation :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (BindingRequest a) where
  toJSON = customToJSON 7
