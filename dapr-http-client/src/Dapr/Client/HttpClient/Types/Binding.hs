module Dapr.Client.HttpClient.Types.Binding where

import Dapr.Client.HttpClient.Types.Internal
import Data.Aeson
import Data.Map as Map
import GHC.Generics (Generic)

newtype Metadata = Metadata (Map String String) deriving (Eq, Show, Generic, ToJSON)

data BindingRequest a = BindingRequest
  { bindingMetadata :: Metadata,
    bindingData :: a,
    bindingOperation :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (BindingRequest a) where
  toJSON = customToJSON 7
