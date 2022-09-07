module Dapr.Client.HttpClient.Types.Binding where
import Data.Aeson
import Dapr.Client.HttpClient.Types.Internal
import GHC.Generics (Generic)
import Data.Map as Map

newtype Metadata = Metadata (Map String String) deriving (Eq, Show, Generic, ToJSON)

data BindingRequest a = BindingRequest 
  {
    bindingMetadata :: Metadata,
    bindingData :: a,
    bindingOperation :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (BindingRequest a) where
  toJSON = customToJSON 7