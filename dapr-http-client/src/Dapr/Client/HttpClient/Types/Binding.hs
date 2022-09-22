-- |
-- Module      : Types.Binding
-- Description : Defines the types used by OutputBinding
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines the types used in OutputBinding.
module Dapr.Client.HttpClient.Types.Binding where

import Dapr.Client.HttpClient.Types.Core
import Dapr.Client.HttpClient.Types.Internal
import Data.Aeson
import Data.Text
import GHC.Generics (Generic)

-- | Name of the Binding
newtype Binding = Binding {getBindingName :: Text}

-- | Represents request used to invoke a binding
data BindingRequest a = BindingRequest
  { -- | A collection of metadata key-value pairs that will be provided to the binding
    bindingMetadata :: RequestMetadata,
    -- | The data to be sent in the binding request
    bindingData :: a,
    -- | The type of operation to perform on the binding
    bindingOperation :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (BindingRequest a) where
  toJSON = customToJSON 7
