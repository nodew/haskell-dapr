-- |
-- Module      : Dapr.Core.Types.Binding
-- Description : Defines the types used by Binding module
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines the types used by Binding module.
module Dapr.Core.Types.Binding where

import Dapr.Core.Types.Common (ExtendedMetadata)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | 'Binding' represents the binding component
newtype Binding = Binding {getBindingName :: Text}
  deriving (Eq, Show, Generic)

-- | 'InvokeBindingRequest' is the message to send data to output bindings
data InvokeBindingRequest a = InvokeBindingRequest
  { -- | The name of the output binding to invoke.
    bindingName :: Binding,
    -- | The name of the operation type for the binding to invoke
    bindingOperation :: Text,
    -- | The data which will be sent to output binding.
    bindingData :: a,
    -- | The metadata passing to output binding components
    bindingMetadata :: ExtendedMetadata
  }
  deriving (Eq, Show, Generic)
