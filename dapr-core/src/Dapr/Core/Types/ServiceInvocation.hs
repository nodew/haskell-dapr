-- |
-- Module      : Dapr.Core.Types.ServiceInvocation
-- Description : Defines the types used by ServiceInvocation module
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines the types used by ServiceInvocation module.
module Dapr.Core.Types.ServiceInvocation where

import Dapr.Core.Types.Common (HttpExtension, RemoteAppId)
import Data.Text (Text)

-- | 'InvokeRequest' is the message to invoke a method with the data.
data InvokeRequest a = InvokeRequest
  { -- | The method name which will be invoked by caller.
    requestMethod :: Text,
    -- | The message which caller sent
    reqeustData :: a,
    -- | The type of data content.
    requestContentType :: Text,
    -- | HTTP specific fields if request conveys http-compatible request
    httpExtension :: HttpExtension
  }

data InvokeResponse a = InvokeResponse
  { responseData :: a,
    responseContentType :: Text
  }

-- | 'InvokeServiceRequest' represents the request message for Service invocation.
data InvokeServiceRequest a = InvokeServiceRequest
  { -- | Callee's app id.
    serviceId :: RemoteAppId,
    -- | The message which will be delivered to callee.
    serviceMessage :: InvokeRequest a
  }
