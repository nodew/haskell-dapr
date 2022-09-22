-- |
-- Module      : Dapr.Core.Types.ServiceInvocation
-- Description : Defines the types used by ServiceInvocation module
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines the types used by ServiceInvocation module.
module Dapr.Core.Types.ServiceInvocation where

import Dapr.Core.Types.Common (RemoteApp)
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)

-- | 'InvokeServiceRequest' represents the request message for Service invocation.
data InvokeServiceRequest method a = InvokeServiceRequest
  { -- | Http method, POST, PUT, GET, DELETE, etc
    httpMethod :: method,
    -- | Callee's app id.
    remoteApp :: RemoteApp,
    -- | The path which will be invoked by caller.
    -- >>> ["api", "method"] => /api/method
    requestMethod :: [Text],
    -- | The message which will be delivered to callee.
    reqeustData :: a,
    -- | The type of data content.
    requestContentType :: Maybe Text
  }

-- | 'InvokeResponse' represents the response of Service invocation.
data InvokeResponse = InvokeResponse
  { -- | Response data as byte string
    responseData :: L.ByteString,
    -- | The content type of response data
    responseContentType :: Text
  }
