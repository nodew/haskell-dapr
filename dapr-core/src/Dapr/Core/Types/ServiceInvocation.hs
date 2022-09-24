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
import Network.HTTP.Types

-- | 'InvokeServiceRequest' represents the request message for Service invocation.
data InvokeServiceRequest m a = InvokeServiceRequest
  { -- | Callee's app id.
    remoteApp :: RemoteApp,
    -- | Http method, POST, PUT, GET, DELETE, etc
    httpMethod :: m,
    -- | The path which will be invoked by caller.
    --
    -- >>> ["api", "method"] => /api/method
    requestEndpoint :: [Text],
    -- | The message which will be delivered to callee.
    reqeustData :: a,
    -- | The type of data content.
    requestContentType :: Maybe Text,
    -- | Optional, query string
    requestQueryString :: Query
  }

-- | 'InvokeResponse' represents the response of Service invocation.
data InvokeResponse = InvokeResponse
  { -- | Response data as byte string
    responseData :: L.ByteString,
    -- | The content type of response data
    responseContentType :: Text
  }
