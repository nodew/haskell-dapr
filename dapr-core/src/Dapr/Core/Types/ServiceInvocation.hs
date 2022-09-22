-- |
-- Module      : Dapr.Core.Types.ServiceInvocation
-- Description : Defines the types used by ServiceInvocation module
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines the types used by ServiceInvocation module.
module Dapr.Core.Types.ServiceInvocation where

import Dapr.Core.Types.Common (RemoteApp)
import Data.Text (Text)
import qualified Data.ByteString.Lazy as L


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

data InvokeResponse = InvokeResponse
  { responseData :: L.ByteString,
    responseContentType :: Text
  }
