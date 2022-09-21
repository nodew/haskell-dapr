module Dapr.Core.Types.ServiceInvocation where

import Dapr.Core.Types.Common
import Data.Text

data InvokeRequest a = InvokeRequest
  { requestMethod :: Text,
    reqeustData :: a,
    requestContentType :: Text,
    httpExtension :: HttpExtension
  }

data InvokeResponse a = InvokeResponse
  { responseData :: a,
    responseContentType :: Text
  }

data InvokeServiceRequest a = InvokeServiceRequest
  { serviceId :: DaprHostedService,
    serviceMessage :: InvokeRequest a
  }
