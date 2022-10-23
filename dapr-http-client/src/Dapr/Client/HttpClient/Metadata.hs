-- |
-- Module      : Dapr.Client.HttpClient.Metadata
-- Description : Provides information about the sidecar allowing runtime discoverability.
-- Copyright   : (c)
-- License     : Apache-2.0
-- This module provides information about the sidecar allowing runtime discoverability. It also allows you to store additional attributes in the format of key-value pairs.
module Dapr.Client.HttpClient.Metadata where

import Dapr.Client.HttpClient.Internal
import Dapr.Client.HttpClient.Req
import Dapr.Core.Types
import Data.Bifunctor (bimap)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Req

-- | Gets the Dapr sidecar information provided by the metadata endpoint.
getMetadata :: DaprHttpClient (Either DaprClientError GetMetadataResponse)
getMetadata = do
  response <- makeHttpRequest GET ["metadata"] NoReqBody jsonResponse mempty
  return $ bimap DaprHttpException responseBody response

-- | Adds a custom label to the Dapr sidecar information stored by the metadata endpoint.
setMetadata :: SetMetadataRequest -> DaprHttpClient (Either DaprClientError ())
setMetadata SetMetadataRequest {..} = do
  let url = ["metadata", key]
  response <- makeHttpRequest PUT url (ReqBodyBs $ encodeUtf8 value) ignoreResponse headerContentTypeText
  return $ bimap DaprHttpException (const ()) response
