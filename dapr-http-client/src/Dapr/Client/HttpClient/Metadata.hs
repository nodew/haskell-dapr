-- |
-- Module      : Metadata
-- Description : Provides information about the sidecar allowing runtime discoverability. It also allows you to store additional attributes in the format of key-value pairs.
-- Copyright   : (c)
-- License     : Apache-2.0
-- This module provides information about the sidecar allowing runtime discoverability. It also allows you to store additional attributes in the format of key-value pairs.
module Dapr.Client.HttpClient.Metadata where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Req
import Dapr.Client.HttpClient.Types
import Data.Bifunctor (bimap)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Req

-- | Gets the Dapr sidecar information provided by the metadata endpoint.
getMetadata :: MonadIO m => DaprConfig -> m (Either DaprClientError DaprMetadata)
getMetadata config = do
  response <- makeHttpRequest config GET ["metadata"] NoReqBody jsonResponse mempty
  return $ bimap DaprHttpException responseBody response

-- | Adds a custom label to the Dapr sidecar information stored by the metadata endpoint.
addCustomLabel :: MonadIO m => DaprConfig -> MetadataAttribute -> RawData -> m (Either DaprClientError ())
addCustomLabel config attribute rawData = do
  let url = ["metadata", attribute]
      options = header "Content-Type" "text/plain"
  response <- makeHttpRequest config PUT url (ReqBodyBs $ encodeUtf8 rawData) ignoreResponse options
  return $ bimap DaprHttpException (const ()) response
