{-# LANGUAGE FlexibleContexts #-}

module Dapr.Client.HttpClient.Internal where

import Dapr.Common
import Network.HTTP.Req
import Data.Map.Strict (foldlWithKey)

mapMetadataToQueryParam :: Maybe RequestMetadata -> Option 'Http
mapMetadataToQueryParam =
  maybe
    mempty
    (foldlWithKey (\query key' value -> query <> queryParam key' (Just value)) mempty)
