{-# LANGUAGE FlexibleContexts #-}

module Dapr.Client.HttpClient.Internal where

import Dapr.Common
import Data.Map.Strict (foldlWithKey)
import Network.HTTP.Req

mapMetadataToQueryParam :: Maybe RequestMetadata -> Option 'Http
mapMetadataToQueryParam =
  maybe
    mempty
    (foldlWithKey (\query key' value -> query <> queryParam key' (Just value)) mempty)
