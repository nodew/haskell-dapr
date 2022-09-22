{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Internal
-- Description : Defines some of the useful internal methods
-- Copyright   : (c)
-- License     : Apache-2.0
module Dapr.Client.HttpClient.Internal where

import Dapr.Client.HttpClient.Types
import Data.Map.Strict (foldlWithKey)
import Network.HTTP.Req

-- | Internal method to map given `RequestMetadata` to Http query parameter `Option`s.
mapMetadataToQueryParam :: Maybe RequestMetadata -> Option 'Http
mapMetadataToQueryParam =
  maybe
    mempty
    (foldlWithKey (\query key' value' -> query <> queryParam key' (Just value')) mempty)
