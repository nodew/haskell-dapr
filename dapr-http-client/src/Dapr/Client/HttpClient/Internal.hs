-- |
-- Module      : Internal
-- Description : Defines some of the useful internal methods
-- Copyright   : (c)
-- License     : Apache-2.0
module Dapr.Client.HttpClient.Internal where

import Dapr.Core.Types (ExtendedMetadata)
import Data.Map.Strict (foldlWithKey)
import Data.Text
import Network.HTTP.Req

mapMetadataToQueryParam :: ExtendedMetadata -> Option 'Http
mapMetadataToQueryParam =
  foldlWithKey (\query key' value' -> query <> queryParam ("metadata." <> key') (Just value')) mempty

mapKeysToParam ::
  -- | Key
  Text ->
  -- | Text values
  [Text] ->
  Option 'Http
mapKeysToParam _ [] = mempty
mapKeysToParam key (x : xs) = queryParam key (Just x) <> mapKeysToParam key xs
