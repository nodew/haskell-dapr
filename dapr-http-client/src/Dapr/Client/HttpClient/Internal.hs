-- |
-- Module      : Dapr.Client.HttpClient.Internal
-- Description : Defines some of the useful internal methods
-- Copyright   : (c)
-- License     : Apache-2.0
module Dapr.Client.HttpClient.Internal where

import Dapr.Core.Types (ExtendedMetadata)
import Data.CaseInsensitive (CI (original))
import Data.Map.Strict (foldlWithKey)
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Req
import Network.HTTP.Types (hContentType)
import Network.HTTP.Types.URI (Query)

mapMetadataToQueryParam :: ExtendedMetadata -> Option scheme
mapMetadataToQueryParam =
  foldlWithKey (\query key' value' -> query <> queryParam ("metadata." <> key') (Just value')) mempty

mapKeysToParam ::
  -- | Key
  Text ->
  -- | Text values
  [Text] ->
  Option scheme
mapKeysToParam _ [] = mempty
mapKeysToParam key (x : xs) = queryParam key (Just x) <> mapKeysToParam key xs

mapQueryToParam :: Query -> Option scheme
mapQueryToParam [] = mempty
mapQueryToParam ((key, value) : xs) = queryParam (decodeUtf8 key) (decodeUtf8 <$> value) <> mapQueryToParam xs

headerContentTypeJSON :: Option scheme
headerContentTypeJSON = header (original hContentType) "application/json"

headerContentTypeText :: Option scheme
headerContentTypeText = header (original hContentType) "text/plain"
