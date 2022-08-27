{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Dapr.HttpClient.Internal where

import Data.Aeson
import Data.Aeson.Types
import Data.Char (toLower)
import GHC.Generics
import RIO
import Network.HTTP.Req
import RIO.Map hiding(drop)
import Dapr.HttpClient.Core

lowerFirstLetter :: String -> String
lowerFirstLetter [] = []
lowerFirstLetter (x : xs) = toLower x : xs

dropFirstNChars :: Int -> [Char] -> String
dropFirstNChars n = lowerFirstLetter . drop n

customParseJSON ::
  (Generic a, GFromJSON Zero (Rep a)) =>
  Int ->
  Value ->
  Parser a
customParseJSON n =
  genericParseJSON
    defaultOptions
      { fieldLabelModifier = dropFirstNChars n
      }

customToJSON :: (Generic a, GToJSON' Value Zero (Rep a)) => Int -> a -> Value
customToJSON n =
  genericToJSON
    defaultOptions
      { fieldLabelModifier = dropFirstNChars n
      }

mapMetadataToQueryParam :: Maybe Metadata -> Option 'Http
mapMetadataToQueryParam =
  maybe
    mempty
    (foldlWithKey (\query key' value -> query <> queryParam key' (Just value)) mempty)
