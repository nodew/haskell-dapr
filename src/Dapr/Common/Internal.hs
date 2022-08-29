{-# LANGUAGE FlexibleContexts #-}
module Dapr.Common.Internal where

import Data.Aeson
import Data.Aeson.Types
import Data.Char (toLower)
import GHC.Generics
import RIO

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
