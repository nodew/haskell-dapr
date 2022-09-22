{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Dapr.Core.Types.Internal
-- Description : Defines some of the internal types and methods used by other module
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines some of the internal types and methods used by other module
module Dapr.Core.Types.Internal where

import Data.Aeson
  ( GFromJSON,
    GToJSON',
    Options (fieldLabelModifier),
    Value,
    Zero,
    defaultOptions,
    genericParseJSON,
    genericToJSON,
  )
import Data.Aeson.Types (Parser)
import Data.Char (toLower)
import GHC.Generics (Generic (Rep))

-- | Converts the given string's first letter to lower case
lowerFirstLetter :: String -> String
lowerFirstLetter [] = []
lowerFirstLetter (x : xs) = toLower x : xs

-- | Drops first \'n\' characters in the given string
dropFirstNChars :: Int -> [Char] -> String
dropFirstNChars n = lowerFirstLetter . drop n

-- | Defines custom JSON parsing logic
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

-- | Defines custom JSON conversion logic
customToJSON :: (Generic a, GToJSON' Value Zero (Rep a)) => Int -> a -> Value
customToJSON n =
  genericToJSON
    defaultOptions
      { fieldLabelModifier = dropFirstNChars n
      }
