-- |
-- Module      : Types.Secrets
-- Description : Defines the types used by Secrets module
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines the types used by Secrets module.
module Dapr.Client.HttpClient.Types.Secrets where

import Data.Map (Map)
import Data.Text (Text)

-- | Represents name of the secret store
newtype SecretStore = SecretStore {getSecretStoreName :: Text}

-- | The name of the secret in the secret store
type SecretName = Text

-- | A collection of key-value pairs of secrets
type Secrets = Map SecretName Text

-- | Represents the secret names as fields and a map of secret keys and values as the field value
type BulkSecrets = Map Text Secrets
