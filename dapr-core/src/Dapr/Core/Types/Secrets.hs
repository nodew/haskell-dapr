-- |
-- Module      : Dapr.Core.Types.Secrets
-- Description : Defines the types used by Secrets module
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines the types used by Secrets module.
module Dapr.Core.Types.Secrets where

import Dapr.Core.Types.Common (ExtendedMetadata)
import Data.Map
import Data.Text (Text)

-- | 'SecretStore' represents secret store.
newtype SecretStore = SecretStore {getSecretStoreName :: Text}

-- | 'SecretKey' represents secret key
newtype SecretKey = SecretKey {getSecretKey :: Text}

-- | 'GetSecretRequest' is the message to get secret from secret store.
data GetSecretRequest = GetSecretRequest
  { -- | The secret store.
    secretStore :: SecretStore,
    -- | The secret key.
    secretKey :: SecretKey,
    -- | The metadata which will be sent to secret store components.
    secretMetadata :: ExtendedMetadata
  }

type Secrets = Map Text Text

newtype GetSecretResponse = GetSecretResponse
  { results :: Secrets
  }

-- | 'GetBulkSecretRequest' is the message to get the secrets from secret store.
data GetBulkSecretRequest = GetBulkSecretRequest
  { -- | The secret store.
    secretStore :: SecretStore,
    -- | The metadata which will be sent to secret store components.
    secretMetadata :: ExtendedMetadata
  }

newtype GetBulkSecretResponse = GetBulkSecretResponse
  { results :: Map Text Secrets
  }
