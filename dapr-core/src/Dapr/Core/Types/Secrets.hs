module Dapr.Core.Types.Secrets where

import Dapr.Core.Types.Common (ExtendedMetadata)
import Data.Map
import Data.Text (Text)

newtype SecretStore = SecretStore {getSecretStoreName :: Text}

newtype SecretKey = SecretKey {getSecretKey :: Text}

data GetSecretRequest = GetSecretRequest
  { secretStore :: SecretStore,
    secretKey :: SecretKey,
    secretMetadata :: ExtendedMetadata
  }

type Secrets = Map Text Text

newtype GetSecretResponse = GetSecretResponse
  { results :: Secrets
  }

data GetBulkSecretRequest = GetBulkSecretRequest
  { secretStore :: SecretStore,
    secretMetadata :: ExtendedMetadata
  }

newtype GetBulkSecretResponse = GetBulkSecretResponse
  { results :: Map Text Secrets
  }
