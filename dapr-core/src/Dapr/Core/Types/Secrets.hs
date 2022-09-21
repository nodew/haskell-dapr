module Dapr.Core.Types.Secrets where

import Data.Map (Map)
import Data.Text (Text)

newtype SecretStore = SecretStore {getSecretStoreName :: Text}

type SecretName = Text

type Secrets = Map SecretName Text

type BulkSecrets = Map Text Secrets
