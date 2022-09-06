module Dapr.Common.Secrets where

import Data.Map (Map)
import Data.Text (Text)

type Secrets = Map Text Text

type BulkSecrets = Map Text Secrets
