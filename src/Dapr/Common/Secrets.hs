module Dapr.Common.Secrets where

import RIO

type Secrets = Map Text Text

type BulkSecrets = Map Text Secrets
