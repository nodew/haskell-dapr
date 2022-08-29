module Dapr.Common.State where

import Dapr.Common.Core
import Dapr.Common.Internal
import Data.Aeson
import qualified Data.Text as T
import RIO

data SaveStateOptions = SaveStateOptions
  { concurrency :: ConcurrencyMode,
    consistency :: ConsistencyMode
  }
  deriving (Eq, Show, Generic, ToJSON)

data SaveStateRequest a = SaveStateRequest
  { stateKey :: Text,
    stateValue :: a,
    stateEtag :: Maybe Text,
    stateOptions :: Maybe SaveStateOptions,
    stateMetadata :: Maybe RequestMetadata
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (SaveStateRequest a) where
  toJSON = customToJSON 5

makeSimpleSaveStateRequest :: Text -> a -> SaveStateRequest a
makeSimpleSaveStateRequest key value = SaveStateRequest key value Nothing Nothing Nothing

data BulkStateRequest = BulkStateRequest
  { keys :: [Text],
    parallelism :: Maybe Int
  }
  deriving (Eq, Show, Generic, ToJSON)

data BulkStateItem a = BulkStateItem
  { itemKey :: Text,
    itemData :: Maybe a,
    itemEtag :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON a => FromJSON (BulkStateItem a) where
  parseJSON = customParseJSON 4

data StateOperationType = Upsert | Delete deriving (Eq)

instance Show StateOperationType where
  show Upsert = "upsert"
  show Delete = "delete"

instance ToJSON StateOperationType where
  toJSON = Data.Aeson.String . T.pack . show

data StateOperationRequest a = StateOperationRequest
  { stateKey :: Text,
    stateValue :: Maybe a,
    stateEtag :: Maybe Text,
    stateOptions :: Maybe SaveStateOptions,
    stateMetadata :: Maybe RequestMetadata
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (StateOperationRequest a) where
  toJSON = customToJSON 5

data StateOperation a = StateOperation
  { operation :: StateOperationType,
    request :: StateOperationRequest a
  }
  deriving (Eq, Show, Generic, ToJSON)

data StateTransaction a = StateTransaction
  { transactionOperations :: [StateOperation a],
    transactionMetadata :: Maybe RequestMetadata
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (StateTransaction a) where
  toJSON = customToJSON 11

data StateQueryItem a = StateQueryItem
  { itemKey :: Text,
    itemData :: Maybe a,
    itemEtag :: Maybe Text,
    itemError :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON a => FromJSON (StateQueryItem a) where
  parseJSON = customParseJSON 4

data StateQueryResponse a = StateQueryResponse
  { results :: [StateQueryItem a],
    token :: Text,
    metadata :: Maybe RequestMetadata
  }
  deriving (Eq, Show, Generic, FromJSON)

data StateQueryOrder = Asc | Desc deriving (Eq)

instance Show StateQueryOrder where
  show Asc = "asc"
  show Desc = "desc"

instance ToJSON StateQueryOrder where
  toJSON = Data.Aeson.String . T.pack . show

data StateQuerySort = StateQuerySort
  { stateOrderKey :: Text,
    stateOrderOrder :: Maybe StateQueryOrder
  }
  deriving (Eq, Show, Generic)

instance ToJSON StateQuerySort where
  toJSON = customToJSON 10

data StateQueryFilter
  = AndFilter [StateQueryFilter]
  | OrFilter [StateQueryFilter]
  | EqFilter (Map Text Text)
  | InFilter (Map Text [Text])
  deriving (Eq, Show)

instance ToJSON StateQueryFilter where
  toJSON (EqFilter a) = object ["EQ" .= a]
  toJSON (InFilter a) = object ["IN" .= a]
  toJSON (AndFilter a) = object ["AND" .= a]
  toJSON (OrFilter a) = object ["OR" .= a]

data StateQueryPagination = StateQueryPagination
  { paginationLimit :: Int,
    paginationToken :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON StateQueryPagination where
  toJSON = customToJSON 10

data StateQuery = StateQuery
  { stateQueryFilter :: StateQueryFilter,
    stateQuerySort :: [StateQuerySort],
    stateQueryPage :: StateQueryPagination
  }
  deriving (Eq, Show, Generic)

instance ToJSON StateQuery where
  toJSON = customToJSON 10
