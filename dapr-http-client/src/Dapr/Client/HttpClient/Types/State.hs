module Dapr.Client.HttpClient.Types.State where

import Dapr.Client.HttpClient.Types.Core
import Dapr.Client.HttpClient.Types.Internal
import Data.Aeson
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

newtype StateStore = StateStore {getStoreName :: Text}

data SaveStateOptions = SaveStateOptions
  { concurrency :: ConcurrencyMode,
    consistency :: ConsistencyMode
  }
  deriving (Eq, Show, Generic, ToJSON)

type StateKey = Text

type ETag = Maybe Text

data SaveStateRequest a = SaveStateRequest
  { stateKey :: StateKey,
    stateValue :: a,
    stateEtag :: ETag,
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
  { stateKey :: StateKey,
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

data StateQueryOrder = StateQueryOrderAsc | StateQueryOrderDesc deriving (Eq)

instance Show StateQueryOrder where
  show StateQueryOrderAsc = "ASC"
  show StateQueryOrderDesc = "DESC"

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
