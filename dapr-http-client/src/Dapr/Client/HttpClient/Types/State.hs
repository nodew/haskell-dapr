-- |
-- Module      : Types.State
-- Description : Defines the types used in State
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines the types used in State.
module Dapr.Client.HttpClient.Types.State where

import Dapr.Client.HttpClient.Types.Core
import Dapr.Client.HttpClient.Types.Internal
import Data.Aeson
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Name of the Dapr state store
newtype StateStore = StateStore {getStoreName :: Text}

-- | Options when performing state operations with Dapr
data SaveStateOptions = SaveStateOptions
  { -- | Concurrency Mode for state operations with Dapr
    concurrency :: ConcurrencyMode,
    -- | ConsistencyMode for state operations with Dapr
    consistency :: ConsistencyMode
  }
  deriving (Eq, Show, Generic, ToJSON)

-- | Represents a State Key
type StateKey = Text

-- | Represents an Etag
type ETag = Maybe Text

-- | Create a Save State request
data SaveStateRequest a = SaveStateRequest
  { -- | The state key
    stateKey :: StateKey,
    -- | The state value
    stateValue :: a,
    -- | An Etag
    stateEtag :: ETag,
    -- | Options for the save state request
    stateOptions :: Maybe SaveStateOptions,
    -- | A collection of metadata key-value pairs that will be provided to the state store. The valida metadata keys and values are determined by the type of state store used.
    stateMetadata :: Maybe RequestMetadata
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (SaveStateRequest a) where
  toJSON = customToJSON 5

-- | Creates a simple `SaveStateRequest` without `ETag`, `SaveStateOptions` and `RequestMetadata`
makeSimpleSaveStateRequest :: Text -> a -> SaveStateRequest a
makeSimpleSaveStateRequest key value = SaveStateRequest key value Nothing Nothing Nothing

-- | Represents the request format for a bulk state request with given list of state keys.
data BulkStateRequest = BulkStateRequest
  { -- | List of StateKeys
    keys :: [StateKey],
    -- | The number of concurrent get operations the Dapr runtime will issue to the state store. A value equal to or smaller than 0 means max parallelism.
    parallelism :: Maybe Int
  }
  deriving (Eq, Show, Generic, ToJSON)

-- | Represents bulk state items
data BulkStateItem a = BulkStateItem
  { -- | The key of the returned item
    itemKey :: Text,
    -- | The value of the returned item
    itemData :: Maybe a,
    -- | The Etag of the returned item
    itemEtag :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON a => FromJSON (BulkStateItem a) where
  parseJSON = customParseJSON 4

-- | Operation type for state operations with Dapr.
data StateOperationType
  = -- | Upsert a new or existing state
    Upsert
  | -- | Delete a State
    Delete
  deriving (Eq)

instance Show StateOperationType where
  show Upsert = "upsert"
  show Delete = "delete"

instance ToJSON StateOperationType where
  toJSON = Data.Aeson.String . T.pack . show

-- | Creates a new state operation request
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

-- | Creates a new State operation
data StateOperation a = StateOperation
  { -- | The Operatoin type of the request
    operation :: StateOperationType,
    -- | The actual request of the state operation
    request :: StateOperationRequest a
  }
  deriving (Eq, Show, Generic, ToJSON)

-- | Creates a new State Transaction
data StateTransaction a = StateTransaction
  { -- | List of State Operations
    transactionOperations :: [StateOperation a],
    -- | Request Metadata (optional)
    transactionMetadata :: Maybe RequestMetadata
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (StateTransaction a) where
  toJSON = customToJSON 11

-- | Represents an individual item from the results of state query
data StateQueryItem a = StateQueryItem
  { -- | The key of the returned item from the matched query
    itemKey :: Text,
    -- | The value of the returned item from the matched query
    itemData :: Maybe a,
    -- | The Etag of the returned item from the matched query
    itemEtag :: Maybe Text,
    -- | The error, if one occurred, of the returned item
    itemError :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON a => FromJSON (StateQueryItem a) where
  parseJSON = customParseJSON 4

-- | Represents the response from a State Query.
data StateQueryResponse a = StateQueryResponse
  { -- | List of State Query items
    results :: [StateQueryItem a],
    -- | The pagination token to continue the query
    token :: Text,
    -- | The metadata to be passed back to the caller
    metadata :: Maybe RequestMetadata
  }
  deriving (Eq, Show, Generic, FromJSON)

-- | Represents Sort Order of the query results
data StateQueryOrder
  = -- | Ascending
    StateQueryOrderAsc
  | -- | Descending
    StateQueryOrderDesc
  deriving (Eq)

instance Show StateQueryOrder where
  show StateQueryOrderAsc = "ASC"
  show StateQueryOrderDesc = "DESC"

instance ToJSON StateQueryOrder where
  toJSON = Data.Aeson.String . T.pack . show

-- | Represents the ordering of the query results by a given key
data StateQuerySort = StateQuerySort
  { stateOrderKey :: Text,
    stateOrderOrder :: Maybe StateQueryOrder
  }
  deriving (Eq, Show, Generic)

instance ToJSON StateQuerySort where
  toJSON = customToJSON 10

-- | Query filter clauses
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

-- | Represents pagination parameters for the query
data StateQueryPagination = StateQueryPagination
  { -- | Limit on the no of results per page
    paginationLimit :: Int,
    -- | The pagination token to continue the query
    paginationToken :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON StateQueryPagination where
  toJSON = customToJSON 10

-- | Represents the query format to the dapr state api
data StateQuery = StateQuery
  { -- | The Query filters that should be part of the request
    stateQueryFilter :: StateQueryFilter,
    -- | List of Sort order and sort key information
    stateQuerySort :: [StateQuerySort],
    -- | Parameters to represent Pagination options
    stateQueryPage :: StateQueryPagination
  }
  deriving (Eq, Show, Generic)

instance ToJSON StateQuery where
  toJSON = customToJSON 10
