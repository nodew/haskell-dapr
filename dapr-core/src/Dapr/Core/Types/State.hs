{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module      : Dapr.Core.Types.State
-- Description : Defines the types used by State module
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines the types used by State module.
module Dapr.Core.Types.State where

import Dapr.Core.Types.Common
import Dapr.Core.Types.Internal
import Data.Aeson
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | 'StateStore' is the name of state store.
newtype StateStore = StateStore {getStoreName :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | 'GetStateRequest' is the message to get key-value states from specific state store.
data GetStateRequest = GetStateRequest
  { -- | The name of state store.
    stateStore :: StateStore,
    -- | The key of the desired state
    stateKey :: StateKey,
    -- | The read consistency of the state store.
    stateConsitency :: Maybe ConsistencyMode,
    -- | The metadata which will be sent to state store components.
    stateMetadata :: ExtendedMetadata
  }
  deriving (Eq, Show)

-- | 'GetBulkStateRequest' is the message to get a list of key-value states from specific state store.
data GetBulkStateRequest = GetBulkStateRequest
  { -- | The name of state store.
    stateStore :: StateStore,
    -- | The keys to get
    stateKeys :: [StateKey],
    -- | The number of parallel operations executed on the state store for a get operation.
    stateParallelism :: Int,
    -- | The metadata which will be sent to state store components.
    stateMetadata :: ExtendedMetadata
  }
  deriving (Eq, Show)

-- | 'BulkStateItem' is the response item for a bulk get operation.
data BulkStateItem a = BulkStateItem
  { -- | State item key
    stateItemKey :: StateKey,
    -- | State data
    stateItemData :: Maybe a,
    -- | The entity tag which represents the specific version of data. ETag format is defined by the corresponding data store.
    stateItemEtag :: Maybe Etag,
    -- | The error that was returned from the state store in case of a failed get operation.
    stateItemError :: Maybe Text,
    -- | The metadata which will be sent to app.
    stateItemMetadata :: Maybe ExtendedMetadata
  }
  deriving (Eq, Show, Generic)

instance FromJSON a => FromJSON (BulkStateItem a) where
  parseJSON = customParseJSON 9

newtype GetBulkStateResponse a = GetBulkStateResponse
  { items :: [BulkStateItem a]
  }

data GetStateResponse a = GetStateResponse
  { stateData :: a,
    stateEtag :: Maybe Etag,
    stateMetadata :: ExtendedMetadata
  }
  deriving (Eq, Show)

data DeleteStateRequest = DeleteStateRequest
  { stateStore :: StateStore,
    stateKey :: StateKey,
    stateEtag :: Maybe Etag,
    stateOption :: Maybe StateOption,
    stateMetadata :: ExtendedMetadata
  }
  deriving (Eq, Show)

data DeleteBulkStateRequest a = DeleteBulkStateRequest
  { stateStore :: StateStore,
    stateItems :: [StateItem a]
  }
  deriving (Eq, Show)

data SaveStateRequest a = SaveStateRequest
  { stateStore :: StateStore,
    stateItems :: [StateItem a]
  }
  deriving (Eq, Show)

-- | 'TransactionalStateOperation' is the message to execute a specified operation with a key-value pair.
data TransactionalStateOperation a = TransactionalStateOperation
  { -- | The type of operation to be executed
    transactionOperation :: TransactionOperation,
    -- | State values to be operated on
    transactionRequest :: StateItem a
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (TransactionalStateOperation a) where
  toJSON = customToJSON 11

-- | 'ExecuteStateTransactionRequest' is the message to execute multiple operations on a specified store.
data ExecuteStateTransactionRequest a = ExecuteStateTransactionRequest
  { stateStore :: StateStore,
    stateOperations :: [TransactionalStateOperation a],
    stateMetadata :: ExtendedMetadata
  }
  deriving (Eq, Show)

data QueryStateRequest = QueryStateRequest
  { stateStore :: StateStore,
    stateQuery :: StateQuery,
    stateMetadata :: ExtendedMetadata
  }
  deriving (Eq, Show)

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

data QueryStateItem a = QueryStateItem
  { stateKey :: StateKey,
    stateData :: a,
    stateEtag :: Maybe Etag,
    stateError :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON a => FromJSON (QueryStateItem a) where
  parseJSON = customParseJSON 4

data QueryStateResponse a = QueryStateResponse
  { results :: [QueryStateItem a],
    token :: Text,
    metadata :: ExtendedMetadata
  }
  deriving (Eq, Show, Generic, FromJSON)
