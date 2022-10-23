-- |
-- Module      : Dapr.Client.HttpClient.StateManagement
-- Description : Manages Dapr state
-- Copyright   : (c)
-- License     : Apache-2.0
-- This module manages Dapr state
module Dapr.Client.HttpClient.StateManagement
  ( getState,
    getBulkState,
    saveState,
    deleteState,
    executeStateTransaction,
    queryState,
  )
where

import Dapr.Client.HttpClient.Internal
import Dapr.Client.HttpClient.Req
import Dapr.Core.Types
import Data.Aeson
import Data.Bifunctor (bimap)
import Data.Either.Extra (mapLeft)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics (Generic)
import Network.HTTP.Req

data BulkStateRequestPayload = BulkStateRequestPayload
  { keys :: [StateKey],
    parallelism :: Int
  }
  deriving (Eq, Show, Generic, ToJSON)

data ExecuteStateTransactionRequestPayload a = ExecuteStateTransactionRequestPayload
  { operations :: [TransactionalStateOperation a],
    metadata :: ExtendedMetadata
  }
  deriving (Eq, Show, Generic, ToJSON)

-- | Tries to save the provided list of `SaveStateRequest`s to the configured Dapr State.
saveState :: (ToJSON a) => SaveStateRequest a -> DaprHttpClient (Either DaprClientError ())
saveState SaveStateRequest {..} = do
  let url = ["state", getStoreName stateStore]
  response <- makeHttpRequest POST url (ReqBodyJson stateItems) ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response

-- | Gets the current value associated with `StateKey` from the configured Dapr State.
getState :: (FromJSON a) => GetStateRequest -> DaprHttpClient (Either DaprClientError a)
getState GetStateRequest {..} = do
  let url = ["state", getStoreName stateStore, getStateKey stateKey]
      metadataQueryParam = mapMetadataToQueryParam stateMetadata
      options = metadataQueryParam <> queryParam "consistency" (show <$> stateConsitency)
  response <- makeHttpRequest GET url NoReqBody lbsResponse options
  return $ case response of
    Right response' -> case responseStatusCode response' of
      200 -> mapLeft (JsonDecodeError . T.pack) $ eitherDecode (responseBody response')
      204 -> Left NotFound
      _ -> Left UnknownError
    Left e -> Left $ DaprHttpException e

-- | Gets the values associated with provided list of `StateKey`s from the configured Dapr State in bulk.
getBulkState :: (FromJSON a) => GetBulkStateRequest -> DaprHttpClient (Either DaprClientError (GetBulkStateResponse a))
getBulkState GetBulkStateRequest {..} = do
  let url = ["state", getStoreName stateStore, "bulk"]
      metadataQueryParam = mapMetadataToQueryParam stateMetadata
  response <- makeHttpRequest POST url (ReqBodyJson (BulkStateRequestPayload stateKeys stateParallelism)) jsonResponse metadataQueryParam
  return $ bimap DaprHttpException (GetBulkStateResponse . responseBody) response

-- | Deletes the value associated with provided `StateKey` from the configured Dapr State.
deleteState :: DeleteStateRequest -> DaprHttpClient (Either DaprClientError ())
deleteState DeleteStateRequest {..} = do
  let url = ["state", getStoreName stateStore, getStateKey stateKey]
      metadataQueryParam = mapMetadataToQueryParam stateMetadata
      params =
        metadataQueryParam
          <> queryParam "concurrency" (show . concurrency <$> stateOption)
          <> queryParam "consistency" (show . consistency <$> stateOption)
      options = maybe mempty (header "If-Match" . T.encodeUtf8 . getEtagValue) stateEtag <> params
  response <- makeHttpRequest DELETE url NoReqBody ignoreResponse options
  return $ bimap DaprHttpException (const ()) response

-- | Saves the provided trasaction of type `StateTransaction` to the configured Dapr store.
executeStateTransaction :: (ToJSON a) => ExecuteStateTransactionRequest a -> DaprHttpClient (Either DaprClientError ())
executeStateTransaction ExecuteStateTransactionRequest {..} = do
  let url = ["state", getStoreName stateStore, "transaction"]
  response <- makeHttpRequest POST url (ReqBodyJson (ExecuteStateTransactionRequestPayload stateOperations stateMetadata)) ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response

-- | Queries the specified state store with the given query of type `StateQuery`. Note that underlying state store must support queries.
queryState ::
  (FromJSON a) =>
  QueryStateRequest ->
  DaprHttpClient (Either DaprClientError (QueryStateResponse a))
queryState QueryStateRequest {..} = do
  let url = ["state", getStoreName stateStore, "query"]
      metadataQueryParam = mapMetadataToQueryParam stateMetadata
  response <- makeHttpRequest POST url (ReqBodyJson stateQuery) jsonResponse metadataQueryParam
  return $ bimap DaprHttpException responseBody response
