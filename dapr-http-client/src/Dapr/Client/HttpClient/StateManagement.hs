{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dapr.Client.HttpClient.StateManagement where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Internal
import Dapr.Client.HttpClient.Req
import Dapr.Client.HttpClient.Types
import Data.Aeson
import Data.Bifunctor (bimap)
import Data.Either.Extra (mapLeft)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Req

saveState :: (MonadIO m, ToJSON a) => DaprConfig -> StateStore -> [SaveStateRequest a] -> m (Either DaprClientError ())
saveState config store body = do
  let url = ["state", getStoreName store]
  response <- makeHttpRequest config POST url (ReqBodyJson body) ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response

saveSingleState :: (MonadIO m, ToJSON a) => DaprConfig -> StateStore -> SaveStateRequest a -> m (Either DaprClientError ())
saveSingleState config store body = saveState config store [body]

getState ::
  (MonadIO m, FromJSON a) =>
  DaprConfig ->
  StateStore ->
  StateKey ->
  Maybe ConsistencyMode ->
  Maybe RequestMetadata ->
  m (Either DaprClientError a)
getState config store key consistency metadata = do
  let url = ["state", getStoreName store, key]
      metadataQueryParam = mapMetadataToQueryParam metadata
      options = metadataQueryParam <> queryParam "consistency" (show <$> consistency)
  response <- makeHttpRequest config GET url NoReqBody lbsResponse options
  return $ case response of
    Right response' -> case responseStatusCode response' of
      200 -> mapLeft (JsonDecodeError . T.pack) $ eitherDecode (responseBody response')
      204 -> Left NotFound
      _ -> Left UnknownError
    Left e -> Left $ DaprHttpException e

getStateSimple :: (MonadIO m, FromJSON a) => DaprConfig -> StateStore -> StateKey -> m (Either DaprClientError a)
getStateSimple config store key = getState config store key Nothing Nothing

getBulkState ::
  (MonadIO m, FromJSON a) =>
  DaprConfig ->
  StateStore ->
  [StateKey] ->
  Maybe Int ->
  Maybe RequestMetadata ->
  m (Either DaprClientError [BulkStateItem a])
getBulkState config store keys parallelism metadata = do
  let url = ["state", getStoreName store, "bulk"]
      metadataQueryParam = mapMetadataToQueryParam metadata
  response <- makeHttpRequest config POST url (ReqBodyJson (BulkStateRequest keys parallelism)) jsonResponse metadataQueryParam
  return $ bimap DaprHttpException responseBody response

getBulkStateSimple ::
  (MonadIO m, FromJSON a) =>
  DaprConfig ->
  StateStore ->
  [StateKey] ->
  m (Either DaprClientError [BulkStateItem a])
getBulkStateSimple config store keys = getBulkState config store keys Nothing Nothing

deleteState ::
  (MonadIO m) =>
  DaprConfig ->
  StateStore ->
  StateKey ->
  ETag ->
  Maybe ConcurrencyMode ->
  Maybe ConsistencyMode ->
  Maybe RequestMetadata ->
  m (Either DaprClientError ())
deleteState config store key etag concurrency consistency metadata = do
  let url = ["state", getStoreName store, key]
      metadataQueryParam = mapMetadataToQueryParam metadata
      params =
        metadataQueryParam
          <> queryParam "concurrency" (show <$> concurrency)
          <> queryParam "consistency" (show <$> consistency)
      options = maybe mempty (header "If-Match" . T.encodeUtf8) etag <> params
  response <- makeHttpRequest config DELETE url NoReqBody ignoreResponse options
  return $ bimap DaprHttpException (const ()) response

deleteStateSimple :: MonadIO m => DaprConfig -> StateStore -> StateKey -> m (Either DaprClientError ())
deleteStateSimple config store key = deleteState config store key Nothing Nothing Nothing Nothing

executeStateTransaction ::
  (MonadIO m, ToJSON a) =>
  DaprConfig ->
  StateStore ->
  StateTransaction a ->
  m (Either DaprClientError ())
executeStateTransaction config store transaction = do
  let url = ["state", getStoreName store, "transaction"]
  response <- makeHttpRequest config POST url (ReqBodyJson transaction) ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response

queryState ::
  (MonadIO m, FromJSON a) =>
  DaprConfig ->
  StateStore ->
  StateQuery ->
  Maybe RequestMetadata ->
  m (Either DaprClientError (StateQueryResponse a))
queryState config store query metadata = do
  let url = ["state", getStoreName store, "query"]
      metadataQueryParam = mapMetadataToQueryParam metadata
  response <- makeHttpRequest config POST url (ReqBodyJson query) jsonResponse metadataQueryParam
  return $ bimap DaprHttpException responseBody response
