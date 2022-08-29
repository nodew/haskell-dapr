{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dapr.Client.HttpClient.StateManagement where

import Dapr.Client.HttpClient.Internal
import Dapr.Client.HttpClient.Req
import Dapr.Common
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Req
import RIO

saveState :: (MonadIO m, ToJSON a) => DaprConfig -> Text -> [SaveStateRequest a] -> m (Either DaprClientError ())
saveState config store body = do
  let url = ["state", store]
      options = header "Content-Type" "application/json"
  response <- makeHttpRequest config POST url (ReqBodyJson body) ignoreResponse options
  return $ bimap DaprHttpException (const ()) response

saveSingleState :: (MonadIO m, ToJSON a) => DaprConfig -> Text -> SaveStateRequest a -> m (Either DaprClientError ())
saveSingleState config store body = saveState config store [body]

getState ::
  (MonadIO m, FromJSON a) =>
  DaprConfig ->
  Text ->
  Text ->
  Maybe ConsistencyMode ->
  Maybe RequestMetadata ->
  m (Either DaprClientError a)
getState config store key consistency metadata = do
  let url = ["state", store, key]
      metadataQueryParam = mapMetadataToQueryParam metadata
      options = metadataQueryParam <> queryParam "consistency" (show <$> consistency)
  response <- makeHttpRequest config GET url NoReqBody lbsResponse options
  return $ case response of
    Right response' -> case responseStatusCode response' of
      200 -> mapLeft (JsonDecodeError . T.pack) $ eitherDecode (responseBody response')
      204 -> Left NotFound
      _ -> Left UnknownError
    Left e -> Left $ DaprHttpException e

getStateSimple :: (MonadIO m, FromJSON a) => DaprConfig -> Text -> Text -> m (Either DaprClientError a)
getStateSimple config store key = getState config store key Nothing Nothing

getBulkState ::
  (MonadIO m, FromJSON a) =>
  DaprConfig ->
  Text ->
  [Text] ->
  Maybe Int ->
  Maybe RequestMetadata ->
  m (Either DaprClientError [BulkStateItem a])
getBulkState config store keys parallelism metadata = do
  let url = ["state", store, "bulk"]
      metadataQueryParam = mapMetadataToQueryParam metadata
      options = metadataQueryParam <> header "Content-Type" "application/json"
  response <- makeHttpRequest config POST url (ReqBodyJson (BulkStateRequest keys parallelism)) jsonResponse options
  return $ bimap DaprHttpException responseBody response

getBulkStateSimple ::
  (MonadIO m, FromJSON a) =>
  DaprConfig ->
  Text ->
  [Text] ->
  m (Either DaprClientError [BulkStateItem a])
getBulkStateSimple config store keys = getBulkState config store keys Nothing Nothing

deleteState ::
  (MonadIO m) =>
  DaprConfig ->
  Text ->
  Text ->
  Maybe Text ->
  Maybe ConcurrencyMode ->
  Maybe ConsistencyMode ->
  Maybe RequestMetadata ->
  m (Either DaprClientError ())
deleteState config store key etag concurrency consistency metadata = do
  let url = ["state", store, key]
      metadataQueryParam = mapMetadataToQueryParam metadata
      params =
        metadataQueryParam
          <> queryParam "concurrency" (show <$> concurrency)
          <> queryParam "consistency" (show <$> consistency)
      options = maybe mempty (header "If-Match" . T.encodeUtf8) etag <> params
  response <- makeHttpRequest config DELETE url NoReqBody ignoreResponse options
  return $ bimap DaprHttpException (const ()) response

deleteStateSimple :: MonadIO m => DaprConfig -> Text -> Text -> m (Either DaprClientError ())
deleteStateSimple config store key = deleteState config store key Nothing Nothing Nothing Nothing

executeStateTransaction ::
  (MonadIO m, ToJSON a) =>
  DaprConfig ->
  Text ->
  StateTransaction a ->
  m (Either DaprClientError ())
executeStateTransaction config store transaction = do
  let url = ["state", store, "transaction"]
      options = header "Content-Type" "application/json"
  response <- makeHttpRequest config POST url (ReqBodyJson transaction) ignoreResponse options
  return $ bimap DaprHttpException (const ()) response

queryState ::
  (MonadIO m, FromJSON a) =>
  DaprConfig ->
  Text ->
  StateQuery ->
  Maybe RequestMetadata ->
  m (Either DaprClientError (StateQueryResponse a))
queryState config store query metadata = do
  let url = ["state", store, "query"]
      metadataQueryParam = mapMetadataToQueryParam metadata
      options = metadataQueryParam <> header "Content-Type" "application/json"
  response <- makeHttpRequest config POST url (ReqBodyJson query) jsonResponse options
  return $ bimap DaprHttpException responseBody response
