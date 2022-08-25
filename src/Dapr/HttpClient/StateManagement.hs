{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dapr.HttpClient.StateManagement where

import Dapr.HttpClient.Core
import Dapr.HttpClient.Internal
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Req
import RIO
import RIO.Map (foldlWithKey)

type StateMetadata = Map Text Text

data ConcurrencyMode = FirstWrite | LastWrite deriving (Eq)

instance Show ConcurrencyMode where
  show FirstWrite = "first-write"
  show LastWrite = "last-write"

instance ToJSON ConcurrencyMode where
  toJSON = Data.Aeson.String . T.pack . show

data ConsistencyMode = Strong | Eventual deriving (Eq)

instance Show ConsistencyMode where
  show Strong = "strong"
  show Eventual = "eventual"

instance ToJSON ConsistencyMode where
  toJSON = Data.Aeson.String . T.pack . show

data SaveStateOptions = SaveStateOptions
  { concurrency :: ConcurrencyMode,
    consistency :: ConsistencyMode
  }
  deriving (Eq, Show, Generic, ToJSON)

data SaveStateReqBody a = SaveStateReqBody
  { stateKey :: Text,
    stateValue :: a,
    stateEtag :: Maybe Text,
    stateOptions :: Maybe SaveStateOptions,
    stateMetadata :: Maybe StateMetadata
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (SaveStateReqBody a) where
  toJSON = customToJSON 5

mkSimleSaveStateReqBody :: Text -> a -> SaveStateReqBody a
mkSimleSaveStateReqBody key value = SaveStateReqBody key value Nothing Nothing Nothing

data BulkStateReqBody = BulkStateReqBody
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

data Operation = Upsert | Delete deriving (Eq)

instance Show Operation where
  show Upsert = "upsert"
  show Delete = "delete"

instance ToJSON Operation where
  toJSON = Data.Aeson.String . T.pack . show

data StateOperationRequest a = StateOperationRequest
  { stateKey :: Text,
    stateValue :: Maybe a,
    stateEtag :: Maybe Text,
    stateOptions :: Maybe SaveStateOptions,
    stateMetadata :: Maybe StateMetadata
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (StateOperationRequest a) where
  toJSON = customToJSON 5

data StateOperation a = StateOperation
  { operation :: Operation,
    request :: StateOperationRequest a
  }
  deriving (Eq, Show, Generic, ToJSON)

data StateTransaction a = StateTransaction
  { operations :: [StateOperation a],
    metadata :: Maybe StateMetadata
  }
  deriving (Eq, Show, Generic, ToJSON)

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
    metadata :: Maybe StateMetadata
  }
  deriving (Eq, Show, Generic, FromJSON)

{-------------------------------------------------- Operations --------------------------------------------------------}

saveState :: (MonadIO m, ToJSON a) => DaprClientConfig -> Text -> [SaveStateReqBody a] -> m (Either DaprClientError ())
saveState config store body = do
  let url = ["state", store]
      options = header "Content-Type" "application/json"
  response <- makeRequest config POST url (ReqBodyJson body) ignoreResponse options
  return $ bimap DaprHttpException (const ()) response

saveSingleState :: (MonadIO m, ToJSON a) => DaprClientConfig -> Text -> SaveStateReqBody a -> m (Either DaprClientError ())
saveSingleState config store body = saveState config store [body]

getState ::
  (MonadIO m, FromJSON a) =>
  DaprClientConfig ->
  Text ->
  Text ->
  Maybe ConsistencyMode ->
  Maybe StateMetadata ->
  m (Either DaprClientError a)
getState config store key consistency metadata = do
  let url = ["state", store, key]
      metadataQueryParam =
        maybe
          mempty
          (foldlWithKey (\query key' value -> query <> queryParam key' (Just value)) mempty)
          metadata
      options = metadataQueryParam <> queryParam "consistency" (show <$> consistency)
  response <- makeRequest config GET url NoReqBody lbsResponse options
  return $ case response of
    Right response' -> case responseStatusCode response' of
      200 -> mapLeft (AesonDecodeError . T.pack) $ eitherDecode (responseBody response')
      204 -> Left NotFound
      _ -> Left UnknownError
    Left e -> Left $ DaprHttpException e

getStateSimple :: (MonadIO m, FromJSON a) => DaprClientConfig -> Text -> Text -> m (Either DaprClientError a)
getStateSimple config store key = getState config store key Nothing Nothing

getBulkState ::
  (MonadIO m, FromJSON a) =>
  DaprClientConfig ->
  Text ->
  [Text] ->
  Maybe Int ->
  Maybe StateMetadata ->
  m (Either DaprClientError [BulkStateItem a])
getBulkState config store keys parallelism metadata = do
  let url = ["state", store, "bulk"]
      metadataQueryParam =
        maybe
          mempty
          (foldlWithKey (\query key' value -> query <> queryParam key' (Just value)) mempty)
          metadata
      options = metadataQueryParam <> header "Content-Type" "application/json"
  response <- makeRequest config POST url (ReqBodyJson (BulkStateReqBody keys parallelism)) jsonResponse options
  return $ bimap DaprHttpException responseBody response

getBulkStateSimple ::
  (MonadIO m, FromJSON a) =>
  DaprClientConfig ->
  Text ->
  [Text] ->
  m (Either DaprClientError [BulkStateItem a])
getBulkStateSimple config store keys = getBulkState config store keys Nothing Nothing

deleteState ::
  (MonadIO m) =>
  DaprClientConfig ->
  Text ->
  Text ->
  Maybe Text ->
  Maybe ConcurrencyMode ->
  Maybe ConsistencyMode ->
  Maybe StateMetadata ->
  m (Either DaprClientError ())
deleteState config store key etag concurrency consistency metadata = do
  let url = ["state", store, key]
      metadataQueryParam =
        maybe
          mempty
          (foldlWithKey (\query key' value -> query <> queryParam key' (Just value)) mempty)
          metadata
      params =
        metadataQueryParam
          <> queryParam "concurrency" (show <$> concurrency)
          <> queryParam "consistency" (show <$> consistency)
      options = maybe mempty (header "If-Match" . T.encodeUtf8) etag <> params
  response <- makeRequest config DELETE url NoReqBody ignoreResponse options
  return $ bimap DaprHttpException (const ()) response

deleteStateSimple :: MonadIO m => DaprClientConfig -> Text -> Text -> m (Either DaprClientError ())
deleteStateSimple config store key = deleteState config store key Nothing Nothing Nothing Nothing

excuteStateTransaction ::
  (MonadIO m, ToJSON a) =>
  DaprClientConfig ->
  Text ->
  StateTransaction a ->
  m (Either DaprClientError ())
excuteStateTransaction config store transaction = do
  let url = ["state", store, "transaction"]
      options = header "Content-Type" "application/json"
  response <- makeRequest config POST url (ReqBodyJson transaction) ignoreResponse options
  return $ bimap DaprHttpException (const ()) response

queryState ::
  (MonadIO m, FromJSON a) =>
  DaprClientConfig ->
  Text ->
  Text ->
  Maybe StateMetadata ->
  m (Either DaprClientError (StateQueryResponse a))
queryState config store jsonQuery metadata = do
  let url = ["state", store, "query"]
      metadataQueryParam =
        maybe
          mempty
          (foldlWithKey (\query key' value -> query <> queryParam key' (Just value)) mempty)
          metadata
      options = metadataQueryParam <> header "Content-Type" "application/json"
  response <- makeRequest config POST url (ReqBodyBs $ T.encodeUtf8 jsonQuery) jsonResponse options
  return $ bimap DaprHttpException responseBody response
