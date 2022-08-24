{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Dapr.HttpClient.StateManagement where

import Dapr.HttpClient.Core
import Dapr.HttpClient.Internal
import Data.Aeson
import qualified Data.Text as T
import Network.HTTP.Req
import RIO
import RIO.Map (foldlWithKey)

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
  { ssoConcurrency :: Text,
    ssoConsistency :: ConsistencyMode
  }
  deriving (Eq, Show, Generic)

instance ToJSON SaveStateOptions where
  toJSON = customToJSON 3

data SaveStateReqBody a = SaveStateReqBody
  { ssrKey :: Text,
    ssrValue :: a,
    ssrEtag :: Maybe Text,
    ssrOptions :: Maybe SaveStateOptions,
    ssrMetadata :: Maybe (Map Text Text)
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (SaveStateReqBody a) where
  toJSON = customToJSON 3

mkSimleSaveStateReqBody :: Text -> a -> SaveStateReqBody a
mkSimleSaveStateReqBody key value = SaveStateReqBody key value Nothing Nothing Nothing

data BulkStateReqBody = BulkStateReqBody
  { bsrKeys :: [Text],
    bsrParallelism :: Maybe Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON BulkStateReqBody where
  toJSON (BulkStateReqBody keys parallelism) =
    object
      [ "keys" .= keys,
        "parallelism" .= parallelism
      ]

data BulkStateItem a = BulkStateItem
  { bulkStateItemKey :: Text,
    bulkStateItemData :: Maybe a,
    bulkStateItemEtag :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON a => FromJSON (BulkStateItem a) where
  parseJSON = customParseJSON 13

saveState :: (MonadIO m, ToJSON a) => DaprClientConfig -> Text -> [SaveStateReqBody a] -> m (Either DaprClientError ())
saveState config store body = do
  let url = "state/" <> store
      options = header "Content-Type" "application/json"
  response <- makeRequest config POST url (ReqBodyJson body) ignoreResponse options
  return $ case responseStatusCode response of
    204 -> Right ()
    400 -> Left $ DaprClientError (HttpException 400) "State store is missing or misconfigured or malformed request"
    500 -> Left $ DaprClientError (HttpException 500) "Failed to save state"
    _ -> Left $ DaprClientError UnknownError "Unknown response status code"

saveSingleState :: (MonadIO m, ToJSON a) => DaprClientConfig -> Text -> SaveStateReqBody a -> m (Either DaprClientError ())
saveSingleState config store body = saveState config store [body]

getState :: (MonadIO m, FromJSON a) => DaprClientConfig -> Text -> Text -> Maybe ConsistencyMode -> Maybe (Map Text Text) -> m (Either DaprClientError a)
getState config store key consistency metadata = do
  let url = "state" <> "/" <> store <> "/" <> key
      metadataQueryParam = maybe mempty (foldlWithKey (\query key' value -> query <> key' =: Just value) mempty) metadata
      options = metadataQueryParam <> "consistency" =: (show <$> consistency)
  response <- makeRequest config GET url NoReqBody lbsResponse options
  return $ case responseStatusCode response of
    200 -> mapLeft (DaprClientError AesonDecodeError . T.pack) $ eitherDecode (responseBody response)
    204 -> Left $ DaprClientError (HttpException 204) "Key is not found"
    400 -> Left $ DaprClientError (HttpException 400) "State store is missing or misconfigured"
    500 -> Left $ DaprClientError (HttpException 500) "Get state failed"
    _ -> Left $ DaprClientError UnknownError "Unknown response status code"

getStateSimple :: (MonadIO m, FromJSON a) => DaprClientConfig -> Text -> Text -> m (Either DaprClientError a)
getStateSimple config store key = getState config store key Nothing Nothing

getBulkState :: (MonadIO m, FromJSON a) => DaprClientConfig -> Text -> [Text] -> Maybe Int -> Maybe (Map Text Text) -> m (Either DaprClientError [BulkStateItem a])
getBulkState config store keys parallelism metadata = do
  let url = "state" <> "/" <> store <> "/" <> "bulk"
      metadataQueryParam = maybe mempty (foldlWithKey (\query key' value -> query <> key' =: Just value) mempty) metadata
      options = metadataQueryParam <> header "Content-Type" "application/json"
  response <- makeRequest config POST url (ReqBodyJson (BulkStateReqBody keys parallelism)) jsonResponse options
  return $ case responseStatusCode response of
    200 -> Right (responseBody response)
    400 -> Left $ DaprClientError (HttpException 400) "State store is missing or misconfigured"
    500 -> Left $ DaprClientError (HttpException 500) "Get state failed"
    _ -> Left $ DaprClientError UnknownError "Unknown response status code"

getBulkStateSimple :: (MonadIO m, FromJSON a) => DaprClientConfig -> Text -> [Text] -> m (Either DaprClientError [BulkStateItem a])
getBulkStateSimple config store keys = getBulkState config store keys Nothing Nothing
