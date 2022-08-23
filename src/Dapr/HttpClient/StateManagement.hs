{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Dapr.HttpClient.StateManagement where

import Dapr.HttpClient.Core
import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), object)
import Network.HTTP.Req
import RIO

data ConsistencyLevel = Strong | Eventual
  deriving (Eq, Show)

instance ToJSON ConsistencyLevel where
  toJSON Strong = "strong"
  toJSON Eventual = "eventual"

data SaveStateOptions = SaveStateOptions
  { ssoConcurrency :: Text,
    ssoConsistency :: ConsistencyLevel
  }
  deriving (Eq, Show)

instance ToJSON SaveStateOptions where
  toJSON SaveStateOptions {..} =
    object
      [ "concurrency" .= ssoConcurrency,
        "consistency" .= ssoConsistency
      ]

data SaveStateReqBody a = SaveStateReqBody
  { ssrKey :: Text,
    ssrValue :: a,
    ssrEtag :: Maybe Text,
    ssrOptions :: Maybe SaveStateOptions,
    ssrMetadata :: Maybe (HashMap Text Text)
  }
  deriving (Eq, Show)

instance ToJSON a => ToJSON (SaveStateReqBody a) where
  toJSON (SaveStateReqBody k v etag opts meta) =
    object
      [ "key" .= k,
        "value" .= v,
        "etag" .= etag,
        "options" .= opts,
        "metadata" .= meta
      ]

mkSimleSaveStateReqBody :: Text -> a -> SaveStateReqBody a
mkSimleSaveStateReqBody key value = SaveStateReqBody key value Nothing Nothing Nothing

saveState :: (MonadIO m, ToJSON a) => DaprClientConfig -> Text -> [SaveStateReqBody a] -> m (Either Text ())
saveState config store body = do
  let url = "state/" <> store
      options = header "Content-Type" "application/json"
  response <- makeRequest config POST url (ReqBodyJson body) ignoreResponse options
  case responseStatusCode response of
    204 -> return $ Right ()
    400 -> return $ Left "State store is missing or misconfigured or malformed request"
    500 -> return $ Left "Failed to save state"
    _ -> return $ Left "Unknown response status code"

saveSingleState :: (MonadIO m, ToJSON a) => DaprClientConfig -> Text -> SaveStateReqBody a -> m (Either Text ())
saveSingleState config store body = saveState config store [body]
