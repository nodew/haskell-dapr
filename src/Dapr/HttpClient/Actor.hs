{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dapr.HttpClient.Actor where

import Dapr.HttpClient.Core
import Dapr.HttpClient.Internal
import Data.Aeson
import qualified Data.Text as T
import Network.HTTP.Req
import RIO

data ActorOperation = Upsert | Delete
  deriving (Eq)

instance Show ActorOperation where
  show Upsert = "upsert"
  show Delete = "delete"

instance ToJSON ActorOperation where
  toJSON = String . T.pack . show

data ActorOperationRequest a = ActorOperationRequest
  { key :: ActorOperation,
    value :: a
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (ActorOperationRequest a) where
  toJSON = genericToJSON defaultOptions

data ActorStateTransactionItem a = ActorStateTransactionItem
  { operation :: ActorOperation,
    request :: ActorOperationRequest a
  }
  deriving (Eq, Show, Generic, ToJSON)

data ActorReminderRequest = ActorReminderRequest
  { reminderDueTime :: Text,
    reminderPeriod :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON ActorReminderRequest where
  toJSON = customToJSON 8

data ActorReminderResponse = ActorReminderResponse
  { reminderDueTime :: Text,
    reminderPeriod :: Text,
    reminderData :: Text
  }
  deriving (Eq, Show, Generic, ToJSON)

instance FromJSON ActorReminderResponse where
  parseJSON = customParseJSON 8

data ActorTimerRequest = ActorTimerRequest
  { reminderDueTime :: Text,
    reminderPeriod :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON ActorTimerRequest where
  toJSON = customToJSON 8

invokeActor ::
  ( HttpBodyAllowed (AllowsBody method) (ProvidesBody body),
    MonadIO m,
    HttpMethod method,
    HttpBody body,
    HttpResponse c
  ) =>
  DaprClientConfig ->
  Text ->
  Text ->
  Text ->
  method ->
  body ->
  Proxy c ->
  Option 'Http ->
  m (Either DaprClientError c)
invokeActor config actorType actorId method reqMethod reqBody handler options = do
  let url = ["actors", actorType, actorId, "method", method]
  response <- makeRequest config reqMethod url reqBody handler options
  return $ first DaprHttpException response

executeActorStateTransaction ::
  (MonadIO m, ToJSON a) =>
  DaprClientConfig ->
  Text ->
  Text ->
  [ActorStateTransactionItem a] ->
  m (Either DaprClientError ())
executeActorStateTransaction config actorType actorId transactions = do
  let url = ["actors", actorType, actorId, "state"]
      options = header "Content-Type" "application/json"
  response <- makeRequest config POST url (ReqBodyJson transactions) ignoreResponse options
  return $ bimap DaprHttpException (const ()) response

getActorState :: (MonadIO m, FromJSON a) => DaprClientConfig -> Text -> Text -> Text -> m (Either DaprClientError a)
getActorState config actorType actorId key = do
  let url = ["actors", actorType, actorId, "state", key]
  response <- makeRequest config GET url NoReqBody jsonResponse mempty
  return $ bimap DaprHttpException responseBody response

createActorReminder :: MonadIO m => DaprClientConfig -> Text -> Text -> Text -> ActorReminderRequest -> m (Either DaprClientError ())
createActorReminder config actorType actorId name reminder = do
  let url = ["actors", actorType, actorId, "reminders", name]
      options = header "Content-Type" "application/json"
  response <- makeRequest config POST url (ReqBodyJson reminder) ignoreResponse options
  return $ bimap DaprHttpException (const ()) response

getActorReminder :: MonadIO m => DaprClientConfig -> Text -> Text -> Text -> m (Either DaprClientError ActorReminderResponse)
getActorReminder config actorType actorId name = do
  let url = ["actors", actorType, actorId, "reminders", name]
  response <- makeRequest config GET url NoReqBody jsonResponse mempty
  return $ bimap DaprHttpException responseBody response

deleteActorReminder :: MonadIO m => DaprClientConfig -> Text -> Text -> Text -> m (Either DaprClientError ())
deleteActorReminder config actorType actorId name = do
  let url = ["actors", actorType, actorId, "reminders", name]
  response <- makeRequest config DELETE url NoReqBody ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response

createActorTimer :: MonadIO m => DaprClientConfig -> Text -> Text -> Text -> ActorTimerRequest -> m (Either DaprClientError ())
createActorTimer config actorType actorId name timer = do
  let url = ["actors", actorType, actorId, "timers", name]
      options = header "Content-Type" "application/json"
  response <- makeRequest config POST url (ReqBodyJson timer) ignoreResponse options
  return $ bimap DaprHttpException (const ()) response

deleteActorTimer :: MonadIO m => DaprClientConfig -> Text -> Text -> Text -> m (Either DaprClientError ())
deleteActorTimer config actorType actorId name = do
  let url = ["actors", actorType, actorId, "timers", name]
  response <- makeRequest config DELETE url NoReqBody ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response
