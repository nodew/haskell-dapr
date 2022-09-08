{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dapr.Client.HttpClient.Actor where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Req
import Dapr.Client.HttpClient.Types
import Dapr.Client.HttpClient.Types.Internal
import Data.Aeson
import Data.Bifunctor (Bifunctor (bimap), first)
import Data.Data (Proxy)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Req

data ActorOperationRequest a = ActorOperationRequest
  { key :: Text,
    value :: a
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (ActorOperationRequest a) where
  toJSON = genericToJSON defaultOptions

data ActorStateTransactionItem a = ActorStateTransactionItem
  { operation :: StateOperationType,
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
  DaprConfig ->
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
  response <- makeHttpRequestWithOptions config reqMethod url reqBody handler options
  return $ first DaprHttpException response

executeActorStateTransaction ::
  (MonadIO m, ToJSON a) =>
  DaprConfig ->
  Text ->
  Text ->
  [ActorStateTransactionItem a] ->
  m (Either DaprClientError ())
executeActorStateTransaction config actorType actorId transactions = do
  let url = ["actors", actorType, actorId, "state"]
  response <- makeHttpRequest config POST url (ReqBodyJson transactions) ignoreResponse
  return $ bimap DaprHttpException (const ()) response

getActorState :: (MonadIO m, FromJSON a) => DaprConfig -> Text -> Text -> Text -> m (Either DaprClientError a)
getActorState config actorType actorId key = do
  let url = ["actors", actorType, actorId, "state", key]
  response <- makeHttpRequest config GET url NoReqBody jsonResponse
  return $ bimap DaprHttpException responseBody response

createActorReminder :: MonadIO m => DaprConfig -> Text -> Text -> Text -> ActorReminderRequest -> m (Either DaprClientError ())
createActorReminder config actorType actorId name reminder = do
  let url = ["actors", actorType, actorId, "reminders", name]
  response <- makeHttpRequest config POST url (ReqBodyJson reminder) ignoreResponse
  return $ bimap DaprHttpException (const ()) response

getActorReminder :: MonadIO m => DaprConfig -> Text -> Text -> Text -> m (Either DaprClientError ActorReminderResponse)
getActorReminder config actorType actorId name = do
  let url = ["actors", actorType, actorId, "reminders", name]
  response <- makeHttpRequest config GET url NoReqBody jsonResponse
  return $ bimap DaprHttpException responseBody response

deleteActorReminder :: MonadIO m => DaprConfig -> Text -> Text -> Text -> m (Either DaprClientError ())
deleteActorReminder config actorType actorId name = do
  let url = ["actors", actorType, actorId, "reminders", name]
  response <- makeHttpRequest config DELETE url NoReqBody ignoreResponse
  return $ bimap DaprHttpException (const ()) response

createActorTimer :: MonadIO m => DaprConfig -> Text -> Text -> Text -> ActorTimerRequest -> m (Either DaprClientError ())
createActorTimer config actorType actorId name timer = do
  let url = ["actors", actorType, actorId, "timers", name]
  response <- makeHttpRequest config POST url (ReqBodyJson timer) ignoreResponse
  return $ bimap DaprHttpException (const ()) response

deleteActorTimer :: MonadIO m => DaprConfig -> Text -> Text -> Text -> m (Either DaprClientError ())
deleteActorTimer config actorType actorId name = do
  let url = ["actors", actorType, actorId, "timers", name]
  response <- makeHttpRequest config DELETE url NoReqBody ignoreResponse
  return $ bimap DaprHttpException (const ()) response
