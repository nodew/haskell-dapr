{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dapr.Client.HttpClient.Actor where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Req
import Dapr.Core.Types
import Data.Aeson
import Data.Bifunctor (Bifunctor (bimap), first)
import Data.Data (Proxy)
import Network.HTTP.Req

invokeActor ::
  ( HttpBodyAllowed (AllowsBody method) (ProvidesBody body),
    MonadIO m,
    HttpMethod method,
    HttpBody body,
    HttpResponse c
  ) =>
  DaprConfig ->
  Actor ->
  ActorMethod ->
  method ->
  body ->
  Proxy c ->
  Option 'Http ->
  m (Either DaprClientError c)
invokeActor config actor method reqMethod reqBody handler options = do
  let url = ["actors", actorType actor, actorId actor, "method", getMethodName method]
  response <- makeHttpRequest config reqMethod url reqBody handler options
  return $ first DaprHttpException response

executeActorStateTransaction ::
  (MonadIO m, ToJSON a) =>
  DaprConfig ->
  Actor ->
  [ActorStateTransactionItem a] ->
  m (Either DaprClientError ())
executeActorStateTransaction config actor transactions = do
  let url = ["actors", actorType actor, actorId actor, "state"]
  response <- makeHttpRequest config POST url (ReqBodyJson transactions) ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response

getActorState :: (MonadIO m, FromJSON a) => DaprConfig -> Actor -> OperationKey -> m (Either DaprClientError a)
getActorState config actor key = do
  let url = ["actors", actorType actor, actorId actor, "state", key]
  response <- makeHttpRequest config GET url NoReqBody jsonResponse mempty
  return $ bimap DaprHttpException responseBody response

createActorReminder :: MonadIO m => DaprConfig -> Actor -> ReminderName -> ActorReminderRequest -> m (Either DaprClientError ())
createActorReminder config actor name reminder = do
  let url = ["actors", actorType actor, actorId actor, "reminders", name]
  response <- makeHttpRequest config POST url (ReqBodyJson reminder) ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response

getActorReminder :: MonadIO m => DaprConfig -> Actor -> ReminderName -> m (Either DaprClientError ActorReminderResponse)
getActorReminder config actor name = do
  let url = ["actors", actorType actor, actorId actor, "reminders", name]
  response <- makeHttpRequest config GET url NoReqBody jsonResponse mempty
  return $ bimap DaprHttpException responseBody response

deleteActorReminder :: MonadIO m => DaprConfig -> Actor -> ReminderName -> m (Either DaprClientError ())
deleteActorReminder config actor name = do
  let url = ["actors", actorType actor, actorId actor, "reminders", name]
  response <- makeHttpRequest config DELETE url NoReqBody ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response

createActorTimer :: MonadIO m => DaprConfig -> Actor -> TimerName -> ActorTimerRequest -> m (Either DaprClientError ())
createActorTimer config actor name timer = do
  let url = ["actors", actorType actor, actorId actor, "timers", name]
  response <- makeHttpRequest config POST url (ReqBodyJson timer) ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response

deleteActorTimer :: MonadIO m => DaprConfig -> Actor -> TimerName -> m (Either DaprClientError ())
deleteActorTimer config actor name = do
  let url = ["actors", actorType actor, actorId actor, "timers", name]
  response <- makeHttpRequest config DELETE url NoReqBody ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response
