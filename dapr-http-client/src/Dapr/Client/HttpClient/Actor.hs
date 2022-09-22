-- |
-- Module      : Dapr.Client.HttpClient.Actor
-- Description : Manages Dapr virtual actors
-- Copyright   : (c)
-- License     : Apache-2.0
-- This module manages Dapr virtual actors
module Dapr.Client.HttpClient.Actor where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Req
import Dapr.Core.Types
import Data.Aeson
import Data.Bifunctor (Bifunctor (bimap), first)
import Data.Data (Proxy)
import Network.HTTP.Req

-- | Invokes an Actor method on Dapr runtime without remoting
invokeActor ::
  ( HttpBodyAllowed (AllowsBody method) (ProvidesBody body),
    MonadIO m,
    HttpMethod method,
    HttpBody body,
    HttpResponse c
  ) =>
  DaprConfig ->
  InvokeActorRequest body ->
  m (Either DaprClientError c)
invokeActor config request = do
  let url = ["actors", actorType actor, actorId actor, "method", getMethodName method]
  response <- makeHttpRequest config reqMethod url reqBody handler options
  return $ first DaprHttpException response

-- | Persists the change to the state for an `Actor` as a multi-item transaction.
-- Note that this operation is dependant on a using state store component that supports multi-item transactions
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

-- | Gets the state for an `Actor` using a specified `OperationKey`
getActorState :: (MonadIO m, FromJSON a) => DaprConfig -> Actor -> OperationKey -> m (Either DaprClientError a)
getActorState config actor key = do
  let url = ["actors", actorType actor, actorId actor, "state", key]
  response <- makeHttpRequest config GET url NoReqBody jsonResponse mempty
  return $ bimap DaprHttpException responseBody response

-- | Creates a persistent reminder for an `Actor`
createActorReminder :: MonadIO m => DaprConfig -> Actor -> ReminderName -> ActorReminderRequest -> m (Either DaprClientError ())
createActorReminder config actor name reminder = do
  let url = ["actors", actorType actor, actorId actor, "reminders", name]
  response <- makeHttpRequest config POST url (ReqBodyJson reminder) ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response

-- | Gets a reminder for an `Actor` by given `ReminderName`
getActorReminder :: MonadIO m => DaprConfig -> Actor -> ReminderName -> m (Either DaprClientError ActorReminderResponse)
getActorReminder config actor name = do
  let url = ["actors", actorType actor, actorId actor, "reminders", name]
  response <- makeHttpRequest config GET url NoReqBody jsonResponse mempty
  return $ bimap DaprHttpException responseBody response

-- | Deletes a reminder for an `Actor` by given `ReminderName`
deleteActorReminder :: MonadIO m => DaprConfig -> Actor -> ReminderName -> m (Either DaprClientError ())
deleteActorReminder config actor name = do
  let url = ["actors", actorType actor, actorId actor, "reminders", name]
  response <- makeHttpRequest config DELETE url NoReqBody ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response

-- | Creates a timer for a give `Actor`
createActorTimer :: MonadIO m => DaprConfig -> Actor -> TimerName -> ActorTimerRequest -> m (Either DaprClientError ())
createActorTimer config actor name timer = do
  let url = ["actors", actorType actor, actorId actor, "timers", name]
  response <- makeHttpRequest config POST url (ReqBodyJson timer) ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response

-- | Deletes a timer for an `Actor` by given `TimerName`
deleteActorTimer :: MonadIO m => DaprConfig -> Actor -> TimerName -> m (Either DaprClientError ())
deleteActorTimer config actor name = do
  let url = ["actors", actorType actor, actorId actor, "timers", name]
  response <- makeHttpRequest config DELETE url NoReqBody ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response
