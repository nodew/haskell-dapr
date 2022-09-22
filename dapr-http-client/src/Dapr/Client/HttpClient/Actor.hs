-- |
-- Module      : Dapr.Client.HttpClient.Actor
-- Description : Manages Dapr virtual actors
-- Copyright   : (c)
-- License     : Apache-2.0
-- This module manages Dapr virtual actors
module Dapr.Client.HttpClient.Actor
  ( executeActorStateTransaction,
    getActorState,
    registerActorReminder,
    unregisterActorReminder,
    getActorReminder,
    registerActorTimer,
    unregisterActorTimer,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Req
import Dapr.Core.Types
import Data.Aeson
import Data.Bifunctor (Bifunctor (bimap))
import GHC.Generics (Generic)
import Network.HTTP.Req

data ExecuteActorStateTransactionItem a = ExecuteActorStateTransactionItem
  { key :: StateKey,
    value :: a
  }
  deriving (Generic, ToJSON)

data ExecuteActorStateTransactionOp a = ExecuteActorStateTransactionOp
  { operation :: TransactionOperation,
    request :: ExecuteActorStateTransactionItem a
  }
  deriving (Generic, ToJSON)

-- | Invokes an Actor method on Dapr runtime without remoting
-- invokeActor ::
--   ( MonadIO m,
--     HttpBody body,
--     HttpResponse response
--   ) =>
--   DaprConfig ->
--   InvokeActorRequest body ->
--   m (Either DaprClientError response)
-- invokeActor config InvokeActorRequest {..} = do
--   let url = ["actors", getActorType $ actorType actor, getActorIdText $ actorId actor, "method", actorMethod]
--   response <- makeHttpRequest config POST url actorData jsonResponse mempty
--   return $ first DaprHttpException response

-- | Persists the change to the state for an `Actor` as a multi-item transaction.
-- Note that this operation is dependant on a using state store component that supports multi-item transactions
executeActorStateTransaction ::
  (MonadIO m, ToJSON a) =>
  DaprConfig ->
  ExecuteActorStateTransactionRequest a ->
  m (Either DaprClientError ())
executeActorStateTransaction config ExecuteActorStateTransactionRequest {..} = do
  let url = ["actors", getActorType $ actorType actor, getActorIdText $ actorId actor, "state"]
      payload = mapOperationToPayload operations
  response <- makeHttpRequest config POST url (ReqBodyJson payload) ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response
  where
    mapOperationToPayload =
      map
        ( \TransactionalActorStateOperation {..} ->
            ExecuteActorStateTransactionOp
              operationType
              (ExecuteActorStateTransactionItem key value)
        )

-- | Gets the state for an `Actor` using a specified `OperationKey`
getActorState :: (MonadIO m, FromJSON a) => DaprConfig -> GetActorStateRequest -> m (Either DaprClientError a)
getActorState config GetActorStateRequest {..} = do
  let url = ["actors", getActorType $ actorType actor, getActorIdText $ actorId actor, "state", getStateKey key]
  response <- makeHttpRequest config GET url NoReqBody jsonResponse mempty
  return $ bimap DaprHttpException responseBody response

-- | Creates a persistent reminder for an `Actor`
registerActorReminder :: (MonadIO m, ToJSON a) => DaprConfig -> RegisterActorReminderRequest a -> m (Either DaprClientError ())
registerActorReminder config RegisterActorReminderRequest {..} = do
  let url = ["actors", getActorType $ actorType reminderActor, getActorIdText $ actorId reminderActor, "reminders", reminderName]
  response <- makeHttpRequest config POST url (ReqBodyJson reminderData) ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response

-- | Gets a reminder for an `Actor` by given `ReminderName`
getActorReminder :: (MonadIO m, FromJSON a) => DaprConfig -> GetActorReminderRequest -> m (Either DaprClientError (GetActorReminderResponse a))
getActorReminder config GetActorReminderRequest {..} = do
  let url = ["actors", getActorType $ actorType reminderActor, getActorIdText $ actorId reminderActor, "reminders", reminderName]
  response <- makeHttpRequest config GET url NoReqBody jsonResponse mempty
  return $ bimap DaprHttpException responseBody response

-- | Deletes a reminder for an `Actor` by given `ReminderName`
unregisterActorReminder :: MonadIO m => DaprConfig -> UnregisterActorReminderRequest -> m (Either DaprClientError ())
unregisterActorReminder config UnregisterActorReminderRequest {..} = do
  let url = ["actors", getActorType $ actorType reminderActor, getActorIdText $ actorId reminderActor, "reminders", reminderName]
  response <- makeHttpRequest config DELETE url NoReqBody ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response

-- | Creates a timer for a give `Actor`
registerActorTimer :: (MonadIO m, ToJSON a) => DaprConfig -> RegisterActorTimerRequest a -> m (Either DaprClientError ())
registerActorTimer config RegisterActorTimerRequest {..} = do
  let url = ["actors", getActorType $ actorType timerActor, getActorIdText $ actorId timerActor, "timers", timerName]
  response <- makeHttpRequest config POST url (ReqBodyJson timerData) ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response

-- | Deletes a timer for an `Actor` by given `TimerName`
unregisterActorTimer :: MonadIO m => DaprConfig -> UnregisterActorTimerRequest -> m (Either DaprClientError ())
unregisterActorTimer config UnregisterActorTimerRequest {..} = do
  let url = ["actors", getActorType $ actorType timerActor, getActorIdText $ actorId timerActor, "timers", timerName]
  response <- makeHttpRequest config DELETE url NoReqBody ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response
