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
    invokeActorMethod,
    invokeActorMethodWithJsonPayload,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Req
import Dapr.Core.Types
import Data.Aeson
import Data.Bifunctor (Bifunctor (bimap))
import Data.CaseInsensitive (CI (original))
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Network.HTTP.Req
import Network.HTTP.Types (hContentType)

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
getActorState :: (MonadIO m, FromJSON a) => DaprConfig -> GetActorStateRequest -> m (Either DaprClientError (GetActorStateResponse a))
getActorState config GetActorStateRequest {..} = do
  let url = ["actors", getActorType $ actorType actor, getActorIdText $ actorId actor, "state", getStateKey key]
  response <- makeHttpRequest config GET url NoReqBody jsonResponse mempty
  return $ bimap DaprHttpException (GetActorStateResponse . responseBody) response

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

-- | Invoke a method on an actor
invokeActorMethod ::
  ( HttpBodyAllowed
      (AllowsBody method)
      (ProvidesBody payload),
    MonadIO m,
    HttpMethod method,
    HttpBody payload
  ) =>
  DaprConfig ->
  InvokeActorRequest method payload ->
  m (Either DaprClientError InvokeActorResponse)
invokeActorMethod config InvokeActorRequest {..} = do
  let url = ["actors", getActorType $ actorType actor, getActorIdText $ actorId actor, "method", actorMethod]
      options = maybe mempty (header (original hContentType) . encodeUtf8) actorContentType
  response <- makeHttpRequest config httpMethod url actorData lbsResponse options
  return $ bimap DaprHttpException getResponse response
  where
    getResponse :: LbsResponse -> InvokeActorResponse
    getResponse response =
      let content = responseBody response
       in InvokeActorResponse content

-- | Invoke a method on an actor, and decode the response if it's JSON
invokeActorMethodWithJsonPayload ::
  ( HttpBodyAllowed (AllowsBody method) 'CanHaveBody,
    MonadIO m,
    HttpMethod method,
    ToJSON payload
  ) =>
  DaprConfig ->
  InvokeActorRequest method payload ->
  m (Either DaprClientError InvokeActorResponse)
invokeActorMethodWithJsonPayload config InvokeActorRequest {..} = do
  let updatedRequest = InvokeActorRequest actor httpMethod actorMethod (ReqBodyJson actorData) actorContentType actorMetadata
  invokeActorMethod config updatedRequest
