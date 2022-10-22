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
  (ToJSON a) =>
  ExecuteActorStateTransactionRequest a ->
  DaprHttpClient (Either DaprClientError ())
executeActorStateTransaction ExecuteActorStateTransactionRequest {..} = do
  let url = ["actors", getActorType $ actorType actor, getActorIdText $ actorId actor, "state"]
      payload = mapOperationToPayload operations
  response <- makeHttpRequest POST url (ReqBodyJson payload) ignoreResponse mempty
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
getActorState :: (FromJSON a) => GetActorStateRequest -> DaprHttpClient (Either DaprClientError (GetActorStateResponse a))
getActorState GetActorStateRequest {..} = do
  let url = ["actors", getActorType $ actorType actor, getActorIdText $ actorId actor, "state", getStateKey key]
  response <- makeHttpRequest GET url NoReqBody jsonResponse mempty
  return $ bimap DaprHttpException (GetActorStateResponse . responseBody) response

-- | Creates a persistent reminder for an `Actor`
registerActorReminder :: (ToJSON a) => RegisterActorReminderRequest a -> DaprHttpClient (Either DaprClientError ())
registerActorReminder RegisterActorReminderRequest {..} = do
  let url = ["actors", getActorType $ actorType reminderActor, getActorIdText $ actorId reminderActor, "reminders", reminderName]
  response <- makeHttpRequest POST url (ReqBodyJson reminderData) ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response

-- | Gets a reminder for an `Actor` by given `ReminderName`
getActorReminder :: (FromJSON a) => GetActorReminderRequest -> DaprHttpClient (Either DaprClientError (GetActorReminderResponse a))
getActorReminder GetActorReminderRequest {..} = do
  let url = ["actors", getActorType $ actorType reminderActor, getActorIdText $ actorId reminderActor, "reminders", reminderName]
  response <- makeHttpRequest GET url NoReqBody jsonResponse mempty
  return $ bimap DaprHttpException responseBody response

-- | Deletes a reminder for an `Actor` by given `ReminderName`
unregisterActorReminder :: UnregisterActorReminderRequest -> DaprHttpClient (Either DaprClientError ())
unregisterActorReminder UnregisterActorReminderRequest {..} = do
  let url = ["actors", getActorType $ actorType reminderActor, getActorIdText $ actorId reminderActor, "reminders", reminderName]
  response <- makeHttpRequest DELETE url NoReqBody ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response

-- | Creates a timer for a give `Actor`
registerActorTimer :: (ToJSON a) => RegisterActorTimerRequest a -> DaprHttpClient (Either DaprClientError ())
registerActorTimer RegisterActorTimerRequest {..} = do
  let url = ["actors", getActorType $ actorType timerActor, getActorIdText $ actorId timerActor, "timers", timerName]
  response <- makeHttpRequest POST url (ReqBodyJson timerData) ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response

-- | Deletes a timer for an `Actor` by given `TimerName`
unregisterActorTimer :: UnregisterActorTimerRequest -> DaprHttpClient (Either DaprClientError ())
unregisterActorTimer UnregisterActorTimerRequest {..} = do
  let url = ["actors", getActorType $ actorType timerActor, getActorIdText $ actorId timerActor, "timers", timerName]
  response <- makeHttpRequest DELETE url NoReqBody ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response

-- | Invoke a method on an actor
invokeActorMethod ::
  ( HttpBodyAllowed
      (AllowsBody method)
      (ProvidesBody payload),
    HttpMethod method,
    HttpBody payload
  ) =>
  InvokeActorRequest method payload ->
  DaprHttpClient (Either DaprClientError InvokeActorResponse)
invokeActorMethod InvokeActorRequest {..} = do
  let url = ["actors", getActorType $ actorType actor, getActorIdText $ actorId actor, "method", actorMethod]
      options = maybe mempty (header (original hContentType) . encodeUtf8) actorContentType
  response <- makeHttpRequest httpMethod url actorData lbsResponse options
  return $ bimap DaprHttpException getResponse response
  where
    getResponse :: LbsResponse -> InvokeActorResponse
    getResponse response =
      let content = responseBody response
       in InvokeActorResponse content

-- | Invoke a method on an actor, and decode the response if it's JSON
invokeActorMethodWithJsonPayload ::
  ( HttpBodyAllowed (AllowsBody method) 'CanHaveBody,
    HttpMethod method,
    ToJSON payload
  ) =>
  InvokeActorRequest method payload ->
  DaprHttpClient (Either DaprClientError InvokeActorResponse)
invokeActorMethodWithJsonPayload InvokeActorRequest {..} = do
  let updatedRequest = InvokeActorRequest actor httpMethod actorMethod (ReqBodyJson actorData) actorContentType actorMetadata
  invokeActorMethod updatedRequest
