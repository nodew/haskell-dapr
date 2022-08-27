{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dapr.HttpClient.Actor where

import Dapr.HttpClient.Core
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

data ActorStateTransaction a = ActorStateTransaction
  { operation :: ActorOperation,
    request :: ActorOperationRequest a
  }
  deriving (Eq, Show, Generic, ToJSON)

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

excuteActorStateTransaction ::
  (MonadIO m, ToJSON a) =>
  DaprClientConfig ->
  Text ->
  Text ->
  [ActorStateTransaction a] ->
  m (Either DaprClientError ())
excuteActorStateTransaction config actorType actorId transactions = do
  let url = ["actors", actorType, actorId, "state"]
      options = header "Content-Type" "application/json"
  response <- makeRequest config POST url (ReqBodyJson transactions) ignoreResponse options
  return $ bimap DaprHttpException (const ()) response
