{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dapr.Client.HttpClient.PubSub where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Internal
import Dapr.Client.HttpClient.Req
import Dapr.Core.Types
import Data.Aeson (ToJSON)
import Data.Bifunctor (bimap)
import Network.HTTP.Req

publishMessage ::
  (MonadIO m, ToJSON message) =>
  DaprConfig ->
  PublishEventRequest message ->
  m (Either DaprClientError ())
publishMessage config request = do
  let url = ["publish", getPubsubName $ pubsubName request, getPubsubTopic $ pubsubTopic request]
      metadataParam = mapMetadataToQueryParam $ pubsubMetadata request
      options = metadataParam <> header "Content-Type" "application/json"
      message = pubsubData request
  response <- makeHttpRequest config POST url (ReqBodyJson message) ignoreResponse options
  return $ bimap DaprHttpException (const ()) response
