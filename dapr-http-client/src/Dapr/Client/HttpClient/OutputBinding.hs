module Dapr.Client.HttpClient.OutputBinding where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Req
import Dapr.Client.HttpClient.Types
import Data.Bifunctor (bimap)
import Data.Text (Text)
import Network.HTTP.Req
import Data.Aeson

invokeBinding ::
  (MonadIO io, ToJSON a) =>
  DaprConfig ->
  Text -> 
  BindingRequest a ->
  io (Either DaprClientError ())
invokeBinding config bindingName requestBody  = do
  let url = ["bindings", bindingName]
  response <- makeHttpRequest config POST url (ReqBodyJson requestBody) ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response
