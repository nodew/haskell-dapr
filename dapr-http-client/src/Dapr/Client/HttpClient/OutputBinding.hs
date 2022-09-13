module Dapr.Client.HttpClient.OutputBinding where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Req
import Dapr.Client.HttpClient.Types
import Data.Aeson
import Data.Bifunctor (bimap)
import Network.HTTP.Req

invokeBinding ::
  (MonadIO io, ToJSON a) =>
  DaprConfig ->
  Binding ->
  BindingRequest a ->
  io (Either DaprClientError ())
invokeBinding config binding requestBody = do
  let url = ["bindings", getBindingName binding]
  response <- makeHttpRequest config POST url (ReqBodyJson requestBody) ignoreResponse mempty
  return $ bimap DaprHttpException (const ()) response
