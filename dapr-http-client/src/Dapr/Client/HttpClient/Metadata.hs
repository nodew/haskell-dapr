module Dapr.Client.HttpClient.Metadata where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient.Req
import Dapr.Client.HttpClient.Types
import Data.Bifunctor (bimap)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Req

getMetadata :: MonadIO m => DaprConfig -> m (Either DaprClientError DaprMetadata)
getMetadata config = do
  response <- makeHttpRequest config GET ["metadata"] NoReqBody jsonResponse mempty
  return $ bimap DaprHttpException responseBody response

addCustomLabel :: MonadIO m => DaprConfig -> Text -> Text -> m (Either DaprClientError ())
addCustomLabel config attribute rawData = do
  let url = ["metadata", attribute] 
      options = header "Content-Type" "text/plain"
  response <- makeHttpRequest config PUT url (ReqBodyBs $ encodeUtf8 rawData) ignoreResponse options
  return $ bimap DaprHttpException (const ()) response
