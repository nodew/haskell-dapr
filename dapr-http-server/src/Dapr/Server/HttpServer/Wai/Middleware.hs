module Dapr.Server.HttpServer.Wai.Middleware where

import Dapr.Server.HttpServer.Types
import Data.Aeson
import Data.ByteString.Builder
import Data.Text
import Network.HTTP.Types
import Network.Wai

addDaprSubscriptions :: [SubscriptionInfo] -> Middleware
addDaprSubscriptions subs app request respond = do
  let path = pathInfo request
  if path == ["dapr", "subscribe"]
    then
      let subscriptionInfo = encode subs
       in respond $ responseBuilder status200 [("Content-Type", "application/json")] (lazyByteString subscriptionInfo)
    else app request respond

subscribeConfigurationChange :: Text -> Text -> (Maybe SubscribedConfiguration -> IO ()) -> Middleware
subscribeConfigurationChange store key' handler app request respond = do
  let path = pathInfo request
  let method = requestMethod request
  if path == ["configuration", store, key'] && method == "POST"
    then do
      rawBody <- lazyRequestBody request
      handler $ decode rawBody
      respond $ responseBuilder status200 [("Content-Type", "plain/text")] (lazyByteString "OK")
    else app request respond
