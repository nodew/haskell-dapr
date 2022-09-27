module Dapr.Server.HttpServer.Wai where

import Dapr.Core.Server
import Data.Aeson
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Types
import Network.Wai
import qualified Data.Text as T
import Data.List
import Dapr.Core.Server (DaprServerConfig(inputBindings))

notFound :: Application
notFound _ respond = do
  respond $ responseBuilder status404 [] (lazyByteString L.empty)

toJsonResponse :: ToJSON a => a -> Response
toJsonResponse content = responseBuilder status200 [("Content-Type", "application/json")] (lazyByteString $ encode content)

runDaprService :: DaprServerConfig IO -> Middleware
runDaprService (DaprServerConfig {..}) app req respond = do
  let path = pathInfo req
      path' = foldl (\p i -> p <> "/" <> i) T.empty path
      method = requestMethod req
      subscribedPaths = map (subscriptionRoute . fst) subscriptions
      bindingPaths = map fst inputBindings
  if path' == "/dapr/subscribe" && method == "GET"
    then respond $ toJsonResponse $ map fst subscriptions
  else if path' `elem` subscribedPaths && method == "POST"
    then app req respond
  else if path' `elem` bindingPaths && method == "POST"
    then app req respond
  else
    app req respond

runDaprServer :: DaprServerConfig IO -> Application
runDaprServer config = runDaprService config notFound
