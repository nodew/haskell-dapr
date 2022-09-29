module Dapr.Server.HttpServer.Wai where

import Dapr.Core.Server
import Data.Aeson
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Types
import qualified Data.Text as T
import Data.List
import Dapr.Core.Server (DaprServerConfig(inputBindings))
import Data.Maybe (fromJust)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Network.Wai as Wai

notFound :: Wai.Application
notFound _ respond = do
  respond $ Wai.responseBuilder status404 [] (lazyByteString L.empty)

toJsonResponse :: ToJSON a => a -> Wai.Response
toJsonResponse content = Wai.responseBuilder status200 [("Content-Type", "application/json")] (lazyByteString $ encode content)

handleTopicEvent :: (MonadIO m, FromJSON a) => Subscription -> Wai.Request -> TopicEventHandler m a -> m Wai.Response
handleTopicEvent _ req handler = do
  body <- liftIO $ Wai.getRequestBodyChunk req
  let topicEvent = fromJust $ decode body
  _ <- handle handler topicEvent
  return $ Wai.responseBuilder status200 [("Content-Type", "application/json")] "TODO"

handleBindingEvent request handler = undefined

runDaprService :: FromJSON a => DaprServerConfig IO a -> Wai.Middleware
runDaprService (DaprServerConfig {..}) app req respond = do
  let path = Wai.pathInfo req
      path' = foldl (\p i -> p <> "/" <> i) T.empty path
      method = Wai.requestMethod req
      subscribedPaths = map (subscriptionRoute . fst) subscriptions
      bindingPaths = map fst inputBindings
  if path' == "/dapr/subscribe" && method == "GET"
    then respond $ toJsonResponse $ map fst subscriptions
  else if path' `elem` subscribedPaths && method == "POST"
    then app req $ \_ -> do
      let (sub, handler) = fromJust $ find (\item -> (subscriptionRoute . fst) item == path') subscriptions
      response <- handleTopicEvent sub req handler
      respond response
  else if path' `elem` bindingPaths && method == "POST"
    then app req $ \_ -> respond $ Wai.responseBuilder status200 [("Content-Type", "application/json")] "TODO"
  else
    app req respond

runDaprServer :: FromJSON a => DaprServerConfig IO a -> Wai.Application
runDaprServer config = runDaprService config notFound
