module Main where

import Control.Concurrent
import Dapr.Client.HttpClient hiding (metadata)
import Dapr.Server.HttpServer.Types
import Dapr.Server.HttpServer.Wai.Middleware
import Data.Aeson (object)
import Data.Aeson.Types
import Data.ByteString hiding (putStrLn)
import qualified Data.ByteString
import Data.ByteString.Builder (lazyByteString)
import qualified Data.ByteString.Lazy as L
import Data.Map
import Data.Text
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)

app :: Application
app req respond = case pathInfo req of
  ["message"] -> do
    body <- strictRequestBody req
    putStrLn $ "Get message: " <> show body
    respond $ responseBuilder status200 [("Content-Type", "application/json")] (lazyByteString "{ \"status\": \"success\"}")
  _ -> respond indexResponse

indexResponse :: Response
indexResponse = responseBuilder status200 [("Content-Type", "plain/text")] (lazyByteString "hello world")

pubsub' :: Text
pubsub' = "pubsub-mqtt"

topic' :: Text
topic' = "http-pubsub-example"

subscriptions :: [SubscriptionInfo]
subscriptions = [SubscriptionInfo {pubsubname = pubsub', topic = topic', route = "/message", metadata = Data.Map.empty}]

server :: IO ()
server = do
  putStrLn "Started server at http://localhost:3000/"
  run 3000 $ addDaprSubscriptions subscriptions app

main :: IO ()
main = do
  _ <- forkIO server
  threadDelay 1000
  _ <- publishJsonMessage defaultDaprConfig (PubSub pubsub') (Topic topic') (object [("hello", "world")]) Nothing
  return ()
