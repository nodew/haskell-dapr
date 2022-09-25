module Main where

import Control.Concurrent
import Dapr.Client.HttpClient
import Dapr.Server.HttpServer.Types
import Dapr.Server.HttpServer.Wai.Middleware
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString hiding (putStrLn)
import qualified Data.ByteString
import Data.ByteString.Builder (lazyByteString)
import qualified Data.ByteString.Lazy as L
import Data.Map
import Data.Text
import GHC.Generics
import Network.HTTP.Req (ReqBodyJson (ReqBodyJson))
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)

newtype TestMessage = TestMessage
  { message :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

helloWorld = TestMessage "Hello World!"

app :: Application
app req respond = case pathInfo req of
  ["message"] -> do
    body <- strictRequestBody req
    let message = decode body :: Maybe (DaprSubMessage TestMessage)
    putStrLn $ "Get message: " <> show (subMsgData <$> message)
    respond $ responseBuilder status200 [("Content-Type", "application/json")] (lazyByteString $ encode $ SubscribedMessageHttpResponse SubscribeSuccess)
  _ -> respond indexResponse

indexResponse :: Response
indexResponse = responseBuilder status200 [("Content-Type", "plain/text")] (lazyByteString "hello world")

pubsub' :: Text
pubsub' = "pubsub-mqtt"

topic' :: Text
topic' = "http-pubsub-example"

subscriptions :: [SubscriptionInfo]
subscriptions = [SubscriptionInfo {pubsubname = pubsub', topic = topic', route = "/message", Dapr.Server.HttpServer.Types.metadata = Data.Map.empty}]

server :: IO ()
server = do
  putStrLn "Started server at http://localhost:3000/"
  run 3000 $ addDaprSubscriptions subscriptions app

main :: IO ()
main = do
  _ <- forkIO server
  threadDelay $ 300 * 1000
  _ <- publishMessageWithJsonPayload defaultDaprConfig (PublishEventRequest (PubsubName pubsub') (PubsubTopic topic') helloWorld Nothing mempty)
  threadDelay $ 300 * 1000
  return ()
