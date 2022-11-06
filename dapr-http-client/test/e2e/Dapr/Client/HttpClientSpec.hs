module Dapr.Client.HttpClientSpec where

import Control.Monad.IO.Class (MonadIO)
import Dapr.Client.HttpClient
import Data.Aeson (ToJSON, object)
import Data.Either
import Data.Map as Map
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Req
import Test.Hspec

newtype TestHelloWorldMessage = TestHelloWorldMessage
  { message :: Text
  }
  deriving (Show, Eq, Generic, ToJSON)

redisStore :: StateStore
redisStore = StateStore "state-redis"

mqttBinding :: Binding
mqttBinding = Binding "binding-mqtt"

pubsubRedis :: PubsubName
pubsubRedis = PubsubName "pubsub-redis"

testTopic :: PubsubTopic
testTopic = PubsubTopic "test-topic"

runReq_ :: MonadIO m => DaprHttpClient a -> m a
runReq_ = runDaprHttpClient defaultDaprConfig

cleanState :: IO ()
cleanState = runReq_ $ do
  let request1 = DeleteStateRequest redisStore (StateKey "key-1") Nothing Nothing mempty
  let request2 = DeleteStateRequest redisStore (StateKey "key-2") Nothing Nothing mempty
  let request3 = DeleteStateRequest redisStore (StateKey "key-3") Nothing Nothing mempty
  _ <- deleteState request1
  _ <- deleteState request2
  _ <- deleteState request3
  return ()

saveDefaultState :: IO (Either DaprClientError ())
saveDefaultState = runReq_ $ do
  let stateItem1 = StateItem (StateKey "key-1") ("value-1" :: Text) Nothing mempty Nothing
  let stateItem2 = StateItem (StateKey "key-2") ("value-2" :: Text) Nothing mempty Nothing
  let stateItem3 = StateItem (StateKey "key-3") ("value-3" :: Text) Nothing mempty Nothing
  saveState
    ( SaveStateRequest
        redisStore
        [stateItem1, stateItem2, stateItem3]
    )

spec :: Spec
spec = do
  describe "Health check" $ do
    it "Dapr service is Health" $ do
      r <- runReq_ checkHealth
      r `shouldBe` DaprHealthy

  describe "Metadata" $ do
    it "Can get metadata" $ do
      r <- runReq_ getMetadata
      isRight r `shouldBe` True
      let d = head $ rights [r]
      metadataId d `shouldBe` "haskell-dapr"

    it "Add custom label" $ do
      _ <- runReq_ $ setMetadata (SetMetadataRequest "testKey" "Hello World")
      r <- runReq_ getMetadata
      isRight r `shouldBe` True
      let d = head $ rights [r]
          extendedData = metadataExtended d
      Map.lookup "testKey" extendedData `shouldBe` Just "Hello World"

  describe "State management" $
    before_ cleanState $ do
      it "Save state" $ do
        r <- saveDefaultState
        isRight r `shouldBe` True
        rs <- runReq_ $ getState (GetStateRequest redisStore (StateKey "key-1") Nothing mempty)
        isRight rs `shouldBe` True
        let d = head $ rights [rs]
        d `shouldBe` ("value-1" :: Text)

      it "Save state with metadata, etag and options" $ do
        let stateItem1 =
              StateItem
                { stateItemKey = StateKey "key-1",
                  stateItemValue = "value-1" :: Text,
                  stateItemEtag = Just $ Etag "1",
                  stateItemMetadata = Map.fromList [("hello", "world")],
                  stateItemOption =
                    Just $
                      StateOption
                        { concurrency = ConcurrencyFirstWrite,
                          consistency = ConsistencyStrong
                        }
                }
            stateItem2 = StateItem (StateKey "key-2") ("value-2" :: Text) Nothing mempty Nothing
            stateItem3 = StateItem (StateKey "key-3") ("value-3" :: Text) Nothing mempty Nothing
        r <-
          runReq_ $
            saveState
              (SaveStateRequest redisStore [stateItem1, stateItem2, stateItem3])
        isRight r `shouldBe` True
        rs <- runReq_ $ getState (GetStateRequest redisStore (StateKey "key-1") Nothing mempty)
        isRight rs `shouldBe` True
        let d = head $ rights [rs]
        d `shouldBe` ("value-1" :: Text)

      it "Get state with non-existed key" $ do
        _ <- saveDefaultState
        r <- runReq_ $ getState (GetStateRequest redisStore (StateKey "key-non-exist") Nothing mempty) :: IO (Either DaprClientError Text)
        isLeft r `shouldBe` True

      it "Get bulk states" $ do
        _ <- saveDefaultState
        r <-
          runReq_ $
            getBulkState
              (GetBulkStateRequest redisStore [StateKey "key-2", StateKey "key-3"] 10 mempty) ::
            IO (Either DaprClientError (GetBulkStateResponse Text))
        isRight r `shouldBe` True
        let (GetBulkStateResponse items) = fromRight (GetBulkStateResponse []) r
        length items `shouldBe` 2
        let (BulkStateItem key1 value1 _ _ _) = head items
        let (BulkStateItem key2 value2 _ _ _) = head $ tail items
        getStateKey key1 `shouldBe` "key-2"
        value1 `shouldBe` Just "value-2"
        getStateKey key2 `shouldBe` "key-3"
        value2 `shouldBe` Just "value-3"

      it "Delete state" $ do
        _ <- saveDefaultState
        r <- runReq_ $ deleteState (DeleteStateRequest redisStore (StateKey "key-1") Nothing Nothing mempty)
        isRight r `shouldBe` True
        rs <-
          runReq_ $
            getState
              (GetStateRequest redisStore (StateKey "key-not-exist") Nothing mempty) ::
            IO (Either DaprClientError Text)
        show rs `shouldBe` "Left NotFound"

      it "Delete state with non-existed key" $ do
        r <-
          runReq_ $
            deleteState
              (DeleteStateRequest redisStore (StateKey "key-non-exist") Nothing Nothing mempty) ::
            IO (Either DaprClientError ())
        isRight r `shouldBe` True

      it "Execute transaction" $ do
        let request =
              ExecuteStateTransactionRequest
                redisStore
                [ TransactionalStateOperation TransactionOperationUpsert (StateItem (StateKey "key-1") (Just "my-new-data-1" :: Maybe Text) Nothing mempty Nothing),
                  TransactionalStateOperation TransactionOperationDelete (StateItem (StateKey "key-2") Nothing Nothing mempty Nothing)
                ]
                mempty
        r <- runReq_ $ executeStateTransaction request
        isRight r `shouldBe` True
        rs <-
          runReq_ $
            getState
              (GetStateRequest redisStore (StateKey "key-1") Nothing mempty)
        isRight rs `shouldBe` True
        let d = head $ rights [rs] :: Text
        d `shouldBe` "my-new-data-1"
        rs' <-
          runReq_ $
            getState
              (GetStateRequest redisStore (StateKey "key-3") Nothing mempty) ::
            IO (Either DaprClientError Text)
        show rs' `shouldBe` "Left NotFound"

  describe "Pubsub" $ it "Can publish message" $ do
    r <- runReq_ $ publishMessage (PublishEventRequest pubsubRedis testTopic (ReqBodyJson (TestHelloWorldMessage "Hello World")) Nothing mempty)
    isRight r `shouldBe` True

  describe "OutputBinding" $ it "Invokes a dapr output binding" $ do
    let dataObject = object [("k1", "v1"), ("k2", "v2")]
        bindingRequest = InvokeBindingRequest mqttBinding "create" dataObject (Map.fromList [("key", "123")])
    r <- runReq_ $ invokeOutputBinding bindingRequest
    isRight r `shouldBe` True

  describe "Secrets" $ do
    it "Can get secret" $ do
      r <- runReq_ $ getSecrets $ GetSecretRequest (SecretStore "secret-localstore") (SecretKey "username") empty
      isRight r `shouldBe` True
      let (GetSecretResponse r') = fromRight (GetSecretResponse empty) r
      Map.lookup "username" r' `shouldBe` Just "admin"

    it "Can get secrets in bulk" $ do
      r <- runReq_ $ getBulkSecrets $ GetBulkSecretRequest (SecretStore "secret-localstore") empty
      isRight r `shouldBe` True
      let (GetBulkSecretResponse r') = fromRight (GetBulkSecretResponse empty) r
      Map.size r' >= 1 `shouldBe` True
