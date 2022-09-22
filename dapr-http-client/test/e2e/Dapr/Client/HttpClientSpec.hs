module Dapr.Client.HttpClientSpec where

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

cleanState :: IO ()
cleanState = do
  let request = DeleteStateRequest redisStore (StateKey "key-1") Nothing Nothing mempty
  _ <- deleteState defaultDaprConfig request
  _ <- deleteState defaultDaprConfig (request {stateKey = StateKey "key-2"})
  _ <- deleteState defaultDaprConfig (request {stateKey = StateKey "key-3"})
  return ()

saveDefaultState :: IO (Either DaprClientError ())
saveDefaultState = do
  let stateItem1 = StateItem (StateKey "key-1") ("value-1" :: Text) Nothing mempty Nothing
  let stateItem2 = StateItem (StateKey "key-2") ("value-2" :: Text) Nothing mempty Nothing
  let stateItem3 = StateItem (StateKey "key-3") ("value-3" :: Text) Nothing mempty Nothing
  saveState
    defaultDaprConfig
    ( SaveStateRequest
        redisStore
        [stateItem1, stateItem2, stateItem3]
    )

spec :: Spec
spec = do
  describe "Health check" $ it "Dapr service is Health" $ do
    r <- checkHealth defaultDaprConfig
    r `shouldBe` DaprHealthy

  describe "Metadata" $ do
    it "Can get metadata" $ do
      r <- getMetadata defaultDaprConfig
      isRight r `shouldBe` True
      let d = head $ rights [r]
      metadataId d `shouldBe` "haskell-dapr"

    it "Add custom label" $ do
      _ <- setMetadata defaultDaprConfig (SetMetadataRequest "testKey" "Hello World")
      r <- getMetadata defaultDaprConfig
      isRight r `shouldBe` True
      let d = head $ rights [r]
          extendedData = metadataExtended d
      Map.lookup "testKey" extendedData `shouldBe` Just "Hello World"

  describe "State management" $
    before_ cleanState $ do
      it "Save state" $ do
        r <- saveDefaultState
        isRight r `shouldBe` True
        rs <- getState defaultDaprConfig (GetStateRequest redisStore (StateKey "key-1") Nothing mempty)
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
          saveState
            defaultDaprConfig
            (SaveStateRequest redisStore [stateItem1, stateItem2, stateItem3])
        isRight r `shouldBe` True
        rs <- getState defaultDaprConfig (GetStateRequest redisStore (StateKey "key-1") Nothing mempty)
        isRight rs `shouldBe` True
        let d = head $ rights [rs]
        d `shouldBe` ("value-1" :: Text)

      it "Get state with non-existed key" $ do
        _ <- saveDefaultState
        r <- getState defaultDaprConfig (GetStateRequest redisStore (StateKey "key-non-exist") Nothing mempty) :: IO (Either DaprClientError Text)
        isLeft r `shouldBe` True

      it "Get bulk states" $ do
        _ <- saveDefaultState
        r <-
          getBulkState
            defaultDaprConfig
            (GetBulkStateRequest redisStore [StateKey "key-2", StateKey "key-3"] 10 mempty) ::
            IO (Either DaprClientError [BulkStateItem Text])
        isRight r `shouldBe` True
        let items = fromRight [] r
        length items `shouldBe` 2
        let (BulkStateItem key1 value1 _ _ _) = head items
        let (BulkStateItem key2 value2 _ _ _) = head $ tail items
        getStateKey key1 `shouldBe` "key-2"
        value1 `shouldBe` Just "value-2"
        getStateKey key2 `shouldBe` "key-3"
        value2 `shouldBe` Just "value-3"

      it "Delete state" $ do
        _ <- saveDefaultState
        r <- deleteState defaultDaprConfig (DeleteStateRequest redisStore (StateKey "key-1") Nothing Nothing mempty)
        isRight r `shouldBe` True
        rs <-
          getState
            defaultDaprConfig
            (GetStateRequest redisStore (StateKey "key-not-exist") Nothing mempty) ::
            IO (Either DaprClientError Text)
        show rs `shouldBe` "Left NotFound"

      it "Delete state with non-existed key" $ do
        r <-
          deleteState
            defaultDaprConfig
            (DeleteStateRequest redisStore (StateKey "key-non-exist") Nothing Nothing mempty) ::
            IO (Either DaprClientError ())
        isRight r `shouldBe` True

      it "Execute transaction" $ do
        let request =
              ExecuteStateTransactionRequest
                redisStore
                [ TransactionalStateOperation TransactionOperationUpsert (TransactionalUpsert (StateKey "key-1") ("my-new-data-1" :: Text)),
                  TransactionalStateOperation TransactionOperationDelete (TransactionalDelete (StateKey "key-3"))
                ]
                mempty
        r <- executeStateTransaction defaultDaprConfig request
        isRight r `shouldBe` True
        rs <-
          getState
            defaultDaprConfig
            (GetStateRequest redisStore (StateKey "key-1") Nothing mempty)
        isRight rs `shouldBe` True
        let d = head $ rights [rs] :: Text
        d `shouldBe` "my-new-data-1"
        rs' <-
          getState
            defaultDaprConfig
            (GetStateRequest redisStore (StateKey "key-3") Nothing mempty) ::
            IO (Either DaprClientError Text)
        show rs' `shouldBe` "Left NotFound"

  describe "Pubsub" $ it "Can publish message" $ do
    r <- publishMessage defaultDaprConfig (PublishEventRequest pubsubRedis testTopic (ReqBodyJson (TestHelloWorldMessage "Hello World")) "application/json" mempty)
    isRight r `shouldBe` True

  describe "OutputBinding" $ it "Invokes a dapr output binding" $ do
    let dataObject = object [("k1", "v1"), ("k2", "v2")]
        bindingRequest = InvokeBindingRequest mqttBinding "create" dataObject (Map.fromList [("key", "123")])
    r <- invokeOutputBinding defaultDaprConfig bindingRequest
    isRight r `shouldBe` True

-- describe "Secrets" $ do
--   it "Can get secret" $ do
--     r <- getSecrets defaultDaprConfig "secret-envvars" "TEST_SECRET_1" Nothing
--     isRight r `shouldBe` True
--     let d = fromRight Map.empty r
--     Map.lookup "TEST_SECRET_1" d `shouldBe` Just "secret_val_1"

--   it "Can get secrets in bulk" $ do
--     r <- getBulkSecrets defaultDaprConfig "secret-envvars" Nothing
--     isRight r `shouldBe` True
--     let d = fromRight Map.empty r
--     Map.size d >= 1 `shouldBe` True
