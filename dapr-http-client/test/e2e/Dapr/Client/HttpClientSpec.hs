module Dapr.Client.HttpClientSpec where

import Dapr.Client.HttpClient
import Dapr.Common
import Data.Either
import Data.Map as Map
import Data.Text (Text)
import Test.Hspec
import Data.Aeson (ToJSON)
import GHC.Generics

newtype TestHelloWorldMessage = TestHelloWorldMessage
  { message :: Text
  } deriving (Show, Eq, Generic, ToJSON)

cleanState :: IO ()
cleanState = do
  _ <- deleteState defaultDaprConfig "state-redis" "key-1" Nothing Nothing Nothing Nothing
  _ <- deleteState defaultDaprConfig "state-redis" "key-2" Nothing Nothing Nothing Nothing
  _ <- deleteState defaultDaprConfig "state-redis" "key-3" Nothing Nothing Nothing Nothing
  return ()

saveDefaultState :: IO (Either DaprClientError ())
saveDefaultState =
  saveState
    defaultDaprConfig
    "state-redis"
    [ makeSimpleSaveStateRequest "key-1" ("value-1" :: Text),
      makeSimpleSaveStateRequest "key-2" ("value-2" :: Text),
      makeSimpleSaveStateRequest "key-3" ("value-3" :: Text)
    ]

spec :: Spec
spec = do
  describe "Health check" $ do
    it "Dapr service is Health" $ do
      r <- checkHealth defaultDaprConfig
      r `shouldBe` Healthy

  describe "Metadata" $ do
    it "Can get metadata" $ do
      r <- getMetadata defaultDaprConfig
      isRight r `shouldBe` True
      let d = head $ rights [r]
      metadataId d `shouldBe` "haskell-dapr"
    it "Add custom label" $ do
      _ <- addCustomLabel defaultDaprConfig "testKey" "Hello World"
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
        rs <- getState defaultDaprConfig "state-redis" "key-1" Nothing Nothing
        isRight rs `shouldBe` True
        let d = head $ rights [rs]
        d `shouldBe` ("value-1" :: Text)

      it "Save state with metadata, etag and options" $ do
        r <-
          saveState
            defaultDaprConfig
            "state-redis"
            [ SaveStateRequest
                { stateKey = "key-1",
                  stateValue = "value-1",
                  stateEtag = Just "1",
                  stateOptions =
                    Just
                      SaveStateOptions
                        { concurrency = FirstWrite,
                          consistency = Strong
                        },
                  stateMetadata = Just $ Map.fromList [("hello", "world")]
                },
              makeSimpleSaveStateRequest "key-2" ("value-2" :: Text),
              makeSimpleSaveStateRequest "key-3" ("value-3" :: Text)
            ]
        isRight r `shouldBe` True
        rs <- getState defaultDaprConfig "state-redis" "key-1" Nothing Nothing
        isRight rs `shouldBe` True
        let d = head $ rights [rs]
        d `shouldBe` ("value-1" :: Text)

      it "Get state with non-existed key" $ do
        _ <- saveDefaultState
        r <- getState defaultDaprConfig "state-redis" "key-not-exist" Nothing Nothing :: IO (Either DaprClientError Text)
        isLeft r `shouldBe` True

      it "Get bulk states" $ do
        _ <- saveDefaultState
        r <- getBulkState defaultDaprConfig "state-redis" ["key-2", "key-3"] Nothing Nothing :: IO (Either DaprClientError [BulkStateItem Text])
        isRight r `shouldBe` True
        let items = fromRight [] r
        length items `shouldBe` 2
        let [BulkStateItem key1 value1 _, BulkStateItem key2 value2 _] = items
        key1 `shouldBe` "key-2"
        value1 `shouldBe` Just "value-2"
        key2 `shouldBe` "key-3"
        value2 `shouldBe` Just "value-3"

      it "Delete state" $ do
        _ <- saveDefaultState
        r <- deleteState defaultDaprConfig "state-redis" "key-1" Nothing Nothing Nothing Nothing
        isRight r `shouldBe` True
        rs <- getState defaultDaprConfig "state-redis" "key-1" Nothing Nothing :: IO (Either DaprClientError Text)
        show rs `shouldBe` "Left NotFound"

      it "Delete state with non-existed key" $ do
        r <- deleteState defaultDaprConfig "state-redis" "key1" Nothing Nothing Nothing Nothing :: IO (Either DaprClientError ())
        isRight r `shouldBe` True

      it "Execute transaction" $ do
        r <-
          executeStateTransaction
            defaultDaprConfig
            "state-redis"
            ( StateTransaction
                [ StateOperation Upsert (StateOperationRequest "key-1" (Just ("my-new-data-1" :: Text)) Nothing Nothing Nothing),
                  StateOperation Delete (StateOperationRequest "key-3" Nothing Nothing Nothing Nothing)
                ]
                Nothing
            )
        isRight r `shouldBe` True
        rs <- getState defaultDaprConfig "state-redis" "key-1" Nothing Nothing
        isRight rs `shouldBe` True
        let d = head $ rights [rs] :: Text
        d `shouldBe` "my-new-data-1"
        rs' <- getState defaultDaprConfig "state-redis" "key-3" Nothing Nothing :: IO (Either DaprClientError Text)
        show rs' `shouldBe` "Left NotFound"

  describe "Pubsub" $ do
    it "Can publish message" $ do
      r <- publishJsonMessage defaultDaprConfig "pubsub-redis" "test-topic" (TestHelloWorldMessage "Hello World") Nothing
      print r
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
