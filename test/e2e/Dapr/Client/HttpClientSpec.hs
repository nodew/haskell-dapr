module Dapr.Client.HttpClientSpec where

import Dapr.Client.HttpClient
import Dapr.Common
import Data.Either
import Data.Text (Text)
import Test.Hspec

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

  describe "State management" $ do
    it "Save state" $ do
      r <- saveState defaultDaprConfig "statestore" [makeSimpleSaveStateRequest "key" ("value" :: Text)]
      isRight r `shouldBe` True

    it "Get state" $ do
      r <- getState defaultDaprConfig "statestore" "key" Nothing Nothing
      isRight r `shouldBe` True
      let d = head $ rights [r]
      d `shouldBe` ("value" :: Text)

    it "Get state with non-existed key" $ do
      r <- getState defaultDaprConfig "statestore" "key-not-exist" Nothing Nothing :: IO (Either DaprClientError Text)
      isLeft r `shouldBe` True

    it "Get bulk states" $ do
      r <- getBulkState defaultDaprConfig "statestore" ["key", "key1"] Nothing Nothing :: IO (Either DaprClientError [BulkStateItem Text])
      isRight r `shouldBe` True
      let items = fromRight [] r
      length items `shouldBe` 2
      let (BulkStateItem key val _) = head items
      key `shouldBe` "key"
      val `shouldBe` Just "value"

    it "Delete state" $ do
      r <- deleteState defaultDaprConfig "statestore" "key" Nothing Nothing Nothing Nothing
      isRight r `shouldBe` True

    it "Delete state with non-existed key" $ do
      r <- deleteState defaultDaprConfig "statestore" "key1" Nothing Nothing Nothing Nothing :: IO (Either DaprClientError ())
      isRight r `shouldBe` True

    it "Execute transaction" $ do
      r <-
        executeStateTransaction
          defaultDaprConfig
          "statestore"
          ( StateTransaction
              [ StateOperation Upsert (StateOperationRequest "key" (Just ("value" :: Text)) Nothing Nothing Nothing),
                StateOperation Delete (StateOperationRequest "key1" Nothing Nothing Nothing Nothing)
              ]
              Nothing
          )
      isRight r `shouldBe` True
