module Dapr.HttpClientSpec where

import Dapr.HttpClient
import Data.List (head)
import RIO
import Test.Hspec

spec :: Spec
spec = do
  describe "Health" $ do
    it "Dapr service is Health" $ do
      r <- checkHealth defaultDaprClientConfig
      r `shouldBe` Healthy

  describe "Metadata" $ do
    it "Can get metadata" $ do
      r <- getMetadata defaultDaprClientConfig
      isRight r `shouldBe` True
      let d = head $ rights [r]
      dmId d `shouldBe` "haskell-dapr"

  describe "State management" $ do
    it "Save state" $ do
      r <- saveState defaultDaprClientConfig "statestore" [mkSimleSaveStateReqBody "key" ("value" :: Text)]
      isRight r `shouldBe` True

    it "Get state" $ do
      r <- getState defaultDaprClientConfig "statestore" "key" Nothing Nothing
      isRight r `shouldBe` True
      let d = head $ rights [r]
      d `shouldBe` ("value" :: Text)

    it "Get state with false key" $ do
      r <- getState defaultDaprClientConfig "statestore" "key1" Nothing Nothing :: IO (Either Text Text)
      isRight r `shouldBe` False
      let d = head $ lefts [r]
      d `shouldBe` "Key is not found"
