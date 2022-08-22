module Dapr.HttpClientSpec where

import Dapr.HttpClient
import Network.HTTP.Req (defaultHttpConfig, runReq)
import RIO
import Test.Hspec
import Data.List ( head )

spec :: Spec
spec = do
  describe "Health" $ do
    it "Dapr service is Health" $ do
      r <- runReq defaultHttpConfig $ checkHealth defaultDaprClientConfig
      r `shouldBe` Healthy

  describe "Metadata" $ do
    it "Can get metadata" $ do
      r <- runReq defaultHttpConfig $ getMetadata defaultDaprClientConfig
      isRight r `shouldBe` True
      let d = head $ rights [r]
      dmId d `shouldBe` "haskell-dapr"

