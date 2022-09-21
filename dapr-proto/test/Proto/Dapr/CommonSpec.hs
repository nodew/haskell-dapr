{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Proto.Dapr.CommonSpec where

import Data.ProtoLens (defMessage, showMessage)
import Lens.Micro
import qualified Proto.Dapr.Proto.Common.V1.Common as P
import qualified Proto.Dapr.Proto.Common.V1.Common_Fields as P
import Test.Hspec

spec :: Spec
spec = do
  describe "Dapr Common proto" $ do
    it "Parse StateItem message" $ do
      let stateItem :: P.StateItem =
            defMessage
              & P.key .~ "Fintan"
              & P.value .~ "24"
      showMessage stateItem `shouldBe` "key: \"Fintan\" value: \"24\""
