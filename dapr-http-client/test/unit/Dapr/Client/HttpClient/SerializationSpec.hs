module Dapr.Client.HttpClient.SerializationSpec where

import Dapr.Client.HttpClient
import Data.Aeson.Encode.Pretty as Aeson
import Data.Map (fromList)
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.Golden

goldenBaseDir :: FilePath
goldenBaseDir = "test" </> "unit" </> ".golden"

goldenText :: String -> Text -> Golden Text
goldenText name actualOutput =
  Golden
    { output = actualOutput,
      encodePretty = show,
      writeToFile = T.writeFile,
      readFromFile = T.readFile,
      -- goldenFile = goldenBaseDir </> name </> name <> ".golden",
      -- actualFile = Just (goldenBaseDir </> name </> name <> ".actual"),
      failFirstTime = False
    }

spec :: Spec
spec = do
  describe "StateQuery" $ do
    it "Parse StateQuery to json" $ do
      let stateQuery =
            StateQuery
              { stateQueryFilter =
                  OrFilter
                    [ EqFilter $ fromList [("person.org", "Dev Ops")],
                      AndFilter
                        [ EqFilter $ fromList [("person.org", "Finance")],
                          InFilter $ fromList [("state", ["CA", "WA"])]
                        ]
                    ],
                stateQuerySort =
                  [ StateQuerySort
                      { stateOrderKey = "state",
                        stateOrderOrder = Just StateQueryOrderDesc
                      },
                    StateQuerySort
                      { stateOrderKey = "person.id",
                        stateOrderOrder = Nothing
                      }
                  ],
                stateQueryPage =
                  StateQueryPagination
                    { paginationLimit = 10,
                      paginationToken = Nothing
                    }
              }
      goldenText "stateQuery" (T.toStrict $ T.decodeUtf8 $ Aeson.encodePretty stateQuery)
