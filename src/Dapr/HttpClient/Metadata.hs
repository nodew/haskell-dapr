module Dapr.HttpClient.Metadata where

import Dapr.HttpClient.Core
import Dapr.HttpClient.Internal
import Data.Aeson
import Network.HTTP.Req
import RIO

data DaprMetadata = DaprMetadata
  { dmId :: Text,
    dmActors :: [DaprMetadataActor],
    dmComponents :: [DaprMetadataComponent],
    dmExtended :: Map Text Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON DaprMetadata where
  parseJSON = customParseJSON 2

data DaprMetadataActor = DaprMetadataActor
  { dmaType :: Text,
    dmaCount :: Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON DaprMetadataActor where
  parseJSON = customParseJSON 3

data DaprMetadataComponent = DaprMetadataComponent
  { dmcName :: Text,
    dmcType :: Text,
    dmcVersion :: Text,
    dmcCapabilities :: [Text]
  }
  deriving (Eq, Show, Generic)

instance FromJSON DaprMetadataComponent where
  parseJSON = customParseJSON 3

getMetadata :: MonadIO m => DaprClientConfig -> m (Either DaprClientError DaprMetadata)
getMetadata config = do
  response <- makeRequest config GET ("metadata" :: Text) NoReqBody jsonResponse mempty
  return $ case responseStatusCode response of
    200 -> Right $ responseBody response
    500 -> Left $ DaprClientError (HttpException 500) "Dapr could not return the metadata information"
    _ -> Left $ DaprClientError UnknownError "Unknonwn response status code"
