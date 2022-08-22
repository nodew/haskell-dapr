module Dapr.HttpClient.Metadata where

import Dapr.HttpClient.Core (DaprClientConfig, makeRequest)
import Dapr.HttpClient.Internal (customParseJSON, isSucceedResponse)
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

getMetadata :: MonadIO m => DaprClientConfig -> m (Either Text DaprMetadata)
getMetadata config = do
  response <- makeRequest config GET ("metadata" :: Text) NoReqBody jsonResponse mempty
  return $
    if isSucceedResponse response
      then Right $ responseBody response
      else Left "Dapr could not return the metadata information"
