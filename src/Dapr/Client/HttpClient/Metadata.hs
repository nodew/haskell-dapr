module Dapr.Client.HttpClient.Metadata where

import Dapr.Client.HttpClient.Core
import Dapr.Client.HttpClient.Internal
import Data.Aeson
import Network.HTTP.Req
import RIO

data DaprMetadata = DaprMetadata
  { metadataId :: Text,
    metadataActors :: [DaprMetadataActor],
    metadataComponents :: [DaprMetadataComponent],
    metadataExtended :: Map Text Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON DaprMetadata where
  parseJSON = customParseJSON 8

data DaprMetadataActor = DaprMetadataActor
  { metadataActorType :: Text,
    metadataActorCount :: Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON DaprMetadataActor where
  parseJSON = customParseJSON 13

data DaprMetadataComponent = DaprMetadataComponent
  { componentName :: Text,
    componentType :: Text,
    componentVersion :: Text,
    componentCapabilities :: [Text]
  }
  deriving (Eq, Show, Generic)

instance FromJSON DaprMetadataComponent where
  parseJSON = customParseJSON 9

getMetadata :: MonadIO m => DaprClientConfig -> m (Either DaprClientError DaprMetadata)
getMetadata config = do
  response <- makeRequest config GET ["metadata"] NoReqBody jsonResponse mempty
  return $ bimap DaprHttpException responseBody response
