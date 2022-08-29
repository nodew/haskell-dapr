module Dapr.Common.Metadata where

import Dapr.Common.Internal
import Data.Aeson
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
