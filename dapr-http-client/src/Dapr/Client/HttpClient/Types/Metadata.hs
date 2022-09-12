module Dapr.Client.HttpClient.Types.Metadata where

import Dapr.Client.HttpClient.Types.Internal
import Data.Aeson
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

data DaprMetadata = DaprMetadata
  { metadataId :: Text,
    metadataActors :: [DaprMetadataActor],
    metadataComponents :: [DaprMetadataComponent],
    metadataExtended :: Map Text Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON DaprMetadata where
  parseJSON = customParseJSON 8

type MetadataAttribute = Text

type RawData = Text

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
