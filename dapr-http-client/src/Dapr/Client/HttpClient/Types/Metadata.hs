-- |
-- Module      : Types.Metadata
-- Description : Defines the types used by Metadata module
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines the types used by Metadata module
module Dapr.Client.HttpClient.Types.Metadata where

import Dapr.Client.HttpClient.Types.Internal
import Data.Aeson
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Dapr Metadata Information by given metadataId
data DaprMetadata = DaprMetadata
  { -- | Application Id
    metadataId :: Text,
    -- | A list of registered actors metadata.
    metadataActors :: [DaprMetadataActor],
    -- | An array of loaded components metadata.
    metadataComponents :: [DaprMetadataComponent],
    -- | A collection of metadata key-value pairs that will be provided to the binding
    metadataExtended :: Map Text Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON DaprMetadata where
  parseJSON = customParseJSON 8

-- | Represents Metadata Attribute
type MetadataAttribute = Text

-- | Represents Raw Text data
type RawData = Text

-- | Represents Metadata API Response Registered Actor
data DaprMetadataActor = DaprMetadataActor
  { -- | The registered actor type.
    metadataActorType :: Text,
    -- | Number of actors running
    metadataActorCount :: Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON DaprMetadataActor where
  parseJSON = customParseJSON 13

-- | Represents Metadata API Response Component
data DaprMetadataComponent = DaprMetadataComponent
  { -- | Name of the component
    componentName :: Text,
    -- | Component type
    componentType :: Text,
    -- | Component version
    componentVersion :: Text,
    -- | Supported capabilities for this component type and version
    componentCapabilities :: [Text]
  }
  deriving (Eq, Show, Generic)

instance FromJSON DaprMetadataComponent where
  parseJSON = customParseJSON 9
