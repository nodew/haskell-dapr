-- |
-- Module      : Dapr.Core.Types.Metadata
-- Description : Defines the types used by Metadata module
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines the types used by Metadata module.
module Dapr.Core.Types.Metadata where

import Dapr.Core.Types.Common (ExtendedMetadata)
import Dapr.Core.Types.Internal (customParseJSON)
import Data.Aeson (FromJSON (parseJSON))
import Data.Text (Text)
import GHC.Generics (Generic)

-- | The 'GetMetadataResponse' is the message that is returned by GetMetadata call
data GetMetadataResponse = GetMetadataResponse
  { -- | Metadata ID
    metadataId :: Text,
    -- | Active actor counts
    metadataActors :: [ActiveActorsCount],
    -- | Registered componoents
    metadataComponents :: [RegisteredComponent],
    -- | Extended metadata
    metadataExtended :: ExtendedMetadata
  }
  deriving (Eq, Show, Generic)

instance FromJSON GetMetadataResponse where
  parseJSON = customParseJSON 8

-- | 'ActiveActorsCount' represents registered actor and count
data ActiveActorsCount = ActiveActorsCount
  { -- | The registered actor type.
    activeActorType :: Text,
    -- | Number of actors running
    activeActorCount :: Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON ActiveActorsCount where
  parseJSON = customParseJSON 11

-- | 'ActiveActorsCount' represents registered component
data RegisteredComponent = RegisteredComponent
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

instance FromJSON RegisteredComponent where
  parseJSON = customParseJSON 9

-- | The 'SetMetadataRequest' is the request message of set metadata call
data SetMetadataRequest = SetMetadataRequest
  { key :: Text,
    value :: Text
  }
