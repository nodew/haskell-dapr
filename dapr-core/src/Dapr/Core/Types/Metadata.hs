module Dapr.Core.Types.Metadata where

import Dapr.Core.Types.Internal
import Dapr.Core.Types.Common
import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

-- | The 'GetMetadataResponse' is the message that is returned by GetMetadata call
data GetMetadataResponse = GetMetadataResponse
  { metadataId :: Text,
    metadataActors :: [ActiveActorsCount],
    metadataComponents :: [RegisteredComponent],
    metadataExtended :: ExtendedMetadata
  }
  deriving (Eq, Show, Generic)

instance FromJSON GetMetadataResponse where
  parseJSON = customParseJSON 8

data ActiveActorsCount = ActiveActorsCount
  { actorType :: Text,
    actorCount :: Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON ActiveActorsCount where
  parseJSON = customParseJSON 5

data RegisteredComponent = RegisteredComponent
  { componentName :: Text,
    componentType :: Text,
    componentVersion :: Text,
    componentCapabilities :: [Text]
  }
  deriving (Eq, Show, Generic)

instance FromJSON RegisteredComponent where
  parseJSON = customParseJSON 9

-- | The 'SetMetadataRequest' is the request message of SetMetadata call
data SetMetadataRequest = SetMetadataRequest
  { key :: Text,
    value :: Text
  }
