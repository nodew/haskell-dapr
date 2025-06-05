{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Dapr.Core.Types.Metadata
-- Description : Defines the types used by Metadata module
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines the types used by Metadata module.
module Dapr.Core.Types.Metadata where

import Dapr.Core.Types.Common (ExtendedMetadata, PubsubSubscriptionType)
import Dapr.Core.Types.Internal (customParseJSON, customToJSON)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | 'GetMetadataRequest' is the message for the GetMetadata request
data GetMetadataRequest = GetMetadataRequest
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | 'ActorRuntimeStatus' indicates the status of the actor runtime
data ActorRuntimeStatus
  = ActorRuntimeInitializing -- ^ Indicates that the actor runtime is still being initialized
  | ActorRuntimeDisabled     -- ^ Indicates that the actor runtime is disabled
  | ActorRuntimeRunning      -- ^ Indicates the actor runtime is running
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | 'ActorRuntime' contains information about the actor runtime
data ActorRuntime = ActorRuntime
  { -- | Contains an enum indicating whether the actor runtime has been initialized
    actorRuntimeStatus :: ActorRuntimeStatus,
    -- | Count of active actors per type
    actorRuntimeActiveActors :: [ActiveActorsCount],
    -- | Indicates whether the actor runtime is ready to host actors
    actorRuntimeHostReady :: Bool,
    -- | Custom message from the placement provider
    actorRuntimePlacement :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON ActorRuntime where
  parseJSON = customParseJSON 12

-- | 'AppConnectionHealthProperties' contains health check configuration
data AppConnectionHealthProperties = AppConnectionHealthProperties
  { -- | Health check path
    healthCheckPath :: Text,
    -- | Health probe interval
    healthProbeInterval :: Text,
    -- | Health probe timeout
    healthProbeTimeout :: Text,
    -- | Health threshold
    healthThreshold :: Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON AppConnectionHealthProperties where
  parseJSON = customParseJSON 11

-- | 'AppConnectionProperties' contains app connection configuration
data AppConnectionProperties = AppConnectionProperties
  { -- | Port number
    appConnectionPort :: Int,
    -- | Protocol used
    appConnectionProtocol :: Text,
    -- | Channel address
    appConnectionChannelAddress :: Text,
    -- | Maximum concurrency
    appConnectionMaxConcurrency :: Int,
    -- | Health properties
    appConnectionHealth :: AppConnectionHealthProperties
  }
  deriving (Eq, Show, Generic)

instance FromJSON AppConnectionProperties where
  parseJSON = customParseJSON 13

-- | 'PubsubSubscriptionRule' represents a rule for pubsub subscription
data PubsubSubscriptionRule = PubsubSubscriptionRule
  { -- | Match criteria
    pubsubRuleMatch :: Text,
    -- | Path for the rule
    pubsubRulePath :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON PubsubSubscriptionRule where
  parseJSON = customParseJSON 11

-- | 'PubsubSubscriptionRules' contains a list of rules
data PubsubSubscriptionRules = PubsubSubscriptionRules
  { -- | List of rules
    pubsubRules :: [PubsubSubscriptionRule]
  }
  deriving (Eq, Show, Generic)

instance FromJSON PubsubSubscriptionRules where
  parseJSON = customParseJSON 7

-- | 'PubsubSubscription' represents a pubsub subscription
data PubsubSubscription = PubsubSubscription
  { -- | Pubsub name
    pubsubSubscriptionPubsubName :: Text,
    -- | Topic name
    pubsubSubscriptionTopic :: Text,
    -- | Metadata
    pubsubSubscriptionMetadata :: Map Text Text,
    -- | Rules
    pubsubSubscriptionRules :: PubsubSubscriptionRules,
    -- | Dead letter topic
    pubsubSubscriptionDeadLetterTopic :: Text,
    -- | Subscription type
    pubsubSubscriptionType :: PubsubSubscriptionType
  }
  deriving (Eq, Show, Generic)

instance FromJSON PubsubSubscription where
  parseJSON = customParseJSON 19

-- | 'MetadataHTTPEndpoint' represents an HTTP endpoint
data MetadataHTTPEndpoint = MetadataHTTPEndpoint
  { -- | Name of the endpoint
    httpEndpointName :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON MetadataHTTPEndpoint where
  parseJSON = customParseJSON 12

-- | 'MetadataScheduler' contains the list of addresses of scheduler connections
data MetadataScheduler = MetadataScheduler
  { -- | Connected addresses
    schedulerConnectedAddresses :: [Text]
  }
  deriving (Eq, Show, Generic)

instance FromJSON MetadataScheduler where
  parseJSON = customParseJSON 9

-- | The 'GetMetadataResponse' is the message that is returned by GetMetadata call
data GetMetadataResponse = GetMetadataResponse
  { -- | Metadata ID
    metadataId :: Text,
    -- | Deprecated alias for actor_runtime.active_actors
    metadataActiveActorsCount :: [ActiveActorsCount],
    -- | Registered components
    metadataRegisteredComponents :: [RegisteredComponent],
    -- | Extended metadata
    metadataExtended :: ExtendedMetadata,
    -- | Subscriptions
    metadataSubscriptions :: [PubsubSubscription],
    -- | HTTP endpoints
    metadataHttpEndpoints :: [MetadataHTTPEndpoint],
    -- | App connection properties
    metadataAppConnectionProperties :: AppConnectionProperties,
    -- | Runtime version
    metadataRuntimeVersion :: Text,
    -- | Enabled features
    metadataEnabledFeatures :: [Text],
    -- | Actor runtime information
    metadataActorRuntime :: ActorRuntime,
    -- | Scheduler information (optional)
    metadataScheduler :: Maybe MetadataScheduler
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

-- | 'RegisteredComponent' represents registered component
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
  { -- | Metadata key
    setMetadataKey :: Text,
    -- | Metadata value
    setMetadataValue :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON SetMetadataRequest where
  toJSON = customToJSON 11
