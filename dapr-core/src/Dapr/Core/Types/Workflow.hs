{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Dapr.Core.Types.Workflow
-- Description : Defines the types used by Workflow module
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines the types used by Workflow module.
module Dapr.Core.Types.Workflow where

import Dapr.Core.Types.Internal (customParseJSON, customToJSON)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Map (Map)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | 'WorkflowComponent' represents the name of workflow component
newtype WorkflowComponent = WorkflowComponent {getWorkflowComponent :: Text}
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | 'WorkflowName' represents the name of workflow
newtype WorkflowName = WorkflowName {getWorkflowName :: Text}
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | 'WorkflowInstanceId' represents the unique identifier for a workflow instance
newtype WorkflowInstanceId = WorkflowInstanceId {getWorkflowInstanceId :: Text}
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | 'WorkflowEventName' represents the name of a workflow event
newtype WorkflowEventName = WorkflowEventName {getWorkflowEventName :: Text}
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | 'WorkflowRuntimeStatus' represents the current status of a workflow instance
data WorkflowRuntimeStatus
  = WorkflowPending
  | WorkflowRunning
  | WorkflowSuspended
  | WorkflowCompleted
  | WorkflowFailed
  | WorkflowTerminated
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | 'StartWorkflowRequest' is the request for starting a workflow
data StartWorkflowRequest a = StartWorkflowRequest
  { -- | The ID to assign to the started workflow instance. If empty, a random ID is generated.
    startWorkflowInstanceId :: Maybe WorkflowInstanceId,
    -- | Name of the workflow component.
    startWorkflowComponent :: WorkflowComponent,
    -- | Name of the workflow.
    startWorkflowName :: WorkflowName,
    -- | Additional component-specific options for starting the workflow instance.
    startWorkflowOptions :: Map Text Text,
    -- | Input data for the workflow instance.
    startWorkflowInput :: a
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (StartWorkflowRequest a) where
  toJSON = customToJSON 13

-- | 'StartWorkflowResponse' is the response for starting a workflow
data StartWorkflowResponse = StartWorkflowResponse
  { -- | ID of the started workflow instance.
    startedWorkflowInstanceId :: WorkflowInstanceId
  }
  deriving (Eq, Show, Generic)

instance FromJSON StartWorkflowResponse where
  parseJSON = customParseJSON 15

-- | 'GetWorkflowRequest' is the request for getting workflow details
data GetWorkflowRequest = GetWorkflowRequest
  { -- | ID of the workflow instance to query.
    getWorkflowInstanceId :: WorkflowInstanceId,
    -- | Name of the workflow component.
    getWorkflowComponent :: WorkflowComponent
  }
  deriving (Eq, Show, Generic)

instance ToJSON GetWorkflowRequest where
  toJSON = customToJSON 11

-- | 'GetWorkflowResponse' is the response for getting workflow details
data GetWorkflowResponse = GetWorkflowResponse
  { -- | ID of the workflow instance.
    workflowInstanceId :: WorkflowInstanceId,
    -- | Name of the workflow.
    workflowName :: WorkflowName,
    -- | The time at which the workflow instance was created.
    workflowCreatedAt :: UTCTime,
    -- | The last time at which the workflow instance had its state changed.
    workflowLastUpdatedAt :: UTCTime,
    -- | The current status of the workflow instance.
    workflowRuntimeStatus :: WorkflowRuntimeStatus,
    -- | Additional component-specific properties of the workflow instance.
    workflowProperties :: Map Text Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON GetWorkflowResponse where
  parseJSON = customParseJSON 8

-- | 'TerminateWorkflowRequest' is the request for terminating a workflow
data TerminateWorkflowRequest = TerminateWorkflowRequest
  { -- | ID of the workflow instance to terminate.
    terminateWorkflowInstanceId :: WorkflowInstanceId,
    -- | Name of the workflow component.
    terminateWorkflowComponent :: WorkflowComponent
  }
  deriving (Eq, Show, Generic)

instance ToJSON TerminateWorkflowRequest where
  toJSON = customToJSON 17

-- | 'PauseWorkflowRequest' is the request for pausing a workflow
data PauseWorkflowRequest = PauseWorkflowRequest
  { -- | ID of the workflow instance to pause.
    pauseWorkflowInstanceId :: WorkflowInstanceId,
    -- | Name of the workflow component.
    pauseWorkflowComponent :: WorkflowComponent
  }
  deriving (Eq, Show, Generic)

instance ToJSON PauseWorkflowRequest where
  toJSON = customToJSON 13

-- | 'ResumeWorkflowRequest' is the request for resuming a workflow
data ResumeWorkflowRequest = ResumeWorkflowRequest
  { -- | ID of the workflow instance to resume.
    resumeWorkflowInstanceId :: WorkflowInstanceId,
    -- | Name of the workflow component.
    resumeWorkflowComponent :: WorkflowComponent
  }
  deriving (Eq, Show, Generic)

instance ToJSON ResumeWorkflowRequest where
  toJSON = customToJSON 14

-- | 'RaiseEventWorkflowRequest' is the request for raising an event to a workflow
data RaiseEventWorkflowRequest a = RaiseEventWorkflowRequest
  { -- | ID of the workflow instance to raise an event for.
    raiseEventWorkflowInstanceId :: WorkflowInstanceId,
    -- | Name of the workflow component.
    raiseEventWorkflowComponent :: WorkflowComponent,
    -- | Name of the event.
    raiseEventWorkflowEventName :: WorkflowEventName,
    -- | Data associated with the event.
    raiseEventWorkflowEventData :: a
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (RaiseEventWorkflowRequest a) where
  toJSON = customToJSON 17

-- | 'PurgeWorkflowRequest' is the request for purging a workflow
data PurgeWorkflowRequest = PurgeWorkflowRequest
  { -- | ID of the workflow instance to purge.
    purgeWorkflowInstanceId :: WorkflowInstanceId,
    -- | Name of the workflow component.
    purgeWorkflowComponent :: WorkflowComponent
  }
  deriving (Eq, Show, Generic)

instance ToJSON PurgeWorkflowRequest where
  toJSON = customToJSON 13
