{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Dapr.Core.Types.Scheduler
-- Description : Defines the types used by Scheduler module
-- Copyright   : (c)
-- License     : Apache-2.0
-- Defines the types used by Scheduler module.
module Dapr.Core.Types.Scheduler where

import Dapr.Core.Types.Internal (customParseJSON, customToJSON)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Map (Map)
import Data.Text (Text)
import Data.Word (Word32)
import GHC.Generics (Generic)

-- | 'JobName' represents the unique name for a job
newtype JobName = JobName {getJobName :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | 'JobTargetType' is the type of the job target
data JobTargetType
  = JobTargetTypeJob
  | JobTargetTypeActorReminder
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | 'Job' is the definition of a job
data Job a = Job
  { -- | The unique name for the job.
    jobName :: JobName,
    -- | Schedule is an optional schedule at which the job is to be run.
    jobSchedule :: Maybe Text,
    -- | Repeats is the optional number of times the job should be triggered.
    jobRepeats :: Maybe Word32,
    -- | Due time is the optional time at which the job should be active.
    jobDueTime :: Maybe Text,
    -- | TTL is the optional time to live or expiration of the job.
    jobTtl :: Maybe Text,
    -- | Payload is the serialized job payload sent to the recipient when triggered.
    jobData :: a,
    -- | If true, allows this job to overwrite an existing job with the same name.
    jobOverwrite :: Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (Job a) where
  toJSON = customToJSON 3

instance FromJSON a => FromJSON (Job a) where
  parseJSON = customParseJSON 3

-- | 'ScheduleJobRequest' is the message to create/schedule a job
data ScheduleJobRequest a = ScheduleJobRequest
  { -- | The job details.
    scheduleJob :: Job a
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (ScheduleJobRequest a) where
  toJSON = customToJSON 8

-- | 'ScheduleJobResponse' is the response to creating/scheduling a job
data ScheduleJobResponse = ScheduleJobResponse
  { -- | Empty response
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | 'GetJobRequest' is the message to retrieve a job
data GetJobRequest = GetJobRequest
  { -- | The name of the job.
    getJobName :: JobName
  }
  deriving (Eq, Show, Generic)

instance ToJSON GetJobRequest where
  toJSON = customToJSON 6

-- | 'GetJobResponse' is the response for a job retrieval
data GetJobResponse a = GetJobResponse
  { -- | The job details.
    retrievedJob :: Job a
  }
  deriving (Eq, Show, Generic)

instance FromJSON a => FromJSON (GetJobResponse a) where
  parseJSON = customParseJSON 9

-- | 'DeleteJobRequest' is the message to delete a job by name
data DeleteJobRequest = DeleteJobRequest
  { -- | The name of the job.
    deleteJobName :: JobName
  }
  deriving (Eq, Show, Generic)

instance ToJSON DeleteJobRequest where
  toJSON = customToJSON 9

-- | 'DeleteJobResponse' is the response to deleting a job
data DeleteJobResponse = DeleteJobResponse
  { -- | Empty response
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | 'FailurePolicy' represents the policy to apply when a job fails
data FailurePolicy = FailurePolicy
  { -- | The policy type (e.g., "constant", "exponential")
    failurePolicyType :: Text,
    -- | The interval between retries
    failurePolicyInterval :: Maybe Text,
    -- | Maximum number of retries
    failurePolicyMaxRetries :: Maybe Word32
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | 'JobWithFailurePolicy' extends Job with failure policy support
data JobWithFailurePolicy a = JobWithFailurePolicy
  { -- | Base job definition
    baseJob :: Job a,
    -- | Failure policy for the job
    jobFailurePolicy :: Maybe FailurePolicy
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (JobWithFailurePolicy a) where
  toJSON = customToJSON 4

instance FromJSON a => FromJSON (JobWithFailurePolicy a) where
  parseJSON = customParseJSON 4
