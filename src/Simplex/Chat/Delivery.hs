{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Delivery where

import Data.ByteString.Char8 (ByteString)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime)
import Simplex.Chat.Messages (GroupChatScopeInfo (..), MessageId)
import Simplex.Chat.Options.DB (FromField (..), ToField (..))
import Simplex.Chat.Protocol
import Simplex.Chat.Types
import Simplex.Chat.Types.Shared
import Simplex.Messaging.Agent.Store.DB (fromTextField_)
import Simplex.Messaging.Encoding.String

type DeliveryWorkerKey = (GroupId, DeliveryWorkerScope)

data DeliveryWorkerScope
  = DWSGroup
  | DWSMemberSupport
  -- | DWSMemberProfileUpdate
  deriving (Eq, Ord, Show)

instance FromField DeliveryWorkerScope where fromField = fromTextField_ textDecode

instance ToField DeliveryWorkerScope where toField = toField . textEncode

instance TextEncoding DeliveryWorkerScope where
  textDecode = \case
    "group" -> Just DWSGroup
    "member_support" -> Just DWSMemberSupport
    -- "member_profile_update" -> Just DWSMemberProfileUpdate
    _ -> Nothing
  textEncode = \case
    DWSGroup -> "group"
    DWSMemberSupport -> "member_support"
    -- DWSMemberProfileUpdate -> "member_profile_update"

data DeliveryJobScope
  = DJSGroup {jobSpec :: DeliveryJobSpec}
  | DJSMemberSupport {supportGMId :: GroupMemberId}
  -- | DJSMemberProfileUpdate
  deriving (Show)

data DeliveryJobSpec
  = DJSpecDeliveryJob {includePending :: Bool}
  | DJSpecRelayRemoved
  deriving (Show)

data DeliveryJobSpecTag
  = DJSTDeliveryJob
  | DJSTRelayRemoved
  deriving (Show)

instance FromField DeliveryJobSpecTag where fromField = fromTextField_ textDecode

instance ToField DeliveryJobSpecTag where toField = toField . textEncode

instance TextEncoding DeliveryJobSpecTag where
  textDecode = \case
    "delivery_job" -> Just DJSTDeliveryJob
    "relay_removed" -> Just DJSTRelayRemoved
    _ -> Nothing
  textEncode = \case
    DJSTDeliveryJob -> "delivery_job"
    DJSTRelayRemoved -> "relay_removed"

toWorkerScope :: DeliveryJobScope -> DeliveryWorkerScope
toWorkerScope = \case
  DJSGroup _ -> DWSGroup
  DJSMemberSupport _ -> DWSMemberSupport
  -- DJSMemberProfileUpdate -> DWSMemberProfileUpdate

isRelayRemoved :: DeliveryJobScope -> Bool
isRelayRemoved = \case
  DJSGroup {jobSpec} -> case jobSpec of
    DJSpecRelayRemoved -> True
    _ -> False
  _ -> False

jobScopeImpliedSpec :: DeliveryJobScope -> DeliveryJobSpec
jobScopeImpliedSpec = \case
  DJSGroup {jobSpec} -> jobSpec
  DJSMemberSupport {} -> DJSpecDeliveryJob {includePending = False}

jobSpecImpliedPending :: DeliveryJobSpec -> Bool
jobSpecImpliedPending = \case
  DJSpecDeliveryJob {includePending} -> includePending
  DJSpecRelayRemoved -> True

infoToDeliveryScope :: GroupInfo -> Maybe GroupChatScopeInfo -> DeliveryJobScope
infoToDeliveryScope GroupInfo {membership} = \case
  Nothing -> DJSGroup {jobSpec = DJSpecDeliveryJob {includePending = False}}
  Just GCSIMemberSupport {groupMember_} ->
    let supportGMId = groupMemberId' $ fromMaybe membership groupMember_
     in DJSMemberSupport {supportGMId}

memberEventDeliveryScope :: GroupMember -> Maybe DeliveryJobScope
memberEventDeliveryScope m@GroupMember {memberRole, memberStatus}
  | memberStatus == GSMemPendingApproval = Nothing
  | memberStatus == GSMemPendingReview = Just $ DJSMemberSupport {supportGMId = groupMemberId' m}
  | memberRole >= GRModerator = Just DJSGroup {jobSpec = DJSpecDeliveryJob {includePending = True}}
  | otherwise = Just DJSGroup {jobSpec = DJSpecDeliveryJob {includePending = False}}

data NewMessageDeliveryTask = NewMessageDeliveryTask
  { messageId :: MessageId,
    jobScope :: DeliveryJobScope,
    messageFromChannel :: MessageFromChannel
  }
  deriving (Show)

data MessageDeliveryTask = MessageDeliveryTask
  { taskId :: Int64,
    jobScope :: DeliveryJobScope,
    senderGMId :: GroupMemberId,
    senderMemberId :: MemberId,
    senderMemberName :: ContactName,
    brokerTs :: UTCTime,
    chatMessage :: ChatMessage 'Json,
    messageFromChannel :: MessageFromChannel
  }
  deriving (Show)

deliveryTaskId :: MessageDeliveryTask -> Int64
deliveryTaskId = taskId

data DeliveryTaskStatus
  = DTSNew -- created for delivery task worker to pick up and convert into a delivery job
  | DTSProcessed -- processed by delivery task worker, delivery job created, task can be deleted
  | DTSError -- permanent error
  deriving (Show)

instance FromField DeliveryTaskStatus where fromField = fromTextField_ textDecode

instance ToField DeliveryTaskStatus where toField = toField . textEncode

instance TextEncoding DeliveryTaskStatus where
  textDecode = \case
    "new" -> Just DTSNew
    "processed" -> Just DTSProcessed
    "error" -> Just DTSError
    _ -> Nothing
  textEncode = \case
    DTSNew -> "new"
    DTSProcessed -> "processed"
    DTSError -> "error"

data MessageDeliveryJob = MessageDeliveryJob
  { jobId :: Int64,
    jobScope :: DeliveryJobScope,
    singleSenderGMId_ :: Maybe GroupMemberId, -- Just for single-sender deliveries, Nothing for multi-sender deliveries
    body :: ByteString,
    cursorGMId_ :: Maybe GroupMemberId
  }
  deriving (Show)

deliveryJobId :: MessageDeliveryJob -> Int64
deliveryJobId = jobId

data DeliveryJobStatus
  = DJSNew -- created for delivery job worker to pick up
  | DJSInProgress -- being processed by delivery job worker
  | DJSComplete -- complete by delivery job worker, job can be deleted
  | DJSError -- permanent error
  deriving (Show)

instance FromField DeliveryJobStatus where fromField = fromTextField_ textDecode

instance ToField DeliveryJobStatus where toField = toField . textEncode

instance TextEncoding DeliveryJobStatus where
  textDecode = \case
    "new" -> Just DJSNew
    "in_progress" -> Just DJSInProgress
    "complete" -> Just DJSComplete
    "error" -> Just DJSError
    _ -> Nothing
  textEncode = \case
    DJSNew -> "new"
    DJSInProgress -> "in_progress"
    DJSComplete -> "complete"
    DJSError -> "error"

-- data MemberProfileUpdateTask = undefined

-- data MemberProfileUpdateJob = undefined
