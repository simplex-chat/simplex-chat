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

type DeliveryWorkerKey = (GroupId, GroupDeliveryScopeType)

data GroupDeliveryScope
  = GDSGroup {includePending :: Bool}
  | GDSMemberSupport {supportGMId :: GroupMemberId}
  -- | GDSMemberProfile -- planned for sending member profiles -- TODO comment
  deriving (Eq, Ord, Show)

data GroupDeliveryScopeType
  = GDSTGroup
  | GDSTMemberSupport
  -- | GDSTMemberProfile
  deriving (Eq, Ord, Show)

instance FromField GroupDeliveryScopeType where fromField = fromTextField_ textDecode

instance ToField GroupDeliveryScopeType where toField = toField . textEncode

instance TextEncoding GroupDeliveryScopeType where
  textDecode = \case
    "group" -> Just GDSTGroup
    "member_support" -> Just GDSTMemberSupport
    -- "member_profile" -> Just GDSTMemberProfile
    _ -> Nothing
  textEncode = \case
    GDSTGroup -> "group"
    GDSTMemberSupport -> "member_support"
    -- GDSTMemberProfile -> "member_profile"

deliveryScopeType :: GroupDeliveryScope -> GroupDeliveryScopeType
deliveryScopeType = \case
  GDSGroup {} -> GDSTGroup
  GDSMemberSupport {} -> GDSTMemberSupport
  -- GDSMemberProfile -> GDSTMemberProfile

infoToDeliveryScope :: GroupInfo -> Maybe GroupChatScopeInfo -> GroupDeliveryScope
infoToDeliveryScope GroupInfo {membership} = \case
  Nothing -> GDSGroup {includePending = False}
  Just GCSIMemberSupport {groupMember_} ->
    let supportGMId = groupMemberId' $ fromMaybe membership groupMember_
     in GDSMemberSupport {supportGMId}

memberEventDeliveryScope :: GroupMember -> Maybe GroupDeliveryScope
memberEventDeliveryScope m@GroupMember {memberRole, memberStatus}
  | memberStatus == GSMemPendingApproval = Nothing
  | memberStatus == GSMemPendingReview = Just $ GDSMemberSupport {supportGMId = groupMemberId' m}
  | memberRole >= GRModerator = Just GDSGroup {includePending = True}
  | otherwise = Just GDSGroup {includePending = False}

data NewGroupDeliveryTask = NewGroupDeliveryTask
  { messageId :: MessageId,
    deliveryScope :: GroupDeliveryScope,
    jobType :: DeliveryJobType,
    messageFromChannel :: MessageFromChannel
  }
  deriving (Show)

data DeliveryJobType
  = DJTMessageForward
  | DJTRelayRemoved
  -- | DJTChatItemsCount -- planned for batching reactions/comments (ChatItemCountsJob)
  -- | DJTMemberProfile -- planned for sending member profiles (MemberProfileJob)
  deriving (Eq, Show)

instance FromField DeliveryJobType where fromField = fromTextField_ textDecode

instance ToField DeliveryJobType where toField = toField . textEncode

instance TextEncoding DeliveryJobType where
  textDecode = \case
    "message_forward" -> Just DJTMessageForward
    "relay_removed" -> Just DJTRelayRemoved
    -- "chat_items_count" -> Just DJTChatItemsCount
    -- "member_profile" -> Just DJTMemberProfile
    _ -> Nothing
  textEncode = \case
    DJTMessageForward -> "message_forward"
    DJTRelayRemoved -> "relay_removed"
    -- DJTChatItemsCount -> "chat_items_count"
    -- DJTMemberProfile -> "member_profile"

data DeliveryTask
  = DTMessageForward {messageForwardTask :: MessageForwardTask}
  | DTRelayRemoved {relayRemovedTask :: RelayRemovedTask}
  -- | DTChatItemsCount {chatItemCountsTask :: ChatItemCountsTask}
  -- | DTMemberProfile {memberProfileTask :: MemberProfileTask}
  deriving (Show)

data MessageForwardTask = MessageForwardTask
  { taskId :: Int64,
    deliveryScope :: GroupDeliveryScope,
    senderGMId :: GroupMemberId,
    senderMemberId :: MemberId,
    senderMemberName :: ContactName,
    brokerTs :: UTCTime,
    chatMessage :: ChatMessage 'Json,
    messageFromChannel :: MessageFromChannel
  }
  deriving (Show)

data RelayRemovedTask = RelayRemovedTask
  { taskId :: Int64,
    senderGMId :: GroupMemberId,
    senderMemberId :: MemberId,
    senderMemberName :: ContactName,
    brokerTs :: UTCTime,
    chatMessage :: ChatMessage 'Json
  }
  deriving (Show)

-- data ChatItemCountsTask = ChatItemCountsTask
--   { taskId :: Int64,
--     deliveryScope :: GroupDeliveryScope,
--     senderGMId :: GroupMemberId,
--     senderMemberId :: MemberId,
--     senderMemberName :: ContactName,
--     brokerTs :: UTCTime,
--     chatMessage :: ChatMessage 'Json
--   }
--   deriving (Show)

-- data MemberProfileTask = ProfileDeliveryTask
--   { taskId :: Int64,
--     member :: GroupMember -- use last_profile_delivery_ts to filter list of recipients
--   }
--   deriving (Show)

deliveryTaskId :: DeliveryTask -> Int64
deliveryTaskId = \case
  DTMessageForward {messageForwardTask = MessageForwardTask {taskId}} -> taskId
  DTRelayRemoved {relayRemovedTask = RelayRemovedTask {taskId}} -> taskId
  -- DTChatItemsCount {chatItemCountsTask = ChatItemCountsTask {taskId}} -> taskId
  -- DTMemberProfile {memberProfileTask = ProfileDeliveryTask {taskId}} -> taskId

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

data DeliveryJob
  = DJMessageForward {messageForwardJob :: MessageForwardJob}
  | DJRelayRemoved {relayRemovedJob :: RelayRemovedJob}
  -- | DJChatItemsCount {chatItemCountsJob :: ChatItemCountsJob}
  -- | DJMemberProfile {memberProfileJob :: MemberProfileJob}
  deriving (Show)

data MessageForwardJob = MessageForwardJob
  { jobId :: Int64,
    deliveryScope :: GroupDeliveryScope,
    singleSenderGMId_ :: Maybe GroupMemberId, -- Just for single-sender deliveries, Nothing for multi-sender deliveries
    messagesBatch :: ByteString,
    cursorGMId :: Maybe GroupMemberId
  }
  deriving (Show)

data RelayRemovedJob = RelayRemovedJob
  { jobId :: Int64,
    singleSenderGMId :: GroupMemberId,
    fwdChatMessage :: ByteString,
    cursorGMId :: Maybe GroupMemberId
  }
  deriving (Show)

-- data ChatItemCountsJob = ChatItemCountsJob
--   { jobId :: Int64,
--     deliveryScope :: GroupDeliveryScope,
--     singleSenderGMId_ :: Maybe GroupMemberId, -- Just for single-sender deliveries, Nothing for multi-sender deliveries
--     countsBatch :: ByteString,
--     cursorGMId :: Maybe GroupMemberId
--   }
--   deriving (Show)

-- data MemberProfileJob = ProfileDeliveryJob
--   { jobId :: Int64,
--     member :: GroupMember, -- use last_profile_delivery_ts to filter list of recipients
--     cursorGMId :: Maybe GroupMemberId
--   }
--   deriving (Show)

deliveryJobId :: DeliveryJob -> Int64
deliveryJobId = \case
  DJMessageForward {messageForwardJob = MessageForwardJob {jobId}} -> jobId
  DJRelayRemoved {relayRemovedJob = RelayRemovedJob {jobId}} -> jobId
  -- DJChatItemsCount {chatItemCountsJob = ChatItemCountsJob {jobId}} -> jobId
  -- DJMemberProfile {memberProfileJob = ProfileDeliveryJob {jobId}} -> jobId

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
