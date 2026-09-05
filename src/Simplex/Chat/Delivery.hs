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
import Simplex.Chat.Messages (ChatItemId, GroupChatScopeInfo (..), MessageId, ShowGroupAsSender)
import Simplex.Chat.Messages.CIContent (CIDeleteMode (..))
import Simplex.Chat.Options.DB (FromField (..), ToField (..))
import Simplex.Chat.Protocol
import Simplex.Chat.Types
import Simplex.Chat.Types.Shared
import Simplex.Messaging.Agent.Store.DB (fromTextField_)
import Simplex.Messaging.Encoding.String

data DeliveryEntity = DEGroup GroupId | DEFeed FeedId
  deriving (Eq, Ord, Show)

type DeliveryWorkerKey = (DeliveryEntity, DeliveryWorkerScope)

data DeliveryWorkerScope
  = DWSGroup
  | DWSMemberSupport
  | DWSFeed
  -- | DWSMemberProfileUpdate
  deriving (Eq, Ord, Show)

instance FromField DeliveryWorkerScope where fromField = fromTextField_ textDecode

instance ToField DeliveryWorkerScope where toField = toField . textEncode

instance TextEncoding DeliveryWorkerScope where
  textDecode = \case
    "group" -> Just DWSGroup
    "member_support" -> Just DWSMemberSupport
    "feed" -> Just DWSFeed
    -- "member_profile_update" -> Just DWSMemberProfileUpdate
    _ -> Nothing
  textEncode = \case
    DWSGroup -> "group"
    DWSMemberSupport -> "member_support"
    DWSFeed -> "feed"
    -- DWSMemberProfileUpdate -> "member_profile_update"

-- Context for creating a delivery task. Separate from DeliveryJobScope because
-- sentAsGroup is only needed for task persistence and batching into XGrpMsgForward events.
-- Once batched into jobs, sentAsGroup=True and sentAsGroup=False messages can be mixed,
-- so jobs don't need this flag.
data DeliveryTaskContext = DeliveryTaskContext
  { jobScope :: DeliveryJobScope,
    sentAsGroup :: ShowGroupAsSender
  }
  deriving (Show)

data DeliveryJobScope
  = DJSGroup {jobSpec :: DeliveryJobSpec}
  | DJSMemberSupport {supportGMId :: GroupMemberId}
  | DJSFeed {feedItemId :: ChatItemId, feedJobSpec :: FeedJobSpec}
  -- | DJSMemberProfileUpdate
  deriving (Show)

data DeliveryJobSpec
  = DJDeliveryJob {includePending :: Bool}
  | DJRelayRemoved
  | DJFeed FeedJobSpec
  deriving (Show)

data FeedJobSpec
  = FJNew
  | FJFileDescr
  | FJUpdate
  | FJDelete CIDeleteMode
  deriving (Show)

data DeliveryJobSpecTag
  = DJSTDeliveryJob
  | DJSTRelayRemoved
  | DJSTFeedNew
  | DJSTFeedFileDescr
  | DJSTFeedUpdate
  | DJSTFeedDelete
  deriving (Show)

instance FromField DeliveryJobSpecTag where fromField = fromTextField_ textDecode

instance ToField DeliveryJobSpecTag where toField = toField . textEncode

instance TextEncoding DeliveryJobSpecTag where
  textDecode = \case
    "delivery_job" -> Just DJSTDeliveryJob
    "relay_removed" -> Just DJSTRelayRemoved
    "feed_new" -> Just DJSTFeedNew
    "feed_file_descr" -> Just DJSTFeedFileDescr
    "feed_update" -> Just DJSTFeedUpdate
    "feed_delete" -> Just DJSTFeedDelete
    _ -> Nothing
  textEncode = \case
    DJSTDeliveryJob -> "delivery_job"
    DJSTRelayRemoved -> "relay_removed"
    DJSTFeedNew -> "feed_new"
    DJSTFeedFileDescr -> "feed_file_descr"
    DJSTFeedUpdate -> "feed_update"
    DJSTFeedDelete -> "feed_delete"

toWorkerScope :: DeliveryJobScope -> DeliveryWorkerScope
toWorkerScope = \case
  DJSGroup _ -> DWSGroup
  DJSMemberSupport _ -> DWSMemberSupport
  DJSFeed {} -> DWSFeed
  -- DJSMemberProfileUpdate -> DWSMemberProfileUpdate

isRelayRemoved :: DeliveryJobScope -> Bool
isRelayRemoved = \case
  DJSGroup {jobSpec} -> case jobSpec of
    DJRelayRemoved -> True
    _ -> False
  _ -> False

jobScopeImpliedSpec :: DeliveryJobScope -> DeliveryJobSpec
jobScopeImpliedSpec = \case
  DJSGroup {jobSpec} -> jobSpec
  DJSMemberSupport {} -> DJDeliveryJob {includePending = False}
  DJSFeed {feedJobSpec} -> DJFeed feedJobSpec

jobSpecImpliedPending :: DeliveryJobSpec -> Bool
jobSpecImpliedPending = \case
  DJDeliveryJob {includePending} -> includePending
  DJRelayRemoved -> True
  DJFeed _ -> False

infoToDeliveryContext :: GroupInfo -> Maybe GroupChatScopeInfo -> ShowGroupAsSender -> DeliveryTaskContext
infoToDeliveryContext GroupInfo {membership} scopeInfo sentAsGroup = DeliveryTaskContext {jobScope, sentAsGroup}
  where
    jobScope = case scopeInfo of
      Nothing -> DJSGroup {jobSpec = DJDeliveryJob {includePending = False}}
      Just GCSIMemberSupport {groupMember_} ->
        let supportGMId = groupMemberId' $ fromMaybe membership groupMember_
         in DJSMemberSupport {supportGMId}

memberEventDeliveryScope :: GroupMember -> Maybe DeliveryJobScope
memberEventDeliveryScope m@GroupMember {memberRole, memberStatus}
  | memberStatus == GSMemPendingApproval = Nothing
  | memberStatus == GSMemPendingReview = Just $ DJSMemberSupport {supportGMId = groupMemberId' m}
  | memberRole >= GRModerator = Just DJSGroup {jobSpec = DJDeliveryJob {includePending = True}}
  | otherwise = Just DJSGroup {jobSpec = DJDeliveryJob {includePending = False}}

data NewMessageDeliveryTask = NewMessageDeliveryTask
  { messageId :: MessageId,
    taskContext :: DeliveryTaskContext
  }
  deriving (Show)

data MessageDeliveryTask = MessageDeliveryTask
  { taskId :: Int64,
    jobScope :: DeliveryJobScope,
    senderGMId :: GroupMemberId,
    fwdSender :: FwdSender,
    brokerTs :: UTCTime,
    verifiedMsg :: VerifiedMsg 'Json
  }

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
    senderGMIds :: [GroupMemberId],
    messageIds :: [MessageId],
    body :: ByteString,
    cursorGMId_ :: Maybe GroupMemberId,
    cursorContactId_ :: Maybe ContactId,
    cursorGroupId_ :: Maybe GroupId
  }
  deriving (Show)

deliveryJobId :: MessageDeliveryJob -> Int64
deliveryJobId = jobId

data DeliveryJobStatus
  = DJSPending -- created for delivery job worker to pick up
  | DJSComplete -- complete by delivery job worker, job can be deleted
  | DJSError -- permanent error
  deriving (Show)

instance FromField DeliveryJobStatus where fromField = fromTextField_ textDecode

instance ToField DeliveryJobStatus where toField = toField . textEncode

instance TextEncoding DeliveryJobStatus where
  textDecode = \case
    "pending" -> Just DJSPending
    "complete" -> Just DJSComplete
    "error" -> Just DJSError
    _ -> Nothing
  textEncode = \case
    DJSPending -> "pending"
    DJSComplete -> "complete"
    DJSError -> "error"

-- data MemberProfileUpdateTask = undefined

-- data MemberProfileUpdateJob = undefined
