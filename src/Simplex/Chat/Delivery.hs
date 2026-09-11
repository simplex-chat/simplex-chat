{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Delivery where

import Data.ByteString.Char8 (ByteString)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as L
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Simplex.Chat.Messages (ChatItemId, GroupChatScopeInfo (..), MessageId, ShowGroupAsSender)
import Simplex.Chat.Options.DB (FromField (..), ToField (..))
import Simplex.Chat.Protocol
import Simplex.Chat.Types
import Simplex.Chat.Types.Shared
import Simplex.Messaging.Agent.Store.DB (fromTextField_)
import Simplex.Messaging.Encoding.String
import Text.Read (readMaybe)

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

type FeedJobKey = (FeedId, FeedWorkerScope)

data FeedWorkerScope = FWSContacts | FWSGroups
  deriving (Eq, Ord, Show)

feedWorkerScopes :: [FeedWorkerScope]
feedWorkerScopes = [FWSContacts, FWSGroups]

instance FromField FeedWorkerScope where fromField = fromTextField_ textDecode

instance ToField FeedWorkerScope where toField = toField . textEncode

instance TextEncoding FeedWorkerScope where
  textDecode = \case
    "feed_contacts" -> Just FWSContacts
    "feed_groups" -> Just FWSGroups
    _ -> Nothing
  textEncode = \case
    FWSContacts -> "feed_contacts"
    FWSGroups -> "feed_groups"

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
  -- | DJSMemberProfileUpdate
  deriving (Show)

data DeliveryJobSpec
  = DJDeliveryJob {includePending :: Bool}
  | DJRelayRemoved
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
    DJRelayRemoved -> True
    _ -> False
  _ -> False

jobScopeImpliedSpec :: DeliveryJobScope -> DeliveryJobSpec
jobScopeImpliedSpec = \case
  DJSGroup {jobSpec} -> jobSpec
  DJSMemberSupport {} -> DJDeliveryJob {includePending = False}

jobSpecImpliedPending :: DeliveryJobSpec -> Bool
jobSpecImpliedPending = \case
  DJDeliveryJob {includePending} -> includePending
  DJRelayRemoved -> True

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

-- NULL and empty string mean []; any other value must parse as a comma-separated Int64 list.
parseIds :: Maybe Text -> Maybe [Int64]
parseIds = \case
  Nothing -> Just []
  Just t
    | T.null t -> Just []
    | otherwise -> traverse (readMaybe . T.unpack) (T.splitOn "," t)

idsColumn :: [Int64] -> Maybe Text
idsColumn ids
  | null ids = Nothing
  | otherwise = Just $ T.intercalate "," $ map (T.pack . show) ids

data MessageDeliveryJob = MessageDeliveryJob
  { jobId :: Int64,
    jobScope :: DeliveryJobScope,
    senderGMIds :: [GroupMemberId],
    body :: ByteString,
    cursorGMId_ :: Maybe GroupMemberId
  }
  deriving (Show)

deliveryJobId :: MessageDeliveryJob -> Int64
deliveryJobId = jobId

data FeedJob = FeedJob
  { feedJobId :: Int64,
    feedItemId :: ChatItemId,
    feedAction :: FeedJobAction,
    cursorId_ :: Maybe Int64
  }
  deriving (Show)

data FeedJobAction
  = FJANew MessageId
  | FJAFileDescr (NonEmpty MessageId)
  | FJAUpdate MessageId
  | FJADeleteBroadcast MessageId
  | FJADeleteInternal
  | FJADeleteMark
  deriving (Show)

data FeedJobActionTag
  = FJATNew
  | FJATFileDescr
  | FJATUpdate
  | FJATDeleteBroadcast
  | FJATDeleteInternal
  | FJATDeleteMark
  deriving (Show)

feedActionTag :: FeedJobAction -> FeedJobActionTag
feedActionTag = \case
  FJANew _ -> FJATNew
  FJAFileDescr _ -> FJATFileDescr
  FJAUpdate _ -> FJATUpdate
  FJADeleteBroadcast _ -> FJATDeleteBroadcast
  FJADeleteInternal -> FJATDeleteInternal
  FJADeleteMark -> FJATDeleteMark

feedActionMsgIds :: FeedJobAction -> [MessageId]
feedActionMsgIds = \case
  FJANew msgId -> [msgId]
  FJAFileDescr msgIds -> L.toList msgIds
  FJAUpdate msgId -> [msgId]
  FJADeleteBroadcast msgId -> [msgId]
  FJADeleteInternal -> []
  FJADeleteMark -> []

feedActionCreates :: FeedJobAction -> Bool
feedActionCreates = \case
  FJANew _ -> True
  _ -> False

feedActionDeletes :: FeedJobAction -> Bool
feedActionDeletes = \case
  FJADeleteBroadcast _ -> True
  FJADeleteInternal -> True
  FJADeleteMark -> True
  _ -> False

feedActionRemovesItem :: FeedJobAction -> Bool
feedActionRemovesItem = \case
  FJADeleteBroadcast _ -> True
  FJADeleteInternal -> True
  _ -> False

instance FromField FeedJobActionTag where fromField = fromTextField_ textDecode

instance ToField FeedJobActionTag where toField = toField . textEncode

instance TextEncoding FeedJobActionTag where
  textDecode = \case
    "feed_new" -> Just FJATNew
    "feed_file_descr" -> Just FJATFileDescr
    "feed_update" -> Just FJATUpdate
    "feed_delete_broadcast" -> Just FJATDeleteBroadcast
    "feed_delete_internal" -> Just FJATDeleteInternal
    "feed_delete_mark" -> Just FJATDeleteMark
    _ -> Nothing
  textEncode = \case
    FJATNew -> "feed_new"
    FJATFileDescr -> "feed_file_descr"
    FJATUpdate -> "feed_update"
    FJATDeleteBroadcast -> "feed_delete_broadcast"
    FJATDeleteInternal -> "feed_delete_internal"
    FJATDeleteMark -> "feed_delete_mark"

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
