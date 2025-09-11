{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Delivery
  ( createNewDeliveryTask,
    deleteGroupDeliveryTasks,
    deleteGroupDeliveryJobs,
    getPendingDeliveryTaskScopes,
    getNextDeliveryTask,
    getNextMsgFwdTasks,
    createMessageForwardJob,
    createRelayRemovedJob,
    updateDeliveryTaskStatus,
    setDeliveryTaskErrStatus,
    deleteDoneDeliveryTasks,
    getPendingDeliveryJobScopes,
    getNextDeliveryJob,
    updateDeliveryJobStatus,
    setDeliveryJobErrStatus,
    getGroupMembersByCursor,
    updateDeliveryJobCursor,
    deleteDoneDeliveryJobs,
  )
where

import Control.Monad.Except
import Data.ByteString.Char8 (ByteString)
import Data.Either (rights)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Simplex.Chat.Delivery
import Simplex.Chat.Protocol hiding (Binary)
import Simplex.Chat.Store.Groups (getGroupMemberById)
import Simplex.Chat.Store.Shared
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Store.AgentStore (getWorkItem, getWorkItems, maybeFirstRow)
import Simplex.Messaging.Agent.Store.DB (Binary (..), BoolInt (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Util (firstRow')
#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..), Query, (:.) (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..), Query, (:.) (..))
import Database.SQLite.Simple.QQ (sql)
#endif

deliveryScopeFields_ :: GroupDeliveryScope -> (GroupDeliveryScopeType, Maybe Bool, Maybe GroupMemberId)
deliveryScopeFields_ = \case
  GDSGroup {includePending = True} -> (GDSTGroup, Just True, Nothing)
  GDSGroup {includePending = False} -> (GDSTGroup, Just False, Nothing)
  GDSMemberSupport {supportGMId} -> (GDSTMemberSupport, Nothing, Just supportGMId)

toDeliveryScope_ :: GroupDeliveryScopeType -> Maybe BoolInt -> Maybe GroupMemberId -> Maybe GroupDeliveryScope
toDeliveryScope_ scopeType includePending_ supportGMId_ = case (scopeType, includePending_, supportGMId_) of
  (GDSTGroup, Just (BI True), Nothing) -> Just $ GDSGroup {includePending = True}
  (GDSTGroup, Just (BI False), Nothing) -> Just $ GDSGroup {includePending = False}
  (GDSTMemberSupport, Nothing, Just supportGMId) -> Just $ GDSMemberSupport {supportGMId}
  _ -> Nothing

createNewDeliveryTask :: DB.Connection -> GroupInfo -> GroupMember -> NewGroupDeliveryTask -> IO ()
createNewDeliveryTask
  db
  GroupInfo {groupId}
  sender
  NewGroupDeliveryTask {messageId, deliveryScope, jobType, messageFromChannel} =
    DB.execute
      db
      [sql|
        INSERT INTO delivery_tasks (
          group_id, delivery_scope_type, delivery_scope_include_pending, delivery_scope_support_gm_id,
          delivery_job_type, sender_group_member_id, message_id, message_from_channel, task_status
        ) VALUES (?,?,?,?,?,?,?,?,?)
      |]
      ( (groupId, scopeType, BI <$> scopeInclPending_, scopeSupportGMId_)
          :. (jobType, groupMemberId' sender, messageId, BI messageFromChannel, DTSNew)
      )
    where
      (scopeType, scopeInclPending_, scopeSupportGMId_) = deliveryScopeFields_ deliveryScope

deleteGroupDeliveryTasks :: DB.Connection -> GroupInfo -> IO ()
deleteGroupDeliveryTasks db GroupInfo {groupId} =
  DB.execute db "DELETE FROM delivery_tasks WHERE group_id = ?" (Only groupId)

deleteGroupDeliveryJobs :: DB.Connection -> GroupInfo -> IO ()
deleteGroupDeliveryJobs db GroupInfo {groupId} =
  DB.execute db "DELETE FROM delivery_jobs WHERE group_id = ?" (Only groupId)

getPendingDeliveryTaskScopes :: DB.Connection -> IO [DeliveryWorkerKey]
getPendingDeliveryTaskScopes db =
  DB.query
    db
    [sql|
      SELECT DISTINCT group_id, delivery_scope_type
      FROM delivery_tasks
      WHERE failed = 0 AND task_status = ?
    |]
    (Only DTSNew)

type DeliveryTaskRow = (Int64, DeliveryJobType, GroupDeliveryScopeType, Maybe BoolInt, Maybe GroupMemberId, GroupMemberId, MemberId, ContactName, UTCTime, ChatMessage 'Json, BoolInt)

getNextDeliveryTask :: DB.Connection -> DeliveryWorkerKey -> IO (Either StoreError (Maybe DeliveryTask))
getNextDeliveryTask db deliveryKey = do
  getWorkItem "delivery task" getTaskId getTask (markDeliveryTaskFailed_ db)
  where
    (groupId, scopeType) = deliveryKey
    getTaskId :: IO (Maybe Int64)
    getTaskId =
      maybeFirstRow fromOnly $
        DB.query
          db
          [sql|
            SELECT delivery_task_id
            FROM delivery_tasks
            WHERE group_id = ? AND delivery_scope_type = ?
              AND failed = 0 AND task_status = ?
            ORDER BY created_at ASC, delivery_task_id ASC
            LIMIT 1
          |]
          (groupId, scopeType, DTSNew)
    getTask :: Int64 -> IO (Either StoreError DeliveryTask)
    getTask taskId =
      firstRow' toDeliveryTask (SEDeliveryJobNotFound taskId) $
        DB.query db deliveryTaskQuery (Only taskId)
      where
        toDeliveryTask :: DeliveryTaskRow -> Either StoreError DeliveryTask
        toDeliveryTask taskRow@(_, jobType, _, _, _, _, _, _, _, _, _) =
          case jobType of
            DJTMessageForward -> DTMessageForward <$> toMsgFwdTask_ taskRow
            DJTRelayRemoved -> DTRelayRemoved <$> toRelayRmvdTask_ taskRow

deliveryTaskQuery :: Query
deliveryTaskQuery =
  [sql|
    SELECT
      t.delivery_task_id, t.delivery_job_type,
      t.delivery_scope_type, t.delivery_scope_include_pending, t.delivery_scope_support_gm_id,
      m.group_member_id, m.member_id, p.display_name,
      COALESCE(msg.broker_ts, msg.created_at), msg.msg_body, t.message_from_channel
    FROM delivery_tasks t
    JOIN messages msg ON msg.message_id = t.message_id
    JOIN group_members m ON m.group_member_id = t.sender_group_member_id
    JOIN contact_profiles p ON p.contact_profile_id = COALESCE(m.member_profile_id, m.contact_profile_id)
    WHERE t.delivery_task_id = ?
  |]

toMsgFwdTask_ :: DeliveryTaskRow -> Either StoreError MessageForwardTask
toMsgFwdTask_ (taskId, _jobType, scopeType, scopeIncludePending_, scopeSupportGMId_, senderGMId, senderMemberId, senderMemberName, brokerTs, chatMessage, BI messageFromChannel) =
  case toDeliveryScope_ scopeType scopeIncludePending_ scopeSupportGMId_ of
    Just deliveryScope -> Right $ MessageForwardTask {taskId, deliveryScope, senderGMId, senderMemberId, senderMemberName, brokerTs, chatMessage, messageFromChannel}
    Nothing -> Left $ SEInvalidDeliveryTask taskId

toRelayRmvdTask_ :: DeliveryTaskRow -> Either StoreError RelayRemovedTask
toRelayRmvdTask_ (taskId, _jobType, scopeType, scopeIncludePending_, scopeSupportGMId_, senderGMId, senderMemberId, senderMemberName, brokerTs, chatMessage, BI _messageFromChannel) =
  case toDeliveryScope_ scopeType scopeIncludePending_ scopeSupportGMId_ of
    Just GDSGroup {includePending = True} -> Right $ RelayRemovedTask {taskId, senderGMId, senderMemberId, senderMemberName, brokerTs, chatMessage}
    _ -> Left $ SEInvalidDeliveryTask taskId

markDeliveryTaskFailed_ :: DB.Connection -> Int64 -> IO ()
markDeliveryTaskFailed_ db taskId =
  DB.execute db "UPDATE delivery_tasks SET failed = 1 where delivery_task_id = ?" (Only taskId)

-- TODO [channels fwd] can optimize to read DJTMessageForward tasks so that message batch exactly fits into single transport message
-- passed MessageForwardTask defines the scope to search for
getNextMsgFwdTasks :: DB.Connection -> GroupInfo -> MessageForwardTask -> IO (Either StoreError [Either StoreError MessageForwardTask])
getNextMsgFwdTasks db gInfo task =
  getWorkItems "message forward task" getTaskIds getMsgFwdTask (markDeliveryTaskFailed_ db)
  where
    GroupInfo {groupId, groupType} = gInfo
    MessageForwardTask {deliveryScope, senderGMId} = task
    (scopeType, scopeInclPending_, scopeSupportGMId_) = deliveryScopeFields_ deliveryScope
    getTaskIds :: IO [Int64]
    getTaskIds =
      case groupType of
        GTChannel ->
          map fromOnly
            <$> DB.query
              db
              -- `AND sender_group_member_id IS NOT NULL` is a dummy condition to trick sqlite
              -- to use covering index idx_delivery_tasks_next_for_job_type
              [sql|
                SELECT delivery_task_id
                FROM delivery_tasks
                WHERE group_id = ?
                  AND delivery_scope_type = ?
                  AND delivery_scope_include_pending IS NOT DISTINCT FROM ?
                  AND delivery_scope_support_gm_id IS NOT DISTINCT FROM ?
                  AND delivery_job_type = ?
                  AND sender_group_member_id IS NOT NULL
                  AND failed = 0
                  AND task_status = ?
                ORDER BY created_at ASC, delivery_task_id ASC
              |]
              (groupId, scopeType, BI <$> scopeInclPending_, scopeSupportGMId_, DJTMessageForward, DTSNew)
        -- For GTSmallGroup we guarantee a singleSenderGMId for a delivery job by additionally filtering
        -- on sender_group_member_id here, so that the job can then retrieve less members as recipients,
        -- optimizing for this single sender (see processDeliveryJob -> getForwardIntroducedMembers, etc.).
        -- We do this optimization in the job to decrease load on admins using mobile devices for clients.
        GTSmallGroup ->
          map fromOnly
            <$> DB.query
              db
              [sql|
                SELECT delivery_task_id
                FROM delivery_tasks
                WHERE group_id = ?
                  AND delivery_scope_type = ?
                  AND delivery_scope_include_pending IS NOT DISTINCT FROM ?
                  AND delivery_scope_support_gm_id IS NOT DISTINCT FROM ?
                  AND delivery_job_type = ?
                  AND sender_group_member_id = ?
                  AND failed = 0
                  AND task_status = ?
                ORDER BY created_at ASC, delivery_task_id ASC
              |]
              (groupId, scopeType, BI <$> scopeInclPending_, scopeSupportGMId_, DJTMessageForward, senderGMId, DTSNew)
    getMsgFwdTask :: Int64 -> IO (Either StoreError MessageForwardTask)
    getMsgFwdTask taskId =
      firstRow' toMsgFwdTask_ (SEDeliveryTaskNotFound taskId) $
        DB.query db deliveryTaskQuery (Only taskId)

createMessageForwardJob :: DB.Connection -> GroupInfo -> GroupDeliveryScope -> Maybe GroupMemberId -> ByteString -> IO ()
createMessageForwardJob db gInfo deliveryScope singleSenderGMId_ deliveryBody =
  createNewDeliveryJob_ db gInfo deliveryScope DJTMessageForward singleSenderGMId_ deliveryBody

createRelayRemovedJob :: DB.Connection -> GroupInfo -> GroupDeliveryScope -> GroupMemberId -> ByteString -> IO ()
createRelayRemovedJob db gInfo deliveryScope singleSenderGMId deliveryBody =
  createNewDeliveryJob_ db gInfo deliveryScope DJTRelayRemoved (Just singleSenderGMId) deliveryBody

createNewDeliveryJob_ :: DB.Connection -> GroupInfo -> GroupDeliveryScope -> DeliveryJobType -> Maybe GroupMemberId -> ByteString -> IO ()
createNewDeliveryJob_ db gInfo deliveryScope jobType singleSenderGMId_ deliveryBody = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      INSERT INTO delivery_jobs (
        group_id, delivery_scope_type, delivery_scope_include_pending, delivery_scope_support_gm_id,
        delivery_job_type, single_sender_group_member_id, delivery_body, job_status, created_at, updated_at
      ) VALUES (?,?,?,?,?,?,?,?,?,?)
    |]
    ( (groupId, scopeType, BI <$> scopeInclPending_, scopeSupportGMId_)
        :. (jobType, singleSenderGMId_, Binary deliveryBody, DJSNew, currentTs, currentTs)
    )
  where
    GroupInfo {groupId} = gInfo
    (scopeType, scopeInclPending_, scopeSupportGMId_) = deliveryScopeFields_ deliveryScope

updateDeliveryTaskStatus :: DB.Connection -> Int64 -> DeliveryTaskStatus -> IO ()
updateDeliveryTaskStatus db taskId status = updateDeliveryTaskStatus_ db taskId status Nothing

setDeliveryTaskErrStatus :: DB.Connection -> Int64 -> Text -> IO ()
setDeliveryTaskErrStatus db taskId errReason = updateDeliveryTaskStatus_ db taskId DTSError (Just errReason)

updateDeliveryTaskStatus_ :: DB.Connection -> Int64 -> DeliveryTaskStatus -> Maybe Text -> IO ()
updateDeliveryTaskStatus_ db taskId status errReason_ = do
  currentTs <- getCurrentTime
  DB.execute
    db
    "UPDATE delivery_tasks SET task_status = ?, task_err_reason = ?, updated_at = ? WHERE delivery_task_id = ?"
    (status, errReason_, currentTs, taskId)

deleteDoneDeliveryTasks :: DB.Connection -> UTCTime -> IO ()
deleteDoneDeliveryTasks db createdAtCutoff = do
  DB.execute
    db
    [sql|
      DELETE FROM delivery_tasks
      WHERE created_at <= ?
        AND (task_status IN (?,?) OR failed = 1)
    |]
    (createdAtCutoff, DTSProcessed, DTSError)

getPendingDeliveryJobScopes :: DB.Connection -> IO [DeliveryWorkerKey]
getPendingDeliveryJobScopes db =
  DB.query
    db
    [sql|
      SELECT DISTINCT group_id, delivery_scope_type
      FROM delivery_jobs
      WHERE failed = 0 AND job_status IN (?, ?)
    |]
    (DJSNew, DJSInProgress)

getNextDeliveryJob :: DB.Connection -> DeliveryWorkerKey -> IO (Either StoreError (Maybe DeliveryJob))
getNextDeliveryJob db deliveryKey = do
  getWorkItem "delivery job" getJobId getJob markJobFailed
  where
    (groupId, scopeType) = deliveryKey
    getJobId :: IO (Maybe Int64)
    getJobId =
      maybeFirstRow fromOnly $
        DB.query
          db
          [sql|
            SELECT delivery_job_id
            FROM delivery_jobs
            WHERE group_id = ? AND delivery_scope_type = ?
              AND failed = 0 AND job_status IN (?, ?)
            ORDER BY created_at ASC, delivery_job_id ASC
            LIMIT 1
          |]
          (groupId, scopeType, DJSNew, DJSInProgress)
    getJob :: Int64 -> IO (Either StoreError DeliveryJob)
    getJob jobId =
      firstRow' toDeliveryJob (SEDeliveryJobNotFound jobId) $
        DB.query
          db
          [sql|
            SELECT
              delivery_scope_type, delivery_scope_include_pending, delivery_scope_support_gm_id,
              delivery_job_type, single_sender_group_member_id, delivery_body, cursor_group_member_id
            FROM delivery_jobs
            WHERE delivery_job_id = ?
          |]
          (Only jobId)
      where
        toDeliveryJob :: (GroupDeliveryScopeType, Maybe BoolInt, Maybe GroupMemberId, DeliveryJobType, Maybe GroupMemberId, Binary ByteString, Maybe GroupMemberId) -> Either StoreError DeliveryJob
        toDeliveryJob (scopeType', scopeIncludePending_, scopeSupportGMId_, jobType, singleSenderGMId_, Binary deliveryBody, cursorGMId) =
          case jobType of
            DJTMessageForward -> case toDeliveryScope_ scopeType' scopeIncludePending_ scopeSupportGMId_ of
              Just deliveryScope -> Right $ DJMessageForward MessageForwardJob {jobId, deliveryScope, singleSenderGMId_, messagesBatch = deliveryBody, cursorGMId}
              Nothing -> Left $ SEInvalidDeliveryJob jobId
            DJTRelayRemoved -> case (singleSenderGMId_, toDeliveryScope_ scopeType' scopeIncludePending_ scopeSupportGMId_) of
              (Just singleSenderGMId, Just GDSGroup {includePending = True}) -> Right $ DJRelayRemoved RelayRemovedJob {jobId, singleSenderGMId, fwdChatMessage = deliveryBody, cursorGMId}
              _ -> Left $ SEInvalidDeliveryJob jobId
    markJobFailed :: Int64 -> IO ()
    markJobFailed jobId =
      DB.execute db "UPDATE delivery_jobs SET failed = 1 where delivery_job_id = ?" (Only jobId)

updateDeliveryJobStatus :: DB.Connection -> Int64 -> DeliveryJobStatus -> IO ()
updateDeliveryJobStatus db jobId status = updateDeliveryJobStatus_ db jobId status Nothing

setDeliveryJobErrStatus :: DB.Connection -> Int64 -> Text -> IO ()
setDeliveryJobErrStatus db jobId errReason = updateDeliveryJobStatus_ db jobId DJSError (Just errReason)

updateDeliveryJobStatus_ :: DB.Connection -> Int64 -> DeliveryJobStatus -> Maybe Text -> IO ()
updateDeliveryJobStatus_ db jobId status errReason_ = do
  currentTs <- getCurrentTime
  DB.execute
    db
    "UPDATE delivery_jobs SET job_status = ?, job_err_reason = ?, updated_at = ? WHERE delivery_job_id = ?"
    (status, errReason_, currentTs, jobId)

-- TODO [channels fwd] possible improvement is to prioritize owners and "active" members
getGroupMembersByCursor :: DB.Connection -> VersionRangeChat -> User -> GroupInfo -> Maybe GroupMemberId -> Maybe GroupMemberId -> Int -> IO [GroupMember]
getGroupMembersByCursor db vr user GroupInfo {groupId} cursorGMId_ singleSenderGMId_ count = do
  memberIds <-
    map fromOnly <$> case cursorGMId_ of
      Nothing ->
        DB.query
          db
          (query <> orderLimit)
          (groupId, singleSenderGMId_, GSMemIntroduced, GSMemIntroInvited, GSMemAccepted, GSMemAnnounced, GSMemConnected, GSMemComplete, count)
      Just cursorGMId ->
        DB.query
          db
          (query <> " AND group_member_id > ?" <> orderLimit)
          (groupId, singleSenderGMId_, GSMemIntroduced, GSMemIntroInvited, GSMemAccepted, GSMemAnnounced, GSMemConnected, GSMemComplete, cursorGMId, count)
  rights <$> mapM (runExceptT . getGroupMemberById db vr user) memberIds
  where
    query =
      [sql|
        SELECT group_member_id
        FROM group_members
        WHERE group_id = ?
          AND group_member_id IS DISTINCT FROM ?
          AND member_status IN (?,?,?,?,?,?)
      |]
    orderLimit = " ORDER BY group_member_id ASC LIMIT ?"

updateDeliveryJobCursor :: DB.Connection -> Int64 -> GroupMemberId -> IO ()
updateDeliveryJobCursor db jobId cursorGMId = do
  currentTs <- getCurrentTime
  DB.execute
    db
    "UPDATE delivery_jobs SET cursor_group_member_id = ?, updated_at = ? WHERE delivery_job_id = ?"
    (cursorGMId, currentTs, jobId)

deleteDoneDeliveryJobs :: DB.Connection -> UTCTime -> IO ()
deleteDoneDeliveryJobs db createdAtCutoff = do
  DB.execute
    db
    [sql|
      DELETE FROM delivery_jobs
      WHERE created_at <= ?
        AND (job_status IN (?,?) OR failed = 1)
    |]
    (createdAtCutoff, DJSComplete, DJSError)
