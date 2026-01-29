{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Simplex.Chat.Store.Delivery
  ( createMsgDeliveryTask,
    deleteGroupDeliveryTasks,
    deleteGroupDeliveryJobs,
    getPendingDeliveryTaskScopes,
    getNextDeliveryTask,
    getNextDeliveryTasks,
    updateDeliveryTaskStatus,
    setDeliveryTaskErrStatus,
    deleteDoneDeliveryTasks,
    createMsgDeliveryJob,
    getPendingDeliveryJobScopes,
    getNextDeliveryJob,
    updateDeliveryJobStatus,
    setDeliveryJobErrStatus,
    getGroupMembersByCursor,
    updateDeliveryJobCursor,
    deleteDoneDeliveryJobs,
  )
where

import Data.ByteString.Char8 (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Simplex.Chat.Delivery
import Simplex.Chat.Protocol hiding (Binary)
import Simplex.Chat.Store.Shared
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Store.AgentStore (getWorkItem, getWorkItems, maybeFirstRow)
import Simplex.Messaging.Agent.Store.DB (Binary (..), BoolInt (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Util (firstRow')
#if defined(dbPostgres)
import Database.PostgreSQL.Simple (In (..), Only (..), (:.) (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Control.Monad.Except
import Data.Either (rights)
import Database.SQLite.Simple (Only (..), (:.) (..))
import Database.SQLite.Simple.QQ (sql)
import Simplex.Chat.Store.Groups (getGroupMemberById)
#endif

type DeliveryJobScopeRow = (DeliveryWorkerScope, Maybe DeliveryJobSpecTag, Maybe BoolInt, Maybe GroupMemberId)

jobScopeRow_ :: DeliveryJobScope -> DeliveryJobScopeRow
jobScopeRow_ = \case
  DJSGroup {jobSpec} -> case jobSpec of
    DJDeliveryJob {includePending} -> (DWSGroup, Just DJSTDeliveryJob, Just (BI includePending), Nothing)
    DJRelayRemoved -> (DWSGroup, Just DJSTRelayRemoved, Nothing, Nothing)
  DJSMemberSupport {supportGMId} -> (DWSMemberSupport, Nothing, Nothing, Just supportGMId)

toJobScope_ :: DeliveryJobScopeRow -> Maybe DeliveryJobScope
toJobScope_ = \case
  (DWSGroup, Just DJSTDeliveryJob, Just (BI includePending), Nothing) -> Just $ DJSGroup {jobSpec = DJDeliveryJob {includePending}}
  (DWSGroup, Just DJSTRelayRemoved, Nothing, Nothing) -> Just $ DJSGroup {jobSpec = DJRelayRemoved}
  (DWSMemberSupport, Nothing, Nothing, Just supportGMId) -> Just $ DJSMemberSupport {supportGMId}
  _ -> Nothing

createMsgDeliveryTask :: DB.Connection -> GroupInfo -> GroupMember -> NewMessageDeliveryTask -> IO ()
createMsgDeliveryTask db gInfo sender newTask = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      INSERT INTO delivery_tasks (
        group_id,
        worker_scope, job_scope_spec_tag, job_scope_include_pending, job_scope_support_gm_id,
        sender_group_member_id, message_id, message_from_channel, task_status,
        created_at, updated_at
      ) VALUES (?,?,?,?,?,?,?,?,?,?,?)
    |]
    ((Only groupId) :. jobScopeRow_ jobScope :. (groupMemberId' sender, messageId, BI messageFromChannel, DTSNew, currentTs, currentTs))
  where
    GroupInfo {groupId} = gInfo
    NewMessageDeliveryTask {messageId, jobScope, messageFromChannel} = newTask

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
      SELECT DISTINCT group_id, worker_scope
      FROM delivery_tasks
      WHERE failed = 0 AND task_status = ?
    |]
    (Only DTSNew)

getNextDeliveryTask :: DB.Connection -> DeliveryWorkerKey -> IO (Either StoreError (Maybe MessageDeliveryTask))
getNextDeliveryTask db deliveryKey = do
  getWorkItem "delivery task" getTaskId (getMsgDeliveryTask_ db) (markDeliveryTaskFailed_ db)
  where
    (groupId, workerScope) = deliveryKey
    getTaskId :: IO (Maybe Int64)
    getTaskId =
      maybeFirstRow fromOnly $
        DB.query
          db
          [sql|
            SELECT delivery_task_id
            FROM delivery_tasks
            WHERE group_id = ? AND worker_scope = ?
              AND failed = 0 AND task_status = ?
            ORDER BY delivery_task_id ASC
            LIMIT 1
          |]
          (groupId, workerScope, DTSNew)

type MessageDeliveryTaskRow = (Only Int64) :. DeliveryJobScopeRow :. (GroupMemberId, MemberId, ContactName, UTCTime, ChatMessage 'Json, BoolInt)

getMsgDeliveryTask_ :: DB.Connection -> Int64 -> IO (Either StoreError MessageDeliveryTask)
getMsgDeliveryTask_ db taskId =
  firstRow' toTask (SEDeliveryTaskNotFound taskId) $
    DB.query
      db
      [sql|
        SELECT
          t.delivery_task_id,
          t.worker_scope, t.job_scope_spec_tag, t.job_scope_include_pending, t.job_scope_support_gm_id,
          m.group_member_id, m.member_id, p.display_name, msg.broker_ts, msg.msg_body, t.message_from_channel
        FROM delivery_tasks t
        JOIN messages msg ON msg.message_id = t.message_id
        JOIN group_members m ON m.group_member_id = t.sender_group_member_id
        JOIN contact_profiles p ON p.contact_profile_id = COALESCE(m.member_profile_id, m.contact_profile_id)
        WHERE t.delivery_task_id = ?
      |]
      (Only taskId)
  where
    toTask :: MessageDeliveryTaskRow -> Either StoreError MessageDeliveryTask
    toTask ((Only taskId') :. jobScopeRow :. (senderGMId, senderMemberId, senderMemberName, brokerTs, chatMessage, BI messageFromChannel)) =
      case toJobScope_ jobScopeRow of
        Just jobScope -> Right $ MessageDeliveryTask {taskId = taskId', jobScope, senderGMId, senderMemberId, senderMemberName, brokerTs, chatMessage, messageFromChannel}
        Nothing -> Left $ SEInvalidDeliveryTask taskId'

markDeliveryTaskFailed_ :: DB.Connection -> Int64 -> IO ()
markDeliveryTaskFailed_ db taskId =
  DB.execute db "UPDATE delivery_tasks SET failed = 1 where delivery_task_id = ?" (Only taskId)

-- TODO [channels fwd] possible optimization is to read and add tasks to batch iteratively to avoid reading too many tasks
-- passed MessageDeliveryTask defines the jobScope to search for
getNextDeliveryTasks :: DB.Connection -> GroupInfo -> MessageDeliveryTask -> IO (Either StoreError [Either StoreError MessageDeliveryTask])
getNextDeliveryTasks db gInfo task =
  getWorkItems "message delivery task" getTaskIds (getMsgDeliveryTask_ db) (markDeliveryTaskFailed_ db)
  where
    GroupInfo {groupId} = gInfo
    MessageDeliveryTask {jobScope, senderGMId} = task
    getTaskIds :: IO [Int64]
    getTaskIds
      | useRelays' gInfo =
          map fromOnly
            <$> DB.query
              db
              [sql|
                SELECT delivery_task_id
                FROM delivery_tasks
                WHERE group_id = ?
                  AND worker_scope = ?
                  AND job_scope_spec_tag IS NOT DISTINCT FROM ?
                  AND job_scope_include_pending IS NOT DISTINCT FROM ?
                  AND job_scope_support_gm_id IS NOT DISTINCT FROM ?
                  AND failed = 0
                  AND task_status = ?
                ORDER BY delivery_task_id ASC
              |]
              ((Only groupId) :. jobScopeRow_ jobScope :. (Only DTSNew))
      | otherwise =
          -- For fully connected groups we guarantee a singleSenderGMId for a delivery job by additionally filtering
          -- on sender_group_member_id here, so that the job can then retrieve less members as recipients,
          -- optimizing for this single sender (see processDeliveryJob -> fully connected group branch).
          -- We do this optimization in the job to decrease load on admins using mobile devices for clients.
          map fromOnly
            <$> DB.query
              db
              [sql|
                SELECT delivery_task_id
                FROM delivery_tasks
                WHERE group_id = ?
                  AND worker_scope = ?
                  AND job_scope_spec_tag IS NOT DISTINCT FROM ?
                  AND job_scope_include_pending IS NOT DISTINCT FROM ?
                  AND job_scope_support_gm_id IS NOT DISTINCT FROM ?
                  AND sender_group_member_id = ?
                  AND failed = 0
                  AND task_status = ?
                ORDER BY delivery_task_id ASC
              |]
              ((Only groupId) :. jobScopeRow_ jobScope :. (senderGMId, DTSNew))

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

createMsgDeliveryJob :: DB.Connection -> GroupInfo -> DeliveryJobScope -> Maybe GroupMemberId -> ByteString -> IO ()
createMsgDeliveryJob db gInfo jobScope singleSenderGMId_ body = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      INSERT INTO delivery_jobs (
        group_id,
        worker_scope, job_scope_spec_tag, job_scope_include_pending, job_scope_support_gm_id,
        single_sender_group_member_id, body, job_status, created_at, updated_at
      ) VALUES (?,?,?,?,?,?,?,?,?,?)
    |]
    ((Only groupId) :. jobScopeRow_ jobScope :. (singleSenderGMId_, Binary body, DJSPending, currentTs, currentTs))
  where
    GroupInfo {groupId} = gInfo

getPendingDeliveryJobScopes :: DB.Connection -> IO [DeliveryWorkerKey]
getPendingDeliveryJobScopes db =
  DB.query
    db
    [sql|
      SELECT DISTINCT group_id, worker_scope
      FROM delivery_jobs
      WHERE failed = 0 AND job_status = ?
    |]
    (Only DJSPending)

type MessageDeliveryJobRow = (Only Int64) :. DeliveryJobScopeRow :. (Maybe GroupMemberId, Binary ByteString, Maybe GroupMemberId)

getNextDeliveryJob :: DB.Connection -> DeliveryWorkerKey -> IO (Either StoreError (Maybe MessageDeliveryJob))
getNextDeliveryJob db deliveryKey = do
  getWorkItem "delivery job" getJobId getJob markJobFailed
  where
    (groupId, workerScope) = deliveryKey
    getJobId :: IO (Maybe Int64)
    getJobId =
      maybeFirstRow fromOnly $
        DB.query
          db
          [sql|
            SELECT delivery_job_id
            FROM delivery_jobs
            WHERE group_id = ? AND worker_scope = ?
              AND failed = 0 AND job_status = ?
            ORDER BY delivery_job_id ASC
            LIMIT 1
          |]
          (groupId, workerScope, DJSPending)
    getJob :: Int64 -> IO (Either StoreError MessageDeliveryJob)
    getJob jobId =
      firstRow' toDeliveryJob (SEDeliveryJobNotFound jobId) $
        DB.query
          db
          [sql|
            SELECT
              delivery_job_id,
              worker_scope, job_scope_spec_tag, job_scope_include_pending, job_scope_support_gm_id,
              single_sender_group_member_id, body, cursor_group_member_id
            FROM delivery_jobs
            WHERE delivery_job_id = ?
          |]
          (Only jobId)
      where
        toDeliveryJob :: MessageDeliveryJobRow -> Either StoreError MessageDeliveryJob
        toDeliveryJob ((Only jobId') :. jobScopeRow :. (singleSenderGMId_, Binary body, cursorGMId_)) =
          case toJobScope_ jobScopeRow of
            Just jobScope -> Right $ MessageDeliveryJob {jobId = jobId', jobScope, singleSenderGMId_, body, cursorGMId_}
            Nothing -> Left $ SEInvalidDeliveryJob jobId'
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
getGroupMembersByCursor db vr user@User {userContactId} GroupInfo {groupId} cursorGMId_ singleSenderGMId_ count = do
  gmIds :: [Int64] <-
    map fromOnly <$> case cursorGMId_ of
      Nothing ->
        DB.query
          db
          (query <> orderLimit)
          ( (groupId, userContactId, singleSenderGMId_, GSMemIntroduced, GSMemIntroInvited, GSMemAccepted, GSMemAnnounced, GSMemConnected, GSMemComplete)
              :. (Only count)
          )
      Just cursorGMId ->
        DB.query
          db
          (query <> " AND group_member_id > ?" <> orderLimit)
          ( (groupId, userContactId, singleSenderGMId_, GSMemIntroduced, GSMemIntroInvited, GSMemAccepted, GSMemAnnounced, GSMemConnected, GSMemComplete)
              :. (cursorGMId, count)
          )
#if defined(dbPostgres)
  map (toContactMember vr user) <$>
    DB.query
      db
      (groupMemberQuery <> " WHERE m.group_member_id IN ?")
      (Only (In gmIds))
#else
  rights <$> mapM (runExceptT . getGroupMemberById db vr user) gmIds
#endif
  where
    query =
      [sql|
        SELECT group_member_id
        FROM group_members
        WHERE group_id = ?
          AND contact_id IS DISTINCT FROM ?
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
