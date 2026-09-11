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
    createFeedJobs,
    getNextFeedDeliveryJob,
    updateFeedDeliveryJobCursor,
    completeFeedJob,
    getFeedJobMessages,
  )
where

import qualified Data.Aeson as J
import Data.ByteString.Char8 (ByteString)
import Data.Int (Int64)
import qualified Data.List.NonEmpty as L
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (catMaybes, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Simplex.Chat.Delivery
import Simplex.Chat.Messages (ChatItemId, ChatType (..), MessageId, SndMessage (..))
import Simplex.Chat.Protocol hiding (Binary)
import Simplex.Chat.Store.Shared
import Simplex.Chat.Types
import Simplex.Chat.Types.Shared (MsgSigStatus (..))
import Simplex.Messaging.Agent.Store.AgentStore (getWorkItem, getWorkItems, maybeFirstRow)
import Simplex.Messaging.Agent.Store.DB (Binary (..), BoolInt (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Encoding (smpDecode)
import Simplex.Messaging.Util (eitherToMaybe, firstRow')
import Text.Read (readMaybe)
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
    ((Only groupId) :. jobScopeRow_ jobScope :. (groupMemberId' sender, messageId, BI sentAsGroup, DTSNew, currentTs, currentTs))
  where
    GroupInfo {groupId} = gInfo
    NewMessageDeliveryTask {messageId, taskContext = DeliveryTaskContext {jobScope, sentAsGroup}} = newTask

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

type MessageDeliveryTaskRow = (Only Int64) :. DeliveryJobScopeRow :. (GroupMemberId, MemberId, ContactName, UTCTime, Binary ByteString, Maybe ChatBinding, Maybe (Binary ByteString), BoolInt)

getMsgDeliveryTask_ :: DB.Connection -> Int64 -> IO (Either StoreError MessageDeliveryTask)
getMsgDeliveryTask_ db taskId =
  firstRow' toTask (SEDeliveryTaskNotFound taskId) $
    DB.query
      db
      [sql|
        SELECT
          t.delivery_task_id,
          t.worker_scope, t.job_scope_spec_tag, t.job_scope_include_pending, t.job_scope_support_gm_id,
          m.group_member_id, m.member_id, p.display_name, msg.broker_ts, msg.msg_body, msg.msg_chat_binding, msg.msg_signatures, t.message_from_channel
        FROM delivery_tasks t
        JOIN messages msg ON msg.message_id = t.message_id
        JOIN group_members m ON m.group_member_id = t.sender_group_member_id
        JOIN contact_profiles p ON p.contact_profile_id = COALESCE(m.member_profile_id, m.contact_profile_id)
        WHERE t.delivery_task_id = ?
      |]
      (Only taskId)
  where
    toTask :: MessageDeliveryTaskRow -> Either StoreError MessageDeliveryTask
    toTask ((Only taskId') :. jobScopeRow :. (senderGMId, senderMemberId, senderMemberName, brokerTs, Binary msgBody, chatBinding_, sigs_, BI showGroupAsSender)) =
      case (toJobScope_ jobScopeRow, J.eitherDecodeStrict' msgBody) of
        (Just jobScope, Right chatMsg) ->
          let fwdSender = if showGroupAsSender && isNothing chatBinding_ then FwdChannel else FwdMember senderMemberId senderMemberName
              -- Re-parsed from msg_body: validates stored content against current code.
              -- Signed: original bytes preserved (re-encoding would invalidate signature).
              -- Unsigned: re-encoded from parsed ChatMessage on forward (sanitizes content).
              verifiedMsg = case (chatBinding_, decodeSigs sigs_) of
                (Just cb, Just sigs) -> VMSigned MSSVerified (SignedMsg cb sigs msgBody) chatMsg
                _ -> VMUnsigned chatMsg
           in Right $ MessageDeliveryTask {taskId = taskId', jobScope, senderGMId, fwdSender, brokerTs, verifiedMsg}
        (Nothing, _) -> Left $ SEInvalidDeliveryTask taskId'
        (_, Left _) -> Left $ SEInvalidDeliveryTask taskId'
    decodeSigs :: Maybe (Binary ByteString) -> Maybe (L.NonEmpty MsgSignature)
    decodeSigs = (>>= eitherToMaybe . smpDecode . (\(Binary bs) -> bs))

markDeliveryTaskFailed_ :: DB.Connection -> Int64 -> IO ()
markDeliveryTaskFailed_ db taskId =
  DB.execute db "UPDATE delivery_tasks SET failed = 1 where delivery_task_id = ?" (Only taskId)

-- TODO [relays] possible optimization is to read and add tasks to batch iteratively to avoid reading too many tasks
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

createMsgDeliveryJob :: DB.Connection -> GroupInfo -> DeliveryJobScope -> [GroupMemberId] -> ByteString -> IO ()
createMsgDeliveryJob db gInfo jobScope senderGMIds body = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      INSERT INTO delivery_jobs (
        group_id,
        worker_scope, job_scope_spec_tag, job_scope_include_pending, job_scope_support_gm_id,
        sender_group_member_ids, body, job_status, created_at, updated_at
      ) VALUES (?,?,?,?,?,?,?,?,?,?)
    |]
    ((Only groupId) :. jobScopeRow_ jobScope :. (senderColumn, Binary body, DJSPending, currentTs, currentTs))
  where
    GroupInfo {groupId} = gInfo
    -- NULL ↔ []; non-empty list ↔ comma-separated decimal Int64s.
    senderColumn :: Maybe Text
    senderColumn
      | null senderGMIds = Nothing
      | otherwise = Just $ T.intercalate "," $ map (T.pack . show) senderGMIds

getPendingDeliveryJobScopes :: DB.Connection -> IO [DeliveryJobKey]
getPendingDeliveryJobScopes db = do
  groupScopes <-
    DB.query
      db
      [sql|
        SELECT DISTINCT group_id, worker_scope
        FROM delivery_jobs
        WHERE failed = 0 AND job_status = ? AND group_id IS NOT NULL
      |]
      (Only DJSPending)
  feedScopes <-
    DB.query
      db
      [sql|
        SELECT DISTINCT feed_id, worker_scope
        FROM delivery_jobs
        WHERE failed = 0 AND job_status = ? AND feed_id IS NOT NULL
      |]
      (Only DJSPending)
  pure $ map (uncurry DJKGroup) groupScopes <> map (uncurry DJKFeed) feedScopes

type MessageDeliveryJobRow = (Only Int64) :. DeliveryJobScopeRow :. (Maybe Text, Binary ByteString, Maybe GroupMemberId)

getNextDeliveryJob :: DB.Connection -> DeliveryWorkerKey -> IO (Either StoreError (Maybe (DeliveryJob 'CTGroup)))
getNextDeliveryJob db deliveryKey = do
  getWorkItem "delivery job" getJobId getJob (markJobFailed db)
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
    getJob :: Int64 -> IO (Either StoreError (DeliveryJob 'CTGroup))
    getJob jobId =
      firstRow' toDeliveryJob (SEDeliveryJobNotFound jobId) $
        DB.query
          db
          [sql|
            SELECT
              delivery_job_id,
              worker_scope, job_scope_spec_tag, job_scope_include_pending, job_scope_support_gm_id,
              sender_group_member_ids, body, cursor_group_member_id
            FROM delivery_jobs
            WHERE delivery_job_id = ?
          |]
          (Only jobId)
      where
        toDeliveryJob :: MessageDeliveryJobRow -> Either StoreError (DeliveryJob 'CTGroup)
        toDeliveryJob ((Only jobId') :. jobScopeRow :. (senderGMIdsText_, Binary body, cursorId_)) = do
          jobScope <- maybe (Left $ SEInvalidDeliveryJob jobId') Right $ toJobScope_ jobScopeRow
          senderGMIds <- parseIds jobId' senderGMIdsText_
          Right DeliveryJob {jobId = jobId', cursorId_, jobWork = DJWGroup {jobScope, senderGMIds, body}}

parseIds :: Int64 -> Maybe Text -> Either StoreError [Int64]
parseIds jobId = \case
  Nothing -> Right []
  Just t
    | T.null t -> Right []
    | otherwise -> maybe (Left $ SEInvalidDeliveryJob jobId) Right $ traverse (readMaybe . T.unpack) (T.splitOn "," t)

idsColumn :: [Int64] -> Maybe Text
idsColumn ids
  | null ids = Nothing
  | otherwise = Just $ T.intercalate "," $ map (T.pack . show) ids

markJobFailed :: DB.Connection -> Int64 -> IO ()
markJobFailed db jobId =
  DB.execute db "UPDATE delivery_jobs SET failed = 1 where delivery_job_id = ?" (Only jobId)

createFeedJobs :: DB.Connection -> FeedId -> ChatItemId -> FeedJobAction -> IO ()
createFeedJobs db feedId feedItemId action = do
  currentTs <- getCurrentTime
  DB.execute
    db
    "DELETE FROM delivery_jobs WHERE chat_item_id = ? AND job_scope_spec_tag = ? AND (job_status = ? OR failed = 1)"
    (feedItemId, tag, DJSError)
  DB.executeMany
    db
    [sql|
      INSERT INTO delivery_jobs (
        feed_id, chat_item_id, worker_scope, job_scope_spec_tag, message_ids,
        job_status, created_at, updated_at
      ) VALUES (?,?,?,?,?,?,?,?)
    |]
    [(feedId, feedItemId, scope, tag, msgIds, DJSPending, currentTs, currentTs) | scope <- feedWorkerScopes]
  where
    tag = feedActionTag action
    msgIds = idsColumn $ feedActionMsgIds action

type FeedDeliveryJobRow = (Int64, ChatItemId, FeedJobActionTag, Maybe Text, Maybe Int64)

getNextFeedDeliveryJob :: DB.Connection -> FeedId -> FeedWorkerScope -> IO (Either StoreError (Maybe (DeliveryJob 'CTFeed)))
getNextFeedDeliveryJob db feedId scope = do
  getWorkItem "feed delivery job" getJobId getJob (markJobFailed db)
  where
    getJobId :: IO (Maybe Int64)
    getJobId =
      maybeFirstRow fromOnly $
        DB.query
          db
          [sql|
            SELECT delivery_job_id
            FROM delivery_jobs
            WHERE feed_id = ? AND worker_scope = ?
              AND failed = 0 AND job_status = ?
            ORDER BY delivery_job_id ASC
            LIMIT 1
          |]
          (feedId, scope, DJSPending)
    getJob :: Int64 -> IO (Either StoreError (DeliveryJob 'CTFeed))
    getJob jobId =
      firstRow' toFeedDeliveryJob (SEDeliveryJobNotFound jobId) $
        DB.query
          db
          [sql|
            SELECT delivery_job_id, chat_item_id, job_scope_spec_tag, message_ids, feed_cursor_id
            FROM delivery_jobs
            WHERE delivery_job_id = ?
          |]
          (Only jobId)
    toFeedDeliveryJob :: FeedDeliveryJobRow -> Either StoreError (DeliveryJob 'CTFeed)
    toFeedDeliveryJob (jobId, feedItemId, actionTag, msgIdsText_, cursorId_) = do
      msgIds <- parseIds jobId msgIdsText_
      feedAction <- case (actionTag, msgIds) of
        (FJATNew, [msgId]) -> Right $ FJANew msgId
        (FJATFileDescr, msgId : msgIds') -> Right $ FJAFileDescr (msgId :| msgIds')
        (FJATUpdate, [msgId]) -> Right $ FJAUpdate msgId
        (FJATDeleteBroadcast, [msgId]) -> Right $ FJADeleteBroadcast msgId
        (FJATDeleteInternal, []) -> Right FJADeleteInternal
        (FJATDeleteMark, []) -> Right FJADeleteMark
        _ -> Left $ SEInvalidDeliveryJob jobId
      Right DeliveryJob {jobId, cursorId_, jobWork = DJWFeed {feedItemId, feedAction}}

updateFeedDeliveryJobCursor :: DB.Connection -> Int64 -> Int64 -> IO ()
updateFeedDeliveryJobCursor db jobId cursorId = do
  currentTs <- getCurrentTime
  DB.execute
    db
    "UPDATE delivery_jobs SET feed_cursor_id = ?, updated_at = ? WHERE delivery_job_id = ?"
    (cursorId, currentTs, jobId)

completeFeedJob :: DB.Connection -> Int64 -> ChatItemId -> FeedJobActionTag -> IO Bool
completeFeedJob db jobId feedItemId actionTag = do
  updateDeliveryJobStatus db jobId DJSComplete
  unfinished <-
    maybeFirstRow fromOnly $
      DB.query
        db
        [sql|
          SELECT COUNT(1)
          FROM delivery_jobs
          WHERE chat_item_id = ? AND job_scope_spec_tag = ? AND job_status != ?
        |]
        (feedItemId, actionTag, DJSComplete)
  pure $ unfinished == Just (0 :: Int)

getFeedJobMessages :: DB.Connection -> [MessageId] -> IO [SndMessage]
getFeedJobMessages db = fmap catMaybes . mapM getMsg
  where
    getMsg msgId =
      maybeFirstRow toSndMessage $
        DB.query db "SELECT shared_msg_id, msg_body FROM messages WHERE message_id = ? AND shared_msg_id IS NOT NULL" (Only msgId)
      where
        toSndMessage (sharedMsgId, Binary msgBody) = SndMessage {msgId, sharedMsgId, msgBody, signedMsg_ = Nothing}

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

-- TODO [relays] possible improvement is to prioritize owners and "active" members
getGroupMembersByCursor :: DB.Connection -> StoreCxt -> User -> GroupInfo -> Maybe GroupMemberId -> Maybe GroupMemberId -> Int -> IO [GroupMember]
getGroupMembersByCursor db cxt user@User {userContactId} GroupInfo {groupId} cursorGMId_ singleSenderGMId_ count = do
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
  currentTs <- getCurrentTime
  map (toContactMember currentTs cxt user) <$>
    DB.query
      db
      (groupMemberQuery <> " WHERE m.group_member_id IN ? ORDER BY m.group_member_id ASC")
      (Only (In gmIds))
#else
  rights <$> mapM (runExceptT . getGroupMemberById db cxt user) gmIds
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
