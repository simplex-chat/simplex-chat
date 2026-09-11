{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Simplex.Chat.Store.Feeds
  ( createFeed,
    getUserFeedId,
    getFeed,
    updateFeedUnreadChat,
    deleteFeedCIs,
    getFeedContactsByCursor,
    getFeedCustomerGroupsByCursor,
    getCustomerGroupsMembersByRange,
    getFeedContactInstancesByCursor,
    getFeedGroupInstancesByCursor,
    updateFeedInstanceStatuses,
    getDeliveredContactIdsByRange,
    getDeliveredMemberIdsByRange,
    updateFeedContactInstances,
    updateFeedGroupInstances,
    deleteFeedInstances,
    markFeedInstancesDeleted,
    detachFeedInstances,
    deleteFeedContactReactions,
    deleteFeedGroupReactions,
    createFeedJobs,
    resumeFeedJobsOnStart,
    getNextFeedJob,
    updateFeedJobCursor,
    setFeedJobErrStatus,
    completeFeedJob,
    getFeedJobMessages,
    deleteDoneFeedJobs,
    feedItemMsg,
    FeedItemMsg (..),
  )
where

import Control.Monad (forM_, unless)
import Control.Monad.Except (ExceptT (..), throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Simplex.Chat.Delivery
import Simplex.Chat.Messages
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Store.Direct (contactQueryFields, contactQueryFrom)
import Simplex.Chat.Store.Shared
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Store.AgentStore (firstRow, getWorkItem, maybeFirstRow)
import Simplex.Messaging.Agent.Store.DB (Binary (..), BoolInt (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Util (firstRow')
#if defined(dbPostgres)
import Database.PostgreSQL.Simple (In (..), Only (..), Query, ToRow, (:.) (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..), Query, ToRow, (:.) (..))
import Database.SQLite.Simple.QQ (sql)
#endif

createFeed :: DB.Connection -> User -> ExceptT StoreError IO ()
createFeed db User {userId} =
  liftIO (DB.query db "SELECT feed_id FROM feeds WHERE user_id = ? LIMIT 1" $ Only userId) >>= \case
    [] -> liftIO $ DB.execute db "INSERT INTO feeds (user_id) VALUES (?)" (Only userId)
    Only feedId : _ -> throwError $ SEFeedAlreadyExists feedId

getUserFeedId :: DB.Connection -> User -> ExceptT StoreError IO FeedId
getUserFeedId db User {userId} =
  ExceptT . firstRow fromOnly SEUserFeedNotFound $
    DB.query db "SELECT feed_id FROM feeds WHERE user_id = ?" (Only userId)

getFeed :: DB.Connection -> User -> FeedId -> ExceptT StoreError IO Feed
getFeed db User {userId} feedId =
  ExceptT . firstRow toFeed (SEFeedNotFound feedId) $
    DB.query
      db
      [sql|
        SELECT created_at, updated_at, chat_ts, favorite, unread_chat
        FROM feeds
        WHERE user_id = ? AND feed_id = ?
      |]
      (userId, feedId)
  where
    toFeed (createdAt, updatedAt, chatTs, BI favorite, BI unread) =
      Feed {feedId, userId, createdAt, updatedAt, chatTs, favorite, unread}

updateFeedUnreadChat :: DB.Connection -> User -> Feed -> Bool -> IO ()
updateFeedUnreadChat db User {userId} Feed {feedId} unreadChat = do
  updatedAt <- getCurrentTime
  DB.execute db "UPDATE feeds SET unread_chat = ?, updated_at = ? WHERE user_id = ? AND feed_id = ?" (BI unreadChat, updatedAt, userId, feedId)

deleteFeedCIs :: DB.Connection -> User -> Feed -> IO ()
deleteFeedCIs db User {userId} Feed {feedId} = do
  DB.execute db "DELETE FROM messages WHERE feed_id = ?" (Only feedId)
  DB.execute db "DELETE FROM chat_items WHERE user_id = ? AND feed_id = ?" (userId, feedId)

getFeedContactsByCursor :: DB.Connection -> StoreCxt -> User -> ChatItemId -> Maybe ContactId -> Int -> IO [(Contact, Maybe ChatItemId)]
getFeedContactsByCursor db cxt user@User {userId} feedItemId cursorId_ count = do
  currentTs <- getCurrentTime
  map (\(row :. Only itemId_) -> (toContact currentTs cxt user [] row, itemId_))
    <$> DB.query
      db
      ( contactQueryFields
          <> ", i.chat_item_id "
          <> contactQueryFrom
          <> " LEFT JOIN chat_items i ON i.feed_item_id = ? AND i.contact_id = ct.contact_id"
          <> " WHERE ct.user_id = ? AND ct.deleted = 0 AND ct.is_user = 0 AND ct.contact_id > ?"
          <> " ORDER BY ct.contact_id ASC LIMIT ?"
      )
      (feedItemId, userId, cursorId cursorId_, count)

getFeedCustomerGroupsByCursor :: DB.Connection -> StoreCxt -> User -> ChatItemId -> Maybe GroupId -> Int -> IO [(GroupInfo, Maybe ChatItemId)]
getFeedCustomerGroupsByCursor db cxt User {userId, userContactId} feedItemId cursorId_ count = do
  currentTs <- getCurrentTime
  map (\(row :. Only itemId_) -> (toGroupInfo currentTs cxt userContactId [] row, itemId_))
    <$> DB.query
      db
      ( groupInfoQueryFields
          <> ", i.chat_item_id "
          <> groupInfoQueryFrom
          <> " LEFT JOIN chat_items i ON i.feed_item_id = ? AND i.group_id = g.group_id"
          <> " WHERE g.user_id = ? AND mu.contact_id = ? AND g.business_chat = ? AND g.group_id > ?"
          <> " ORDER BY g.group_id ASC LIMIT ?"
      )
      (feedItemId, userId, userContactId, BCCustomer, cursorId cursorId_, count)

getCustomerGroupsMembersByRange :: DB.Connection -> StoreCxt -> User -> GroupId -> GroupId -> IO (Map GroupId [GroupMember])
getCustomerGroupsMembersByRange db cxt user@User {userId, userContactId} fromId toId = do
  currentTs <- getCurrentTime
  foldMembers . map (toContactMember currentTs cxt user)
    <$> DB.query
      db
      ( groupMemberQuery
          <> [sql|
                JOIN groups g ON g.group_id = m.group_id
                WHERE m.user_id = ? AND g.business_chat = ?
                  AND m.group_id > ? AND m.group_id <= ?
                  AND (m.contact_id IS NULL OR m.contact_id != ?)
             |]
      )
      (userId, BCCustomer, fromId, toId, userContactId)
  where
    foldMembers = foldr (\m -> M.insertWith (<>) (memberGroupId m) [m]) M.empty
    memberGroupId GroupMember {groupId} = groupId

data FeedItemMsg = FeedItemMsg
  { feedSharedMsgId :: SharedMsgId,
    feedContent :: CIContent 'MDSnd,
    feedItemText :: Text,
    feedHasLink :: Bool
  }

feedItemMsg :: CChatItem 'CTFeed -> Maybe FeedItemMsg
feedItemMsg (CChatItem _ ChatItem {content, meta = CIMeta {itemSharedMsgId, itemText, hasLink}}) = case content of
  CISndMsgContent _ ->
    (\smId -> FeedItemMsg {feedSharedMsgId = smId, feedContent = content, feedItemText = itemText, feedHasLink = isTrue hasLink})
      <$> itemSharedMsgId
  _ -> Nothing

instanceSpecCond :: FeedJobAction -> Query
instanceSpecCond = \case
  FJAUpdate _ -> " AND i.item_feed = 1"
  FJAFileDescr _ -> " AND i.item_deleted = 0"
  _ -> " AND i.item_feed > 0"

getFeedContactInstancesByCursor :: DB.Connection -> StoreCxt -> User -> ChatItemId -> FeedJobAction -> Maybe ContactId -> Int -> IO [(Contact, ChatItemId)]
getFeedContactInstancesByCursor db cxt user@User {userId} feedItemId spec cursorId_ count = do
  currentTs <- getCurrentTime
  map (\(row :. Only itemId) -> (toContact currentTs cxt user [] row, itemId))
    <$> DB.query
      db
      ( contactQueryFields
          <> ", i.chat_item_id "
          <> contactQueryFrom
          <> " JOIN chat_items i ON i.contact_id = ct.contact_id"
          <> " WHERE i.user_id = ? AND i.feed_item_id = ? AND i.contact_id > ?"
          <> instanceSpecCond spec
          <> " ORDER BY i.contact_id ASC LIMIT ?"
      )
      (userId, feedItemId, cursorId cursorId_, count)

getFeedGroupInstancesByCursor :: DB.Connection -> StoreCxt -> User -> ChatItemId -> FeedJobAction -> Maybe GroupId -> Int -> IO [(GroupInfo, ChatItemId)]
getFeedGroupInstancesByCursor db cxt User {userId, userContactId} feedItemId spec cursorId_ count = do
  currentTs <- getCurrentTime
  map (\(row :. Only itemId) -> (toGroupInfo currentTs cxt userContactId [] row, itemId))
    <$> DB.query
      db
      ( groupInfoQueryFields
          <> ", i.chat_item_id "
          <> groupInfoQueryFrom
          <> " JOIN chat_items i ON i.group_id = g.group_id"
          <> " WHERE i.user_id = ? AND mu.contact_id = ? AND i.feed_item_id = ? AND i.group_id > ?"
          <> instanceSpecCond spec
          <> " ORDER BY i.group_id ASC LIMIT ?"
      )
      (userId, userContactId, feedItemId, cursorId cursorId_, count)

updateFeedInstanceStatuses :: DB.Connection -> [(ChatItemId, CIStatus 'MDSnd)] -> IO ()
updateFeedInstanceStatuses db statuses = do
  currentTs <- getCurrentTime
  forM_ statuses $ \(itemId, status) ->
    DB.execute
      db
      "UPDATE chat_items SET item_status = ?, updated_at = ? WHERE chat_item_id = ?"
      (status, currentTs, itemId)

getDeliveredContactIdsByRange :: DB.Connection -> MessageId -> ContactId -> ContactId -> IO [ContactId]
getDeliveredContactIdsByRange db msgId fromId toId =
  map fromOnly
    <$> DB.query
      db
      [sql|
        SELECT c.contact_id
        FROM msg_deliveries d
        JOIN connections c ON c.connection_id = d.connection_id
        WHERE d.message_id = ? AND c.contact_id > ? AND c.contact_id <= ?
      |]
      (msgId, fromId, toId)

getDeliveredMemberIdsByRange :: DB.Connection -> MessageId -> GroupId -> GroupId -> IO [GroupMemberId]
getDeliveredMemberIdsByRange db msgId fromId toId =
  map fromOnly
    <$> DB.query
      db
      [sql|
        SELECT gm.group_member_id
        FROM msg_deliveries d
        JOIN connections c ON c.connection_id = d.connection_id
        JOIN group_members gm ON gm.group_member_id = c.group_member_id
        WHERE d.message_id = ? AND gm.group_id > ? AND gm.group_id <= ?
      |]
      (msgId, fromId, toId)

updateFeedContactInstances :: DB.Connection -> User -> ChatItemId -> ContactId -> ContactId -> FeedItemMsg -> IO ()
updateFeedContactInstances db user feedItemId = updateFeedInstances_ db user feedItemId "contact_id"

updateFeedGroupInstances :: DB.Connection -> User -> ChatItemId -> GroupId -> GroupId -> FeedItemMsg -> IO ()
updateFeedGroupInstances db user feedItemId = updateFeedInstances_ db user feedItemId "group_id"

updateFeedInstances_ :: DB.Connection -> User -> ChatItemId -> Query -> Int64 -> Int64 -> FeedItemMsg -> IO ()
updateFeedInstances_ db User {userId} feedItemId chatIdColumn fromId toId FeedItemMsg {feedContent = content, feedItemText = itemText, feedHasLink = hasLink} = do
  currentTs <- getCurrentTime
  DB.execute
    db
    ( "UPDATE chat_items SET item_content = ?, item_text = ?, item_edited = 1, has_link = ?, updated_at = ?"
        <> " WHERE user_id = ? AND feed_item_id = ? AND item_feed = 1"
        <> " AND " <> chatIdColumn <> " > ? AND " <> chatIdColumn <> " <= ?"
    )
    (content, itemText, BI hasLink, currentTs, userId, feedItemId, fromId, toId)

deleteFeedInstances :: DB.Connection -> [ChatItemId] -> IO ()
deleteFeedInstances db itemIds = do
  itemIdsStmt db "DELETE FROM chat_item_versions WHERE chat_item_id" () itemIds
  itemIdsStmt db "DELETE FROM chat_items WHERE chat_item_id" () itemIds

markFeedInstancesDeleted :: DB.Connection -> [ChatItemId] -> UTCTime -> IO ()
markFeedInstancesDeleted db itemIds deletedTs = do
  currentTs <- getCurrentTime
  itemIdsStmt
    db
    "UPDATE chat_items SET item_deleted = 1, item_deleted_ts = ?, updated_at = ? WHERE chat_item_id"
    (deletedTs, currentTs)
    itemIds

detachFeedInstances :: DB.Connection -> [ChatItemId] -> IO ()
detachFeedInstances db itemIds =
  itemIdsStmt db "UPDATE chat_items SET item_feed = 2 WHERE item_feed = 1 AND chat_item_id" () itemIds

deleteFeedContactReactions :: DB.Connection -> SharedMsgId -> [ContactId] -> IO ()
deleteFeedContactReactions db sharedMsgId contactIds =
  itemIdsStmt db "DELETE FROM chat_item_reactions WHERE shared_msg_id = ? AND contact_id" (Only sharedMsgId) contactIds

deleteFeedGroupReactions :: DB.Connection -> SharedMsgId -> [(GroupId, MemberId)] -> IO ()
deleteFeedGroupReactions db sharedMsgId groupMemberIds =
  forM_ groupMemberIds $ \(groupId, memberId) ->
    DB.execute
      db
      "DELETE FROM chat_item_reactions WHERE group_id = ? AND shared_msg_id = ? AND item_member_id = ?"
      (groupId, sharedMsgId, memberId)

itemIdsStmt :: ToRow p => DB.Connection -> Query -> p -> [Int64] -> IO ()
itemIdsStmt db stmt params itemIds = unless (null itemIds) execIds
  where
#if defined(dbPostgres)
    execIds = DB.execute db (stmt <> " IN ?") (params :. Only (In itemIds))
#else
    execIds = forM_ itemIds $ \itemId -> DB.execute db (stmt <> " = ?") (params :. Only itemId)
#endif

cursorId :: Maybe Int64 -> Int64
cursorId = fromMaybe 0

createFeedJobs :: DB.Connection -> FeedId -> ChatItemId -> FeedJobAction -> IO ()
createFeedJobs db feedId feedItemId action = do
  currentTs <- getCurrentTime
  DB.execute
    db
    "DELETE FROM feed_jobs WHERE chat_item_id = ? AND action_tag = ? AND (job_status = ? OR failed = 1)"
    (feedItemId, tag, DJSError)
  DB.executeMany
    db
    [sql|
      INSERT INTO feed_jobs (
        feed_id, chat_item_id, worker_scope, action_tag, message_ids,
        job_status, created_at, updated_at
      ) VALUES (?,?,?,?,?,?,?,?)
    |]
    [(feedId, feedItemId, scope, tag, msgIds, DJSPending, currentTs, currentTs) | scope <- feedWorkerScopes]
  where
    tag = feedActionTag action
    msgIds = idsColumn $ feedActionMsgIds action

resumeFeedJobsOnStart :: DB.Connection -> IO [FeedJobKey]
resumeFeedJobsOnStart db = do
  currentTs <- getCurrentTime
  DB.execute
    db
    "UPDATE feed_jobs SET job_status = ?, updated_at = ? WHERE failed = 0 AND job_status = ?"
    (DJSPending, currentTs, DJSError)
  DB.query
    db
    [sql|
      SELECT DISTINCT feed_id, worker_scope
      FROM feed_jobs
      WHERE failed = 0 AND job_status = ?
    |]
    (Only DJSPending)

type FeedJobRow = (Int64, ChatItemId, FeedJobActionTag, Maybe Text, Maybe Int64)

getNextFeedJob :: DB.Connection -> FeedJobKey -> IO (Either StoreError (Maybe FeedJob))
getNextFeedJob db (feedId, scope) =
  getWorkItem "feed job" getJobId getJob markJobFailed
  where
    getJobId :: IO (Maybe Int64)
    getJobId =
      maybeFirstRow fromOnly $
        DB.query
          db
          [sql|
            SELECT feed_job_id
            FROM feed_jobs
            WHERE feed_id = ? AND worker_scope = ?
              AND failed = 0 AND job_status = ?
            ORDER BY feed_job_id ASC
            LIMIT 1
          |]
          (feedId, scope, DJSPending)
    getJob :: Int64 -> IO (Either StoreError FeedJob)
    getJob jobId =
      firstRow' toFeedJob (SEFeedJobNotFound jobId) $
        DB.query
          db
          [sql|
            SELECT feed_job_id, chat_item_id, action_tag, message_ids, cursor_id
            FROM feed_jobs
            WHERE feed_job_id = ?
          |]
          (Only jobId)
      where
        toFeedJob :: FeedJobRow -> Either StoreError FeedJob
        toFeedJob (feedJobId, feedItemId, actionTag, msgIdsText_, cursorId_) = do
          msgIds <- maybe (Left $ SEInvalidFeedJob feedJobId) Right $ parseIds msgIdsText_
          feedAction <- case (actionTag, msgIds) of
            (FJATNew, [msgId]) -> Right $ FJANew msgId
            (FJATFileDescr, msgId : msgIds') -> Right $ FJAFileDescr (msgId :| msgIds')
            (FJATUpdate, [msgId]) -> Right $ FJAUpdate msgId
            (FJATDeleteBroadcast, [msgId]) -> Right $ FJADeleteBroadcast msgId
            (FJATDeleteInternal, []) -> Right FJADeleteInternal
            (FJATDeleteMark, []) -> Right FJADeleteMark
            _ -> Left $ SEInvalidFeedJob feedJobId
          Right FeedJob {feedJobId, feedItemId, feedAction, cursorId_}
    markJobFailed :: Int64 -> IO ()
    markJobFailed jobId =
      DB.execute db "UPDATE feed_jobs SET failed = 1 WHERE feed_job_id = ?" (Only jobId)

updateFeedJobCursor :: DB.Connection -> Int64 -> Int64 -> IO ()
updateFeedJobCursor db jobId cursorId_ = do
  currentTs <- getCurrentTime
  DB.execute
    db
    "UPDATE feed_jobs SET cursor_id = ?, updated_at = ? WHERE feed_job_id = ?"
    (cursorId_, currentTs, jobId)

setFeedJobErrStatus :: DB.Connection -> Int64 -> Text -> IO ()
setFeedJobErrStatus db jobId errReason = updateFeedJobStatus_ db jobId DJSError (Just errReason)

updateFeedJobStatus_ :: DB.Connection -> Int64 -> DeliveryJobStatus -> Maybe Text -> IO ()
updateFeedJobStatus_ db jobId status errReason_ = do
  currentTs <- getCurrentTime
  DB.execute
    db
    "UPDATE feed_jobs SET job_status = ?, job_err_reason = ?, updated_at = ? WHERE feed_job_id = ?"
    (status, errReason_, currentTs, jobId)

completeFeedJob :: DB.Connection -> Int64 -> ChatItemId -> FeedJobActionTag -> IO Bool
completeFeedJob db jobId feedItemId actionTag = do
  updateFeedJobStatus_ db jobId DJSComplete Nothing
  unfinished <-
    maybeFirstRow fromOnly $
      DB.query
        db
        [sql|
          SELECT COUNT(1)
          FROM feed_jobs
          WHERE chat_item_id = ? AND action_tag = ? AND job_status != ?
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

deleteDoneFeedJobs :: DB.Connection -> UTCTime -> IO ()
deleteDoneFeedJobs db createdAtCutoff =
  DB.execute
    db
    "DELETE FROM feed_jobs WHERE created_at <= ? AND (job_status = ? OR failed = 1)"
    (createdAtCutoff, DJSComplete)
