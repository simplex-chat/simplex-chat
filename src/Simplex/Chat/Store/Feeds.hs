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
    getFeedInstanceContactIdsByRange,
    getFeedInstanceGroupIdsByRange,
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
    feedItemMsg,
    FeedItemMsg (..),
  )
where

import Control.Monad (forM_, unless)
import Control.Monad.Except (ExceptT (..), throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Simplex.Chat.Delivery (FeedInstanceSpec (..))
import Simplex.Chat.Messages
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Store.Shared
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Store.AgentStore (firstRow)
import Simplex.Messaging.Agent.Store.DB (BoolInt (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
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

getFeedContactsByCursor :: DB.Connection -> StoreCxt -> User -> Maybe ContactId -> Int -> IO [Contact]
getFeedContactsByCursor db cxt user@User {userId} cursorId_ count = do
  currentTs <- getCurrentTime
  map (toContact currentTs cxt user [])
    <$> DB.query
      db
      (contactQuery <> " WHERE ct.user_id = ? AND ct.deleted = 0 AND ct.is_user = 0 AND ct.contact_id > ? ORDER BY ct.contact_id ASC LIMIT ?")
      (userId, cursorId cursorId_, count)

getFeedCustomerGroupsByCursor :: DB.Connection -> StoreCxt -> User -> Maybe GroupId -> Int -> IO [GroupInfo]
getFeedCustomerGroupsByCursor db cxt User {userId, userContactId} cursorId_ count = do
  currentTs <- getCurrentTime
  map (toGroupInfo currentTs cxt userContactId [])
    <$> DB.query
      db
      (groupInfoQuery <> " WHERE g.user_id = ? AND mu.contact_id = ? AND g.business_chat = ? AND g.group_id > ? ORDER BY g.group_id ASC LIMIT ?")
      (userId, userContactId, BCCustomer, cursorId cursorId_, count)

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

instanceSpecCond :: FeedInstanceSpec -> Query
instanceSpecCond = \case
  FISLinked -> " AND i.item_feed = 1"
  FISAny -> " AND i.item_feed > 0"
  FISUndeleted -> " AND i.item_deleted = 0"

getFeedContactInstancesByCursor :: DB.Connection -> StoreCxt -> User -> ChatItemId -> FeedInstanceSpec -> Maybe ContactId -> Int -> IO [(Contact, ChatItemId)]
getFeedContactInstancesByCursor db cxt user@User {userId} feedItemId spec cursorId_ count = do
  currentTs <- getCurrentTime
  map (\(Only itemId :. row) -> (toContact currentTs cxt user [] row, itemId))
    <$> DB.query
      db
      ( "SELECT i.chat_item_id, "
          <> contactQueryFields
          <> " "
          <> contactQueryFrom
          <> " JOIN chat_items i ON i.contact_id = ct.contact_id"
          <> " WHERE i.user_id = ? AND i.feed_item_id = ? AND i.contact_id > ?"
          <> instanceSpecCond spec
          <> " ORDER BY i.contact_id ASC LIMIT ?"
      )
      (userId, feedItemId, cursorId cursorId_, count)

getFeedGroupInstancesByCursor :: DB.Connection -> StoreCxt -> User -> ChatItemId -> FeedInstanceSpec -> Maybe GroupId -> Int -> IO [(GroupInfo, ChatItemId)]
getFeedGroupInstancesByCursor db cxt User {userId, userContactId} feedItemId spec cursorId_ count = do
  currentTs <- getCurrentTime
  map (\(Only itemId :. row) -> (toGroupInfo currentTs cxt userContactId [] row, itemId))
    <$> DB.query
      db
      ( "SELECT i.chat_item_id, "
          <> groupInfoQueryFields
          <> " "
          <> groupInfoQueryFrom
          <> " JOIN chat_items i ON i.group_id = g.group_id"
          <> " WHERE i.user_id = ? AND mu.contact_id = ? AND i.feed_item_id = ? AND i.group_id > ?"
          <> instanceSpecCond spec
          <> " ORDER BY i.group_id ASC LIMIT ?"
      )
      (userId, userContactId, feedItemId, cursorId cursorId_, count)

getFeedInstanceContactIdsByRange :: DB.Connection -> User -> ChatItemId -> ContactId -> ContactId -> IO (Map ContactId ChatItemId)
getFeedInstanceContactIdsByRange db user feedItemId = getFeedInstanceIdsByRange_ db user feedItemId "contact_id"

getFeedInstanceGroupIdsByRange :: DB.Connection -> User -> ChatItemId -> GroupId -> GroupId -> IO (Map GroupId ChatItemId)
getFeedInstanceGroupIdsByRange db user feedItemId = getFeedInstanceIdsByRange_ db user feedItemId "group_id"

getFeedInstanceIdsByRange_ :: DB.Connection -> User -> ChatItemId -> Query -> Int64 -> Int64 -> IO (Map Int64 ChatItemId)
getFeedInstanceIdsByRange_ db User {userId} feedItemId chatIdColumn fromId toId =
  M.fromList
    <$> DB.query
      db
      ( "SELECT " <> chatIdColumn <> ", chat_item_id FROM chat_items"
          <> " WHERE user_id = ? AND feed_item_id = ?"
          <> " AND " <> chatIdColumn <> " > ? AND " <> chatIdColumn <> " <= ?"
      )
      (userId, feedItemId, fromId, toId)

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
  itemIdsStmt db "DELETE FROM chat_item_messages WHERE chat_item_id" () itemIds
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
