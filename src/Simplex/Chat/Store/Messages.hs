{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Simplex.Chat.Store.Messages
  ( getContactConnIds_,
    getDirectChatReactions_,

    -- * Message and chat item functions
    deleteContactCIs,
    getGroupFileInfo,
    deleteGroupCIs,
    createNewSndMessage,
    createSndMsgDelivery,
    createNewMessageAndRcvMsgDelivery,
    createSndMsgDeliveryEvent,
    createRcvMsgDeliveryEvent,
    createPendingGroupMessage,
    getPendingGroupMessages,
    deletePendingGroupMessage,
    deleteOldMessages,
    updateChatTs,
    createNewSndChatItem,
    createNewRcvChatItem,
    createNewChatItemNoMsg,
    getChatPreviews,
    getDirectChat,
    getGroupChat,
    getDirectChatItemsLast,
    getAllChatItems,
    getAChatItem,
    updateDirectChatItem,
    updateDirectChatItem',
    addInitialAndNewCIVersions,
    createChatItemVersion,
    deleteDirectChatItem,
    markDirectChatItemDeleted,
    updateGroupChatItemStatus,
    updateGroupChatItem,
    deleteGroupChatItem,
    updateGroupChatItemModerated,
    markGroupChatItemDeleted,
    updateDirectChatItemsRead,
    getDirectUnreadTimedItems,
    setDirectChatItemDeleteAt,
    updateGroupChatItemsRead,
    getGroupUnreadTimedItems,
    setGroupChatItemDeleteAt,
    getChatRefViaItemId,
    getChatItemVersions,
    getDirectCIReactions,
    getDirectReactions,
    setDirectReaction,
    getGroupCIReactions,
    getGroupReactions,
    setGroupReaction,
    getChatItemIdByAgentMsgId,
    getDirectChatItem,
    getDirectChatItemBySharedMsgId,
    getDirectChatItemByAgentMsgId,
    getGroupChatItem,
    getGroupChatItemBySharedMsgId,
    getGroupMemberCIBySharedMsgId,
    getGroupChatItemByAgentMsgId,
    getGroupMemberChatItemLast,
    getDirectChatItemIdByText,
    getDirectChatItemIdByText',
    getGroupChatItemIdByText,
    getGroupChatItemIdByText',
    getChatItemByFileId,
    getChatItemByGroupId,
    updateDirectChatItemStatus,
    getTimedItems,
    getChatItemTTL,
    setChatItemTTL,
    getContactExpiredFileInfo,
    deleteContactExpiredCIs,
    getGroupExpiredFileInfo,
    deleteGroupExpiredCIs,
    createCIModeration,
    getCIModeration,
    deleteCIModeration,
    createGroupSndStatus,
    getGroupSndStatus,
    updateGroupSndStatus,
    getGroupSndStatuses,
    getGroupSndStatusCounts,
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Crypto.Random (ChaChaDRG)
import Data.Bifunctor (first)
import Data.ByteString.Char8 (ByteString)
import Data.Either (fromRight, rights)
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Time (addUTCTime)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Database.SQLite.Simple (NamedParam (..), Only (..), (:.) (..))
import Database.SQLite.Simple.QQ (sql)
import Simplex.Chat.Markdown
import Simplex.Chat.Messages
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Protocol
import Simplex.Chat.Store.Direct
import Simplex.Chat.Store.Groups
import Simplex.Chat.Store.Shared
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Protocol (AgentMsgId, ConnId, MsgMeta (..), UserId)
import Simplex.Messaging.Agent.Store.SQLite (firstRow, firstRow', maybeFirstRow)
import qualified Simplex.Messaging.Agent.Store.SQLite.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.File (CryptoFile (..), CryptoFileArgs (..))
import Simplex.Messaging.Util (eitherToMaybe)
import UnliftIO.STM

deleteContactCIs :: DB.Connection -> User -> Contact -> IO ()
deleteContactCIs db user@User {userId} ct@Contact {contactId} = do
  connIds <- getContactConnIds_ db user ct
  forM_ connIds $ \connId ->
    DB.execute db "DELETE FROM messages WHERE connection_id = ?" (Only connId)
  DB.execute db "DELETE FROM chat_item_reactions WHERE contact_id = ?" (Only contactId)
  DB.execute db "DELETE FROM chat_items WHERE user_id = ? AND contact_id = ?" (userId, contactId)

getContactConnIds_ :: DB.Connection -> User -> Contact -> IO [Int64]
getContactConnIds_ db User {userId} Contact {contactId} =
  map fromOnly
    <$> DB.query db "SELECT connection_id FROM connections WHERE user_id = ? AND contact_id = ?" (userId, contactId)

getGroupFileInfo :: DB.Connection -> User -> GroupInfo -> IO [CIFileInfo]
getGroupFileInfo db User {userId} GroupInfo {groupId} =
  map toFileInfo
    <$> DB.query db (fileInfoQuery <> " WHERE i.user_id = ? AND i.group_id = ?") (userId, groupId)

deleteGroupCIs :: DB.Connection -> User -> GroupInfo -> IO ()
deleteGroupCIs db User {userId} GroupInfo {groupId} = do
  DB.execute db "DELETE FROM messages WHERE group_id = ?" (Only groupId)
  DB.execute db "DELETE FROM chat_item_reactions WHERE group_id = ?" (Only groupId)
  DB.execute db "DELETE FROM chat_items WHERE user_id = ? AND group_id = ?" (userId, groupId)

createNewSndMessage :: MsgEncodingI e => DB.Connection -> TVar ChaChaDRG -> ConnOrGroupId -> (SharedMsgId -> NewMessage e) -> ExceptT StoreError IO SndMessage
createNewSndMessage db gVar connOrGroupId mkMessage =
  createWithRandomId gVar $ \sharedMsgId -> do
    let NewMessage {chatMsgEvent, msgBody} = mkMessage $ SharedMsgId sharedMsgId
    createdAt <- getCurrentTime
    DB.execute
      db
      [sql|
        INSERT INTO messages (
          msg_sent, chat_msg_event, msg_body, connection_id, group_id,
          shared_msg_id, shared_msg_id_user, created_at, updated_at
        ) VALUES (?,?,?,?,?,?,?,?,?)
      |]
      (MDSnd, toCMEventTag chatMsgEvent, msgBody, connId_, groupId_, sharedMsgId, Just True, createdAt, createdAt)
    msgId <- insertedRowId db
    pure SndMessage {msgId, sharedMsgId = SharedMsgId sharedMsgId, msgBody}
  where
    (connId_, groupId_) = case connOrGroupId of
      ConnectionId connId -> (Just connId, Nothing)
      GroupId groupId -> (Nothing, Just groupId)

createSndMsgDelivery :: DB.Connection -> SndMsgDelivery -> MessageId -> IO Int64
createSndMsgDelivery db sndMsgDelivery messageId = do
  currentTs <- getCurrentTime
  msgDeliveryId <- createSndMsgDelivery_ db sndMsgDelivery messageId currentTs
  createMsgDeliveryEvent_ db msgDeliveryId MDSSndAgent currentTs
  pure msgDeliveryId

createNewMessageAndRcvMsgDelivery :: forall e. MsgEncodingI e => DB.Connection -> ConnOrGroupId -> NewMessage e -> Maybe SharedMsgId -> RcvMsgDelivery -> IO RcvMessage
createNewMessageAndRcvMsgDelivery db connOrGroupId NewMessage {chatMsgEvent, msgBody} sharedMsgId_ RcvMsgDelivery {connId, agentMsgId, agentMsgMeta, agentAckCmdId} = do
  currentTs <- getCurrentTime
  DB.execute
    db
    "INSERT INTO messages (msg_sent, chat_msg_event, msg_body, created_at, updated_at, connection_id, group_id, shared_msg_id) VALUES (?,?,?,?,?,?,?,?)"
    (MDRcv, toCMEventTag chatMsgEvent, msgBody, currentTs, currentTs, connId_, groupId_, sharedMsgId_)
  msgId <- insertedRowId db
  DB.execute
    db
    "INSERT INTO msg_deliveries (message_id, connection_id, agent_msg_id, agent_msg_meta, agent_ack_cmd_id, chat_ts, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?)"
    (msgId, connId, agentMsgId, msgMetaJson agentMsgMeta, agentAckCmdId, snd $ broker agentMsgMeta, currentTs, currentTs)
  msgDeliveryId <- insertedRowId db
  createMsgDeliveryEvent_ db msgDeliveryId MDSRcvAgent currentTs
  pure RcvMessage {msgId, chatMsgEvent = ACME (encoding @e) chatMsgEvent, sharedMsgId_, msgBody}
  where
    (connId_, groupId_) = case connOrGroupId of
      ConnectionId connId' -> (Just connId', Nothing)
      GroupId groupId -> (Nothing, Just groupId)

createSndMsgDeliveryEvent :: DB.Connection -> Int64 -> AgentMsgId -> MsgDeliveryStatus 'MDSnd -> ExceptT StoreError IO ()
createSndMsgDeliveryEvent db connId agentMsgId sndMsgDeliveryStatus = do
  msgDeliveryId <- getMsgDeliveryId_ db connId agentMsgId
  liftIO $ do
    currentTs <- getCurrentTime
    createMsgDeliveryEvent_ db msgDeliveryId sndMsgDeliveryStatus currentTs

createRcvMsgDeliveryEvent :: DB.Connection -> Int64 -> CommandId -> MsgDeliveryStatus 'MDRcv -> IO ()
createRcvMsgDeliveryEvent db connId cmdId rcvMsgDeliveryStatus = do
  msgDeliveryId <- getMsgDeliveryIdByCmdId_ db connId cmdId
  forM_ msgDeliveryId $ \mdId -> do
    currentTs <- getCurrentTime
    createMsgDeliveryEvent_ db mdId rcvMsgDeliveryStatus currentTs

createSndMsgDelivery_ :: DB.Connection -> SndMsgDelivery -> MessageId -> UTCTime -> IO Int64
createSndMsgDelivery_ db SndMsgDelivery {connId, agentMsgId} messageId createdAt = do
  DB.execute
    db
    [sql|
      INSERT INTO msg_deliveries
        (message_id, connection_id, agent_msg_id, agent_msg_meta, chat_ts, created_at, updated_at)
      VALUES (?,?,?,NULL,?,?,?)
    |]
    (messageId, connId, agentMsgId, createdAt, createdAt, createdAt)
  insertedRowId db

createMsgDeliveryEvent_ :: DB.Connection -> Int64 -> MsgDeliveryStatus d -> UTCTime -> IO ()
createMsgDeliveryEvent_ db msgDeliveryId msgDeliveryStatus createdAt = do
  DB.execute
    db
    [sql|
      INSERT INTO msg_delivery_events
        (msg_delivery_id, delivery_status, created_at, updated_at)
      VALUES (?,?,?,?)
    |]
    (msgDeliveryId, msgDeliveryStatus, createdAt, createdAt)

getMsgDeliveryId_ :: DB.Connection -> Int64 -> AgentMsgId -> ExceptT StoreError IO Int64
getMsgDeliveryId_ db connId agentMsgId =
  ExceptT . firstRow fromOnly (SENoMsgDelivery connId agentMsgId) $
    DB.query
      db
      [sql|
        SELECT msg_delivery_id
        FROM msg_deliveries m
        WHERE m.connection_id = ? AND m.agent_msg_id = ?
        LIMIT 1
      |]
      (connId, agentMsgId)

getMsgDeliveryIdByCmdId_ :: DB.Connection -> Int64 -> CommandId -> IO (Maybe AgentMsgId)
getMsgDeliveryIdByCmdId_ db connId cmdId =
  maybeFirstRow fromOnly $
    DB.query
      db
      [sql|
        SELECT msg_delivery_id
        FROM msg_deliveries
        WHERE connection_id = ? AND agent_ack_cmd_id = ?
        LIMIT 1
      |]
      (connId, cmdId)

createPendingGroupMessage :: DB.Connection -> Int64 -> MessageId -> Maybe Int64 -> IO ()
createPendingGroupMessage db groupMemberId messageId introId_ = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      INSERT INTO pending_group_messages
        (group_member_id, message_id, group_member_intro_id, created_at, updated_at) VALUES (?,?,?,?,?)
    |]
    (groupMemberId, messageId, introId_, currentTs, currentTs)

getPendingGroupMessages :: DB.Connection -> Int64 -> IO [PendingGroupMessage]
getPendingGroupMessages db groupMemberId =
  map pendingGroupMessage
    <$> DB.query
      db
      [sql|
        SELECT pgm.message_id, m.chat_msg_event, m.msg_body, pgm.group_member_intro_id
        FROM pending_group_messages pgm
        JOIN messages m USING (message_id)
        WHERE pgm.group_member_id = ?
        ORDER BY pgm.message_id ASC
      |]
      (Only groupMemberId)
  where
    pendingGroupMessage (msgId, cmEventTag, msgBody, introId_) =
      PendingGroupMessage {msgId, cmEventTag, msgBody, introId_}

deletePendingGroupMessage :: DB.Connection -> Int64 -> MessageId -> IO ()
deletePendingGroupMessage db groupMemberId messageId =
  DB.execute db "DELETE FROM pending_group_messages WHERE group_member_id = ? AND message_id = ?" (groupMemberId, messageId)

deleteOldMessages :: DB.Connection -> UTCTime -> IO ()
deleteOldMessages db createdAtCutoff = do
  DB.execute db "DELETE FROM messages WHERE created_at <= ?" (Only createdAtCutoff)

type NewQuoteRow = (Maybe SharedMsgId, Maybe UTCTime, Maybe MsgContent, Maybe Bool, Maybe MemberId)

updateChatTs :: DB.Connection -> User -> ChatDirection c d -> UTCTime -> IO ()
updateChatTs db User {userId} chatDirection chatTs = case toChatInfo chatDirection of
  DirectChat Contact {contactId} ->
    DB.execute
      db
      "UPDATE contacts SET chat_ts = ? WHERE user_id = ? AND contact_id = ?"
      (chatTs, userId, contactId)
  GroupChat GroupInfo {groupId} ->
    DB.execute
      db
      "UPDATE groups SET chat_ts = ? WHERE user_id = ? AND group_id = ?"
      (chatTs, userId, groupId)
  _ -> pure ()

createNewSndChatItem :: DB.Connection -> User -> ChatDirection c 'MDSnd -> SndMessage -> CIContent 'MDSnd -> Maybe (CIQuote c) -> Maybe CITimed -> Bool -> UTCTime -> IO ChatItemId
createNewSndChatItem db user chatDirection SndMessage {msgId, sharedMsgId} ciContent quotedItem timed live createdAt =
  createNewChatItem_ db user chatDirection createdByMsgId (Just sharedMsgId) ciContent quoteRow timed live createdAt createdAt
  where
    createdByMsgId = if msgId == 0 then Nothing else Just msgId
    quoteRow :: NewQuoteRow
    quoteRow = case quotedItem of
      Nothing -> (Nothing, Nothing, Nothing, Nothing, Nothing)
      Just CIQuote {chatDir, sharedMsgId = quotedSharedMsgId, sentAt, content} ->
        uncurry (quotedSharedMsgId,Just sentAt,Just content,,) $ case chatDir of
          CIQDirectSnd -> (Just True, Nothing)
          CIQDirectRcv -> (Just False, Nothing)
          CIQGroupSnd -> (Just True, Nothing)
          CIQGroupRcv (Just GroupMember {memberId}) -> (Just False, Just memberId)
          CIQGroupRcv Nothing -> (Just False, Nothing)

createNewRcvChatItem :: DB.Connection -> User -> ChatDirection c 'MDRcv -> RcvMessage -> Maybe SharedMsgId -> CIContent 'MDRcv -> Maybe CITimed -> Bool -> UTCTime -> UTCTime -> IO (ChatItemId, Maybe (CIQuote c))
createNewRcvChatItem db user chatDirection RcvMessage {msgId, chatMsgEvent} sharedMsgId_ ciContent timed live itemTs createdAt = do
  ciId <- createNewChatItem_ db user chatDirection (Just msgId) sharedMsgId_ ciContent quoteRow timed live itemTs createdAt
  quotedItem <- mapM (getChatItemQuote_ db user chatDirection) quotedMsg
  pure (ciId, quotedItem)
  where
    quotedMsg = cmToQuotedMsg chatMsgEvent
    quoteRow :: NewQuoteRow
    quoteRow = case quotedMsg of
      Nothing -> (Nothing, Nothing, Nothing, Nothing, Nothing)
      Just QuotedMsg {msgRef = MsgRef {msgId = sharedMsgId, sentAt, sent, memberId}, content} ->
        uncurry (sharedMsgId,Just sentAt,Just content,,) $ case chatDirection of
          CDDirectRcv _ -> (Just $ not sent, Nothing)
          CDGroupRcv GroupInfo {membership = GroupMember {memberId = userMemberId}} _ ->
            (Just $ Just userMemberId == memberId, memberId)

createNewChatItemNoMsg :: forall c d. MsgDirectionI d => DB.Connection -> User -> ChatDirection c d -> CIContent d -> UTCTime -> UTCTime -> IO ChatItemId
createNewChatItemNoMsg db user chatDirection ciContent =
  createNewChatItem_ db user chatDirection Nothing Nothing ciContent quoteRow Nothing False
  where
    quoteRow :: NewQuoteRow
    quoteRow = (Nothing, Nothing, Nothing, Nothing, Nothing)

createNewChatItem_ :: forall c d. MsgDirectionI d => DB.Connection -> User -> ChatDirection c d -> Maybe MessageId -> Maybe SharedMsgId -> CIContent d -> NewQuoteRow -> Maybe CITimed -> Bool -> UTCTime -> UTCTime -> IO ChatItemId
createNewChatItem_ db User {userId} chatDirection msgId_ sharedMsgId ciContent quoteRow timed live itemTs createdAt = do
  DB.execute
    db
    [sql|
      INSERT INTO chat_items (
        -- user and IDs
        user_id, created_by_msg_id, contact_id, group_id, group_member_id,
        -- meta
        item_sent, item_ts, item_content, item_text, item_status, shared_msg_id, created_at, updated_at, item_live, timed_ttl, timed_delete_at,
        -- quote
        quoted_shared_msg_id, quoted_sent_at, quoted_content, quoted_sent, quoted_member_id
      ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
    |]
    ((userId, msgId_) :. idsRow :. itemRow :. quoteRow)
  ciId <- insertedRowId db
  forM_ msgId_ $ \msgId -> insertChatItemMessage_ db ciId msgId createdAt
  pure ciId
  where
    itemRow :: (SMsgDirection d, UTCTime, CIContent d, Text, CIStatus d, Maybe SharedMsgId) :. (UTCTime, UTCTime, Maybe Bool) :. (Maybe Int, Maybe UTCTime)
    itemRow = (msgDirection @d, itemTs, ciContent, ciContentToText ciContent, ciCreateStatus ciContent, sharedMsgId) :. (createdAt, createdAt, justTrue live) :. ciTimedRow timed
    idsRow :: (Maybe Int64, Maybe Int64, Maybe Int64)
    idsRow = case chatDirection of
      CDDirectRcv Contact {contactId} -> (Just contactId, Nothing, Nothing)
      CDDirectSnd Contact {contactId} -> (Just contactId, Nothing, Nothing)
      CDGroupRcv GroupInfo {groupId} GroupMember {groupMemberId} -> (Nothing, Just groupId, Just groupMemberId)
      CDGroupSnd GroupInfo {groupId} -> (Nothing, Just groupId, Nothing)

ciTimedRow :: Maybe CITimed -> (Maybe Int, Maybe UTCTime)
ciTimedRow (Just CITimed {ttl, deleteAt}) = (Just ttl, deleteAt)
ciTimedRow _ = (Nothing, Nothing)

insertChatItemMessage_ :: DB.Connection -> ChatItemId -> MessageId -> UTCTime -> IO ()
insertChatItemMessage_ db ciId msgId ts = DB.execute db "INSERT INTO chat_item_messages (chat_item_id, message_id, created_at, updated_at) VALUES (?,?,?,?)" (ciId, msgId, ts, ts)

getChatItemQuote_ :: DB.Connection -> User -> ChatDirection c 'MDRcv -> QuotedMsg -> IO (CIQuote c)
getChatItemQuote_ db User {userId, userContactId} chatDirection QuotedMsg {msgRef = MsgRef {msgId, sentAt, sent, memberId}, content} =
  case chatDirection of
    CDDirectRcv Contact {contactId} -> getDirectChatItemQuote_ contactId (not sent)
    CDGroupRcv GroupInfo {groupId, membership = GroupMember {memberId = userMemberId}} sender@GroupMember {memberId = senderMemberId} ->
      case memberId of
        Just mId
          | mId == userMemberId -> (`ciQuote` CIQGroupSnd) <$> getUserGroupChatItemId_ groupId
          | mId == senderMemberId -> (`ciQuote` CIQGroupRcv (Just sender)) <$> getGroupChatItemId_ groupId mId
          | otherwise -> getGroupChatItemQuote_ groupId mId
        _ -> pure . ciQuote Nothing $ CIQGroupRcv Nothing
  where
    ciQuote :: Maybe ChatItemId -> CIQDirection c -> CIQuote c
    ciQuote itemId dir = CIQuote dir itemId msgId sentAt content . parseMaybeMarkdownList $ msgContentText content
    getDirectChatItemQuote_ :: Int64 -> Bool -> IO (CIQuote 'CTDirect)
    getDirectChatItemQuote_ contactId userSent = do
      fmap ciQuoteDirect . maybeFirstRow fromOnly $
        DB.query
          db
          "SELECT chat_item_id FROM chat_items WHERE user_id = ? AND contact_id = ? AND shared_msg_id = ? AND item_sent = ?"
          (userId, contactId, msgId, userSent)
      where
        ciQuoteDirect :: Maybe ChatItemId -> CIQuote 'CTDirect
        ciQuoteDirect = (`ciQuote` if userSent then CIQDirectSnd else CIQDirectRcv)
    getUserGroupChatItemId_ :: Int64 -> IO (Maybe ChatItemId)
    getUserGroupChatItemId_ groupId =
      maybeFirstRow fromOnly $
        DB.query
          db
          "SELECT chat_item_id FROM chat_items WHERE user_id = ? AND group_id = ? AND shared_msg_id = ? AND item_sent = ? AND group_member_id IS NULL"
          (userId, groupId, msgId, MDSnd)
    getGroupChatItemId_ :: Int64 -> MemberId -> IO (Maybe ChatItemId)
    getGroupChatItemId_ groupId mId =
      maybeFirstRow fromOnly $
        DB.query
          db
          "SELECT chat_item_id FROM chat_items WHERE user_id = ? AND group_id = ? AND shared_msg_id = ? AND item_sent = ? AND group_member_id = ?"
          (userId, groupId, msgId, MDRcv, mId)
    getGroupChatItemQuote_ :: Int64 -> MemberId -> IO (CIQuote 'CTGroup)
    getGroupChatItemQuote_ groupId mId = do
      ciQuoteGroup
        <$> DB.queryNamed
          db
          [sql|
            SELECT i.chat_item_id, 
              -- GroupMember
              m.group_member_id, m.group_id, m.member_id, m.member_role, m.member_category,
              m.member_status, m.invited_by, m.local_display_name, m.contact_id, m.contact_profile_id, p.contact_profile_id,
              p.display_name, p.full_name, p.image, p.contact_link, p.local_alias, p.preferences
            FROM group_members m
            JOIN contact_profiles p ON p.contact_profile_id = COALESCE(m.member_profile_id, m.contact_profile_id)
            LEFT JOIN contacts c ON m.contact_id = c.contact_id
            LEFT JOIN chat_items i ON i.group_id = m.group_id
                                      AND m.group_member_id = i.group_member_id
                                      AND i.shared_msg_id = :msg_id
            WHERE m.user_id = :user_id AND m.group_id = :group_id AND m.member_id = :member_id
          |]
          [":user_id" := userId, ":group_id" := groupId, ":member_id" := mId, ":msg_id" := msgId]
      where
        ciQuoteGroup :: [Only (Maybe ChatItemId) :. GroupMemberRow] -> CIQuote 'CTGroup
        ciQuoteGroup [] = ciQuote Nothing $ CIQGroupRcv Nothing
        ciQuoteGroup ((Only itemId :. memberRow) : _) = ciQuote itemId . CIQGroupRcv . Just $ toGroupMember userContactId memberRow

getChatPreviews :: DB.Connection -> User -> Bool -> IO [AChat]
getChatPreviews db user withPCC = do
  directChats <- getDirectChatPreviews_ db user
  groupChats <- getGroupChatPreviews_ db user
  cReqChats <- getContactRequestChatPreviews_ db user
  connChats <- getContactConnectionChatPreviews_ db user withPCC
  pure $ sortOn (Down . ts) (directChats <> groupChats <> cReqChats <> connChats)
  where
    ts :: AChat -> UTCTime
    ts (AChat _ Chat {chatInfo, chatItems}) = case chatInfoChatTs chatInfo of
      Just chatTs -> chatTs
      Nothing -> case chatItems of
        ci : _ -> max (chatItemTs ci) (chatInfoUpdatedAt chatInfo)
        _ -> chatInfoUpdatedAt chatInfo

getDirectChatPreviews_ :: DB.Connection -> User -> IO [AChat]
getDirectChatPreviews_ db user@User {userId} = do
  currentTs <- getCurrentTime
  map (toDirectChatPreview currentTs)
    <$> DB.query
      db
      [sql|
        SELECT
          -- Contact
          ct.contact_id, ct.contact_profile_id, ct.local_display_name, ct.via_group, cp.display_name, cp.full_name, cp.image, cp.contact_link, cp.local_alias, ct.contact_used, ct.enable_ntfs, ct.send_rcpts, ct.favorite,
          cp.preferences, ct.user_preferences, ct.created_at, ct.updated_at, ct.chat_ts, ct.contact_group_member_id, ct.contact_grp_inv_sent,
          -- Connection
          c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id, c.conn_status, c.conn_type, c.local_alias,
          c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at, c.auth_err_counter,
          c.peer_chat_min_version, c.peer_chat_max_version,
          -- ChatStats
          COALESCE(ChatStats.UnreadCount, 0), COALESCE(ChatStats.MinUnread, 0), ct.unread_chat,
          -- ChatItem
          i.chat_item_id, i.item_ts, i.item_sent, i.item_content, i.item_text, i.item_status, i.shared_msg_id, i.item_deleted, i.item_deleted_ts, i.item_edited, i.created_at, i.updated_at, i.timed_ttl, i.timed_delete_at, i.item_live,
          -- CIFile
          f.file_id, f.file_name, f.file_size, f.file_path, f.file_crypto_key, f.file_crypto_nonce, f.ci_file_status, f.protocol,
          -- DirectQuote
          ri.chat_item_id, i.quoted_shared_msg_id, i.quoted_sent_at, i.quoted_content, i.quoted_sent
        FROM contacts ct
        JOIN contact_profiles cp ON ct.contact_profile_id = cp.contact_profile_id
        JOIN connections c ON c.contact_id = ct.contact_id
        LEFT JOIN (
          SELECT contact_id, MAX(chat_item_id) AS MaxId
          FROM chat_items
          GROUP BY contact_id
        ) MaxIds ON MaxIds.contact_id = ct.contact_id
        LEFT JOIN chat_items i ON i.contact_id = MaxIds.contact_id
                               AND i.chat_item_id = MaxIds.MaxId
        LEFT JOIN files f ON f.chat_item_id = i.chat_item_id
        LEFT JOIN (
          SELECT contact_id, COUNT(1) AS UnreadCount, MIN(chat_item_id) AS MinUnread
          FROM chat_items
          WHERE item_status = ?
          GROUP BY contact_id
        ) ChatStats ON ChatStats.contact_id = ct.contact_id
        LEFT JOIN chat_items ri ON ri.user_id = i.user_id AND ri.contact_id = i.contact_id AND ri.shared_msg_id = i.quoted_shared_msg_id
        WHERE ct.user_id = ?
          AND ((c.conn_level = 0 AND c.via_group_link = 0) OR ct.contact_used = 1)
          AND ct.deleted = 0
          AND c.connection_id = (
            SELECT cc_connection_id FROM (
              SELECT
                cc.connection_id AS cc_connection_id,
                (CASE WHEN cc.conn_status = ? OR cc.conn_status = ? THEN 1 ELSE 0 END) AS cc_conn_status_ord
              FROM connections cc
              WHERE cc.user_id = ct.user_id AND cc.contact_id = ct.contact_id
              ORDER BY cc_conn_status_ord DESC, cc_connection_id DESC
              LIMIT 1
            )
          )
        ORDER BY i.item_ts DESC
      |]
      (CISRcvNew, userId, ConnReady, ConnSndReady)
  where
    toDirectChatPreview :: UTCTime -> ContactRow :. ConnectionRow :. ChatStatsRow :. MaybeChatItemRow :. QuoteRow -> AChat
    toDirectChatPreview currentTs (contactRow :. connRow :. statsRow :. ciRow_) =
      let contact = toContact user $ contactRow :. connRow
          ci_ = toDirectChatItemList currentTs ciRow_
          stats = toChatStats statsRow
       in AChat SCTDirect $ Chat (DirectChat contact) ci_ stats

getGroupChatPreviews_ :: DB.Connection -> User -> IO [AChat]
getGroupChatPreviews_ db User {userId, userContactId} = do
  currentTs <- getCurrentTime
  map (toGroupChatPreview currentTs)
    <$> DB.query
      db
      [sql|
        SELECT
          -- GroupInfo
          g.group_id, g.local_display_name, gp.display_name, gp.full_name, gp.description, gp.image, g.host_conn_custom_user_profile_id, g.enable_ntfs, g.send_rcpts, g.favorite, gp.preferences, g.created_at, g.updated_at, g.chat_ts,
          -- GroupMember - membership
          mu.group_member_id, mu.group_id, mu.member_id, mu.member_role, mu.member_category,
          mu.member_status, mu.invited_by, mu.local_display_name, mu.contact_id, mu.contact_profile_id, pu.contact_profile_id,
          pu.display_name, pu.full_name, pu.image, pu.contact_link, pu.local_alias, pu.preferences,
          -- ChatStats
          COALESCE(ChatStats.UnreadCount, 0), COALESCE(ChatStats.MinUnread, 0), g.unread_chat,
          -- ChatItem
          i.chat_item_id, i.item_ts, i.item_sent, i.item_content, i.item_text, i.item_status, i.shared_msg_id, i.item_deleted, i.item_deleted_ts, i.item_edited, i.created_at, i.updated_at, i.timed_ttl, i.timed_delete_at, i.item_live,
          -- CIFile
          f.file_id, f.file_name, f.file_size, f.file_path, f.file_crypto_key, f.file_crypto_nonce, f.ci_file_status, f.protocol,
          -- Maybe GroupMember - sender
          m.group_member_id, m.group_id, m.member_id, m.member_role, m.member_category,
          m.member_status, m.invited_by, m.local_display_name, m.contact_id, m.contact_profile_id, p.contact_profile_id,
          p.display_name, p.full_name, p.image, p.contact_link, p.local_alias, p.preferences,
          -- quoted ChatItem
          ri.chat_item_id, i.quoted_shared_msg_id, i.quoted_sent_at, i.quoted_content, i.quoted_sent,
          -- quoted GroupMember
          rm.group_member_id, rm.group_id, rm.member_id, rm.member_role, rm.member_category,
          rm.member_status, rm.invited_by, rm.local_display_name, rm.contact_id, rm.contact_profile_id, rp.contact_profile_id,
          rp.display_name, rp.full_name, rp.image, rp.contact_link, rp.local_alias, rp.preferences,
          -- deleted by GroupMember
          dbm.group_member_id, dbm.group_id, dbm.member_id, dbm.member_role, dbm.member_category,
          dbm.member_status, dbm.invited_by, dbm.local_display_name, dbm.contact_id, dbm.contact_profile_id, dbp.contact_profile_id,
          dbp.display_name, dbp.full_name, dbp.image, dbp.contact_link, dbp.local_alias, dbp.preferences
        FROM groups g
        JOIN group_profiles gp ON gp.group_profile_id = g.group_profile_id
        JOIN group_members mu ON mu.group_id = g.group_id
        JOIN contact_profiles pu ON pu.contact_profile_id = COALESCE(mu.member_profile_id, mu.contact_profile_id)
        LEFT JOIN (
          SELECT group_id, MAX(chat_item_id) AS MaxId
          FROM chat_items
          GROUP BY group_id
        ) MaxIds ON MaxIds.group_id = g.group_id
        LEFT JOIN chat_items i ON i.group_id = MaxIds.group_id
                               AND i.chat_item_id = MaxIds.MaxId
        LEFT JOIN files f ON f.chat_item_id = i.chat_item_id
        LEFT JOIN (
          SELECT group_id, COUNT(1) AS UnreadCount, MIN(chat_item_id) AS MinUnread
          FROM chat_items
          WHERE item_status = ?
          GROUP BY group_id
        ) ChatStats ON ChatStats.group_id = g.group_id
        LEFT JOIN group_members m ON m.group_member_id = i.group_member_id
        LEFT JOIN contact_profiles p ON p.contact_profile_id = COALESCE(m.member_profile_id, m.contact_profile_id)
        LEFT JOIN chat_items ri ON ri.shared_msg_id = i.quoted_shared_msg_id AND ri.group_id = i.group_id
        LEFT JOIN group_members rm ON rm.group_member_id = ri.group_member_id
        LEFT JOIN contact_profiles rp ON rp.contact_profile_id = COALESCE(rm.member_profile_id, rm.contact_profile_id)
        LEFT JOIN group_members dbm ON dbm.group_member_id = i.item_deleted_by_group_member_id
        LEFT JOIN contact_profiles dbp ON dbp.contact_profile_id = COALESCE(dbm.member_profile_id, dbm.contact_profile_id)
        WHERE g.user_id = ? AND mu.contact_id = ?
        ORDER BY i.item_ts DESC
      |]
      (CISRcvNew, userId, userContactId)
  where
    toGroupChatPreview :: UTCTime -> GroupInfoRow :. ChatStatsRow :. MaybeGroupChatItemRow -> AChat
    toGroupChatPreview currentTs (groupInfoRow :. statsRow :. ciRow_) =
      let groupInfo = toGroupInfo userContactId groupInfoRow
          ci_ = toGroupChatItemList currentTs userContactId ciRow_
          stats = toChatStats statsRow
       in AChat SCTGroup $ Chat (GroupChat groupInfo) ci_ stats

getContactRequestChatPreviews_ :: DB.Connection -> User -> IO [AChat]
getContactRequestChatPreviews_ db User {userId} =
  map toContactRequestChatPreview
    <$> DB.query
      db
      [sql|
        SELECT
          cr.contact_request_id, cr.local_display_name, cr.agent_invitation_id, cr.user_contact_link_id,
          c.agent_conn_id, cr.contact_profile_id, p.display_name, p.full_name, p.image, p.contact_link, cr.xcontact_id, p.preferences, cr.created_at, cr.updated_at,
          cr.peer_chat_min_version, cr.peer_chat_max_version
        FROM contact_requests cr
        JOIN connections c ON c.user_contact_link_id = cr.user_contact_link_id
        JOIN contact_profiles p ON p.contact_profile_id = cr.contact_profile_id
        JOIN user_contact_links uc ON uc.user_contact_link_id = cr.user_contact_link_id
        WHERE cr.user_id = ? AND uc.user_id = ? AND uc.local_display_name = '' AND uc.group_id IS NULL
      |]
      (userId, userId)
  where
    toContactRequestChatPreview :: ContactRequestRow -> AChat
    toContactRequestChatPreview cReqRow =
      let cReq = toContactRequest cReqRow
          stats = ChatStats {unreadCount = 0, minUnreadItemId = 0, unreadChat = False}
       in AChat SCTContactRequest $ Chat (ContactRequest cReq) [] stats

getContactConnectionChatPreviews_ :: DB.Connection -> User -> Bool -> IO [AChat]
getContactConnectionChatPreviews_ _ _ False = pure []
getContactConnectionChatPreviews_ db User {userId} _ =
  map toContactConnectionChatPreview
    <$> DB.query
      db
      [sql|
        SELECT connection_id, agent_conn_id, conn_status, via_contact_uri_hash, via_user_contact_link, group_link_id, custom_user_profile_id, conn_req_inv, local_alias, created_at, updated_at
        FROM connections
        WHERE user_id = ? AND conn_type = ? AND contact_id IS NULL AND conn_level = 0 AND via_contact IS NULL AND (via_group_link = 0 || (via_group_link = 1 AND group_link_id IS NOT NULL))
      |]
      (userId, ConnContact)
  where
    toContactConnectionChatPreview :: (Int64, ConnId, ConnStatus, Maybe ByteString, Maybe Int64, Maybe GroupLinkId, Maybe Int64, Maybe ConnReqInvitation, LocalAlias, UTCTime, UTCTime) -> AChat
    toContactConnectionChatPreview connRow =
      let conn = toPendingContactConnection connRow
          stats = ChatStats {unreadCount = 0, minUnreadItemId = 0, unreadChat = False}
       in AChat SCTContactConnection $ Chat (ContactConnection conn) [] stats

getDirectChat :: DB.Connection -> User -> Int64 -> ChatPagination -> Maybe String -> ExceptT StoreError IO (Chat 'CTDirect)
getDirectChat db user contactId pagination search_ = do
  let search = fromMaybe "" search_
  ct <- getContact db user contactId
  liftIO . getDirectChatReactions_ db ct =<< case pagination of
    CPLast count -> getDirectChatLast_ db user ct count search
    CPAfter afterId count -> getDirectChatAfter_ db user ct afterId count search
    CPBefore beforeId count -> getDirectChatBefore_ db user ct beforeId count search

getDirectChatLast_ :: DB.Connection -> User -> Contact -> Int -> String -> ExceptT StoreError IO (Chat 'CTDirect)
getDirectChatLast_ db user ct@Contact {contactId} count search = do
  let stats = ChatStats {unreadCount = 0, minUnreadItemId = 0, unreadChat = False}
  chatItems <- getDirectChatItemsLast db user contactId count search
  pure $ Chat (DirectChat ct) (reverse chatItems) stats

-- the last items in reverse order (the last item in the conversation is the first in the returned list)
getDirectChatItemsLast :: DB.Connection -> User -> ContactId -> Int -> String -> ExceptT StoreError IO [CChatItem 'CTDirect]
getDirectChatItemsLast db User {userId} contactId count search = ExceptT $ do
  currentTs <- getCurrentTime
  mapM (toDirectChatItem currentTs)
    <$> DB.query
      db
      [sql|
        SELECT
          -- ChatItem
          i.chat_item_id, i.item_ts, i.item_sent, i.item_content, i.item_text, i.item_status, i.shared_msg_id, i.item_deleted, i.item_deleted_ts, i.item_edited, i.created_at, i.updated_at, i.timed_ttl, i.timed_delete_at, i.item_live,
          -- CIFile
          f.file_id, f.file_name, f.file_size, f.file_path, f.file_crypto_key, f.file_crypto_nonce, f.ci_file_status, f.protocol,
          -- DirectQuote
          ri.chat_item_id, i.quoted_shared_msg_id, i.quoted_sent_at, i.quoted_content, i.quoted_sent
        FROM chat_items i
        LEFT JOIN files f ON f.chat_item_id = i.chat_item_id
        LEFT JOIN chat_items ri ON ri.user_id = i.user_id AND ri.contact_id = i.contact_id AND ri.shared_msg_id = i.quoted_shared_msg_id
        WHERE i.user_id = ? AND i.contact_id = ? AND i.item_text LIKE '%' || ? || '%'
        ORDER BY i.chat_item_id DESC
        LIMIT ?
      |]
      (userId, contactId, search, count)

getDirectChatAfter_ :: DB.Connection -> User -> Contact -> ChatItemId -> Int -> String -> ExceptT StoreError IO (Chat 'CTDirect)
getDirectChatAfter_ db User {userId} ct@Contact {contactId} afterChatItemId count search = do
  let stats = ChatStats {unreadCount = 0, minUnreadItemId = 0, unreadChat = False}
  chatItems <- ExceptT getDirectChatItemsAfter_
  pure $ Chat (DirectChat ct) chatItems stats
  where
    getDirectChatItemsAfter_ :: IO (Either StoreError [CChatItem 'CTDirect])
    getDirectChatItemsAfter_ = do
      currentTs <- getCurrentTime
      mapM (toDirectChatItem currentTs)
        <$> DB.query
          db
          [sql|
            SELECT
              -- ChatItem
              i.chat_item_id, i.item_ts, i.item_sent, i.item_content, i.item_text, i.item_status, i.shared_msg_id, i.item_deleted, i.item_deleted_ts, i.item_edited, i.created_at, i.updated_at, i.timed_ttl, i.timed_delete_at, i.item_live,
              -- CIFile
              f.file_id, f.file_name, f.file_size, f.file_path, f.file_crypto_key, f.file_crypto_nonce, f.ci_file_status, f.protocol,
              -- DirectQuote
              ri.chat_item_id, i.quoted_shared_msg_id, i.quoted_sent_at, i.quoted_content, i.quoted_sent
            FROM chat_items i
            LEFT JOIN files f ON f.chat_item_id = i.chat_item_id
            LEFT JOIN chat_items ri ON ri.user_id = i.user_id AND ri.contact_id = i.contact_id AND ri.shared_msg_id = i.quoted_shared_msg_id
            WHERE i.user_id = ? AND i.contact_id = ? AND i.item_text LIKE '%' || ? || '%'
              AND i.chat_item_id > ?
            ORDER BY i.chat_item_id ASC
            LIMIT ?
          |]
          (userId, contactId, search, afterChatItemId, count)

getDirectChatBefore_ :: DB.Connection -> User -> Contact -> ChatItemId -> Int -> String -> ExceptT StoreError IO (Chat 'CTDirect)
getDirectChatBefore_ db User {userId} ct@Contact {contactId} beforeChatItemId count search = do
  let stats = ChatStats {unreadCount = 0, minUnreadItemId = 0, unreadChat = False}
  chatItems <- ExceptT getDirectChatItemsBefore_
  pure $ Chat (DirectChat ct) (reverse chatItems) stats
  where
    getDirectChatItemsBefore_ :: IO (Either StoreError [CChatItem 'CTDirect])
    getDirectChatItemsBefore_ = do
      currentTs <- getCurrentTime
      mapM (toDirectChatItem currentTs)
        <$> DB.query
          db
          [sql|
            SELECT
              -- ChatItem
              i.chat_item_id, i.item_ts, i.item_sent, i.item_content, i.item_text, i.item_status, i.shared_msg_id, i.item_deleted, i.item_deleted_ts, i.item_edited, i.created_at, i.updated_at, i.timed_ttl, i.timed_delete_at, i.item_live,
              -- CIFile
              f.file_id, f.file_name, f.file_size, f.file_path, f.file_crypto_key, f.file_crypto_nonce, f.ci_file_status, f.protocol,
              -- DirectQuote
              ri.chat_item_id, i.quoted_shared_msg_id, i.quoted_sent_at, i.quoted_content, i.quoted_sent
            FROM chat_items i
            LEFT JOIN files f ON f.chat_item_id = i.chat_item_id
            LEFT JOIN chat_items ri ON ri.user_id = i.user_id AND ri.contact_id = i.contact_id AND ri.shared_msg_id = i.quoted_shared_msg_id
            WHERE i.user_id = ? AND i.contact_id = ? AND i.item_text LIKE '%' || ? || '%'
              AND i.chat_item_id < ?
            ORDER BY i.chat_item_id DESC
            LIMIT ?
          |]
          (userId, contactId, search, beforeChatItemId, count)

getGroupChat :: DB.Connection -> User -> Int64 -> ChatPagination -> Maybe String -> ExceptT StoreError IO (Chat 'CTGroup)
getGroupChat db user groupId pagination search_ = do
  let search = fromMaybe "" search_
  g <- getGroupInfo db user groupId
  liftIO . getGroupChatReactions_ db g =<< case pagination of
    CPLast count -> getGroupChatLast_ db user g count search
    CPAfter afterId count -> getGroupChatAfter_ db user g afterId count search
    CPBefore beforeId count -> getGroupChatBefore_ db user g beforeId count search

getGroupChatLast_ :: DB.Connection -> User -> GroupInfo -> Int -> String -> ExceptT StoreError IO (Chat 'CTGroup)
getGroupChatLast_ db user@User {userId} g@GroupInfo {groupId} count search = do
  let stats = ChatStats {unreadCount = 0, minUnreadItemId = 0, unreadChat = False}
  chatItemIds <- liftIO getGroupChatItemIdsLast_
  chatItems <- mapM (getGroupChatItem db user groupId) chatItemIds
  pure $ Chat (GroupChat g) (reverse chatItems) stats
  where
    getGroupChatItemIdsLast_ :: IO [ChatItemId]
    getGroupChatItemIdsLast_ =
      map fromOnly
        <$> DB.query
          db
          [sql|
            SELECT chat_item_id
            FROM chat_items
            WHERE user_id = ? AND group_id = ? AND item_text LIKE '%' || ? || '%'
            ORDER BY item_ts DESC, chat_item_id DESC
            LIMIT ?
          |]
          (userId, groupId, search, count)

getGroupMemberChatItemLast :: DB.Connection -> User -> GroupId -> GroupMemberId -> ExceptT StoreError IO (CChatItem 'CTGroup)
getGroupMemberChatItemLast db user@User {userId} groupId groupMemberId = do
  chatItemId <-
    ExceptT . firstRow fromOnly (SEChatItemNotFoundByGroupId groupId) $
      DB.query
        db
        [sql|
          SELECT chat_item_id
          FROM chat_items
          WHERE user_id = ? AND group_id = ? AND group_member_id = ?
          ORDER BY item_ts DESC, chat_item_id DESC
          LIMIT 1
        |]
        (userId, groupId, groupMemberId)
  getGroupChatItem db user groupId chatItemId

getGroupChatAfter_ :: DB.Connection -> User -> GroupInfo -> ChatItemId -> Int -> String -> ExceptT StoreError IO (Chat 'CTGroup)
getGroupChatAfter_ db user@User {userId} g@GroupInfo {groupId} afterChatItemId count search = do
  let stats = ChatStats {unreadCount = 0, minUnreadItemId = 0, unreadChat = False}
  afterChatItem <- getGroupChatItem db user groupId afterChatItemId
  chatItemIds <- liftIO $ getGroupChatItemIdsAfter_ (chatItemTs afterChatItem)
  chatItems <- mapM (getGroupChatItem db user groupId) chatItemIds
  pure $ Chat (GroupChat g) chatItems stats
  where
    getGroupChatItemIdsAfter_ :: UTCTime -> IO [ChatItemId]
    getGroupChatItemIdsAfter_ afterChatItemTs =
      map fromOnly
        <$> DB.query
          db
          [sql|
            SELECT chat_item_id
            FROM chat_items
            WHERE user_id = ? AND group_id = ? AND item_text LIKE '%' || ? || '%'
              AND (item_ts > ? OR (item_ts = ? AND chat_item_id > ?))
            ORDER BY item_ts ASC, chat_item_id ASC
            LIMIT ?
          |]
          (userId, groupId, search, afterChatItemTs, afterChatItemTs, afterChatItemId, count)

getGroupChatBefore_ :: DB.Connection -> User -> GroupInfo -> ChatItemId -> Int -> String -> ExceptT StoreError IO (Chat 'CTGroup)
getGroupChatBefore_ db user@User {userId} g@GroupInfo {groupId} beforeChatItemId count search = do
  let stats = ChatStats {unreadCount = 0, minUnreadItemId = 0, unreadChat = False}
  beforeChatItem <- getGroupChatItem db user groupId beforeChatItemId
  chatItemIds <- liftIO $ getGroupChatItemIdsBefore_ (chatItemTs beforeChatItem)
  chatItems <- mapM (getGroupChatItem db user groupId) chatItemIds
  pure $ Chat (GroupChat g) (reverse chatItems) stats
  where
    getGroupChatItemIdsBefore_ :: UTCTime -> IO [ChatItemId]
    getGroupChatItemIdsBefore_ beforeChatItemTs =
      map fromOnly
        <$> DB.query
          db
          [sql|
            SELECT chat_item_id
            FROM chat_items
            WHERE user_id = ? AND group_id = ? AND item_text LIKE '%' || ? || '%'
              AND (item_ts < ? OR (item_ts = ? AND chat_item_id < ?))
            ORDER BY item_ts DESC, chat_item_id DESC
            LIMIT ?
          |]
          (userId, groupId, search, beforeChatItemTs, beforeChatItemTs, beforeChatItemId, count)

toChatItemRef :: (ChatItemId, Maybe Int64, Maybe Int64) -> Either StoreError (ChatRef, ChatItemId)
toChatItemRef = \case
  (itemId, Just contactId, Nothing) -> Right (ChatRef CTDirect contactId, itemId)
  (itemId, Nothing, Just groupId) -> Right (ChatRef CTGroup groupId, itemId)
  (itemId, _, _) -> Left $ SEBadChatItem itemId

updateDirectChatItemsRead :: DB.Connection -> User -> ContactId -> Maybe (ChatItemId, ChatItemId) -> IO ()
updateDirectChatItemsRead db User {userId} contactId itemsRange_ = do
  currentTs <- getCurrentTime
  case itemsRange_ of
    Just (fromItemId, toItemId) ->
      DB.execute
        db
        [sql|
          UPDATE chat_items SET item_status = ?, updated_at = ?
          WHERE user_id = ? AND contact_id = ? AND chat_item_id >= ? AND chat_item_id <= ? AND item_status = ?
        |]
        (CISRcvRead, currentTs, userId, contactId, fromItemId, toItemId, CISRcvNew)
    _ ->
      DB.execute
        db
        [sql|
          UPDATE chat_items SET item_status = ?, updated_at = ?
          WHERE user_id = ? AND contact_id = ? AND item_status = ?
        |]
        (CISRcvRead, currentTs, userId, contactId, CISRcvNew)

getDirectUnreadTimedItems :: DB.Connection -> User -> ContactId -> Maybe (ChatItemId, ChatItemId) -> IO [(ChatItemId, Int)]
getDirectUnreadTimedItems db User {userId} contactId itemsRange_ = case itemsRange_ of
  Just (fromItemId, toItemId) ->
    DB.query
      db
      [sql|
        SELECT chat_item_id, timed_ttl
        FROM chat_items
        WHERE user_id = ? AND contact_id = ?
          AND chat_item_id >= ? AND chat_item_id <= ?
          AND item_status = ?
          AND timed_ttl IS NOT NULL AND timed_delete_at IS NULL
          AND (item_live IS NULL OR item_live = ?)
      |]
      (userId, contactId, fromItemId, toItemId, CISRcvNew, False)
  _ ->
    DB.query
      db
      [sql|
        SELECT chat_item_id, timed_ttl
        FROM chat_items
        WHERE user_id = ? AND contact_id = ? AND item_status = ? AND timed_ttl IS NOT NULL AND timed_delete_at IS NULL
      |]
      (userId, contactId, CISRcvNew)

setDirectChatItemDeleteAt :: DB.Connection -> User -> ContactId -> ChatItemId -> UTCTime -> IO ()
setDirectChatItemDeleteAt db User {userId} contactId chatItemId deleteAt =
  DB.execute
    db
    "UPDATE chat_items SET timed_delete_at = ? WHERE user_id = ? AND contact_id = ? AND chat_item_id = ?"
    (deleteAt, userId, contactId, chatItemId)

updateGroupChatItemsRead :: DB.Connection -> UserId -> GroupId -> Maybe (ChatItemId, ChatItemId) -> IO ()
updateGroupChatItemsRead db userId groupId itemsRange_ = do
  currentTs <- getCurrentTime
  case itemsRange_ of
    Just (fromItemId, toItemId) ->
      DB.execute
        db
        [sql|
          UPDATE chat_items SET item_status = ?, updated_at = ?
          WHERE user_id = ? AND group_id = ? AND chat_item_id >= ? AND chat_item_id <= ? AND item_status = ?
        |]
        (CISRcvRead, currentTs, userId, groupId, fromItemId, toItemId, CISRcvNew)
    _ ->
      DB.execute
        db
        [sql|
          UPDATE chat_items SET item_status = ?, updated_at = ?
          WHERE user_id = ? AND group_id = ? AND item_status = ?
        |]
        (CISRcvRead, currentTs, userId, groupId, CISRcvNew)

getGroupUnreadTimedItems :: DB.Connection -> User -> GroupId -> Maybe (ChatItemId, ChatItemId) -> IO [(ChatItemId, Int)]
getGroupUnreadTimedItems db User {userId} groupId itemsRange_ = case itemsRange_ of
  Just (fromItemId, toItemId) ->
    DB.query
      db
      [sql|
        SELECT chat_item_id, timed_ttl
        FROM chat_items
        WHERE user_id = ? AND group_id = ?
          AND chat_item_id >= ? AND chat_item_id <= ?
          AND item_status = ?
          AND timed_ttl IS NOT NULL AND timed_delete_at IS NULL
          AND (item_live IS NULL OR item_live = ?)
      |]
      (userId, groupId, fromItemId, toItemId, CISRcvNew, False)
  _ ->
    DB.query
      db
      [sql|
        SELECT chat_item_id, timed_ttl
        FROM chat_items
        WHERE user_id = ? AND group_id = ? AND item_status = ? AND timed_ttl IS NOT NULL AND timed_delete_at IS NULL
      |]
      (userId, groupId, CISRcvNew)

setGroupChatItemDeleteAt :: DB.Connection -> User -> GroupId -> ChatItemId -> UTCTime -> IO ()
setGroupChatItemDeleteAt db User {userId} groupId chatItemId deleteAt =
  DB.execute
    db
    "UPDATE chat_items SET timed_delete_at = ? WHERE user_id = ? AND group_id = ? AND chat_item_id = ?"
    (deleteAt, userId, groupId, chatItemId)

type ChatStatsRow = (Int, ChatItemId, Bool)

toChatStats :: ChatStatsRow -> ChatStats
toChatStats (unreadCount, minUnreadItemId, unreadChat) = ChatStats {unreadCount, minUnreadItemId, unreadChat}

type MaybeCIFIleRow = (Maybe Int64, Maybe String, Maybe Integer, Maybe FilePath, Maybe C.SbKey, Maybe C.CbNonce, Maybe ACIFileStatus, Maybe FileProtocol)

type ChatItemModeRow = (Maybe Int, Maybe UTCTime, Maybe Bool)

type ChatItemRow = (Int64, ChatItemTs, AMsgDirection, Text, Text, ACIStatus, Maybe SharedMsgId) :. (Bool, Maybe UTCTime, Maybe Bool, UTCTime, UTCTime) :. ChatItemModeRow :. MaybeCIFIleRow

type MaybeChatItemRow = (Maybe Int64, Maybe ChatItemTs, Maybe AMsgDirection, Maybe Text, Maybe Text, Maybe ACIStatus, Maybe SharedMsgId) :. (Maybe Bool, Maybe UTCTime, Maybe Bool, Maybe UTCTime, Maybe UTCTime) :. ChatItemModeRow :. MaybeCIFIleRow

type QuoteRow = (Maybe ChatItemId, Maybe SharedMsgId, Maybe UTCTime, Maybe MsgContent, Maybe Bool)

toDirectQuote :: QuoteRow -> Maybe (CIQuote 'CTDirect)
toDirectQuote qr@(_, _, _, _, quotedSent) = toQuote qr $ direction <$> quotedSent
  where
    direction sent = if sent then CIQDirectSnd else CIQDirectRcv

toQuote :: QuoteRow -> Maybe (CIQDirection c) -> Maybe (CIQuote c)
toQuote (quotedItemId, quotedSharedMsgId, quotedSentAt, quotedMsgContent, _) dir =
  CIQuote <$> dir <*> pure quotedItemId <*> pure quotedSharedMsgId <*> quotedSentAt <*> quotedMsgContent <*> (parseMaybeMarkdownList . msgContentText <$> quotedMsgContent)

-- this function can be changed so it never fails, not only avoid failure on invalid json
toDirectChatItem :: UTCTime -> ChatItemRow :. QuoteRow -> Either StoreError (CChatItem 'CTDirect)
toDirectChatItem currentTs (((itemId, itemTs, AMsgDirection msgDir, itemContentText, itemText, itemStatus, sharedMsgId) :. (itemDeleted, deletedTs, itemEdited, createdAt, updatedAt) :. (timedTTL, timedDeleteAt, itemLive) :. (fileId_, fileName_, fileSize_, filePath, fileKey, fileNonce, fileStatus_, fileProtocol_)) :. quoteRow) =
  chatItem $ fromRight invalid $ dbParseACIContent itemContentText
  where
    invalid = ACIContent msgDir $ CIInvalidJSON itemContentText
    chatItem itemContent = case (itemContent, itemStatus, fileStatus_) of
      (ACIContent SMDSnd ciContent, ACIStatus SMDSnd ciStatus, Just (AFS SMDSnd fileStatus)) ->
        Right $ cItem SMDSnd CIDirectSnd ciStatus ciContent (maybeCIFile fileStatus)
      (ACIContent SMDSnd ciContent, ACIStatus SMDSnd ciStatus, Nothing) ->
        Right $ cItem SMDSnd CIDirectSnd ciStatus ciContent Nothing
      (ACIContent SMDRcv ciContent, ACIStatus SMDRcv ciStatus, Just (AFS SMDRcv fileStatus)) ->
        Right $ cItem SMDRcv CIDirectRcv ciStatus ciContent (maybeCIFile fileStatus)
      (ACIContent SMDRcv ciContent, ACIStatus SMDRcv ciStatus, Nothing) ->
        Right $ cItem SMDRcv CIDirectRcv ciStatus ciContent Nothing
      _ -> badItem
    maybeCIFile :: CIFileStatus d -> Maybe (CIFile d)
    maybeCIFile fileStatus =
      case (fileId_, fileName_, fileSize_, fileProtocol_) of
        (Just fileId, Just fileName, Just fileSize, Just fileProtocol) ->
          let cfArgs = CFArgs <$> fileKey <*> fileNonce
              fileSource = (`CryptoFile` cfArgs) <$> filePath
           in Just CIFile {fileId, fileName, fileSize, fileSource, fileStatus, fileProtocol}
        _ -> Nothing
    cItem :: MsgDirectionI d => SMsgDirection d -> CIDirection 'CTDirect d -> CIStatus d -> CIContent d -> Maybe (CIFile d) -> CChatItem 'CTDirect
    cItem d chatDir ciStatus content file =
      CChatItem d ChatItem {chatDir, meta = ciMeta content ciStatus, content, formattedText = parseMaybeMarkdownList itemText, quotedItem = toDirectQuote quoteRow, reactions = [], file}
    badItem = Left $ SEBadChatItem itemId
    ciMeta :: CIContent d -> CIStatus d -> CIMeta 'CTDirect d
    ciMeta content status =
      let itemDeleted' = if itemDeleted then Just (CIDeleted @'CTDirect deletedTs) else Nothing
          itemEdited' = fromMaybe False itemEdited
       in mkCIMeta itemId content itemText status sharedMsgId itemDeleted' itemEdited' ciTimed itemLive currentTs itemTs createdAt updatedAt
    ciTimed :: Maybe CITimed
    ciTimed = timedTTL >>= \ttl -> Just CITimed {ttl, deleteAt = timedDeleteAt}

toDirectChatItemList :: UTCTime -> MaybeChatItemRow :. QuoteRow -> [CChatItem 'CTDirect]
toDirectChatItemList currentTs (((Just itemId, Just itemTs, Just msgDir, Just itemContent, Just itemText, Just itemStatus, sharedMsgId) :. (Just itemDeleted, deletedTs, itemEdited, Just createdAt, Just updatedAt) :. (timedTTL, timedDeleteAt, itemLive) :. fileRow) :. quoteRow) =
  either (const []) (: []) $ toDirectChatItem currentTs (((itemId, itemTs, msgDir, itemContent, itemText, itemStatus, sharedMsgId) :. (itemDeleted, deletedTs, itemEdited, createdAt, updatedAt) :. (timedTTL, timedDeleteAt, itemLive) :. fileRow) :. quoteRow)
toDirectChatItemList _ _ = []

type GroupQuoteRow = QuoteRow :. MaybeGroupMemberRow

type MaybeGroupChatItemRow = MaybeChatItemRow :. MaybeGroupMemberRow :. GroupQuoteRow :. MaybeGroupMemberRow

toGroupQuote :: QuoteRow -> Maybe GroupMember -> Maybe (CIQuote 'CTGroup)
toGroupQuote qr@(_, _, _, _, quotedSent) quotedMember_ = toQuote qr $ direction quotedSent quotedMember_
  where
    direction (Just True) _ = Just CIQGroupSnd
    direction (Just False) (Just member) = Just . CIQGroupRcv $ Just member
    direction (Just False) Nothing = Just $ CIQGroupRcv Nothing
    direction _ _ = Nothing

-- this function can be changed so it never fails, not only avoid failure on invalid json
toGroupChatItem :: UTCTime -> Int64 -> ChatItemRow :. MaybeGroupMemberRow :. GroupQuoteRow :. MaybeGroupMemberRow -> Either StoreError (CChatItem 'CTGroup)
toGroupChatItem currentTs userContactId (((itemId, itemTs, AMsgDirection msgDir, itemContentText, itemText, itemStatus, sharedMsgId) :. (itemDeleted, deletedTs, itemEdited, createdAt, updatedAt) :. (timedTTL, timedDeleteAt, itemLive) :. (fileId_, fileName_, fileSize_, filePath, fileKey, fileNonce, fileStatus_, fileProtocol_)) :. memberRow_ :. (quoteRow :. quotedMemberRow_) :. deletedByGroupMemberRow_) = do
  chatItem $ fromRight invalid $ dbParseACIContent itemContentText
  where
    member_ = toMaybeGroupMember userContactId memberRow_
    quotedMember_ = toMaybeGroupMember userContactId quotedMemberRow_
    deletedByGroupMember_ = toMaybeGroupMember userContactId deletedByGroupMemberRow_
    invalid = ACIContent msgDir $ CIInvalidJSON itemContentText
    chatItem itemContent = case (itemContent, itemStatus, member_, fileStatus_) of
      (ACIContent SMDSnd ciContent, ACIStatus SMDSnd ciStatus, _, Just (AFS SMDSnd fileStatus)) ->
        Right $ cItem SMDSnd CIGroupSnd ciStatus ciContent (maybeCIFile fileStatus)
      (ACIContent SMDSnd ciContent, ACIStatus SMDSnd ciStatus, _, Nothing) ->
        Right $ cItem SMDSnd CIGroupSnd ciStatus ciContent Nothing
      (ACIContent SMDRcv ciContent, ACIStatus SMDRcv ciStatus, Just member, Just (AFS SMDRcv fileStatus)) ->
        Right $ cItem SMDRcv (CIGroupRcv member) ciStatus ciContent (maybeCIFile fileStatus)
      (ACIContent SMDRcv ciContent, ACIStatus SMDRcv ciStatus, Just member, Nothing) ->
        Right $ cItem SMDRcv (CIGroupRcv member) ciStatus ciContent Nothing
      _ -> badItem
    maybeCIFile :: CIFileStatus d -> Maybe (CIFile d)
    maybeCIFile fileStatus =
      case (fileId_, fileName_, fileSize_, fileProtocol_) of
        (Just fileId, Just fileName, Just fileSize, Just fileProtocol) ->
          let cfArgs = CFArgs <$> fileKey <*> fileNonce
              fileSource = (`CryptoFile` cfArgs) <$> filePath
           in Just CIFile {fileId, fileName, fileSize, fileSource, fileStatus, fileProtocol}
        _ -> Nothing
    cItem :: MsgDirectionI d => SMsgDirection d -> CIDirection 'CTGroup d -> CIStatus d -> CIContent d -> Maybe (CIFile d) -> CChatItem 'CTGroup
    cItem d chatDir ciStatus content file =
      CChatItem d ChatItem {chatDir, meta = ciMeta content ciStatus, content, formattedText = parseMaybeMarkdownList itemText, quotedItem = toGroupQuote quoteRow quotedMember_, reactions = [], file}
    badItem = Left $ SEBadChatItem itemId
    ciMeta :: CIContent d -> CIStatus d -> CIMeta 'CTGroup d
    ciMeta content status =
      let itemDeleted' =
            if itemDeleted
              then Just (maybe (CIDeleted @'CTGroup deletedTs) (CIModerated deletedTs) deletedByGroupMember_)
              else Nothing
          itemEdited' = fromMaybe False itemEdited
       in mkCIMeta itemId content itemText status sharedMsgId itemDeleted' itemEdited' ciTimed itemLive currentTs itemTs createdAt updatedAt
    ciTimed :: Maybe CITimed
    ciTimed = timedTTL >>= \ttl -> Just CITimed {ttl, deleteAt = timedDeleteAt}

toGroupChatItemList :: UTCTime -> Int64 -> MaybeGroupChatItemRow -> [CChatItem 'CTGroup]
toGroupChatItemList currentTs userContactId (((Just itemId, Just itemTs, Just msgDir, Just itemContent, Just itemText, Just itemStatus, sharedMsgId) :. (Just itemDeleted, deletedTs, itemEdited, Just createdAt, Just updatedAt) :. (timedTTL, timedDeleteAt, itemLive) :. fileRow) :. memberRow_ :. (quoteRow :. quotedMemberRow_) :. deletedByGroupMemberRow_) =
  either (const []) (: []) $ toGroupChatItem currentTs userContactId (((itemId, itemTs, msgDir, itemContent, itemText, itemStatus, sharedMsgId) :. (itemDeleted, deletedTs, itemEdited, createdAt, updatedAt) :. (timedTTL, timedDeleteAt, itemLive) :. fileRow) :. memberRow_ :. (quoteRow :. quotedMemberRow_) :. deletedByGroupMemberRow_)
toGroupChatItemList _ _ _ = []

getAllChatItems :: DB.Connection -> User -> ChatPagination -> Maybe String -> ExceptT StoreError IO [AChatItem]
getAllChatItems db user@User {userId} pagination search_ = do
  itemRefs <-
    rights . map toChatItemRef <$> case pagination of
      CPLast count -> liftIO $ getAllChatItemsLast_ count
      CPAfter afterId count -> liftIO . getAllChatItemsAfter_ afterId count . aChatItemTs =<< getAChatItem_ afterId
      CPBefore beforeId count -> liftIO . getAllChatItemsBefore_ beforeId count . aChatItemTs =<< getAChatItem_ beforeId
  mapM (uncurry (getAChatItem db user) >=> liftIO . getACIReactions db) itemRefs
  where
    search = fromMaybe "" search_
    getAChatItem_ itemId = do
      chatRef <- getChatRefViaItemId db user itemId
      getAChatItem db user chatRef itemId
    getAllChatItemsLast_ count =
      reverse
        <$> DB.query
          db
          [sql|
            SELECT chat_item_id, contact_id, group_id
            FROM chat_items
            WHERE user_id = ? AND item_text LIKE '%' || ? || '%'
            ORDER BY item_ts DESC, chat_item_id DESC
            LIMIT ?
          |]
          (userId, search, count)
    getAllChatItemsAfter_ afterId count afterTs =
      DB.query
        db
        [sql|
          SELECT chat_item_id, contact_id, group_id
          FROM chat_items
          WHERE user_id = ? AND item_text LIKE '%' || ? || '%'
            AND (item_ts > ? OR (item_ts = ? AND chat_item_id > ?))
          ORDER BY item_ts ASC, chat_item_id ASC
          LIMIT ?
        |]
        (userId, search, afterTs, afterTs, afterId, count)
    getAllChatItemsBefore_ beforeId count beforeTs =
      reverse
        <$> DB.query
          db
          [sql|
            SELECT chat_item_id, contact_id, group_id
            FROM chat_items
            WHERE user_id = ? AND item_text LIKE '%' || ? || '%'
              AND (item_ts < ? OR (item_ts = ? AND chat_item_id < ?))
            ORDER BY item_ts DESC, chat_item_id DESC
            LIMIT ?
          |]
          (userId, search, beforeTs, beforeTs, beforeId, count)

getChatItemIdByAgentMsgId :: DB.Connection -> Int64 -> AgentMsgId -> IO (Maybe ChatItemId)
getChatItemIdByAgentMsgId db connId msgId =
  fmap join . maybeFirstRow fromOnly $
    DB.query
      db
      [sql|
        SELECT chat_item_id
        FROM chat_item_messages
        WHERE message_id = (
          SELECT message_id
          FROM msg_deliveries
          WHERE connection_id = ? AND agent_msg_id = ?
          LIMIT 1
        )
      |]
      (connId, msgId)

updateDirectChatItemStatus :: forall d. MsgDirectionI d => DB.Connection -> User -> Int64 -> ChatItemId -> CIStatus d -> ExceptT StoreError IO (ChatItem 'CTDirect d)
updateDirectChatItemStatus db user@User {userId} contactId itemId itemStatus = do
  ci <- liftEither . correctDir =<< getDirectChatItem db user contactId itemId
  currentTs <- liftIO getCurrentTime
  liftIO $ DB.execute db "UPDATE chat_items SET item_status = ?, updated_at = ? WHERE user_id = ? AND contact_id = ? AND chat_item_id = ?" (itemStatus, currentTs, userId, contactId, itemId)
  pure ci {meta = (meta ci) {itemStatus}}
  where
    correctDir :: CChatItem c -> Either StoreError (ChatItem c d)
    correctDir (CChatItem _ ci) = first SEInternalError $ checkDirection ci

updateDirectChatItem :: forall d. MsgDirectionI d => DB.Connection -> User -> Int64 -> ChatItemId -> CIContent d -> Bool -> Maybe MessageId -> ExceptT StoreError IO (ChatItem 'CTDirect d)
updateDirectChatItem db user contactId itemId newContent live msgId_ = do
  ci <- liftEither . correctDir =<< getDirectChatItem db user contactId itemId
  liftIO $ updateDirectChatItem' db user contactId ci newContent live msgId_
  where
    correctDir :: CChatItem c -> Either StoreError (ChatItem c d)
    correctDir (CChatItem _ ci) = first SEInternalError $ checkDirection ci

updateDirectChatItem' :: forall d. MsgDirectionI d => DB.Connection -> User -> Int64 -> ChatItem 'CTDirect d -> CIContent d -> Bool -> Maybe MessageId -> IO (ChatItem 'CTDirect d)
updateDirectChatItem' db User {userId} contactId ci newContent live msgId_ = do
  currentTs <- liftIO getCurrentTime
  let ci' = updatedChatItem ci newContent live currentTs
  liftIO $ updateDirectChatItem_ db userId contactId ci' msgId_
  pure ci'

updatedChatItem :: ChatItem c d -> CIContent d -> Bool -> UTCTime -> ChatItem c d
updatedChatItem ci@ChatItem {meta = meta@CIMeta {itemStatus, itemEdited, itemTimed, itemLive}} newContent live currentTs =
  let newText = ciContentToText newContent
      edited' = itemEdited || (itemLive /= Just True)
      live' = (live &&) <$> itemLive
      timed' = case (itemStatus, itemTimed, itemLive, live) of
        (CISRcvNew, _, _, _) -> itemTimed
        (_, Just CITimed {ttl, deleteAt = Nothing}, Just True, False) ->
          -- timed item, sent or read, not set for deletion, was live, now not live
          let deleteAt' = addUTCTime (realToFrac ttl) currentTs
           in Just CITimed {ttl, deleteAt = Just deleteAt'}
        _ -> itemTimed
   in ci {content = newContent, meta = meta {itemText = newText, itemEdited = edited', itemTimed = timed', itemLive = live'}, formattedText = parseMaybeMarkdownList newText}

-- this function assumes that direct item with correct chat direction already exists,
-- it should be checked before calling it
updateDirectChatItem_ :: forall d. MsgDirectionI d => DB.Connection -> UserId -> Int64 -> ChatItem 'CTDirect d -> Maybe MessageId -> IO ()
updateDirectChatItem_ db userId contactId ChatItem {meta, content} msgId_ = do
  let CIMeta {itemId, itemText, itemStatus, itemDeleted, itemEdited, itemTimed, itemLive, updatedAt} = meta
      itemDeleted' = isJust itemDeleted
      itemDeletedTs' = itemDeletedTs =<< itemDeleted
  DB.execute
    db
    [sql|
      UPDATE chat_items
      SET item_content = ?, item_text = ?, item_status = ?, item_deleted = ?, item_deleted_ts = ?, item_edited = ?, item_live = ?, updated_at = ?, timed_ttl = ?, timed_delete_at = ?
      WHERE user_id = ? AND contact_id = ? AND chat_item_id = ?
    |]
    ((content, itemText, itemStatus, itemDeleted', itemDeletedTs', itemEdited, itemLive, updatedAt) :. ciTimedRow itemTimed :. (userId, contactId, itemId))
  forM_ msgId_ $ \msgId -> liftIO $ insertChatItemMessage_ db itemId msgId updatedAt

addInitialAndNewCIVersions :: DB.Connection -> ChatItemId -> (UTCTime, MsgContent) -> (UTCTime, MsgContent) -> IO ()
addInitialAndNewCIVersions db itemId (initialTs, initialMC) (newTs, newMC) = do
  versionsCount <- getChatItemVersionsCount db itemId
  when (versionsCount == 0) $
    createChatItemVersion db itemId initialTs initialMC
  createChatItemVersion db itemId newTs newMC

getChatItemVersionsCount :: DB.Connection -> ChatItemId -> IO Int
getChatItemVersionsCount db itemId = do
  count <-
    maybeFirstRow fromOnly $
      DB.query db "SELECT COUNT(1) FROM chat_item_versions WHERE chat_item_id = ?" (Only itemId)
  pure $ fromMaybe 0 count

createChatItemVersion :: DB.Connection -> ChatItemId -> UTCTime -> MsgContent -> IO ()
createChatItemVersion db itemId itemVersionTs msgContent =
  DB.execute
    db
    [sql|
      INSERT INTO chat_item_versions (chat_item_id, msg_content, item_version_ts)
      VALUES (?,?,?)
    |]
    (itemId, toMCText msgContent, itemVersionTs)

deleteDirectChatItem :: DB.Connection -> User -> Contact -> CChatItem 'CTDirect -> IO ()
deleteDirectChatItem db User {userId} Contact {contactId} (CChatItem _ ci) = do
  let itemId = chatItemId' ci
  deleteChatItemMessages_ db itemId
  deleteChatItemVersions_ db itemId
  deleteDirectCIReactions_ db contactId ci
  DB.execute
    db
    [sql|
      DELETE FROM chat_items
      WHERE user_id = ? AND contact_id = ? AND chat_item_id = ?
    |]
    (userId, contactId, itemId)

deleteChatItemMessages_ :: DB.Connection -> ChatItemId -> IO ()
deleteChatItemMessages_ db itemId =
  DB.execute
    db
    [sql|
      DELETE FROM messages
      WHERE message_id IN (
        SELECT message_id
        FROM chat_item_messages
        WHERE chat_item_id = ?
      )
    |]
    (Only itemId)

deleteChatItemVersions_ :: DB.Connection -> ChatItemId -> IO ()
deleteChatItemVersions_ db itemId =
  DB.execute db "DELETE FROM chat_item_versions WHERE chat_item_id = ?" (Only itemId)

markDirectChatItemDeleted :: DB.Connection -> User -> Contact -> CChatItem 'CTDirect -> MessageId -> UTCTime -> IO ()
markDirectChatItemDeleted db User {userId} Contact {contactId} (CChatItem _ ci) msgId deletedTs = do
  currentTs <- liftIO getCurrentTime
  let itemId = chatItemId' ci
  insertChatItemMessage_ db itemId msgId currentTs
  DB.execute
    db
    [sql|
      UPDATE chat_items
      SET item_deleted = 1, item_deleted_ts = ?, updated_at = ?
      WHERE user_id = ? AND contact_id = ? AND chat_item_id = ?
    |]
    (deletedTs, currentTs, userId, contactId, itemId)

getDirectChatItemBySharedMsgId :: DB.Connection -> User -> ContactId -> SharedMsgId -> ExceptT StoreError IO (CChatItem 'CTDirect)
getDirectChatItemBySharedMsgId db user@User {userId} contactId sharedMsgId = do
  itemId <- getDirectChatItemIdBySharedMsgId_ db userId contactId sharedMsgId
  getDirectChatItem db user contactId itemId

getDirectChatItemByAgentMsgId :: DB.Connection -> User -> ContactId -> Int64 -> AgentMsgId -> IO (Maybe (CChatItem 'CTDirect))
getDirectChatItemByAgentMsgId db user contactId connId msgId = do
  itemId_ <- getChatItemIdByAgentMsgId db connId msgId
  maybe (pure Nothing) (fmap eitherToMaybe . runExceptT . getDirectChatItem db user contactId) itemId_

getDirectChatItemIdBySharedMsgId_ :: DB.Connection -> UserId -> Int64 -> SharedMsgId -> ExceptT StoreError IO Int64
getDirectChatItemIdBySharedMsgId_ db userId contactId sharedMsgId =
  ExceptT . firstRow fromOnly (SEChatItemSharedMsgIdNotFound sharedMsgId) $
    DB.query
      db
      [sql|
        SELECT chat_item_id
        FROM chat_items
        WHERE user_id = ? AND contact_id = ? AND shared_msg_id = ?
        ORDER BY chat_item_id DESC
        LIMIT 1
      |]
      (userId, contactId, sharedMsgId)

getDirectChatItem :: DB.Connection -> User -> Int64 -> ChatItemId -> ExceptT StoreError IO (CChatItem 'CTDirect)
getDirectChatItem db User {userId} contactId itemId = ExceptT $ do
  currentTs <- getCurrentTime
  join <$> firstRow (toDirectChatItem currentTs) (SEChatItemNotFound itemId) getItem
  where
    getItem =
      DB.query
        db
        [sql|
          SELECT
            -- ChatItem
            i.chat_item_id, i.item_ts, i.item_sent, i.item_content, i.item_text, i.item_status, i.shared_msg_id, i.item_deleted, i.item_deleted_ts, i.item_edited, i.created_at, i.updated_at, i.timed_ttl, i.timed_delete_at, i.item_live,
            -- CIFile
            f.file_id, f.file_name, f.file_size, f.file_path, f.file_crypto_key, f.file_crypto_nonce, f.ci_file_status, f.protocol,
            -- DirectQuote
            ri.chat_item_id, i.quoted_shared_msg_id, i.quoted_sent_at, i.quoted_content, i.quoted_sent
          FROM chat_items i
          LEFT JOIN files f ON f.chat_item_id = i.chat_item_id
          LEFT JOIN chat_items ri ON ri.user_id = i.user_id AND ri.contact_id = i.contact_id AND ri.shared_msg_id = i.quoted_shared_msg_id
          WHERE i.user_id = ? AND i.contact_id = ? AND i.chat_item_id = ?
        |]
        (userId, contactId, itemId)

getDirectChatItemIdByText :: DB.Connection -> UserId -> Int64 -> SMsgDirection d -> Text -> ExceptT StoreError IO ChatItemId
getDirectChatItemIdByText db userId contactId msgDir quotedMsg =
  ExceptT . firstRow fromOnly (SEChatItemNotFoundByText quotedMsg) $
    DB.query
      db
      [sql|
        SELECT chat_item_id
        FROM chat_items
        WHERE user_id = ? AND contact_id = ? AND item_sent = ? AND item_text LIKE ?
        ORDER BY chat_item_id DESC
        LIMIT 1
      |]
      (userId, contactId, msgDir, quotedMsg <> "%")

getDirectChatItemIdByText' :: DB.Connection -> User -> ContactId -> Text -> ExceptT StoreError IO ChatItemId
getDirectChatItemIdByText' db User {userId} contactId msg =
  ExceptT . firstRow fromOnly (SEChatItemNotFoundByText msg) $
    DB.query
      db
      [sql|
        SELECT chat_item_id
        FROM chat_items
        WHERE user_id = ? AND contact_id = ? AND item_text LIKE ?
        ORDER BY chat_item_id DESC
        LIMIT 1
      |]
      (userId, contactId, msg <> "%")

updateGroupChatItemStatus :: forall d. MsgDirectionI d => DB.Connection -> User -> GroupId -> ChatItemId -> CIStatus d -> ExceptT StoreError IO (ChatItem 'CTGroup d)
updateGroupChatItemStatus db user@User {userId} groupId itemId itemStatus = do
  ci <- liftEither . correctDir =<< getGroupChatItem db user groupId itemId
  currentTs <- liftIO getCurrentTime
  liftIO $ DB.execute db "UPDATE chat_items SET item_status = ?, updated_at = ? WHERE user_id = ? AND group_id = ? AND chat_item_id = ?" (itemStatus, currentTs, userId, groupId, itemId)
  pure ci {meta = (meta ci) {itemStatus}}
  where
    correctDir :: CChatItem c -> Either StoreError (ChatItem c d)
    correctDir (CChatItem _ ci) = first SEInternalError $ checkDirection ci

updateGroupChatItem :: forall d. MsgDirectionI d => DB.Connection -> User -> Int64 -> ChatItem 'CTGroup d -> CIContent d -> Bool -> Maybe MessageId -> IO (ChatItem 'CTGroup d)
updateGroupChatItem db user groupId ci newContent live msgId_ = do
  currentTs <- liftIO getCurrentTime
  let ci' = updatedChatItem ci newContent live currentTs
  liftIO $ updateGroupChatItem_ db user groupId ci' msgId_
  pure ci'

-- this function assumes that the group item with correct chat direction already exists,
-- it should be checked before calling it
updateGroupChatItem_ :: forall d. MsgDirectionI d => DB.Connection -> User -> Int64 -> ChatItem 'CTGroup d -> Maybe MessageId -> IO ()
updateGroupChatItem_ db User {userId} groupId ChatItem {content, meta} msgId_ = do
  let CIMeta {itemId, itemText, itemStatus, itemDeleted, itemEdited, itemTimed, itemLive, updatedAt} = meta
      itemDeleted' = isJust itemDeleted
      itemDeletedTs' = itemDeletedTs =<< itemDeleted
  DB.execute
    db
    [sql|
      UPDATE chat_items
      SET item_content = ?, item_text = ?, item_status = ?, item_deleted = ?, item_deleted_ts = ?, item_edited = ?, item_live = ?, updated_at = ?, timed_ttl = ?, timed_delete_at = ?
      WHERE user_id = ? AND group_id = ? AND chat_item_id = ?
    |]
    ((content, itemText, itemStatus, itemDeleted', itemDeletedTs', itemEdited, itemLive, updatedAt) :. ciTimedRow itemTimed :. (userId, groupId, itemId))
  forM_ msgId_ $ \msgId -> insertChatItemMessage_ db itemId msgId updatedAt

deleteGroupChatItem :: DB.Connection -> User -> GroupInfo -> CChatItem 'CTGroup -> IO ()
deleteGroupChatItem db User {userId} g@GroupInfo {groupId} (CChatItem _ ci) = do
  let itemId = chatItemId' ci
  deleteChatItemMessages_ db itemId
  deleteChatItemVersions_ db itemId
  deleteGroupCIReactions_ db g ci
  DB.execute
    db
    [sql|
      DELETE FROM chat_items
      WHERE user_id = ? AND group_id = ? AND chat_item_id = ?
    |]
    (userId, groupId, itemId)

updateGroupChatItemModerated :: DB.Connection -> User -> GroupInfo -> CChatItem 'CTGroup -> GroupMember -> UTCTime -> IO AChatItem
updateGroupChatItemModerated db User {userId} gInfo@GroupInfo {groupId} (CChatItem msgDir ci) m@GroupMember {groupMemberId} deletedTs = do
  currentTs <- getCurrentTime
  let toContent = msgDirToModeratedContent_ msgDir
      toText = ciModeratedText
      itemId = chatItemId' ci
  deleteChatItemMessages_ db itemId
  deleteChatItemVersions_ db itemId
  liftIO $
    DB.execute
      db
      [sql|
        UPDATE chat_items
        SET item_deleted = 1, item_deleted_ts = ?, item_deleted_by_group_member_id = ?, item_content = ?, item_text = ?, updated_at = ?
        WHERE user_id = ? AND group_id = ? AND chat_item_id = ?
      |]
      (deletedTs, groupMemberId, toContent, toText, currentTs, userId, groupId, itemId)
  pure $ AChatItem SCTGroup msgDir (GroupChat gInfo) (ci {content = toContent, meta = (meta ci) {itemText = toText, itemDeleted = Just (CIModerated (Just currentTs) m), editable = False}, formattedText = Nothing})

markGroupChatItemDeleted :: DB.Connection -> User -> GroupInfo -> CChatItem 'CTGroup -> MessageId -> Maybe GroupMember -> UTCTime -> IO ()
markGroupChatItemDeleted db User {userId} GroupInfo {groupId} (CChatItem _ ci) msgId byGroupMember_ deletedTs = do
  currentTs <- liftIO getCurrentTime
  let itemId = chatItemId' ci
      deletedByGroupMemberId = case byGroupMember_ of
        Just GroupMember {groupMemberId} -> Just groupMemberId
        _ -> Nothing
  insertChatItemMessage_ db itemId msgId currentTs
  DB.execute
    db
    [sql|
      UPDATE chat_items
      SET item_deleted = 1, item_deleted_ts = ?, item_deleted_by_group_member_id = ?, updated_at = ?
      WHERE user_id = ? AND group_id = ? AND chat_item_id = ?
    |]
    (deletedTs, deletedByGroupMemberId, currentTs, userId, groupId, itemId)

getGroupChatItemBySharedMsgId :: DB.Connection -> User -> GroupId -> GroupMemberId -> SharedMsgId -> ExceptT StoreError IO (CChatItem 'CTGroup)
getGroupChatItemBySharedMsgId db user@User {userId} groupId groupMemberId sharedMsgId = do
  itemId <-
    ExceptT . firstRow fromOnly (SEChatItemSharedMsgIdNotFound sharedMsgId) $
      DB.query
        db
        [sql|
          SELECT chat_item_id
          FROM chat_items
          WHERE user_id = ? AND group_id = ? AND group_member_id = ? AND shared_msg_id = ?
          ORDER BY chat_item_id DESC
          LIMIT 1
        |]
        (userId, groupId, groupMemberId, sharedMsgId)
  getGroupChatItem db user groupId itemId

getGroupMemberCIBySharedMsgId :: DB.Connection -> User -> GroupId -> MemberId -> SharedMsgId -> ExceptT StoreError IO (CChatItem 'CTGroup)
getGroupMemberCIBySharedMsgId db user@User {userId} groupId memberId sharedMsgId = do
  itemId <-
    ExceptT . firstRow fromOnly (SEChatItemSharedMsgIdNotFound sharedMsgId) $
      DB.query
        db
        [sql|
          SELECT i.chat_item_id
          FROM chat_items i
          JOIN group_members m ON m.group_id = i.group_id
                              AND ((i.group_member_id IS NULL AND m.member_category = ?)
                                  OR i.group_member_id = m.group_member_id)
          WHERE i.user_id = ? AND i.group_id = ? AND m.member_id = ? AND i.shared_msg_id = ?
          ORDER BY i.chat_item_id DESC
          LIMIT 1
        |]
        (GCUserMember, userId, groupId, memberId, sharedMsgId)
  getGroupChatItem db user groupId itemId

getGroupChatItemByAgentMsgId :: DB.Connection -> User -> GroupId -> Int64 -> AgentMsgId -> IO (Maybe (CChatItem 'CTGroup))
getGroupChatItemByAgentMsgId db user groupId connId msgId = do
  itemId_ <- getChatItemIdByAgentMsgId db connId msgId
  maybe (pure Nothing) (fmap eitherToMaybe . runExceptT . getGroupChatItem db user groupId) itemId_

getGroupChatItem :: DB.Connection -> User -> Int64 -> ChatItemId -> ExceptT StoreError IO (CChatItem 'CTGroup)
getGroupChatItem db User {userId, userContactId} groupId itemId = ExceptT $ do
  currentTs <- getCurrentTime
  join <$> firstRow (toGroupChatItem currentTs userContactId) (SEChatItemNotFound itemId) getItem
  where
    getItem =
      DB.query
        db
        [sql|
          SELECT
            -- ChatItem
            i.chat_item_id, i.item_ts, i.item_sent, i.item_content, i.item_text, i.item_status, i.shared_msg_id, i.item_deleted, i.item_deleted_ts, i.item_edited, i.created_at, i.updated_at, i.timed_ttl, i.timed_delete_at, i.item_live,
            -- CIFile
            f.file_id, f.file_name, f.file_size, f.file_path, f.file_crypto_key, f.file_crypto_nonce, f.ci_file_status, f.protocol,
            -- GroupMember
            m.group_member_id, m.group_id, m.member_id, m.member_role, m.member_category,
            m.member_status, m.invited_by, m.local_display_name, m.contact_id, m.contact_profile_id, p.contact_profile_id,
            p.display_name, p.full_name, p.image, p.contact_link, p.local_alias, p.preferences,
            -- quoted ChatItem
            ri.chat_item_id, i.quoted_shared_msg_id, i.quoted_sent_at, i.quoted_content, i.quoted_sent,
            -- quoted GroupMember
            rm.group_member_id, rm.group_id, rm.member_id, rm.member_role, rm.member_category,
            rm.member_status, rm.invited_by, rm.local_display_name, rm.contact_id, rm.contact_profile_id, rp.contact_profile_id,
            rp.display_name, rp.full_name, rp.image, rp.contact_link, rp.local_alias, rp.preferences,
            -- deleted by GroupMember
            dbm.group_member_id, dbm.group_id, dbm.member_id, dbm.member_role, dbm.member_category,
            dbm.member_status, dbm.invited_by, dbm.local_display_name, dbm.contact_id, dbm.contact_profile_id, dbp.contact_profile_id,
            dbp.display_name, dbp.full_name, dbp.image, dbp.contact_link, dbp.local_alias, dbp.preferences
          FROM chat_items i
          LEFT JOIN files f ON f.chat_item_id = i.chat_item_id
          LEFT JOIN group_members m ON m.group_member_id = i.group_member_id
          LEFT JOIN contact_profiles p ON p.contact_profile_id = COALESCE(m.member_profile_id, m.contact_profile_id)
          LEFT JOIN chat_items ri ON ri.shared_msg_id = i.quoted_shared_msg_id AND ri.group_id = i.group_id
          LEFT JOIN group_members rm ON rm.group_member_id = ri.group_member_id
          LEFT JOIN contact_profiles rp ON rp.contact_profile_id = COALESCE(rm.member_profile_id, rm.contact_profile_id)
          LEFT JOIN group_members dbm ON dbm.group_member_id = i.item_deleted_by_group_member_id
          LEFT JOIN contact_profiles dbp ON dbp.contact_profile_id = COALESCE(dbm.member_profile_id, dbm.contact_profile_id)
          WHERE i.user_id = ? AND i.group_id = ? AND i.chat_item_id = ?
        |]
        (userId, groupId, itemId)

getGroupChatItemIdByText :: DB.Connection -> User -> GroupId -> Maybe ContactName -> Text -> ExceptT StoreError IO ChatItemId
getGroupChatItemIdByText db User {userId, localDisplayName = userName} groupId contactName_ quotedMsg =
  ExceptT . firstRow fromOnly (SEChatItemNotFoundByText quotedMsg) $ case contactName_ of
    Nothing -> anyMemberChatItem_
    Just cName
      | userName == cName -> userChatItem_
      | otherwise -> memberChatItem_ cName
  where
    anyMemberChatItem_ =
      DB.query
        db
        [sql|
          SELECT chat_item_id
          FROM chat_items
          WHERE user_id = ? AND group_id = ? AND item_text like ?
          ORDER BY chat_item_id DESC
          LIMIT 1
        |]
        (userId, groupId, quotedMsg <> "%")
    userChatItem_ =
      DB.query
        db
        [sql|
          SELECT chat_item_id
          FROM chat_items
          WHERE user_id = ? AND group_id = ? AND group_member_id IS NULL AND item_text like ?
          ORDER BY chat_item_id DESC
          LIMIT 1
        |]
        (userId, groupId, quotedMsg <> "%")
    memberChatItem_ cName =
      DB.query
        db
        [sql|
          SELECT i.chat_item_id
          FROM chat_items i
          JOIN group_members m ON m.group_member_id = i.group_member_id
          JOIN contacts c ON c.contact_id = m.contact_id
          WHERE i.user_id = ? AND i.group_id = ? AND c.local_display_name = ? AND i.item_text like ?
          ORDER BY i.chat_item_id DESC
          LIMIT 1
        |]
        (userId, groupId, cName, quotedMsg <> "%")

getGroupChatItemIdByText' :: DB.Connection -> User -> GroupId -> Text -> ExceptT StoreError IO ChatItemId
getGroupChatItemIdByText' db User {userId} groupId msg =
  ExceptT . firstRow fromOnly (SEChatItemNotFoundByText msg) $
    DB.query
      db
      [sql|
        SELECT chat_item_id
        FROM chat_items
        WHERE user_id = ? AND group_id = ? AND item_text like ?
        ORDER BY chat_item_id DESC
        LIMIT 1
      |]
      (userId, groupId, msg <> "%")

getChatItemByFileId :: DB.Connection -> User -> Int64 -> ExceptT StoreError IO AChatItem
getChatItemByFileId db user@User {userId} fileId = do
  (chatRef, itemId) <-
    ExceptT . firstRow' toChatItemRef (SEChatItemNotFoundByFileId fileId) $
      DB.query
        db
        [sql|
            SELECT i.chat_item_id, i.contact_id, i.group_id
            FROM chat_items i
            JOIN files f ON f.chat_item_id = i.chat_item_id
            WHERE f.user_id = ? AND f.file_id = ?
            LIMIT 1
          |]
        (userId, fileId)
  getAChatItem db user chatRef itemId

getChatItemByGroupId :: DB.Connection -> User -> GroupId -> ExceptT StoreError IO AChatItem
getChatItemByGroupId db user@User {userId} groupId = do
  (chatRef, itemId) <-
    ExceptT . firstRow' toChatItemRef (SEChatItemNotFoundByGroupId groupId) $
      DB.query
        db
        [sql|
          SELECT i.chat_item_id, i.contact_id, i.group_id
          FROM chat_items i
          JOIN groups g ON g.chat_item_id = i.chat_item_id
          WHERE g.user_id = ? AND g.group_id = ?
          LIMIT 1
        |]
        (userId, groupId)
  getAChatItem db user chatRef itemId

getChatRefViaItemId :: DB.Connection -> User -> ChatItemId -> ExceptT StoreError IO ChatRef
getChatRefViaItemId db User {userId} itemId = do
  ExceptT . firstRow' toChatRef (SEChatItemNotFound itemId) $
    DB.query db "SELECT contact_id, group_id FROM chat_items WHERE user_id = ? AND chat_item_id = ?" (userId, itemId)
  where
    toChatRef = \case
      (Just contactId, Nothing) -> Right $ ChatRef CTDirect contactId
      (Nothing, Just groupId) -> Right $ ChatRef CTGroup groupId
      (_, _) -> Left $ SEBadChatItem itemId

getAChatItem :: DB.Connection -> User -> ChatRef -> ChatItemId -> ExceptT StoreError IO AChatItem
getAChatItem db user chatRef itemId = case chatRef of
  ChatRef CTDirect contactId -> do
    ct <- getContact db user contactId
    (CChatItem msgDir ci) <- getDirectChatItem db user contactId itemId
    pure $ AChatItem SCTDirect msgDir (DirectChat ct) ci
  ChatRef CTGroup groupId -> do
    gInfo <- getGroupInfo db user groupId
    (CChatItem msgDir ci) <- getGroupChatItem db user groupId itemId
    pure $ AChatItem SCTGroup msgDir (GroupChat gInfo) ci
  _ -> throwError $ SEChatItemNotFound itemId

getChatItemVersions :: DB.Connection -> ChatItemId -> IO [ChatItemVersion]
getChatItemVersions db itemId = do
  map toChatItemVersion
    <$> DB.query
      db
      [sql|
        SELECT chat_item_version_id, msg_content, item_version_ts, created_at
        FROM chat_item_versions
        WHERE chat_item_id = ?
        ORDER BY chat_item_version_id DESC
      |]
      (Only itemId)
  where
    toChatItemVersion :: (Int64, MsgContent, UTCTime, UTCTime) -> ChatItemVersion
    toChatItemVersion (chatItemVersionId, msgContent, itemVersionTs, createdAt) =
      let formattedText = parseMaybeMarkdownList $ msgContentText msgContent
       in ChatItemVersion {chatItemVersionId, msgContent, formattedText, itemVersionTs, createdAt}

getDirectChatReactions_ :: DB.Connection -> Contact -> Chat 'CTDirect -> IO (Chat 'CTDirect)
getDirectChatReactions_ db ct c@Chat {chatItems} = do
  chatItems' <- forM chatItems $ \(CChatItem md ci@ChatItem {meta = CIMeta {itemSharedMsgId}}) -> do
    reactions <- maybe (pure []) (getDirectCIReactions db ct) itemSharedMsgId
    pure $ CChatItem md ci {reactions}
  pure c {chatItems = chatItems'}

getGroupChatReactions_ :: DB.Connection -> GroupInfo -> Chat 'CTGroup -> IO (Chat 'CTGroup)
getGroupChatReactions_ db g c@Chat {chatItems} = do
  chatItems' <- forM chatItems $ \(CChatItem md ci@ChatItem {meta = CIMeta {itemSharedMsgId}}) -> do
    let GroupMember {memberId} = chatItemMember g ci
    reactions <- maybe (pure []) (getGroupCIReactions db g memberId) itemSharedMsgId
    pure $ CChatItem md ci {reactions}
  pure c {chatItems = chatItems'}

getDirectCIReactions :: DB.Connection -> Contact -> SharedMsgId -> IO [CIReactionCount]
getDirectCIReactions db Contact {contactId} itemSharedMsgId =
  map toCIReaction
    <$> DB.query
      db
      [sql|
        SELECT reaction, MAX(reaction_sent), COUNT(chat_item_reaction_id)
        FROM chat_item_reactions
        WHERE contact_id = ? AND shared_msg_id = ?
        GROUP BY reaction
      |]
      (contactId, itemSharedMsgId)

getGroupCIReactions :: DB.Connection -> GroupInfo -> MemberId -> SharedMsgId -> IO [CIReactionCount]
getGroupCIReactions db GroupInfo {groupId} itemMemberId itemSharedMsgId =
  map toCIReaction
    <$> DB.query
      db
      [sql|
        SELECT reaction, MAX(reaction_sent), COUNT(chat_item_reaction_id)
        FROM chat_item_reactions
        WHERE group_id = ? AND item_member_id = ? AND shared_msg_id = ?
        GROUP BY reaction
      |]
      (groupId, itemMemberId, itemSharedMsgId)

getACIReactions :: DB.Connection -> AChatItem -> IO AChatItem
getACIReactions db aci@(AChatItem _ md chat ci@ChatItem {meta = CIMeta {itemSharedMsgId}}) = case itemSharedMsgId of
  Just itemSharedMId -> case chat of
    DirectChat ct -> do
      reactions <- getDirectCIReactions db ct itemSharedMId
      pure $ AChatItem SCTDirect md chat ci {reactions}
    GroupChat g -> do
      let GroupMember {memberId} = chatItemMember g ci
      reactions <- getGroupCIReactions db g memberId itemSharedMId
      pure $ AChatItem SCTGroup md chat ci {reactions}
    _ -> pure aci
  _ -> pure aci

deleteDirectCIReactions_ :: DB.Connection -> ContactId -> ChatItem 'CTDirect d -> IO ()
deleteDirectCIReactions_ db contactId ChatItem {meta = CIMeta {itemSharedMsgId}} =
  forM_ itemSharedMsgId $ \itemSharedMId ->
    DB.execute db "DELETE FROM chat_item_reactions WHERE contact_id = ? AND shared_msg_id = ?" (contactId, itemSharedMId)

deleteGroupCIReactions_ :: DB.Connection -> GroupInfo -> ChatItem 'CTGroup d -> IO ()
deleteGroupCIReactions_ db g@GroupInfo {groupId} ci@ChatItem {meta = CIMeta {itemSharedMsgId}} =
  forM_ itemSharedMsgId $ \itemSharedMId -> do
    let GroupMember {memberId} = chatItemMember g ci
    DB.execute
      db
      "DELETE FROM chat_item_reactions WHERE group_id = ? AND shared_msg_id = ? AND item_member_id = ?"
      (groupId, itemSharedMId, memberId)

toCIReaction :: (MsgReaction, Bool, Int) -> CIReactionCount
toCIReaction (reaction, userReacted, totalReacted) = CIReactionCount {reaction, userReacted, totalReacted}

getDirectReactions :: DB.Connection -> Contact -> SharedMsgId -> Bool -> IO [MsgReaction]
getDirectReactions db ct itemSharedMId sent =
  map fromOnly
    <$> DB.query
      db
      [sql|
        SELECT reaction
        FROM chat_item_reactions
        WHERE contact_id = ? AND shared_msg_id = ? AND reaction_sent = ?
      |]
      (contactId' ct, itemSharedMId, sent)

setDirectReaction :: DB.Connection -> Contact -> SharedMsgId -> Bool -> MsgReaction -> Bool -> MessageId -> UTCTime -> IO ()
setDirectReaction db ct itemSharedMId sent reaction add msgId reactionTs
  | add =
    DB.execute
      db
      [sql|
        INSERT INTO chat_item_reactions
          (contact_id, shared_msg_id, reaction_sent, reaction, created_by_msg_id, reaction_ts)
          VALUES (?,?,?,?,?,?)
      |]
      (contactId' ct, itemSharedMId, sent, reaction, msgId, reactionTs)
  | otherwise =
    DB.execute
      db
      [sql|
        DELETE FROM chat_item_reactions
        WHERE contact_id = ? AND shared_msg_id = ? AND reaction_sent = ? AND reaction = ?
      |]
      (contactId' ct, itemSharedMId, sent, reaction)

getGroupReactions :: DB.Connection -> GroupInfo -> GroupMember -> MemberId -> SharedMsgId -> Bool -> IO [MsgReaction]
getGroupReactions db GroupInfo {groupId} m itemMemberId itemSharedMId sent =
  map fromOnly
    <$> DB.query
      db
      [sql|
        SELECT reaction
        FROM chat_item_reactions
        WHERE group_id = ? AND group_member_id = ? AND item_member_id = ? AND shared_msg_id = ? AND reaction_sent = ?
      |]
      (groupId, groupMemberId' m, itemMemberId, itemSharedMId, sent)

setGroupReaction :: DB.Connection -> GroupInfo -> GroupMember -> MemberId -> SharedMsgId -> Bool -> MsgReaction -> Bool -> MessageId -> UTCTime -> IO ()
setGroupReaction db GroupInfo {groupId} m itemMemberId itemSharedMId sent reaction add msgId reactionTs
  | add =
    DB.execute
      db
      [sql|
        INSERT INTO chat_item_reactions
          (group_id, group_member_id, item_member_id, shared_msg_id, reaction_sent, reaction, created_by_msg_id, reaction_ts)
          VALUES (?,?,?,?,?,?,?,?)
      |]
      (groupId, groupMemberId' m, itemMemberId, itemSharedMId, sent, reaction, msgId, reactionTs)
  | otherwise =
    DB.execute
      db
      [sql|
        DELETE FROM chat_item_reactions
        WHERE group_id = ? AND group_member_id = ? AND shared_msg_id = ? AND item_member_id = ? AND reaction_sent = ? AND reaction = ?
      |]
      (groupId, groupMemberId' m, itemSharedMId, itemMemberId, sent, reaction)

getTimedItems :: DB.Connection -> User -> UTCTime -> IO [((ChatRef, ChatItemId), UTCTime)]
getTimedItems db User {userId} startTimedThreadCutoff =
  mapMaybe toCIRefDeleteAt
    <$> DB.query
      db
      [sql|
        SELECT chat_item_id, contact_id, group_id, timed_delete_at
        FROM chat_items
        WHERE user_id = ? AND timed_delete_at IS NOT NULL AND timed_delete_at <= ?
      |]
      (userId, startTimedThreadCutoff)
  where
    toCIRefDeleteAt :: (ChatItemId, Maybe ContactId, Maybe GroupId, UTCTime) -> Maybe ((ChatRef, ChatItemId), UTCTime)
    toCIRefDeleteAt = \case
      (itemId, Just contactId, Nothing, deleteAt) -> Just ((ChatRef CTDirect contactId, itemId), deleteAt)
      (itemId, Nothing, Just groupId, deleteAt) -> Just ((ChatRef CTGroup groupId, itemId), deleteAt)
      _ -> Nothing

getChatItemTTL :: DB.Connection -> User -> IO (Maybe Int64)
getChatItemTTL db User {userId} =
  fmap join . maybeFirstRow fromOnly $ DB.query db "SELECT chat_item_ttl FROM settings WHERE user_id = ? LIMIT 1" (Only userId)

setChatItemTTL :: DB.Connection -> User -> Maybe Int64 -> IO ()
setChatItemTTL db User {userId} chatItemTTL = do
  currentTs <- getCurrentTime
  r :: (Maybe Int64) <- maybeFirstRow fromOnly $ DB.query db "SELECT 1 FROM settings WHERE user_id = ? LIMIT 1" (Only userId)
  case r of
    Just _ -> do
      DB.execute
        db
        "UPDATE settings SET chat_item_ttl = ?, updated_at = ? WHERE user_id = ?"
        (chatItemTTL, currentTs, userId)
    Nothing -> do
      DB.execute
        db
        "INSERT INTO settings (user_id, chat_item_ttl, created_at, updated_at) VALUES (?,?,?,?)"
        (userId, chatItemTTL, currentTs, currentTs)

getContactExpiredFileInfo :: DB.Connection -> User -> Contact -> UTCTime -> IO [CIFileInfo]
getContactExpiredFileInfo db User {userId} Contact {contactId} expirationDate =
  map toFileInfo
    <$> DB.query
      db
      (fileInfoQuery <> " WHERE i.user_id = ? AND i.contact_id = ? AND i.created_at <= ?")
      (userId, contactId, expirationDate)

deleteContactExpiredCIs :: DB.Connection -> User -> Contact -> UTCTime -> IO ()
deleteContactExpiredCIs db user@User {userId} ct@Contact {contactId} expirationDate = do
  connIds <- getContactConnIds_ db user ct
  forM_ connIds $ \connId ->
    DB.execute db "DELETE FROM messages WHERE connection_id = ? AND created_at <= ?" (connId, expirationDate)
  DB.execute db "DELETE FROM chat_item_reactions WHERE contact_id = ? AND created_at <= ?" (contactId, expirationDate)
  DB.execute db "DELETE FROM chat_items WHERE user_id = ? AND contact_id = ? AND created_at <= ?" (userId, contactId, expirationDate)

getGroupExpiredFileInfo :: DB.Connection -> User -> GroupInfo -> UTCTime -> UTCTime -> IO [CIFileInfo]
getGroupExpiredFileInfo db User {userId} GroupInfo {groupId} expirationDate createdAtCutoff =
  map toFileInfo
    <$> DB.query
      db
      (fileInfoQuery <> " WHERE i.user_id = ? AND i.group_id = ? AND i.item_ts <= ? AND i.created_at <= ?")
      (userId, groupId, expirationDate, createdAtCutoff)

deleteGroupExpiredCIs :: DB.Connection -> User -> GroupInfo -> UTCTime -> UTCTime -> IO ()
deleteGroupExpiredCIs db User {userId} GroupInfo {groupId} expirationDate createdAtCutoff = do
  DB.execute db "DELETE FROM messages WHERE group_id = ? AND created_at <= ?" (groupId, min expirationDate createdAtCutoff)
  DB.execute db "DELETE FROM chat_item_reactions WHERE group_id = ? AND reaction_ts <= ? AND created_at <= ?" (groupId, expirationDate, createdAtCutoff)
  DB.execute db "DELETE FROM chat_items WHERE user_id = ? AND group_id = ? AND item_ts <= ? AND created_at <= ?" (userId, groupId, expirationDate, createdAtCutoff)

createCIModeration :: DB.Connection -> GroupInfo -> GroupMember -> MemberId -> SharedMsgId -> MessageId -> UTCTime -> IO ()
createCIModeration db GroupInfo {groupId} moderatorMember itemMemberId itemSharedMId msgId moderatedAtTs =
  DB.execute
    db
    [sql|
      INSERT INTO chat_item_moderations
        (group_id, moderator_member_id, item_member_id, shared_msg_id, created_by_msg_id, moderated_at)
        VALUES (?,?,?,?,?,?)
    |]
    (groupId, groupMemberId' moderatorMember, itemMemberId, itemSharedMId, msgId, moderatedAtTs)

getCIModeration :: DB.Connection -> User -> GroupInfo -> MemberId -> Maybe SharedMsgId -> IO (Maybe CIModeration)
getCIModeration _ _ _ _ Nothing = pure Nothing
getCIModeration db user GroupInfo {groupId} itemMemberId (Just sharedMsgId) = do
  r_ <-
    maybeFirstRow id $
      DB.query
        db
        [sql|
          SELECT chat_item_moderation_id, moderator_member_id, created_by_msg_id, moderated_at
          FROM chat_item_moderations
          WHERE group_id = ? AND item_member_id = ? AND shared_msg_id = ?
          LIMIT 1
        |]
        (groupId, itemMemberId, sharedMsgId)
  case r_ of
    Just (moderationId, moderatorId, createdByMsgId, moderatedAt) -> do
      runExceptT (getGroupMember db user groupId moderatorId) >>= \case
        Right moderatorMember -> pure (Just CIModeration {moderationId, moderatorMember, createdByMsgId, moderatedAt})
        _ -> pure Nothing
    _ -> pure Nothing

deleteCIModeration :: DB.Connection -> GroupInfo -> MemberId -> Maybe SharedMsgId -> IO ()
deleteCIModeration _ _ _ Nothing = pure ()
deleteCIModeration db GroupInfo {groupId} itemMemberId (Just sharedMsgId) =
  DB.execute
    db
    "DELETE FROM chat_item_moderations WHERE group_id = ? AND item_member_id = ? AND shared_msg_id = ?"
    (groupId, itemMemberId, sharedMsgId)

createGroupSndStatus :: DB.Connection -> ChatItemId -> GroupMemberId -> CIStatus 'MDSnd -> IO ()
createGroupSndStatus db itemId memberId status =
  DB.execute
    db
    "INSERT INTO group_snd_item_statuses (chat_item_id, group_member_id, group_snd_item_status) VALUES (?,?,?)"
    (itemId, memberId, status)

getGroupSndStatus :: DB.Connection -> ChatItemId -> GroupMemberId -> ExceptT StoreError IO (CIStatus 'MDSnd)
getGroupSndStatus db itemId memberId =
  ExceptT . firstRow fromOnly (SENoGroupSndStatus itemId memberId) $
    DB.query
      db
      [sql|
        SELECT group_snd_item_status
        FROM group_snd_item_statuses
        WHERE chat_item_id = ? AND group_member_id = ?
        LIMIT 1
      |]
      (itemId, memberId)

updateGroupSndStatus :: DB.Connection -> ChatItemId -> GroupMemberId -> CIStatus 'MDSnd -> IO ()
updateGroupSndStatus db itemId memberId status = do
  currentTs <- liftIO getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE group_snd_item_statuses
      SET group_snd_item_status = ?, updated_at = ?
      WHERE chat_item_id = ? AND group_member_id  = ?
    |]
    (status, currentTs, itemId, memberId)

getGroupSndStatuses :: DB.Connection -> ChatItemId -> IO [(GroupMemberId, CIStatus 'MDSnd)]
getGroupSndStatuses db itemId =
  DB.query
    db
    [sql|
      SELECT group_member_id, group_snd_item_status
      FROM group_snd_item_statuses
      WHERE chat_item_id = ?
    |]
    (Only itemId)

getGroupSndStatusCounts :: DB.Connection -> ChatItemId -> IO [(CIStatus 'MDSnd, Int)]
getGroupSndStatusCounts db itemId =
  DB.query
    db
    [sql|
      SELECT group_snd_item_status, COUNT(1)
      FROM group_snd_item_statuses
      WHERE chat_item_id = ?
      GROUP BY group_snd_item_status
    |]
    (Only itemId)
