{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Simplex.Chat.Store.Messages
  ( getContactConnIds_,

    -- * Message and chat item functions
    deleteContactCIs,
    getGroupFileInfo,
    getGroupMemberFileInfo,
    deleteGroupChatItemsMessages,
    createNewSndMessage,
    createSndMsgDelivery,
    createNewMessageAndRcvMsgDelivery,
    getLastRcvMsgInfo,
    createNewRcvMessage,
    updateSndMsgDeliveryStatus,
    createPendingGroupMessage,
    getPendingGroupMessages,
    deletePendingGroupMessage,
    deleteOldMessages,
    updateChatTs,
    createNewSndChatItem,
    createNewRcvChatItem,
    createNewChatItemNoMsg,
    createNewChatItem_,
    getChatPreviews,
    getDirectChat,
    getGroupChat,
    getLocalChat,
    getDirectChatItemLast,
    getAllChatItems,
    getAChatItem,
    getAChatItemBySharedMsgId,
    updateDirectChatItem,
    updateDirectChatItem',
    addInitialAndNewCIVersions,
    createChatItemVersion,
    deleteDirectChatItem,
    markDirectChatItemDeleted,
    updateGroupChatItemStatus,
    updateGroupChatItem,
    createGroupCIMentions,
    updateGroupCIMentions,
    deleteGroupChatItem,
    updateGroupChatItemModerated,
    updateMemberCIsModerated,
    updateGroupCIBlockedByAdmin,
    markGroupChatItemDeleted,
    markMemberCIsDeleted,
    markGroupChatItemBlocked,
    markGroupCIBlockedByAdmin,
    markMessageReportsDeleted,
    markReceivedGroupReportsDeleted,
    deleteLocalChatItem,
    updateDirectChatItemsRead,
    getDirectUnreadTimedItems,
    updateDirectChatItemsReadList,
    setDirectChatItemRead,
    setDirectChatItemsDeleteAt,
    updateGroupChatItemsRead,
    getGroupUnreadTimedItems,
    updateGroupChatItemsReadList,
    setGroupChatItemsDeleteAt,
    updateLocalChatItemsRead,
    getChatRefViaItemId,
    getChatItemVersions,
    getDirectCIReactions,
    getDirectReactions,
    setDirectReaction,
    getGroupCIReactions,
    getGroupReactions,
    setGroupReaction,
    getReactionMembers,
    getChatItemIdsByAgentMsgId,
    getDirectChatItem,
    getDirectCIWithReactions,
    getDirectChatItemBySharedMsgId,
    getDirectChatItemsByAgentMsgId,
    getGroupChatItem,
    getGroupCIWithReactions,
    getGroupChatItemBySharedMsgId,
    getGroupMemberCIBySharedMsgId,
    getGroupChatItemsByAgentMsgId,
    getGroupMemberChatItemLast,
    getLocalChatItem,
    updateLocalChatItem',
    getDirectChatItemIdByText,
    getDirectChatItemIdByText',
    getGroupChatItemIdByText,
    getGroupChatItemIdByText',
    getLocalChatItemIdByText,
    getLocalChatItemIdByText',
    getChatItemByFileId,
    lookupChatItemByFileId,
    getChatItemByGroupId,
    updateDirectChatItemStatus,
    setDirectSndChatItemViaProxy,
    getTimedItems,
    getChatItemTTL,
    setChatItemTTL,
    getChatTTLCount,
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
    setGroupSndViaProxy,
    getGroupSndStatuses,
    getGroupSndStatusCounts,
    getGroupHistoryItems,
  )
where

import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Crypto.Random (ChaChaDRG)
import Data.Bifunctor (first)
import Data.ByteString.Char8 (ByteString)
import Data.Either (fromRight, rights)
import Data.Int (Int64)
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (addUTCTime)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Simplex.Chat.Controller (ChatListQuery (..), ChatPagination (..), PaginationByTime (..))
import Simplex.Chat.Markdown
import Simplex.Chat.Messages
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Protocol
import Simplex.Chat.Store.Direct
import Simplex.Chat.Store.Groups
import Simplex.Chat.Store.NoteFolders
import Simplex.Chat.Store.Shared
import Simplex.Chat.Types
import Simplex.Chat.Types.Shared
import Simplex.Messaging.Agent.Protocol (AgentMsgId, ConnId, ConnShortLink, ConnectionMode (..), MsgMeta (..), UserId)
import Simplex.Messaging.Agent.Store.AgentStore (firstRow, firstRow', maybeFirstRow)
import Simplex.Messaging.Agent.Store.DB (BoolInt (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.File (CryptoFile (..), CryptoFileArgs (..))
import Simplex.Messaging.Util (eitherToMaybe)
import UnliftIO.STM
#if defined(dbPostgres)
import Database.PostgreSQL.Simple (FromRow, Only (..), Query, ToRow, (:.) (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (FromRow, Only (..), Query, ToRow, (:.) (..))
import Database.SQLite.Simple.QQ (sql)
#endif

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

getGroupMemberFileInfo :: DB.Connection -> User -> GroupInfo -> GroupMember -> IO [CIFileInfo]
getGroupMemberFileInfo db User {userId} GroupInfo {groupId} GroupMember {groupMemberId} =
  map toFileInfo
    <$> DB.query db (fileInfoQuery <> " WHERE i.user_id = ? AND i.group_id = ? AND i.group_member_id = ?") (userId, groupId, groupMemberId)

deleteGroupChatItemsMessages :: DB.Connection -> User -> GroupInfo -> IO ()
deleteGroupChatItemsMessages db User {userId} GroupInfo {groupId} = do
  DB.execute db "DELETE FROM messages WHERE group_id = ?" (Only groupId)
  DB.execute db "DELETE FROM chat_item_reactions WHERE group_id = ?" (Only groupId)
  DB.execute db "DELETE FROM chat_items WHERE user_id = ? AND group_id = ?" (userId, groupId)

createNewSndMessage :: MsgEncodingI e => DB.Connection -> TVar ChaChaDRG -> ConnOrGroupId -> ChatMsgEvent e -> (SharedMsgId -> EncodedChatMessage) -> ExceptT StoreError IO SndMessage
createNewSndMessage db gVar connOrGroupId chatMsgEvent encodeMessage =
  createWithRandomId' gVar $ \sharedMsgId ->
    case encodeMessage (SharedMsgId sharedMsgId) of
      ECMLarge -> pure $ Left SELargeMsg
      ECMEncoded msgBody -> do
        createdAt <- getCurrentTime
        DB.execute
          db
          [sql|
            INSERT INTO messages (
              msg_sent, chat_msg_event, msg_body, connection_id, group_id,
              shared_msg_id, shared_msg_id_user, created_at, updated_at
            ) VALUES (?,?,?,?,?,?,?,?,?)
          |]
          (MDSnd, toCMEventTag chatMsgEvent, DB.Binary msgBody, connId_, groupId_, DB.Binary sharedMsgId, Just (BI True), createdAt, createdAt)
        msgId <- insertedRowId db
        pure $ Right SndMessage {msgId, sharedMsgId = SharedMsgId sharedMsgId, msgBody}
  where
    (connId_, groupId_) = case connOrGroupId of
      ConnectionId connId -> (Just connId, Nothing)
      GroupId groupId -> (Nothing, Just groupId)

createSndMsgDelivery :: DB.Connection -> SndMsgDelivery -> MessageId -> IO Int64
createSndMsgDelivery db SndMsgDelivery {connId, agentMsgId} messageId = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      INSERT INTO msg_deliveries
        (message_id, connection_id, agent_msg_id, chat_ts, created_at, updated_at, delivery_status)
      VALUES (?,?,?,?,?,?,?)
    |]
    (messageId, connId, agentMsgId, currentTs, currentTs, currentTs, MDSSndAgent)
  insertedRowId db

createNewMessageAndRcvMsgDelivery :: forall e. MsgEncodingI e => DB.Connection -> ConnOrGroupId -> NewRcvMessage e -> Maybe SharedMsgId -> RcvMsgDelivery -> Maybe GroupMemberId -> ExceptT StoreError IO RcvMessage
createNewMessageAndRcvMsgDelivery db connOrGroupId newMessage sharedMsgId_ RcvMsgDelivery {connId, agentMsgId, agentMsgMeta} authorGroupMemberId_ = do
  msg@RcvMessage {msgId} <- createNewRcvMessage db connOrGroupId newMessage sharedMsgId_ authorGroupMemberId_ Nothing
  liftIO $ do
    currentTs <- getCurrentTime
    DB.execute
      db
      [sql|
        INSERT INTO msg_deliveries
          (message_id, connection_id, agent_msg_id, agent_msg_meta, chat_ts, created_at, updated_at, delivery_status)
        VALUES (?,?,?,?,?,?,?,?)
      |]
      (msgId, connId, agentMsgId, msgMetaJson agentMsgMeta, snd $ broker agentMsgMeta, currentTs, currentTs, MDSRcvAgent)
  pure msg

getLastRcvMsgInfo :: DB.Connection -> Int64 -> IO (Maybe RcvMsgInfo)
getLastRcvMsgInfo db connId =
  maybeFirstRow rcvMsgInfo $
    DB.query
      db
      [sql|
        SELECT message_id, msg_delivery_id, delivery_status, agent_msg_id, agent_msg_meta
        FROM msg_deliveries
        WHERE connection_id = ? AND delivery_status IN (?, ?)
        ORDER BY created_at DESC, msg_delivery_id DESC
        LIMIT 1
      |]
      (connId, MDSRcvAgent, MDSRcvAcknowledged)
  where
    rcvMsgInfo (msgId, msgDeliveryId, msgDeliveryStatus, agentMsgId, agentMsgMeta) =
      RcvMsgInfo {msgId, msgDeliveryId, msgDeliveryStatus, agentMsgId, agentMsgMeta}

createNewRcvMessage :: forall e. MsgEncodingI e => DB.Connection -> ConnOrGroupId -> NewRcvMessage e -> Maybe SharedMsgId -> Maybe GroupMemberId -> Maybe GroupMemberId -> ExceptT StoreError IO RcvMessage
createNewRcvMessage db connOrGroupId NewRcvMessage {chatMsgEvent, msgBody} sharedMsgId_ authorMember forwardedByMember =
  case connOrGroupId of
    ConnectionId connId -> liftIO $ insertRcvMsg (Just connId) Nothing
    GroupId groupId -> case sharedMsgId_ of
      Just sharedMsgId ->
        liftIO (duplicateGroupMsgMemberIds groupId sharedMsgId) >>= \case
          Just (duplAuthorId, duplFwdMemberId) ->
            throwError $ SEDuplicateGroupMessage groupId sharedMsgId duplAuthorId duplFwdMemberId
          Nothing -> liftIO $ insertRcvMsg Nothing $ Just groupId
      Nothing -> liftIO $ insertRcvMsg Nothing $ Just groupId
  where
    duplicateGroupMsgMemberIds :: Int64 -> SharedMsgId -> IO (Maybe (Maybe GroupMemberId, Maybe GroupMemberId))
    duplicateGroupMsgMemberIds groupId sharedMsgId =
      maybeFirstRow id $
        DB.query
          db
          [sql|
            SELECT author_group_member_id, forwarded_by_group_member_id
            FROM messages
            WHERE group_id = ? AND shared_msg_id = ? LIMIT 1
          |]
          (groupId, sharedMsgId)
    insertRcvMsg connId_ groupId_ = do
      currentTs <- getCurrentTime
      DB.execute
        db
        [sql|
          INSERT INTO messages
            (msg_sent, chat_msg_event, msg_body, created_at, updated_at, connection_id, group_id, shared_msg_id, author_group_member_id, forwarded_by_group_member_id)
          VALUES (?,?,?,?,?,?,?,?,?,?)
        |]
        (MDRcv, toCMEventTag chatMsgEvent, DB.Binary msgBody, currentTs, currentTs, connId_, groupId_, sharedMsgId_, authorMember, forwardedByMember)
      msgId <- insertedRowId db
      pure RcvMessage {msgId, chatMsgEvent = ACME (encoding @e) chatMsgEvent, sharedMsgId_, msgBody, authorMember, forwardedByMember}

updateSndMsgDeliveryStatus :: DB.Connection -> Int64 -> AgentMsgId -> MsgDeliveryStatus 'MDSnd -> IO ()
updateSndMsgDeliveryStatus db connId agentMsgId sndMsgDeliveryStatus = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE msg_deliveries
      SET delivery_status = ?, updated_at = ?
      WHERE connection_id = ? AND agent_msg_id = ?
    |]
    (sndMsgDeliveryStatus, currentTs, connId, agentMsgId)

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

getPendingGroupMessages :: DB.Connection -> Int64 -> IO [(SndMessage, ACMEventTag, Maybe Int64)]
getPendingGroupMessages db groupMemberId =
  map pendingGroupMessage
    <$> DB.query
      db
      [sql|
        SELECT pgm.message_id, m.shared_msg_id, m.msg_body, m.chat_msg_event, pgm.group_member_intro_id
        FROM pending_group_messages pgm
        JOIN messages m USING (message_id)
        WHERE pgm.group_member_id = ?
        ORDER BY pgm.created_at ASC, pgm.message_id ASC
      |]
      (Only groupMemberId)
  where
    pendingGroupMessage (msgId, sharedMsgId, msgBody, cmEventTag, introId_) =
      (SndMessage {msgId, sharedMsgId, msgBody}, cmEventTag, introId_)

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
      "UPDATE contacts SET chat_ts = ?, chat_deleted = 0 WHERE user_id = ? AND contact_id = ?"
      (chatTs, userId, contactId)
  GroupChat GroupInfo {groupId} ->
    DB.execute
      db
      "UPDATE groups SET chat_ts = ? WHERE user_id = ? AND group_id = ?"
      (chatTs, userId, groupId)
  LocalChat NoteFolder {noteFolderId} ->
    DB.execute
      db
      "UPDATE note_folders SET chat_ts = ? WHERE user_id = ? AND note_folder_id = ?"
      (chatTs, userId, noteFolderId)
  _ -> pure ()

createNewSndChatItem :: DB.Connection -> User -> ChatDirection c 'MDSnd -> Maybe NotInHistory -> SndMessage -> CIContent 'MDSnd -> Maybe (CIQuote c) -> Maybe CIForwardedFrom -> Maybe CITimed -> Bool -> UTCTime -> IO ChatItemId
createNewSndChatItem db user chatDirection notInHistory_ SndMessage {msgId, sharedMsgId} ciContent quotedItem itemForwarded timed live createdAt =
  createNewChatItem_ db user chatDirection notInHistory_ createdByMsgId (Just sharedMsgId) ciContent quoteRow itemForwarded timed live False createdAt Nothing createdAt
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

createNewRcvChatItem :: ChatTypeQuotable c => DB.Connection -> User -> ChatDirection c 'MDRcv -> Maybe NotInHistory -> RcvMessage -> Maybe SharedMsgId -> CIContent 'MDRcv -> Maybe CITimed -> Bool -> Bool -> UTCTime -> UTCTime -> IO (ChatItemId, Maybe (CIQuote c), Maybe CIForwardedFrom)
createNewRcvChatItem db user chatDirection notInHistory_ RcvMessage {msgId, chatMsgEvent, forwardedByMember} sharedMsgId_ ciContent timed live userMention itemTs createdAt = do
  ciId <- createNewChatItem_ db user chatDirection notInHistory_ (Just msgId) sharedMsgId_ ciContent quoteRow itemForwarded timed live userMention itemTs forwardedByMember createdAt
  quotedItem <- mapM (getChatItemQuote_ db user chatDirection) quotedMsg
  pure (ciId, quotedItem, itemForwarded)
  where
    itemForwarded = cmForwardedFrom chatMsgEvent
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
createNewChatItemNoMsg db user chatDirection ciContent itemTs =
  createNewChatItem_ db user chatDirection Nothing Nothing Nothing ciContent quoteRow Nothing Nothing False False itemTs Nothing
  where
    quoteRow :: NewQuoteRow
    quoteRow = (Nothing, Nothing, Nothing, Nothing, Nothing)

createNewChatItem_ :: forall c d. MsgDirectionI d => DB.Connection -> User -> ChatDirection c d -> Maybe NotInHistory -> Maybe MessageId -> Maybe SharedMsgId -> CIContent d -> NewQuoteRow -> Maybe CIForwardedFrom -> Maybe CITimed -> Bool -> Bool -> UTCTime -> Maybe GroupMemberId -> UTCTime -> IO ChatItemId
createNewChatItem_ db User {userId} chatDirection notInHistory_ msgId_ sharedMsgId ciContent quoteRow itemForwarded timed live userMention itemTs forwardedByMember createdAt = do
  DB.execute
    db
    [sql|
      INSERT INTO chat_items (
        -- user and IDs
        user_id, created_by_msg_id, contact_id, group_id, group_member_id, note_folder_id,
        -- meta
        item_sent, item_ts, item_content, item_content_tag, item_text, item_status, msg_content_tag, shared_msg_id,
        forwarded_by_group_member_id, include_in_history, created_at, updated_at, item_live, user_mention, timed_ttl, timed_delete_at,
        -- quote
        quoted_shared_msg_id, quoted_sent_at, quoted_content, quoted_sent, quoted_member_id,
        -- forwarded from
        fwd_from_tag, fwd_from_chat_name, fwd_from_msg_dir, fwd_from_contact_id, fwd_from_group_id, fwd_from_chat_item_id
      ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
    |]
    ((userId, msgId_) :. idsRow :. itemRow :. quoteRow' :. forwardedFromRow)
  ciId <- insertedRowId db
  forM_ msgId_ $ \msgId -> insertChatItemMessage_ db ciId msgId createdAt
  pure ciId
  where
    itemRow :: (SMsgDirection d, UTCTime, CIContent d, Text, Text, CIStatus d, Maybe MsgContentTag, Maybe SharedMsgId, Maybe GroupMemberId, BoolInt) :. (UTCTime, UTCTime, Maybe BoolInt, BoolInt) :. (Maybe Int, Maybe UTCTime)
    itemRow = (msgDirection @d, itemTs, ciContent, toCIContentTag ciContent, ciContentToText ciContent, ciCreateStatus ciContent, msgContentTag <$> ciMsgContent ciContent, sharedMsgId, forwardedByMember, BI includeInHistory) :. (createdAt, createdAt, BI <$> (justTrue live), BI userMention) :. ciTimedRow timed
    quoteRow' = let (a, b, c, d, e) = quoteRow in (a, b, c, BI <$> d, e)
    idsRow :: (Maybe Int64, Maybe Int64, Maybe Int64, Maybe Int64)
    idsRow = case chatDirection of
      CDDirectRcv Contact {contactId} -> (Just contactId, Nothing, Nothing, Nothing)
      CDDirectSnd Contact {contactId} -> (Just contactId, Nothing, Nothing, Nothing)
      CDGroupRcv GroupInfo {groupId} GroupMember {groupMemberId} -> (Nothing, Just groupId, Just groupMemberId, Nothing)
      CDGroupSnd GroupInfo {groupId} -> (Nothing, Just groupId, Nothing, Nothing)
      CDLocalRcv NoteFolder {noteFolderId} -> (Nothing, Nothing, Nothing, Just noteFolderId)
      CDLocalSnd NoteFolder {noteFolderId} -> (Nothing, Nothing, Nothing, Just noteFolderId)
    includeInHistory :: Bool
    includeInHistory =
      let (_, groupId_, _, _) = idsRow
       in isJust groupId_ && isNothing notInHistory_ && isJust (ciMsgContent ciContent) && ((msgContentTag <$> ciMsgContent ciContent) /= Just MCReport_)
    forwardedFromRow :: (Maybe CIForwardedFromTag, Maybe Text, Maybe MsgDirection, Maybe Int64, Maybe Int64, Maybe Int64)
    forwardedFromRow = case itemForwarded of
      Nothing ->
        (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
      Just CIFFUnknown ->
        (Just CIFFUnknown_, Nothing, Nothing, Nothing, Nothing, Nothing)
      Just CIFFContact {chatName, msgDir, contactId, chatItemId} ->
        (Just CIFFContact_, Just chatName, Just msgDir, contactId, Nothing, chatItemId)
      Just CIFFGroup {chatName, msgDir, groupId, chatItemId} ->
        (Just CIFFGroup_, Just chatName, Just msgDir, Nothing, groupId, chatItemId)

ciTimedRow :: Maybe CITimed -> (Maybe Int, Maybe UTCTime)
ciTimedRow (Just CITimed {ttl, deleteAt}) = (Just ttl, deleteAt)
ciTimedRow _ = (Nothing, Nothing)

insertChatItemMessage_ :: DB.Connection -> ChatItemId -> MessageId -> UTCTime -> IO ()
insertChatItemMessage_ db ciId msgId ts = DB.execute db "INSERT INTO chat_item_messages (chat_item_id, message_id, created_at, updated_at) VALUES (?,?,?,?)" (ciId, msgId, ts, ts)

getChatItemQuote_ :: ChatTypeQuotable c => DB.Connection -> User -> ChatDirection c 'MDRcv -> QuotedMsg -> IO (CIQuote c)
getChatItemQuote_ db User {userId, userContactId} chatDirection QuotedMsg {msgRef = MsgRef {msgId, sentAt, sent, memberId}, content} =
  case chatDirection of
    CDDirectRcv Contact {contactId} -> getDirectChatItemQuote_ contactId (not sent)
    CDGroupRcv GroupInfo {groupId, membership = GroupMember {memberId = userMemberId}} sender@GroupMember {groupMemberId = senderGMId, memberId = senderMemberId} ->
      case memberId of
        Just mId
          | mId == userMemberId -> (`ciQuote` CIQGroupSnd) <$> getUserGroupChatItemId_ groupId
          | mId == senderMemberId -> (`ciQuote` CIQGroupRcv (Just sender)) <$> getGroupChatItemId_ groupId senderGMId
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
          (userId, contactId, msgId, BI userSent)
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
    getGroupChatItemId_ :: Int64 -> GroupMemberId -> IO (Maybe ChatItemId)
    getGroupChatItemId_ groupId groupMemberId =
      maybeFirstRow fromOnly $
        DB.query
          db
          "SELECT chat_item_id FROM chat_items WHERE user_id = ? AND group_id = ? AND shared_msg_id = ? AND item_sent = ? AND group_member_id = ?"
          (userId, groupId, msgId, MDRcv, groupMemberId)
    getGroupChatItemQuote_ :: Int64 -> MemberId -> IO (CIQuote 'CTGroup)
    getGroupChatItemQuote_ groupId mId = do
      ciQuoteGroup
        <$> DB.query
          db
          [sql|
            SELECT i.chat_item_id,
              -- GroupMember
              m.group_member_id, m.group_id, m.member_id, m.peer_chat_min_version, m.peer_chat_max_version, m.member_role, m.member_category,
              m.member_status, m.show_messages, m.member_restriction, m.invited_by, m.invited_by_group_member_id, m.local_display_name, m.contact_id, m.contact_profile_id, p.contact_profile_id,
              p.display_name, p.full_name, p.image, p.contact_link, p.local_alias, p.preferences,
              m.created_at, m.updated_at
            FROM group_members m
            JOIN contact_profiles p ON p.contact_profile_id = COALESCE(m.member_profile_id, m.contact_profile_id)
            LEFT JOIN contacts c ON m.contact_id = c.contact_id
            LEFT JOIN chat_items i ON i.user_id = m.user_id
                                      AND i.group_id = m.group_id
                                      AND m.group_member_id = i.group_member_id
                                      AND i.shared_msg_id = ?
            WHERE m.user_id = ? AND m.group_id = ? AND m.member_id = ?
          |]
          (msgId, userId, groupId, mId)
      where
        ciQuoteGroup :: [Only (Maybe ChatItemId) :. GroupMemberRow] -> CIQuote 'CTGroup
        ciQuoteGroup [] = ciQuote Nothing $ CIQGroupRcv Nothing
        ciQuoteGroup ((Only itemId :. memberRow) : _) = ciQuote itemId . CIQGroupRcv . Just $ toGroupMember userContactId memberRow

getChatPreviews :: DB.Connection -> VersionRangeChat -> User -> Bool -> PaginationByTime -> ChatListQuery -> IO [Either StoreError AChat]
getChatPreviews db vr user withPCC pagination query = do
  directChats <- findDirectChatPreviews_ db user pagination query
  groupChats <- findGroupChatPreviews_ db user pagination query
  localChats <- findLocalChatPreviews_ db user pagination query
  cReqChats <- getContactRequestChatPreviews_ db user pagination query
  connChats <- if withPCC then getContactConnectionChatPreviews_ db user pagination query else pure []
  let refs = sortTake $ concat [directChats, groupChats, localChats, cReqChats, connChats]
  mapM (runExceptT <$> getChatPreview) refs
  where
    ts :: AChatPreviewData -> UTCTime
    ts (ACPD _ cpd) = case cpd of
      (DirectChatPD t _ _ _) -> t
      (GroupChatPD t _ _ _) -> t
      (LocalChatPD t _ _ _) -> t
      (ContactRequestPD t _) -> t
      (ContactConnectionPD t _) -> t
    sortTake = case pagination of
      PTLast count -> take count . sortBy (comparing $ Down . ts)
      PTAfter _ count -> reverse . take count . sortBy (comparing ts)
      PTBefore _ count -> take count . sortBy (comparing $ Down . ts)
    getChatPreview :: AChatPreviewData -> ExceptT StoreError IO AChat
    getChatPreview (ACPD cType cpd) = case cType of
      SCTDirect -> getDirectChatPreview_ db vr user cpd
      SCTGroup -> getGroupChatPreview_ db vr user cpd
      SCTLocal -> getLocalChatPreview_ db user cpd
      SCTContactRequest -> let (ContactRequestPD _ chat) = cpd in pure chat
      SCTContactConnection -> let (ContactConnectionPD _ chat) = cpd in pure chat

data ChatPreviewData (c :: ChatType) where
  DirectChatPD :: UTCTime -> ContactId -> Maybe ChatItemId -> ChatStats -> ChatPreviewData 'CTDirect
  GroupChatPD :: UTCTime -> GroupId -> Maybe ChatItemId -> ChatStats -> ChatPreviewData 'CTGroup
  LocalChatPD :: UTCTime -> NoteFolderId -> Maybe ChatItemId -> ChatStats -> ChatPreviewData 'CTLocal
  ContactRequestPD :: UTCTime -> AChat -> ChatPreviewData 'CTContactRequest
  ContactConnectionPD :: UTCTime -> AChat -> ChatPreviewData 'CTContactConnection

data AChatPreviewData = forall c. ChatTypeI c => ACPD (SChatType c) (ChatPreviewData c)

type ChatStatsRow = (Int, Int, ChatItemId, BoolInt)

toChatStats :: ChatStatsRow -> ChatStats
toChatStats (unreadCount, reportsCount, minUnreadItemId, BI unreadChat) = ChatStats {unreadCount, unreadMentions = 0, reportsCount, minUnreadItemId, unreadChat}

type GroupStatsRow = (Int, Int, Int, ChatItemId, BoolInt)

toGroupStats :: GroupStatsRow -> ChatStats
toGroupStats (unreadCount, unreadMentions, reportsCount, minUnreadItemId, BI unreadChat) = ChatStats {unreadCount, unreadMentions, reportsCount, minUnreadItemId, unreadChat}

findDirectChatPreviews_ :: DB.Connection -> User -> PaginationByTime -> ChatListQuery -> IO [AChatPreviewData]
findDirectChatPreviews_ db User {userId} pagination clq =
  map toPreview <$> getPreviews
  where
    toPreview :: (ContactId, UTCTime, Maybe ChatItemId) :. ChatStatsRow -> AChatPreviewData
    toPreview ((contactId, ts, lastItemId_) :. statsRow) =
      ACPD SCTDirect $ DirectChatPD ts contactId lastItemId_ (toChatStats statsRow)
    baseQuery =
      [sql|
        SELECT
          ct.contact_id,
          ct.chat_ts,
          (
            SELECT chat_item_id
            FROM chat_items ci
            WHERE ci.user_id = ? AND ci.contact_id = ct.contact_id
            ORDER BY ci.created_at DESC
            LIMIT 1
          ) AS chat_item_id,
          COALESCE(ChatStats.UnreadCount, 0),
          0,
          COALESCE(ChatStats.MinUnread, 0),
          ct.unread_chat
        FROM contacts ct
        LEFT JOIN (
          SELECT contact_id, COUNT(1) AS UnreadCount, MIN(chat_item_id) AS MinUnread
          FROM chat_items
          WHERE user_id = ? AND contact_id IS NOT NULL AND item_status = ?
          GROUP BY contact_id
        ) ChatStats ON ChatStats.contact_id = ct.contact_id
      |]
    baseParams = (userId, userId, CISRcvNew)
    getPreviews = case clq of
      CLQFilters {favorite = False, unread = False} -> do
        let q = baseQuery <> " WHERE ct.user_id = ? AND ct.is_user = 0 AND ct.deleted = 0 AND ct.contact_used = 1"
            p = baseParams :. Only userId
        queryWithPagination q p
      CLQFilters {favorite = True, unread = False} -> do
        let q =
              baseQuery
                <> " "
                <> [sql|
                      WHERE ct.user_id = ? AND ct.is_user = 0 AND ct.deleted = 0 AND ct.contact_used = 1
                        AND ct.favorite = 1
                   |]
            p = baseParams :. Only userId
        queryWithPagination q p
      CLQFilters {favorite = False, unread = True} -> do
        let q =
              baseQuery
                <> " "
                <> [sql|
                      WHERE ct.user_id = ? AND ct.is_user = 0 AND ct.deleted = 0 AND ct.contact_used = 1
                        AND (ct.unread_chat = 1 OR ChatStats.UnreadCount > 0)
                   |]
            p = baseParams :. Only userId
        queryWithPagination q p
      CLQFilters {favorite = True, unread = True} -> do
        let q =
              baseQuery
                <> " "
                <> [sql|
                      WHERE ct.user_id = ? AND ct.is_user = 0 AND ct.deleted = 0 AND ct.contact_used = 1
                        AND (ct.favorite = 1
                          OR ct.unread_chat = 1 OR ChatStats.UnreadCount > 0)
                   |]
            p = baseParams :. Only userId
        queryWithPagination q p
      CLQSearch {search} -> do
        let q =
              baseQuery
                <> " "
                <> [sql|
                      JOIN contact_profiles cp ON ct.contact_profile_id = cp.contact_profile_id
                      WHERE ct.user_id = ? AND ct.is_user = 0 AND ct.deleted = 0 AND ct.contact_used = 1
                        AND (
                          LOWER(ct.local_display_name) LIKE '%' || LOWER(?) || '%'
                          OR LOWER(cp.display_name) LIKE '%' || LOWER(?) || '%'
                          OR LOWER(cp.full_name) LIKE '%' || LOWER(?) || '%'
                          OR LOWER(cp.local_alias) LIKE '%' || LOWER(?) || '%'
                        )
                   |]
            p = baseParams :. (userId, search, search, search, search)
        queryWithPagination q p
    queryWithPagination :: ToRow p => Query -> p -> IO [(ContactId, UTCTime, Maybe ChatItemId) :. ChatStatsRow]
    queryWithPagination query params = case pagination of
      PTLast count -> DB.query db (query <> " ORDER BY ct.chat_ts DESC LIMIT ?") (params :. Only count)
      PTAfter ts count -> DB.query db (query <> " AND ct.chat_ts > ? ORDER BY ct.chat_ts ASC LIMIT ?") (params :. (ts, count))
      PTBefore ts count -> DB.query db (query <> " AND ct.chat_ts < ? ORDER BY ct.chat_ts DESC LIMIT ?") (params :. (ts, count))

getDirectChatPreview_ :: DB.Connection -> VersionRangeChat -> User -> ChatPreviewData 'CTDirect -> ExceptT StoreError IO AChat
getDirectChatPreview_ db vr user (DirectChatPD _ contactId lastItemId_ stats) = do
  contact <- getContact db vr user contactId
  lastItem <- case lastItemId_ of
    Just lastItemId -> (: []) <$> getDirectChatItem db user contactId lastItemId
    Nothing -> pure []
  pure $ AChat SCTDirect (Chat (DirectChat contact) lastItem stats)

findGroupChatPreviews_ :: DB.Connection -> User -> PaginationByTime -> ChatListQuery -> IO [AChatPreviewData]
findGroupChatPreviews_ db User {userId} pagination clq =
  map toPreview <$> getPreviews
  where
    toPreview :: (GroupId, UTCTime, Maybe ChatItemId) :. GroupStatsRow -> AChatPreviewData
    toPreview ((groupId, ts, lastItemId_) :. statsRow) =
      ACPD SCTGroup $ GroupChatPD ts groupId lastItemId_ (toGroupStats statsRow)
    baseQuery =
      [sql|
        SELECT
          g.group_id,
          g.chat_ts,
          (
            SELECT chat_item_id
            FROM chat_items ci
            WHERE ci.user_id = ? AND ci.group_id = g.group_id
            ORDER BY ci.item_ts DESC
            LIMIT 1
          ) AS chat_item_id,
          COALESCE(ChatStats.UnreadCount, 0),
          COALESCE(ChatStats.UnreadMentions, 0),
          COALESCE(ReportCount.Count, 0),
          COALESCE(ChatStats.MinUnread, 0),
          g.unread_chat
        FROM groups g
        LEFT JOIN (
          SELECT group_id, COUNT(1) AS UnreadCount, SUM(user_mention) as UnreadMentions, MIN(chat_item_id) AS MinUnread
          FROM chat_items
          WHERE user_id = ? AND group_id IS NOT NULL AND item_status = ?
          GROUP BY group_id
        ) ChatStats ON ChatStats.group_id = g.group_id
        LEFT JOIN (
          SELECT group_id, COUNT(1) AS Count
          FROM chat_items
          WHERE user_id = ? AND group_id IS NOT NULL
            AND msg_content_tag = ? AND item_deleted = ? AND item_sent = 0
          GROUP BY group_id
        ) ReportCount ON ReportCount.group_id = g.group_id
      |]
    baseParams = (userId, userId, CISRcvNew, userId, MCReport_, BI False)
    getPreviews = case clq of
      CLQFilters {favorite = False, unread = False} -> do
        let q = baseQuery <> " WHERE g.user_id = ?"
            p = baseParams :. Only userId
        queryWithPagination q p
      CLQFilters {favorite = True, unread = False} -> do
        let q =
              baseQuery
                <> " "
                <> [sql|
                      WHERE g.user_id = ?
                        AND g.favorite = 1
                   |]
            p = baseParams :. Only userId
        queryWithPagination q p
      CLQFilters {favorite = False, unread = True} -> do
        let q =
              baseQuery
                <> " "
                <> [sql|
                      WHERE g.user_id = ?
                        AND (g.unread_chat = 1 OR ChatStats.UnreadCount > 0)
                   |]
            p = baseParams :. Only userId
        queryWithPagination q p
      CLQFilters {favorite = True, unread = True} -> do
        let q =
              baseQuery
                <> " "
                <> [sql|
                      WHERE g.user_id = ?
                        AND (g.favorite = 1
                          OR g.unread_chat = 1 OR ChatStats.UnreadCount > 0)
                   |]
            p = baseParams :. Only userId
        queryWithPagination q p
      CLQSearch {search} -> do
        let q =
              baseQuery
                <> " "
                <> [sql|
                      JOIN group_profiles gp ON gp.group_profile_id = g.group_profile_id
                      WHERE g.user_id = ?
                        AND (
                          LOWER(g.local_display_name) LIKE '%' || LOWER(?) || '%'
                          OR LOWER(gp.display_name) LIKE '%' || LOWER(?) || '%'
                          OR LOWER(gp.full_name) LIKE '%' || LOWER(?) || '%'
                          OR LOWER(gp.description) LIKE '%' || LOWER(?) || '%'
                        )
                   |]
            p = baseParams :. (userId, search, search, search, search)
        queryWithPagination q p
    queryWithPagination :: ToRow p => Query -> p -> IO [(GroupId, UTCTime, Maybe ChatItemId) :. GroupStatsRow]
    queryWithPagination query params = case pagination of
      PTLast count -> DB.query db (query <> " ORDER BY g.chat_ts DESC LIMIT ?") (params :. Only count)
      PTAfter ts count -> DB.query db (query <> " AND g.chat_ts > ? ORDER BY g.chat_ts ASC LIMIT ?") (params :. (ts, count))
      PTBefore ts count -> DB.query db (query <> " AND g.chat_ts < ? ORDER BY g.chat_ts DESC LIMIT ?") (params :. (ts, count))

getGroupChatPreview_ :: DB.Connection -> VersionRangeChat -> User -> ChatPreviewData 'CTGroup -> ExceptT StoreError IO AChat
getGroupChatPreview_ db vr user (GroupChatPD _ groupId lastItemId_ stats) = do
  groupInfo <- getGroupInfo db vr user groupId
  lastItem <- case lastItemId_ of
    Just lastItemId -> (: []) <$> getGroupCIWithReactions db user groupInfo lastItemId
    Nothing -> pure []
  pure $ AChat SCTGroup (Chat (GroupChat groupInfo) lastItem stats)

findLocalChatPreviews_ :: DB.Connection -> User -> PaginationByTime -> ChatListQuery -> IO [AChatPreviewData]
findLocalChatPreviews_ db User {userId} pagination clq =
  map toPreview <$> getPreviews
  where
    toPreview :: (NoteFolderId, UTCTime, Maybe ChatItemId) :. ChatStatsRow -> AChatPreviewData
    toPreview ((noteFolderId, ts, lastItemId_) :. statsRow) =
      ACPD SCTLocal $ LocalChatPD ts noteFolderId lastItemId_ (toChatStats statsRow)
    baseQuery =
      [sql|
        SELECT
          nf.note_folder_id,
          nf.chat_ts,
          (
            SELECT chat_item_id
            FROM chat_items ci
            WHERE ci.user_id = ? AND ci.note_folder_id = nf.note_folder_id
            ORDER BY ci.created_at DESC
            LIMIT 1
          ) AS chat_item_id,
          COALESCE(ChatStats.UnreadCount, 0),
          0,
          COALESCE(ChatStats.MinUnread, 0),
          nf.unread_chat
        FROM note_folders nf
        LEFT JOIN (
          SELECT note_folder_id, COUNT(1) AS UnreadCount, MIN(chat_item_id) AS MinUnread
          FROM chat_items
          WHERE user_id = ? AND note_folder_id IS NOT NULL AND item_status = ?
          GROUP BY note_folder_id
        ) ChatStats ON ChatStats.note_folder_id = nf.note_folder_id
      |]
    baseParams = (userId, userId, CISRcvNew)
    getPreviews = case clq of
      CLQFilters {favorite = False, unread = False} -> do
        let q = baseQuery <> " WHERE nf.user_id = ?"
            p = baseParams :. Only userId
        queryWithPagination q p
      CLQFilters {favorite = True, unread = False} -> do
        let q =
              baseQuery
                <> " "
                <> [sql|
                      WHERE nf.user_id = ?
                        AND nf.favorite = 1
                   |]
            p = baseParams :. Only userId
        queryWithPagination q p
      CLQFilters {favorite = False, unread = True} -> do
        let q =
              baseQuery
                <> " "
                <> [sql|
                      WHERE nf.user_id = ?
                        AND (nf.unread_chat = 1 OR ChatStats.UnreadCount > 0)
                   |]
            p = baseParams :. Only userId
        queryWithPagination q p
      CLQFilters {favorite = True, unread = True} -> do
        let q =
              baseQuery
                <> " "
                <> [sql|
                      WHERE nf.user_id = ?
                        AND (nf.favorite = 1
                          OR nf.unread_chat = 1 OR ChatStats.UnreadCount > 0)
                   |]
            p = baseParams :. Only userId
        queryWithPagination q p
      CLQSearch {} -> pure []
    queryWithPagination :: ToRow p => Query -> p -> IO [(NoteFolderId, UTCTime, Maybe ChatItemId) :. ChatStatsRow]
    queryWithPagination query params = case pagination of
      PTLast count -> DB.query db (query <> " ORDER BY nf.chat_ts DESC LIMIT ?") (params :. Only count)
      PTAfter ts count -> DB.query db (query <> " AND nf.chat_ts > ? ORDER BY nf.chat_ts ASC LIMIT ?") (params :. (ts, count))
      PTBefore ts count -> DB.query db (query <> " AND nf.chat_ts < ? ORDER BY nf.chat_ts DESC LIMIT ?") (params :. (ts, count))

getLocalChatPreview_ :: DB.Connection -> User -> ChatPreviewData 'CTLocal -> ExceptT StoreError IO AChat
getLocalChatPreview_ db user (LocalChatPD _ noteFolderId lastItemId_ stats) = do
  nf <- getNoteFolder db user noteFolderId
  lastItem <- case lastItemId_ of
    Just lastItemId -> (: []) <$> getLocalChatItem db user noteFolderId lastItemId
    Nothing -> pure []
  pure $ AChat SCTLocal (Chat (LocalChat nf) lastItem stats)

-- this function can be changed so it never fails, not only avoid failure on invalid json
toLocalChatItem :: UTCTime -> ChatItemRow -> Either StoreError (CChatItem 'CTLocal)
toLocalChatItem currentTs ((itemId, itemTs, AMsgDirection msgDir, itemContentText, itemText, itemStatus, sentViaProxy, sharedMsgId) :. (itemDeleted, deletedTs, itemEdited, createdAt, updatedAt) :. forwardedFromRow :. (timedTTL, timedDeleteAt, itemLive, BI userMention) :. (fileId_, fileName_, fileSize_, filePath, fileKey, fileNonce, fileStatus_, fileProtocol_)) =
  chatItem $ fromRight invalid $ dbParseACIContent itemContentText
  where
    invalid = ACIContent msgDir $ CIInvalidJSON itemContentText
    chatItem itemContent = case (itemContent, itemStatus, fileStatus_) of
      (ACIContent SMDSnd ciContent, ACIStatus SMDSnd ciStatus, Just (AFS SMDSnd fileStatus)) ->
        Right $ cItem SMDSnd CILocalSnd ciStatus ciContent (maybeCIFile fileStatus)
      (ACIContent SMDSnd ciContent, ACIStatus SMDSnd ciStatus, Nothing) ->
        Right $ cItem SMDSnd CILocalSnd ciStatus ciContent Nothing
      (ACIContent SMDRcv ciContent, ACIStatus SMDRcv ciStatus, Just (AFS SMDRcv fileStatus)) ->
        Right $ cItem SMDRcv CILocalRcv ciStatus ciContent (maybeCIFile fileStatus)
      (ACIContent SMDRcv ciContent, ACIStatus SMDRcv ciStatus, Nothing) ->
        Right $ cItem SMDRcv CILocalRcv ciStatus ciContent Nothing
      _ -> badItem
    maybeCIFile :: CIFileStatus d -> Maybe (CIFile d)
    maybeCIFile fileStatus =
      case (fileId_, fileName_, fileSize_, fileProtocol_) of
        (Just fileId, Just fileName, Just fileSize, Just fileProtocol) ->
          let cfArgs = CFArgs <$> fileKey <*> fileNonce
              fileSource = (`CryptoFile` cfArgs) <$> filePath
           in Just CIFile {fileId, fileName, fileSize, fileSource, fileStatus, fileProtocol}
        _ -> Nothing
    cItem :: MsgDirectionI d => SMsgDirection d -> CIDirection 'CTLocal d -> CIStatus d -> CIContent d -> Maybe (CIFile d) -> CChatItem 'CTLocal
    cItem d chatDir ciStatus content file =
      CChatItem d ChatItem {chatDir, meta = ciMeta content ciStatus, content, mentions = M.empty, formattedText = parseMaybeMarkdownList itemText, quotedItem = Nothing, reactions = [], file}
    badItem = Left $ SEBadChatItem itemId (Just itemTs)
    ciMeta :: CIContent d -> CIStatus d -> CIMeta 'CTLocal d
    ciMeta content status =
      let itemDeleted' = case itemDeleted of
            DBCINotDeleted -> Nothing
            _ -> Just (CIDeleted @'CTLocal deletedTs)
          itemEdited' = maybe False unBI itemEdited
          itemForwarded = toCIForwardedFrom forwardedFromRow
       in mkCIMeta itemId content itemText status (unBI <$> sentViaProxy) sharedMsgId itemForwarded itemDeleted' itemEdited' ciTimed (unBI <$> itemLive) userMention currentTs itemTs Nothing createdAt updatedAt
    ciTimed :: Maybe CITimed
    ciTimed = timedTTL >>= \ttl -> Just CITimed {ttl, deleteAt = timedDeleteAt}

getContactRequestChatPreviews_ :: DB.Connection -> User -> PaginationByTime -> ChatListQuery -> IO [AChatPreviewData]
getContactRequestChatPreviews_ db User {userId} pagination clq = case clq of
  CLQFilters {favorite = False, unread = False} -> map toPreview <$> getPreviews ""
  CLQFilters {favorite = True, unread = False} -> pure []
  CLQFilters {favorite = False, unread = True} -> map toPreview <$> getPreviews ""
  CLQFilters {favorite = True, unread = True} -> map toPreview <$> getPreviews ""
  CLQSearch {search} -> map toPreview <$> getPreviews search
  where
    query =
      [sql|
        SELECT
          cr.contact_request_id, cr.local_display_name, cr.agent_invitation_id, cr.contact_id, cr.user_contact_link_id,
          c.agent_conn_id, cr.contact_profile_id, p.display_name, p.full_name, p.image, p.contact_link, cr.xcontact_id, cr.pq_support, p.preferences,
          cr.created_at, cr.updated_at,
          cr.peer_chat_min_version, cr.peer_chat_max_version
        FROM contact_requests cr
        JOIN connections c ON c.user_contact_link_id = cr.user_contact_link_id
        JOIN contact_profiles p ON p.contact_profile_id = cr.contact_profile_id
        JOIN user_contact_links uc ON uc.user_contact_link_id = cr.user_contact_link_id
        WHERE cr.user_id = ?
          AND uc.user_id = ?
          AND uc.local_display_name = ''
          AND uc.group_id IS NULL
          AND (
            LOWER(cr.local_display_name) LIKE '%' || LOWER(?) || '%'
            OR LOWER(p.display_name) LIKE '%' || LOWER(?) || '%'
            OR LOWER(p.full_name) LIKE '%' || LOWER(?) || '%'
          )
      |]
    params search = (userId, userId, search, search, search)
    getPreviews search = case pagination of
      PTLast count -> DB.query db (query <> " ORDER BY cr.updated_at DESC LIMIT ?") (params search :. Only count)
      PTAfter ts count -> DB.query db (query <> " AND cr.updated_at > ? ORDER BY cr.updated_at ASC LIMIT ?") (params search :. (ts, count))
      PTBefore ts count -> DB.query db (query <> " AND cr.updated_at < ? ORDER BY cr.updated_at DESC LIMIT ?") (params search :. (ts, count))
    toPreview :: ContactRequestRow -> AChatPreviewData
    toPreview cReqRow =
      let cReq@UserContactRequest {updatedAt} = toContactRequest cReqRow
          aChat = AChat SCTContactRequest $ Chat (ContactRequest cReq) [] emptyChatStats
       in ACPD SCTContactRequest $ ContactRequestPD updatedAt aChat

getContactConnectionChatPreviews_ :: DB.Connection -> User -> PaginationByTime -> ChatListQuery -> IO [AChatPreviewData]
getContactConnectionChatPreviews_ db User {userId} pagination clq = case clq of
  CLQFilters {favorite = False, unread = False} -> map toPreview <$> getPreviews ""
  CLQFilters {favorite = True, unread = False} -> pure []
  CLQFilters {favorite = False, unread = True} -> pure []
  CLQFilters {favorite = True, unread = True} -> pure []
  CLQSearch {search} -> map toPreview <$> getPreviews search
  where
    query =
      [sql|
        SELECT
          connection_id, agent_conn_id, conn_status, via_contact_uri_hash, via_user_contact_link, group_link_id,
          custom_user_profile_id, conn_req_inv, short_link_inv, local_alias, created_at, updated_at
        FROM connections
        WHERE user_id = ?
          AND conn_type = ?
          AND conn_status != ?
          AND contact_id IS NULL
          AND conn_level = 0
          AND via_contact IS NULL
          AND (via_group_link = 0 OR (via_group_link = 1 AND group_link_id IS NOT NULL))
          AND LOWER(local_alias) LIKE '%' || LOWER(?) || '%'
      |]
    params search = (userId, ConnContact, ConnPrepared, search)
    getPreviews search = case pagination of
      PTLast count -> DB.query db (query <> " ORDER BY updated_at DESC LIMIT ?") (params search :. Only count)
      PTAfter ts count -> DB.query db (query <> " AND updated_at > ? ORDER BY updated_at ASC LIMIT ?") (params search :. (ts, count))
      PTBefore ts count -> DB.query db (query <> " AND updated_at < ? ORDER BY updated_at DESC LIMIT ?") (params search :. (ts, count))
    toPreview :: (Int64, ConnId, ConnStatus, Maybe ByteString, Maybe Int64, Maybe GroupLinkId, Maybe Int64, Maybe ConnReqInvitation, Maybe (ConnShortLink 'CMInvitation), LocalAlias, UTCTime, UTCTime) -> AChatPreviewData
    toPreview connRow =
      let conn@PendingContactConnection {updatedAt} = toPendingContactConnection connRow
          aChat = AChat SCTContactConnection $ Chat (ContactConnection conn) [] emptyChatStats
       in ACPD SCTContactConnection $ ContactConnectionPD updatedAt aChat

getDirectChat :: DB.Connection -> VersionRangeChat -> User -> Int64 -> ChatPagination -> Maybe String -> ExceptT StoreError IO (Chat 'CTDirect, Maybe NavigationInfo)
getDirectChat db vr user contactId pagination search_ = do
  let search = fromMaybe "" search_
  ct <- getContact db vr user contactId
  case pagination of
    CPLast count -> liftIO $ (,Nothing) <$> getDirectChatLast_ db user ct count search
    CPAfter afterId count -> (,Nothing) <$> getDirectChatAfter_ db user ct afterId count search
    CPBefore beforeId count -> (,Nothing) <$> getDirectChatBefore_ db user ct beforeId count search
    CPAround aroundId count -> getDirectChatAround_ db user ct aroundId count search
    CPInitial count -> do
      unless (null search) $ throwError $ SEInternalError "initial chat pagination doesn't support search"
      getDirectChatInitial_ db user ct count

-- the last items in reverse order (the last item in the conversation is the first in the returned list)
getDirectChatLast_ :: DB.Connection -> User -> Contact -> Int -> String -> IO (Chat 'CTDirect)
getDirectChatLast_ db user ct count search = do
  ciIds <- getDirectChatItemIdsLast_ db user ct count search
  ts <- getCurrentTime
  cis <- mapM (safeGetDirectItem db user ct ts) ciIds
  pure $ Chat (DirectChat ct) (reverse cis) emptyChatStats

getDirectChatItemIdsLast_ :: DB.Connection -> User -> Contact -> Int -> String -> IO [ChatItemId]
getDirectChatItemIdsLast_ db User {userId} Contact {contactId} count search =
  map fromOnly
    <$> DB.query
      db
      [sql|
        SELECT chat_item_id
        FROM chat_items
        WHERE user_id = ? AND contact_id = ? AND LOWER(item_text) LIKE '%' || LOWER(?) || '%'
        ORDER BY created_at DESC, chat_item_id DESC
        LIMIT ?
      |]
      (userId, contactId, search, count)

safeGetDirectItem :: DB.Connection -> User -> Contact -> UTCTime -> ChatItemId -> IO (CChatItem 'CTDirect)
safeGetDirectItem db user ct currentTs itemId =
  runExceptT (getDirectCIWithReactions db user ct itemId)
    >>= pure <$> safeToDirectItem currentTs itemId

safeToDirectItem :: UTCTime -> ChatItemId -> Either StoreError (CChatItem 'CTDirect) -> CChatItem 'CTDirect
safeToDirectItem currentTs itemId = \case
  Right ci -> ci
  Left e@(SEBadChatItem _ (Just itemTs)) -> badDirectItem itemTs e
  Left e -> badDirectItem currentTs e
  where
    badDirectItem :: UTCTime -> StoreError -> CChatItem 'CTDirect
    badDirectItem ts e =
      let errorText = T.pack $ show e
       in CChatItem
            SMDSnd
            ChatItem
              { chatDir = CIDirectSnd,
                meta = dummyMeta itemId ts errorText,
                content = CIInvalidJSON errorText,
                mentions = M.empty,
                formattedText = Nothing,
                quotedItem = Nothing,
                reactions = [],
                file = Nothing
              }

getDirectChatItemLast :: DB.Connection -> User -> ContactId -> ExceptT StoreError IO (CChatItem 'CTDirect)
getDirectChatItemLast db user@User {userId} contactId = do
  chatItemId <-
    ExceptT . firstRow fromOnly (SEChatItemNotFoundByContactId contactId) $
      DB.query
        db
        [sql|
          SELECT chat_item_id
          FROM chat_items
          WHERE user_id = ? AND contact_id = ?
          ORDER BY created_at DESC, chat_item_id DESC
          LIMIT 1
        |]
        (userId, contactId)
  getDirectChatItem db user contactId chatItemId

getDirectChatAfter_ :: DB.Connection -> User -> Contact -> ChatItemId -> Int -> String -> ExceptT StoreError IO (Chat 'CTDirect)
getDirectChatAfter_ db user ct@Contact {contactId} afterId count search = do
  afterCI <- getDirectChatItem db user contactId afterId
  ciIds <- liftIO $ getDirectCIsAfter_ db user ct afterCI count search
  ts <- liftIO getCurrentTime
  cis <- liftIO $ mapM (safeGetDirectItem db user ct ts) ciIds
  pure $ Chat (DirectChat ct) cis emptyChatStats

getDirectCIsAfter_ :: DB.Connection -> User -> Contact -> CChatItem 'CTDirect -> Int -> String -> IO [ChatItemId]
getDirectCIsAfter_ db User {userId} Contact {contactId} afterCI count search =
  map fromOnly
    <$> DB.query
      db
      [sql|
        SELECT chat_item_id
        FROM chat_items
        WHERE user_id = ? AND contact_id = ? AND LOWER(item_text) LIKE '%' || LOWER(?) || '%'
          AND (created_at > ? OR (created_at = ? AND chat_item_id > ?))
        ORDER BY created_at ASC, chat_item_id ASC
        LIMIT ?
      |]
      (userId, contactId, search, ciCreatedAt afterCI, ciCreatedAt afterCI, cChatItemId afterCI, count)

getDirectChatBefore_ :: DB.Connection -> User -> Contact -> ChatItemId -> Int -> String -> ExceptT StoreError IO (Chat 'CTDirect)
getDirectChatBefore_ db user ct@Contact {contactId} beforeId count search = do
  beforeCI <- getDirectChatItem db user contactId beforeId
  ciIds <- liftIO $ getDirectCIsBefore_ db user ct beforeCI count search
  ts <- liftIO getCurrentTime
  cis <- liftIO $ mapM (safeGetDirectItem db user ct ts) ciIds
  pure $ Chat (DirectChat ct) (reverse cis) emptyChatStats

getDirectCIsBefore_ :: DB.Connection -> User -> Contact -> CChatItem 'CTDirect -> Int -> String -> IO [ChatItemId]
getDirectCIsBefore_ db User {userId} Contact {contactId} beforeCI count search =
  map fromOnly
    <$> DB.query
      db
      [sql|
        SELECT chat_item_id
        FROM chat_items
        WHERE user_id = ? AND contact_id = ? AND LOWER(item_text) LIKE '%' || LOWER(?) || '%'
          AND (created_at < ? OR (created_at = ? AND chat_item_id < ?))
        ORDER BY created_at DESC, chat_item_id DESC
        LIMIT ?
      |]
      (userId, contactId, search, ciCreatedAt beforeCI, ciCreatedAt beforeCI, cChatItemId beforeCI, count)

getDirectChatAround_ :: DB.Connection -> User -> Contact -> ChatItemId -> Int -> String -> ExceptT StoreError IO (Chat 'CTDirect, Maybe NavigationInfo)
getDirectChatAround_ db user ct aroundId count search = do
  stats <- liftIO $ getContactStats_ db user ct
  getDirectChatAround' db user ct aroundId count search stats

getDirectChatAround' :: DB.Connection -> User -> Contact -> ChatItemId -> Int -> String -> ChatStats -> ExceptT StoreError IO (Chat 'CTDirect, Maybe NavigationInfo)
getDirectChatAround' db user ct@Contact {contactId} aroundId count search stats = do
  aroundCI <- getDirectChatItem db user contactId aroundId
  beforeIds <- liftIO $ getDirectCIsBefore_ db user ct aroundCI count search
  afterIds <- liftIO $ getDirectCIsAfter_ db user ct aroundCI count search
  ts <- liftIO getCurrentTime
  beforeCIs <- liftIO $ mapM (safeGetDirectItem db user ct ts) beforeIds
  afterCIs <- liftIO $ mapM (safeGetDirectItem db user ct ts) afterIds
  let cis = reverse beforeCIs <> [aroundCI] <> afterCIs
  navInfo <- liftIO $ getNavInfo cis
  pure (Chat (DirectChat ct) cis stats, Just navInfo)
  where
    getNavInfo cis_ = case cis_ of
      [] -> pure $ NavigationInfo 0 0
      cis -> getContactNavInfo_ db user ct (last cis)

getDirectChatInitial_ :: DB.Connection -> User -> Contact -> Int -> ExceptT StoreError IO (Chat 'CTDirect, Maybe NavigationInfo)
getDirectChatInitial_ db user ct count = do
  liftIO (getContactMinUnreadId_ db user ct) >>= \case
    Just minUnreadItemId -> do
      unreadCount <- liftIO $ getContactUnreadCount_ db user ct
      let stats = emptyChatStats {unreadCount, minUnreadItemId}
      getDirectChatAround' db user ct minUnreadItemId count "" stats
    Nothing -> liftIO $ (,Just $ NavigationInfo 0 0) <$> getDirectChatLast_ db user ct count ""

getContactStats_ :: DB.Connection -> User -> Contact -> IO ChatStats
getContactStats_ db user ct = do
  minUnreadItemId <- fromMaybe 0 <$> getContactMinUnreadId_ db user ct
  unreadCount <- getContactUnreadCount_ db user ct
  pure emptyChatStats {unreadCount, minUnreadItemId}

getContactMinUnreadId_ :: DB.Connection -> User -> Contact -> IO (Maybe ChatItemId)
getContactMinUnreadId_ db User {userId} Contact {contactId} =
  fmap join . maybeFirstRow fromOnly $
    DB.query
      db
      [sql|
        SELECT chat_item_id
        FROM chat_items
        WHERE user_id = ? AND contact_id = ? AND item_status = ?
        ORDER BY created_at ASC, chat_item_id ASC
        LIMIT 1
      |]
      (userId, contactId, CISRcvNew)

getContactUnreadCount_ :: DB.Connection -> User -> Contact -> IO Int
getContactUnreadCount_ db User {userId} Contact {contactId} =
  fromOnly . head
    <$> DB.query
      db
      [sql|
        SELECT COUNT(1)
        FROM chat_items
        WHERE user_id = ? AND contact_id = ? AND item_status = ?
      |]
      (userId, contactId, CISRcvNew)

getContactNavInfo_ :: DB.Connection -> User -> Contact -> CChatItem 'CTDirect -> IO NavigationInfo
getContactNavInfo_ db User {userId} Contact {contactId} afterCI = do
  afterUnread <- getAfterUnreadCount
  afterTotal <- getAfterTotalCount
  pure NavigationInfo {afterUnread, afterTotal}
  where
    getAfterUnreadCount :: IO Int
    getAfterUnreadCount =
      fromOnly . head
        <$> DB.query
          db
          [sql|
            SELECT COUNT(1)
            FROM (
              SELECT 1
              FROM chat_items
              WHERE user_id = ? AND contact_id = ? AND item_status = ?
                AND created_at > ?
              UNION ALL
              SELECT 1
              FROM chat_items
              WHERE user_id = ? AND contact_id = ? AND item_status = ?
                AND created_at = ? AND chat_item_id > ?
            ) ci
          |]
          ( (userId, contactId, CISRcvNew, ciCreatedAt afterCI)
              :. (userId, contactId, CISRcvNew, ciCreatedAt afterCI, cChatItemId afterCI)
          )
    getAfterTotalCount :: IO Int
    getAfterTotalCount =
      fromOnly . head
        <$> DB.query
          db
          [sql|
            SELECT COUNT(1)
            FROM (
              SELECT 1
              FROM chat_items
              WHERE user_id = ? AND contact_id = ?
                AND created_at > ?
              UNION ALL
              SELECT 1
              FROM chat_items
              WHERE user_id = ? AND contact_id = ?
                AND created_at = ? AND chat_item_id > ?
            ) ci
          |]
          ( (userId, contactId, ciCreatedAt afterCI)
              :. (userId, contactId, ciCreatedAt afterCI, cChatItemId afterCI)
          )

getGroupChat :: DB.Connection -> VersionRangeChat -> User -> Int64 -> Maybe MsgContentTag -> ChatPagination -> Maybe String -> ExceptT StoreError IO (Chat 'CTGroup, Maybe NavigationInfo)
getGroupChat db vr user groupId contentFilter pagination search_ = do
  let search = fromMaybe "" search_
  g <- getGroupInfo db vr user groupId
  case pagination of
    CPLast count -> liftIO $ (,Nothing) <$> getGroupChatLast_ db user g contentFilter count search emptyChatStats
    CPAfter afterId count -> (,Nothing) <$> getGroupChatAfter_ db user g contentFilter afterId count search
    CPBefore beforeId count -> (,Nothing) <$> getGroupChatBefore_ db user g contentFilter beforeId count search
    CPAround aroundId count -> getGroupChatAround_ db user g contentFilter aroundId count search
    CPInitial count -> do
      unless (null search) $ throwError $ SEInternalError "initial chat pagination doesn't support search"
      getGroupChatInitial_ db user g contentFilter count

getGroupChatLast_ :: DB.Connection -> User -> GroupInfo -> Maybe MsgContentTag -> Int -> String -> ChatStats -> IO (Chat 'CTGroup)
getGroupChatLast_ db user g contentFilter count search stats = do
  ciIds <- getGroupChatItemIDs db user g contentFilter GRLast count search
  ts <- getCurrentTime
  cis <- mapM (safeGetGroupItem db user g ts) ciIds
  pure $ Chat (GroupChat g) (reverse cis) stats

data GroupItemIDsRange = GRLast | GRAfter UTCTime ChatItemId | GRBefore UTCTime ChatItemId

getGroupChatItemIDs :: DB.Connection -> User -> GroupInfo -> Maybe MsgContentTag -> GroupItemIDsRange -> Int -> String -> IO [ChatItemId]
getGroupChatItemIDs db User {userId} GroupInfo {groupId} contentFilter range count search = case contentFilter of
  Just mcTag -> idsQuery (baseCond <> " AND msg_content_tag = ? ") (userId, groupId, mcTag)
  Nothing -> idsQuery baseCond (userId, groupId)
  where
    baseQuery = " SELECT chat_item_id FROM chat_items WHERE "
    baseCond = " user_id = ? AND group_id = ? "
    idsQuery :: ToRow p => Query -> p -> IO [ChatItemId]
    idsQuery c p = case range of
      GRLast -> rangeQuery c p " ORDER BY item_ts DESC, chat_item_id DESC "
      GRAfter ts itemId ->
        rangeQuery
          (" item_ts > ? " `orCond` " item_ts = ? AND chat_item_id > ? ")
          (orParams ts itemId)
          " ORDER BY item_ts ASC, chat_item_id ASC "
      GRBefore ts itemId ->
        rangeQuery
          (" item_ts < ? " `orCond` " item_ts = ? AND chat_item_id < ? ")
          (orParams ts itemId)
          " ORDER BY item_ts DESC, chat_item_id DESC "
      where
        orCond c1 c2 = " ((" <> c <> " AND " <> c1 <> ") OR (" <> c <> " AND " <> c2 <> ")) "
        orParams ts itemId = (p :. (Only ts) :. p :. (ts, itemId))
    rangeQuery :: ToRow p => Query -> p -> Query -> IO [ChatItemId]
    rangeQuery c p ob
      | null search = searchQuery "" ()
      | otherwise = searchQuery " AND LOWER(item_text) LIKE '%' || LOWER(?) || '%' " (Only search)
      where
        searchQuery :: ToRow p' => Query -> p' -> IO [ChatItemId]
        searchQuery c' p' =
          map fromOnly <$> DB.query db (baseQuery <> c <> c' <> ob <> " LIMIT ?") (p :. p' :. Only count)

safeGetGroupItem :: DB.Connection -> User -> GroupInfo -> UTCTime -> ChatItemId -> IO (CChatItem 'CTGroup)
safeGetGroupItem db user g currentTs itemId =
  runExceptT (getGroupCIWithReactions db user g itemId)
    >>= pure <$> safeToGroupItem currentTs itemId

safeToGroupItem :: UTCTime -> ChatItemId -> Either StoreError (CChatItem 'CTGroup) -> CChatItem 'CTGroup
safeToGroupItem currentTs itemId = \case
  Right ci -> ci
  Left e@(SEBadChatItem _ (Just itemTs)) -> badGroupItem itemTs e
  Left e -> badGroupItem currentTs e
  where
    badGroupItem :: UTCTime -> StoreError -> CChatItem 'CTGroup
    badGroupItem ts e =
      let errorText = T.pack $ show e
       in CChatItem
            SMDSnd
            ChatItem
              { chatDir = CIGroupSnd,
                meta = dummyMeta itemId ts errorText,
                content = CIInvalidJSON errorText,
                mentions = M.empty,
                formattedText = Nothing,
                quotedItem = Nothing,
                reactions = [],
                file = Nothing
              }

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

getGroupChatAfter_ :: DB.Connection -> User -> GroupInfo -> Maybe MsgContentTag -> ChatItemId -> Int -> String -> ExceptT StoreError IO (Chat 'CTGroup)
getGroupChatAfter_ db user g@GroupInfo {groupId} contentFilter afterId count search = do
  afterCI <- getGroupChatItem db user groupId afterId
  let range = GRAfter (chatItemTs afterCI) (cChatItemId afterCI)
  ciIds <- liftIO $ getGroupChatItemIDs db user g contentFilter range count search
  ts <- liftIO getCurrentTime
  cis <- liftIO $ mapM (safeGetGroupItem db user g ts) ciIds
  pure $ Chat (GroupChat g) cis emptyChatStats

getGroupChatBefore_ :: DB.Connection -> User -> GroupInfo -> Maybe MsgContentTag -> ChatItemId -> Int -> String -> ExceptT StoreError IO (Chat 'CTGroup)
getGroupChatBefore_ db user g@GroupInfo {groupId} contentFilter beforeId count search = do
  beforeCI <- getGroupChatItem db user groupId beforeId
  let range = GRBefore (chatItemTs beforeCI) (cChatItemId beforeCI)
  ciIds <- liftIO $ getGroupChatItemIDs db user g contentFilter range count search
  ts <- liftIO getCurrentTime
  cis <- liftIO $ mapM (safeGetGroupItem db user g ts) ciIds
  pure $ Chat (GroupChat g) (reverse cis) emptyChatStats

getGroupChatAround_ :: DB.Connection -> User -> GroupInfo -> Maybe MsgContentTag -> ChatItemId -> Int -> String -> ExceptT StoreError IO (Chat 'CTGroup, Maybe NavigationInfo)
getGroupChatAround_ db user g contentFilter aroundId count search = do
  stats <- liftIO $ getGroupStats_ db user g
  getGroupChatAround' db user g contentFilter aroundId count search stats

getGroupChatAround' :: DB.Connection -> User -> GroupInfo -> Maybe MsgContentTag -> ChatItemId -> Int -> String -> ChatStats -> ExceptT StoreError IO (Chat 'CTGroup, Maybe NavigationInfo)
getGroupChatAround' db user g contentFilter aroundId count search stats = do
  aroundCI <- getGroupCIWithReactions db user g aroundId
  let beforeRange = GRBefore (chatItemTs aroundCI) (cChatItemId aroundCI)
      afterRange = GRAfter (chatItemTs aroundCI) (cChatItemId aroundCI)
  beforeIds <- liftIO $ getGroupChatItemIDs db user g contentFilter beforeRange count search
  afterIds <- liftIO $ getGroupChatItemIDs db user g contentFilter afterRange count search
  ts <- liftIO getCurrentTime
  beforeCIs <- liftIO $ mapM (safeGetGroupItem db user g ts) beforeIds
  afterCIs <- liftIO $ mapM (safeGetGroupItem db user g ts) afterIds
  let cis = reverse beforeCIs <> [aroundCI] <> afterCIs
  navInfo <- liftIO $ getNavInfo cis
  pure (Chat (GroupChat g) cis stats, Just navInfo)
  where
    getNavInfo cis_ = case cis_ of
      [] -> pure $ NavigationInfo 0 0
      cis -> getGroupNavInfo_ db user g (last cis)

getGroupChatInitial_ :: DB.Connection -> User -> GroupInfo -> Maybe MsgContentTag -> Int -> ExceptT StoreError IO (Chat 'CTGroup, Maybe NavigationInfo)
getGroupChatInitial_ db user g contentFilter count = do
  liftIO (getGroupMinUnreadId_ db user g contentFilter) >>= \case
    Just minUnreadItemId -> do
      stats <- liftIO $ getStats minUnreadItemId =<< getGroupUnreadCount_ db user g Nothing
      getGroupChatAround' db user g contentFilter minUnreadItemId count "" stats
    Nothing -> liftIO $ do
      stats <- getStats 0 (0, 0)
      (,Just $ NavigationInfo 0 0) <$> getGroupChatLast_ db user g contentFilter count "" stats
  where
    getStats minUnreadItemId (unreadCount, unreadMentions) = do
      reportsCount <- getGroupReportsCount_ db user g False
      pure ChatStats {unreadCount, unreadMentions, reportsCount, minUnreadItemId, unreadChat = False}

getGroupStats_ :: DB.Connection -> User -> GroupInfo -> IO ChatStats
getGroupStats_ db user g = do
  minUnreadItemId <- fromMaybe 0 <$> getGroupMinUnreadId_ db user g Nothing
  (unreadCount, unreadMentions) <- getGroupUnreadCount_ db user g Nothing
  reportsCount <- getGroupReportsCount_ db user g False
  pure ChatStats {unreadCount, unreadMentions, reportsCount, minUnreadItemId, unreadChat = False}

getGroupMinUnreadId_ :: DB.Connection -> User -> GroupInfo -> Maybe MsgContentTag -> IO (Maybe ChatItemId)
getGroupMinUnreadId_ db user g contentFilter =
  fmap join . maybeFirstRow fromOnly $
    queryUnreadGroupItems db user g contentFilter baseQuery orderLimit
  where
    baseQuery = "SELECT chat_item_id FROM chat_items WHERE user_id = ? AND group_id = ? "
    orderLimit = " ORDER BY item_ts ASC, chat_item_id ASC LIMIT 1"

getGroupUnreadCount_ :: DB.Connection -> User -> GroupInfo -> Maybe MsgContentTag -> IO (Int, Int)
getGroupUnreadCount_ db user g contentFilter =
  head <$> queryUnreadGroupItems db user g contentFilter baseQuery ""
  where
    baseQuery = "SELECT COUNT(1), COALESCE(SUM(user_mention), 0) FROM chat_items WHERE user_id = ? AND group_id = ? "

getGroupReportsCount_ :: DB.Connection -> User -> GroupInfo -> Bool -> IO Int
getGroupReportsCount_ db User {userId} GroupInfo {groupId} archived =
  fromOnly . head
    <$> DB.query
      db
      "SELECT COUNT(1) FROM chat_items WHERE user_id = ? AND group_id = ? AND msg_content_tag = ? AND item_deleted = ? AND item_sent = 0"
      (userId, groupId, MCReport_, BI archived)

queryUnreadGroupItems :: FromRow r => DB.Connection -> User -> GroupInfo -> Maybe MsgContentTag -> Query -> Query -> IO [r]
queryUnreadGroupItems db User {userId} GroupInfo {groupId} contentFilter baseQuery orderLimit =
  case contentFilter of
    Just mcTag ->
      DB.query
        db
        (baseQuery <> " AND msg_content_tag = ? AND item_status = ? " <> orderLimit)
        (userId, groupId, mcTag, CISRcvNew)
    Nothing ->
      DB.query
        db
        (baseQuery <> " AND item_status = ? " <> orderLimit)
        (userId, groupId, CISRcvNew)

getGroupNavInfo_ :: DB.Connection -> User -> GroupInfo -> CChatItem 'CTGroup -> IO NavigationInfo
getGroupNavInfo_ db User {userId} GroupInfo {groupId} afterCI = do
  afterUnread <- getAfterUnreadCount
  afterTotal <- getAfterTotalCount
  pure NavigationInfo {afterUnread, afterTotal}
  where
    getAfterUnreadCount :: IO Int
    getAfterUnreadCount =
      fromOnly . head
        <$> DB.query
          db
          [sql|
            SELECT COUNT(1)
            FROM (
              SELECT 1
              FROM chat_items
              WHERE user_id = ? AND group_id = ? AND item_status = ?
                AND item_ts > ?
              UNION ALL
              SELECT 1
              FROM chat_items
              WHERE user_id = ? AND group_id = ? AND item_status = ?
                AND item_ts = ? AND chat_item_id > ?
            ) ci
          |]
          ( (userId, groupId, CISRcvNew, chatItemTs afterCI)
              :. (userId, groupId, CISRcvNew, chatItemTs afterCI, cChatItemId afterCI)
          )
    getAfterTotalCount :: IO Int
    getAfterTotalCount =
      fromOnly . head
        <$> DB.query
          db
          [sql|
            SELECT COUNT(1)
            FROM (
              SELECT 1
              FROM chat_items
              WHERE user_id = ? AND group_id = ?
                AND item_ts > ?
              UNION ALL
              SELECT 1
              FROM chat_items
              WHERE user_id = ? AND group_id = ?
                AND item_ts = ? AND chat_item_id > ?
            ) ci
          |]
          ( (userId, groupId, chatItemTs afterCI)
              :. (userId, groupId, chatItemTs afterCI, cChatItemId afterCI)
          )

getLocalChat :: DB.Connection -> User -> Int64 -> ChatPagination -> Maybe String -> ExceptT StoreError IO (Chat 'CTLocal, Maybe NavigationInfo)
getLocalChat db user folderId pagination search_ = do
  let search = fromMaybe "" search_
  nf <- getNoteFolder db user folderId
  case pagination of
    CPLast count -> liftIO $ (,Nothing) <$> getLocalChatLast_ db user nf count search
    CPAfter afterId count -> (,Nothing) <$> getLocalChatAfter_ db user nf afterId count search
    CPBefore beforeId count -> (,Nothing) <$> getLocalChatBefore_ db user nf beforeId count search
    CPAround aroundId count -> getLocalChatAround_ db user nf aroundId count search
    CPInitial count -> do
      unless (null search) $ throwError $ SEInternalError "initial chat pagination doesn't support search"
      getLocalChatInitial_ db user nf count

getLocalChatLast_ :: DB.Connection -> User -> NoteFolder -> Int -> String -> IO (Chat 'CTLocal)
getLocalChatLast_ db user nf count search = do
  ciIds <- getLocalChatItemIdsLast_ db user nf count search
  ts <- getCurrentTime
  cis <- mapM (safeGetLocalItem db user nf ts) ciIds
  pure $ Chat (LocalChat nf) (reverse cis) emptyChatStats

getLocalChatItemIdsLast_ :: DB.Connection -> User -> NoteFolder -> Int -> String -> IO [ChatItemId]
getLocalChatItemIdsLast_ db User {userId} NoteFolder {noteFolderId} count search =
  map fromOnly
    <$> DB.query
      db
      [sql|
        SELECT chat_item_id
        FROM chat_items
        WHERE user_id = ? AND note_folder_id = ? AND LOWER(item_text) LIKE '%' || LOWER(?) || '%'
        ORDER BY created_at DESC, chat_item_id DESC
        LIMIT ?
      |]
      (userId, noteFolderId, search, count)

safeGetLocalItem :: DB.Connection -> User -> NoteFolder -> UTCTime -> ChatItemId -> IO (CChatItem 'CTLocal)
safeGetLocalItem db user NoteFolder {noteFolderId} currentTs itemId =
  runExceptT (getLocalChatItem db user noteFolderId itemId)
    >>= pure <$> safeToLocalItem currentTs itemId

safeToLocalItem :: UTCTime -> ChatItemId -> Either StoreError (CChatItem 'CTLocal) -> CChatItem 'CTLocal
safeToLocalItem currentTs itemId = \case
  Right ci -> ci
  Left e@(SEBadChatItem _ (Just itemTs)) -> badLocalItem itemTs e
  Left e -> badLocalItem currentTs e
  where
    badLocalItem :: UTCTime -> StoreError -> CChatItem 'CTLocal
    badLocalItem ts e =
      let errorText = T.pack $ show e
       in CChatItem
            SMDSnd
            ChatItem
              { chatDir = CILocalSnd,
                meta = dummyMeta itemId ts errorText,
                content = CIInvalidJSON errorText,
                mentions = M.empty,
                formattedText = Nothing,
                quotedItem = Nothing,
                reactions = [],
                file = Nothing
              }

getLocalChatAfter_ :: DB.Connection -> User -> NoteFolder -> ChatItemId -> Int -> String -> ExceptT StoreError IO (Chat 'CTLocal)
getLocalChatAfter_ db user nf@NoteFolder {noteFolderId} afterId count search = do
  afterCI <- getLocalChatItem db user noteFolderId afterId
  ciIds <- liftIO $ getLocalCIsAfter_ db user nf afterCI count search
  ts <- liftIO getCurrentTime
  cis <- liftIO $ mapM (safeGetLocalItem db user nf ts) ciIds
  pure $ Chat (LocalChat nf) cis emptyChatStats

getLocalCIsAfter_ :: DB.Connection -> User -> NoteFolder -> CChatItem 'CTLocal -> Int -> String -> IO [ChatItemId]
getLocalCIsAfter_ db User {userId} NoteFolder {noteFolderId} afterCI count search =
  map fromOnly
    <$> DB.query
      db
      [sql|
        SELECT chat_item_id
        FROM chat_items
        WHERE user_id = ? AND note_folder_id = ? AND LOWER(item_text) LIKE '%' || LOWER(?) || '%'
          AND (created_at > ? OR (created_at = ? AND chat_item_id > ?))
        ORDER BY created_at ASC, chat_item_id ASC
        LIMIT ?
      |]
      (userId, noteFolderId, search, ciCreatedAt afterCI, ciCreatedAt afterCI, cChatItemId afterCI, count)

getLocalChatBefore_ :: DB.Connection -> User -> NoteFolder -> ChatItemId -> Int -> String -> ExceptT StoreError IO (Chat 'CTLocal)
getLocalChatBefore_ db user nf@NoteFolder {noteFolderId} beforeId count search = do
  beforeCI <- getLocalChatItem db user noteFolderId beforeId
  ciIds <- liftIO $ getLocalCIsBefore_ db user nf beforeCI count search
  ts <- liftIO getCurrentTime
  cis <- liftIO $ mapM (safeGetLocalItem db user nf ts) ciIds
  pure $ Chat (LocalChat nf) (reverse cis) emptyChatStats

getLocalCIsBefore_ :: DB.Connection -> User -> NoteFolder -> CChatItem 'CTLocal -> Int -> String -> IO [ChatItemId]
getLocalCIsBefore_ db User {userId} NoteFolder {noteFolderId} beforeCI count search =
  map fromOnly
    <$> DB.query
      db
      [sql|
        SELECT chat_item_id
        FROM chat_items
        WHERE user_id = ? AND note_folder_id = ? AND LOWER(item_text) LIKE '%' || LOWER(?) || '%'
          AND (created_at < ? OR (created_at = ? AND chat_item_id < ?))
        ORDER BY created_at DESC, chat_item_id DESC
        LIMIT ?
      |]
      (userId, noteFolderId, search, ciCreatedAt beforeCI, ciCreatedAt beforeCI, cChatItemId beforeCI, count)

getLocalChatAround_ :: DB.Connection -> User -> NoteFolder -> ChatItemId -> Int -> String -> ExceptT StoreError IO (Chat 'CTLocal, Maybe NavigationInfo)
getLocalChatAround_ db user nf aroundId count search = do
  stats <- liftIO $ getLocalStats_ db user nf
  getLocalChatAround' db user nf aroundId count search stats

getLocalChatAround' :: DB.Connection -> User -> NoteFolder -> ChatItemId -> Int -> String -> ChatStats -> ExceptT StoreError IO (Chat 'CTLocal, Maybe NavigationInfo)
getLocalChatAround' db user nf@NoteFolder {noteFolderId} aroundId count search stats = do
  aroundCI <- getLocalChatItem db user noteFolderId aroundId
  beforeIds <- liftIO $ getLocalCIsBefore_ db user nf aroundCI count search
  afterIds <- liftIO $ getLocalCIsAfter_ db user nf aroundCI count search
  ts <- liftIO getCurrentTime
  beforeCIs <- liftIO $ mapM (safeGetLocalItem db user nf ts) beforeIds
  afterCIs <- liftIO $ mapM (safeGetLocalItem db user nf ts) afterIds
  let cis = reverse beforeCIs <> [aroundCI] <> afterCIs
  navInfo <- liftIO $ getNavInfo cis
  pure (Chat (LocalChat nf) cis stats, Just navInfo)
  where
    getNavInfo cis_ = case cis_ of
      [] -> pure $ NavigationInfo 0 0
      cis -> getLocalNavInfo_ db user nf (last cis)

getLocalChatInitial_ :: DB.Connection -> User -> NoteFolder -> Int -> ExceptT StoreError IO (Chat 'CTLocal, Maybe NavigationInfo)
getLocalChatInitial_ db user nf count = do
  liftIO (getLocalMinUnreadId_ db user nf) >>= \case
    Just minUnreadItemId -> do
      unreadCount <- liftIO $ getLocalUnreadCount_ db user nf
      let stats = emptyChatStats {unreadCount, minUnreadItemId}
      getLocalChatAround' db user nf minUnreadItemId count "" stats
    Nothing -> liftIO $ (,Just $ NavigationInfo 0 0) <$> getLocalChatLast_ db user nf count ""

getLocalStats_ :: DB.Connection -> User -> NoteFolder -> IO ChatStats
getLocalStats_ db user nf = do
  minUnreadItemId <- fromMaybe 0 <$> getLocalMinUnreadId_ db user nf
  unreadCount <- getLocalUnreadCount_ db user nf
  pure emptyChatStats {unreadCount, minUnreadItemId}

getLocalMinUnreadId_ :: DB.Connection -> User -> NoteFolder -> IO (Maybe ChatItemId)
getLocalMinUnreadId_ db User {userId} NoteFolder {noteFolderId} =
  fmap join . maybeFirstRow fromOnly $
    DB.query
      db
      [sql|
        SELECT chat_item_id
        FROM chat_items
        WHERE user_id = ? AND note_folder_id = ? AND item_status = ?
        ORDER BY created_at ASC, chat_item_id ASC
        LIMIT 1
      |]
      (userId, noteFolderId, CISRcvNew)

getLocalUnreadCount_ :: DB.Connection -> User -> NoteFolder -> IO Int
getLocalUnreadCount_ db User {userId} NoteFolder {noteFolderId} =
  fromOnly . head
    <$> DB.query
      db
      [sql|
        SELECT COUNT(1)
        FROM chat_items
        WHERE user_id = ? AND note_folder_id = ? AND item_status = ?
      |]
      (userId, noteFolderId, CISRcvNew)

getLocalNavInfo_ :: DB.Connection -> User -> NoteFolder -> CChatItem 'CTLocal -> IO NavigationInfo
getLocalNavInfo_ db User {userId} NoteFolder {noteFolderId} afterCI = do
  afterUnread <- getAfterUnreadCount
  afterTotal <- getAfterTotalCount
  pure NavigationInfo {afterUnread, afterTotal}
  where
    getAfterUnreadCount :: IO Int
    getAfterUnreadCount =
      fromOnly . head
        <$> DB.query
          db
          [sql|
            SELECT COUNT(1)
            FROM (
              SELECT 1
              FROM chat_items
              WHERE user_id = ? AND note_folder_id = ? AND item_status = ?
                AND created_at > ?
              UNION ALL
              SELECT 1
              FROM chat_items
              WHERE user_id = ? AND note_folder_id = ? AND item_status = ?
                AND created_at = ? AND chat_item_id > ?
            ) ci
          |]
          ( (userId, noteFolderId, CISRcvNew, ciCreatedAt afterCI)
              :. (userId, noteFolderId, CISRcvNew, ciCreatedAt afterCI, cChatItemId afterCI)
          )
    getAfterTotalCount :: IO Int
    getAfterTotalCount =
      fromOnly . head
        <$> DB.query
          db
          [sql|
            SELECT COUNT(1)
            FROM (
              SELECT 1
              FROM chat_items
              WHERE user_id = ? AND note_folder_id = ?
                AND created_at > ?
              UNION ALL
              SELECT 1
              FROM chat_items
              WHERE user_id = ? AND note_folder_id = ?
                AND created_at = ? AND chat_item_id > ?
            ) ci
          |]
          ( (userId, noteFolderId, ciCreatedAt afterCI)
              :. (userId, noteFolderId, ciCreatedAt afterCI, cChatItemId afterCI)
          )

toChatItemRef :: (ChatItemId, Maybe Int64, Maybe Int64, Maybe Int64) -> Either StoreError (ChatRef, ChatItemId)
toChatItemRef = \case
  (itemId, Just contactId, Nothing, Nothing) -> Right (ChatRef CTDirect contactId, itemId)
  (itemId, Nothing, Just groupId, Nothing) -> Right (ChatRef CTGroup groupId, itemId)
  (itemId, Nothing, Nothing, Just folderId) -> Right (ChatRef CTLocal folderId, itemId)
  (itemId, _, _, _) -> Left $ SEBadChatItem itemId Nothing

updateDirectChatItemsRead :: DB.Connection -> User -> ContactId -> IO ()
updateDirectChatItemsRead db User {userId} contactId = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE chat_items SET item_status = ?, updated_at = ?
      WHERE user_id = ? AND contact_id = ? AND item_status = ?
    |]
    (CISRcvRead, currentTs, userId, contactId, CISRcvNew)

getDirectUnreadTimedItems :: DB.Connection -> User -> ContactId -> IO [(ChatItemId, Int)]
getDirectUnreadTimedItems db User {userId} contactId =
  DB.query
    db
    [sql|
      SELECT chat_item_id, timed_ttl
      FROM chat_items
      WHERE user_id = ? AND contact_id = ? AND item_status = ? AND timed_ttl IS NOT NULL AND timed_delete_at IS NULL
    |]
    (userId, contactId, CISRcvNew)

updateDirectChatItemsReadList :: DB.Connection -> User -> ContactId -> NonEmpty ChatItemId -> IO [(ChatItemId, Int)]
updateDirectChatItemsReadList db user@User {userId} contactId itemIds = do
  currentTs <- getCurrentTime
  catMaybes . L.toList <$> mapM (getUpdateDirectItem currentTs) itemIds
  where
    getUpdateDirectItem currentTs itemId = do
      ttl_ <- maybeFirstRow fromOnly getUnreadTimedItem
      setDirectChatItemRead_ db user contactId itemId currentTs
      pure $ (itemId,) <$> ttl_
      where
        getUnreadTimedItem =
          DB.query
            db
            [sql|
              SELECT timed_ttl
              FROM chat_items
              WHERE user_id = ? AND contact_id = ? AND item_status = ? AND chat_item_id = ? AND timed_ttl IS NOT NULL AND timed_delete_at IS NULL
            |]
            (userId, contactId, CISRcvNew, itemId)

setDirectChatItemRead :: DB.Connection -> User -> ContactId -> ChatItemId -> IO ()
setDirectChatItemRead db user contactId itemId =
  setDirectChatItemRead_ db user contactId itemId =<< getCurrentTime

setDirectChatItemRead_ :: DB.Connection -> User -> ContactId -> ChatItemId -> UTCTime -> IO ()
setDirectChatItemRead_ db User {userId} contactId itemId currentTs =
  DB.execute
    db
    [sql|
      UPDATE chat_items SET item_status = ?, updated_at = ?
      WHERE user_id = ? AND contact_id = ? AND item_status = ? AND chat_item_id = ?
    |]
    (CISRcvRead, currentTs, userId, contactId, CISRcvNew, itemId)

setDirectChatItemsDeleteAt :: DB.Connection -> User -> ContactId -> [(ChatItemId, Int)] -> UTCTime -> IO [(ChatItemId, UTCTime)]
setDirectChatItemsDeleteAt db User {userId} contactId itemIds currentTs = forM itemIds $ \(chatItemId, ttl) -> do
  let deleteAt = addUTCTime (realToFrac ttl) currentTs
  DB.execute
    db
    "UPDATE chat_items SET timed_delete_at = ? WHERE user_id = ? AND contact_id = ? AND chat_item_id = ?"
    (deleteAt, userId, contactId, chatItemId)
  pure (chatItemId, deleteAt)

updateGroupChatItemsRead :: DB.Connection -> User -> GroupId -> IO ()
updateGroupChatItemsRead db User {userId} groupId = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE chat_items SET item_status = ?, updated_at = ?
      WHERE user_id = ? AND group_id = ? AND item_status = ?
    |]
    (CISRcvRead, currentTs, userId, groupId, CISRcvNew)

getGroupUnreadTimedItems :: DB.Connection -> User -> GroupId -> IO [(ChatItemId, Int)]
getGroupUnreadTimedItems db User {userId} groupId =
  DB.query
    db
    [sql|
      SELECT chat_item_id, timed_ttl
      FROM chat_items
      WHERE user_id = ? AND group_id = ? AND item_status = ? AND timed_ttl IS NOT NULL AND timed_delete_at IS NULL
    |]
    (userId, groupId, CISRcvNew)

updateGroupChatItemsReadList :: DB.Connection -> User -> GroupId -> NonEmpty ChatItemId -> IO [(ChatItemId, Int)]
updateGroupChatItemsReadList db User {userId} groupId itemIds = do
  currentTs <- getCurrentTime
  catMaybes . L.toList <$> mapM (getUpdateGroupItem currentTs) itemIds
  where
    getUpdateGroupItem currentTs itemId = do
      ttl_ <- maybeFirstRow fromOnly getUnreadTimedItem
      setItemRead
      pure $ (itemId,) <$> ttl_
      where
        getUnreadTimedItem =
          DB.query
            db
            [sql|
              SELECT timed_ttl
              FROM chat_items
              WHERE user_id = ? AND group_id = ? AND item_status = ? AND chat_item_id = ? AND timed_ttl IS NOT NULL AND timed_delete_at IS NULL
            |]
            (userId, groupId, CISRcvNew, itemId)
        setItemRead =
          DB.execute
            db
            [sql|
              UPDATE chat_items SET item_status = ?, updated_at = ?
              WHERE user_id = ? AND group_id = ? AND item_status = ? AND chat_item_id = ?
            |]
            (CISRcvRead, currentTs, userId, groupId, CISRcvNew, itemId)

setGroupChatItemsDeleteAt :: DB.Connection -> User -> GroupId -> [(ChatItemId, Int)] -> UTCTime -> IO [(ChatItemId, UTCTime)]
setGroupChatItemsDeleteAt db User {userId} groupId itemIds currentTs = forM itemIds $ \(chatItemId, ttl) -> do
  let deleteAt = addUTCTime (realToFrac ttl) currentTs
  DB.execute
    db
    "UPDATE chat_items SET timed_delete_at = ? WHERE user_id = ? AND group_id = ? AND chat_item_id = ?"
    (deleteAt, userId, groupId, chatItemId)
  pure (chatItemId, deleteAt)

updateLocalChatItemsRead :: DB.Connection -> User -> NoteFolderId -> IO ()
updateLocalChatItemsRead db User {userId} noteFolderId = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE chat_items SET item_status = ?, updated_at = ?
      WHERE user_id = ? AND note_folder_id = ? AND item_status = ?
    |]
    (CISRcvRead, currentTs, userId, noteFolderId, CISRcvNew)

type MaybeCIFIleRow = (Maybe Int64, Maybe String, Maybe Integer, Maybe FilePath, Maybe C.SbKey, Maybe C.CbNonce, Maybe ACIFileStatus, Maybe FileProtocol)

type ChatItemModeRow = (Maybe Int, Maybe UTCTime, Maybe BoolInt, BoolInt)

type ChatItemForwardedFromRow = (Maybe CIForwardedFromTag, Maybe Text, Maybe MsgDirection, Maybe Int64, Maybe Int64, Maybe Int64)

type ChatItemRow =
  (Int64, ChatItemTs, AMsgDirection, Text, Text, ACIStatus, Maybe BoolInt, Maybe SharedMsgId)
    :. (Int, Maybe UTCTime, Maybe BoolInt, UTCTime, UTCTime)
    :. ChatItemForwardedFromRow
    :. ChatItemModeRow
    :. MaybeCIFIleRow

type QuoteRow = (Maybe ChatItemId, Maybe SharedMsgId, Maybe UTCTime, Maybe MsgContent, Maybe BoolInt)

toDirectQuote :: QuoteRow -> Maybe (CIQuote 'CTDirect)
toDirectQuote qr@(_, _, _, _, quotedSent) = toQuote qr $ direction . unBI <$> quotedSent
  where
    direction sent = if sent then CIQDirectSnd else CIQDirectRcv

toQuote :: QuoteRow -> Maybe (CIQDirection c) -> Maybe (CIQuote c)
toQuote (quotedItemId, quotedSharedMsgId, quotedSentAt, quotedMsgContent, _) dir =
  CIQuote <$> dir <*> pure quotedItemId <*> pure quotedSharedMsgId <*> quotedSentAt <*> quotedMsgContent <*> (parseMaybeMarkdownList . msgContentText <$> quotedMsgContent)

-- this function can be changed so it never fails, not only avoid failure on invalid json
toDirectChatItem :: UTCTime -> ChatItemRow :. QuoteRow -> Either StoreError (CChatItem 'CTDirect)
toDirectChatItem currentTs (((itemId, itemTs, AMsgDirection msgDir, itemContentText, itemText, itemStatus, sentViaProxy, sharedMsgId) :. (itemDeleted, deletedTs, itemEdited, createdAt, updatedAt) :. forwardedFromRow :. (timedTTL, timedDeleteAt, itemLive, BI userMention) :. (fileId_, fileName_, fileSize_, filePath, fileKey, fileNonce, fileStatus_, fileProtocol_)) :. quoteRow) =
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
      CChatItem d ChatItem {chatDir, meta = ciMeta content ciStatus, content, mentions = M.empty, formattedText = parseMaybeMarkdownList itemText, quotedItem = toDirectQuote quoteRow, reactions = [], file}
    badItem = Left $ SEBadChatItem itemId (Just itemTs)
    ciMeta :: CIContent d -> CIStatus d -> CIMeta 'CTDirect d
    ciMeta content status =
      let itemDeleted' = case itemDeleted of
            DBCINotDeleted -> Nothing
            _ -> Just (CIDeleted @'CTDirect deletedTs)
          itemEdited' = maybe False unBI itemEdited
          itemForwarded = toCIForwardedFrom forwardedFromRow
       in mkCIMeta itemId content itemText status (unBI <$> sentViaProxy) sharedMsgId itemForwarded itemDeleted' itemEdited' ciTimed (unBI <$> itemLive) userMention currentTs itemTs Nothing createdAt updatedAt
    ciTimed :: Maybe CITimed
    ciTimed = timedTTL >>= \ttl -> Just CITimed {ttl, deleteAt = timedDeleteAt}

toCIForwardedFrom :: ChatItemForwardedFromRow -> Maybe CIForwardedFrom
toCIForwardedFrom (fwdFromTag, fwdFromChatName, fwdFromMsgDir, fwdFromContactId, fwdFromGroupId, fwdFromChatItemId) =
  case (fwdFromTag, fwdFromChatName, fwdFromMsgDir, fwdFromContactId, fwdFromGroupId, fwdFromChatItemId) of
    (Just CIFFUnknown_, Nothing, Nothing, Nothing, Nothing, Nothing) -> Just CIFFUnknown
    (Just CIFFContact_, Just chatName, Just msgDir, contactId, Nothing, ciId) -> Just $ CIFFContact chatName msgDir contactId ciId
    (Just CIFFGroup_, Just chatName, Just msgDir, Nothing, groupId, ciId) -> Just $ CIFFGroup chatName msgDir groupId ciId
    _ -> Nothing

type GroupQuoteRow = QuoteRow :. MaybeGroupMemberRow

toGroupQuote :: QuoteRow -> Maybe GroupMember -> Maybe (CIQuote 'CTGroup)
toGroupQuote qr@(_, _, _, _, quotedSent) quotedMember_ = toQuote qr $ direction quotedSent quotedMember_
  where
    direction (Just (BI True)) _ = Just CIQGroupSnd
    direction (Just (BI False)) (Just member) = Just . CIQGroupRcv $ Just member
    direction (Just (BI False)) Nothing = Just $ CIQGroupRcv Nothing
    direction _ _ = Nothing

-- this function can be changed so it never fails, not only avoid failure on invalid json
toGroupChatItem :: UTCTime -> Int64 -> ChatItemRow :. Only (Maybe GroupMemberId) :. MaybeGroupMemberRow :. GroupQuoteRow :. MaybeGroupMemberRow -> Either StoreError (CChatItem 'CTGroup)
toGroupChatItem currentTs userContactId (((itemId, itemTs, AMsgDirection msgDir, itemContentText, itemText, itemStatus, sentViaProxy, sharedMsgId) :. (itemDeleted, deletedTs, itemEdited, createdAt, updatedAt) :. forwardedFromRow :. (timedTTL, timedDeleteAt, itemLive, BI userMention) :. (fileId_, fileName_, fileSize_, filePath, fileKey, fileNonce, fileStatus_, fileProtocol_)) :. Only forwardedByMember :. memberRow_ :. (quoteRow :. quotedMemberRow_) :. deletedByGroupMemberRow_) = do
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
      CChatItem d ChatItem {chatDir, meta = ciMeta content ciStatus, content, mentions = M.empty, formattedText = parseMaybeMarkdownList itemText, quotedItem = toGroupQuote quoteRow quotedMember_, reactions = [], file}
    badItem = Left $ SEBadChatItem itemId (Just itemTs)
    ciMeta :: CIContent d -> CIStatus d -> CIMeta 'CTGroup d
    ciMeta content status =
      let itemDeleted' = case itemDeleted of
            DBCINotDeleted -> Nothing
            DBCIBlocked -> Just (CIBlocked deletedTs)
            DBCIBlockedByAdmin -> Just (CIBlockedByAdmin deletedTs)
            _ -> Just (maybe (CIDeleted @'CTGroup deletedTs) (CIModerated deletedTs) deletedByGroupMember_)
          itemEdited' = maybe False unBI itemEdited
          itemForwarded = toCIForwardedFrom forwardedFromRow
       in mkCIMeta itemId content itemText status (unBI <$> sentViaProxy) sharedMsgId itemForwarded itemDeleted' itemEdited' ciTimed (unBI <$> itemLive) userMention currentTs itemTs forwardedByMember createdAt updatedAt
    ciTimed :: Maybe CITimed
    ciTimed = timedTTL >>= \ttl -> Just CITimed {ttl, deleteAt = timedDeleteAt}

getAllChatItems :: DB.Connection -> VersionRangeChat -> User -> ChatPagination -> Maybe String -> ExceptT StoreError IO [AChatItem]
getAllChatItems db vr user@User {userId} pagination search_ = do
  itemRefs <-
    rights . map toChatItemRef <$> case pagination of
      CPLast count -> liftIO $ getAllChatItemsLast_ count
      CPAfter afterId count -> liftIO . getAllChatItemsAfter_ afterId count . aChatItemTs =<< getAChatItem_ afterId
      CPBefore beforeId count -> liftIO . getAllChatItemsBefore_ beforeId count . aChatItemTs =<< getAChatItem_ beforeId
      CPAround aroundId count -> liftIO . getAllChatItemsAround_ aroundId count . aChatItemTs =<< getAChatItem_ aroundId
      CPInitial count -> do
        unless (null search) $ throwError $ SEInternalError "initial chat pagination doesn't support search"
        liftIO getFirstUnreadItemId_ >>= \case
          Just itemId -> liftIO . getAllChatItemsAround_ itemId count . aChatItemTs =<< getAChatItem_ itemId
          Nothing -> liftIO $ getAllChatItemsLast_ count
  mapM (uncurry (getAChatItem db vr user)) itemRefs
  where
    search = fromMaybe "" search_
    getAChatItem_ itemId = do
      chatRef <- getChatRefViaItemId db user itemId
      getAChatItem db vr user chatRef itemId
    getAllChatItemsLast_ count =
      reverse
        <$> DB.query
          db
          [sql|
            SELECT chat_item_id, contact_id, group_id, note_folder_id
            FROM chat_items
            WHERE user_id = ? AND LOWER(item_text) LIKE '%' || LOWER(?) || '%'
            ORDER BY item_ts DESC, chat_item_id DESC
            LIMIT ?
          |]
          (userId, search, count)
    getAllChatItemsAfter_ afterId count afterTs =
      DB.query
        db
        [sql|
          SELECT chat_item_id, contact_id, group_id, note_folder_id
          FROM chat_items
          WHERE user_id = ? AND LOWER(item_text) LIKE '%' || LOWER(?) || '%'
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
            SELECT chat_item_id, contact_id, group_id, note_folder_id
            FROM chat_items
            WHERE user_id = ? AND LOWER(item_text) LIKE '%' || LOWER(?) || '%'
              AND (item_ts < ? OR (item_ts = ? AND chat_item_id < ?))
            ORDER BY item_ts DESC, chat_item_id DESC
            LIMIT ?
          |]
          (userId, search, beforeTs, beforeTs, beforeId, count)
    getChatItem chatId =
      DB.query
        db
        [sql|
          SELECT chat_item_id, contact_id, group_id, note_folder_id
          FROM chat_items
          WHERE chat_item_id = ?
        |]
        (Only chatId)
    getAllChatItemsAround_ aroundId count aroundTs = do
      itemsBefore <- getAllChatItemsBefore_ aroundId count aroundTs
      item <- getChatItem aroundId
      itemsAfter <- getAllChatItemsAfter_ aroundId count aroundTs
      pure $ itemsBefore <> item <> itemsAfter
    getFirstUnreadItemId_ =
      fmap join . maybeFirstRow fromOnly $
        DB.query
          db
          [sql|
            SELECT MIN(chat_item_id)
            FROM chat_items
            WHERE user_id = ? AND item_status = ?
          |]
          (userId, CISRcvNew)

getChatItemIdsByAgentMsgId :: DB.Connection -> Int64 -> AgentMsgId -> IO [ChatItemId]
getChatItemIdsByAgentMsgId db connId msgId =
  map fromOnly
    <$> DB.query
      db
      [sql|
        SELECT chat_item_id
        FROM chat_item_messages
        WHERE message_id IN (
          SELECT message_id
          FROM msg_deliveries
          WHERE connection_id = ? AND agent_msg_id = ?
        )
      |]
      (connId, msgId)

updateDirectChatItemStatus :: forall d. MsgDirectionI d => DB.Connection -> User -> Contact -> ChatItemId -> CIStatus d -> ExceptT StoreError IO (ChatItem 'CTDirect d)
updateDirectChatItemStatus db user@User {userId} ct@Contact {contactId} itemId itemStatus = do
  ci <- liftEither . correctDir =<< getDirectCIWithReactions db user ct itemId
  currentTs <- liftIO getCurrentTime
  liftIO $ DB.execute db "UPDATE chat_items SET item_status = ?, updated_at = ? WHERE user_id = ? AND contact_id = ? AND chat_item_id = ?" (itemStatus, currentTs, userId, contactId, itemId)
  pure ci {meta = (meta ci) {itemStatus}}

setDirectSndChatItemViaProxy :: DB.Connection -> User -> Contact -> ChatItem 'CTDirect 'MDSnd -> Bool -> IO (ChatItem 'CTDirect 'MDSnd)
setDirectSndChatItemViaProxy db User {userId} Contact {contactId} ci viaProxy = do
  DB.execute db "UPDATE chat_items SET via_proxy = ? WHERE user_id = ? AND contact_id = ? AND chat_item_id = ?" (BI viaProxy, userId, contactId, chatItemId' ci)
  pure ci {meta = (meta ci) {sentViaProxy = Just viaProxy}}

updateDirectChatItem :: MsgDirectionI d => DB.Connection -> User -> Contact -> ChatItemId -> CIContent d -> Bool -> Bool -> Maybe CITimed -> Maybe MessageId -> ExceptT StoreError IO (ChatItem 'CTDirect d)
updateDirectChatItem db user ct@Contact {contactId} itemId newContent edited live timed_ msgId_ = do
  ci <- liftEither . correctDir =<< getDirectCIWithReactions db user ct itemId
  liftIO $ updateDirectChatItem' db user contactId ci newContent edited live timed_ msgId_

getDirectCIWithReactions :: DB.Connection -> User -> Contact -> ChatItemId -> ExceptT StoreError IO (CChatItem 'CTDirect)
getDirectCIWithReactions db user ct@Contact {contactId} itemId =
  liftIO . directCIWithReactions db ct =<< getDirectChatItem db user contactId itemId

correctDir :: MsgDirectionI d => CChatItem c -> Either StoreError (ChatItem c d)
correctDir (CChatItem _ ci) = first SEInternalError $ checkDirection ci

updateDirectChatItem' :: forall d. MsgDirectionI d => DB.Connection -> User -> Int64 -> ChatItem 'CTDirect d -> CIContent d -> Bool -> Bool -> Maybe CITimed -> Maybe MessageId -> IO (ChatItem 'CTDirect d)
updateDirectChatItem' db User {userId} contactId ci newContent edited live timed_ msgId_ = do
  currentTs <- liftIO getCurrentTime
  let ci' = updatedChatItem ci newContent edited live timed_ currentTs
  liftIO $ updateDirectChatItem_ db userId contactId ci' msgId_
  pure ci'

updatedChatItem :: ChatItem c d -> CIContent d -> Bool -> Bool -> Maybe CITimed -> UTCTime -> ChatItem c d
updatedChatItem ci@ChatItem {meta = meta@CIMeta {itemStatus, itemEdited, itemTimed, itemLive}} newContent edited live timed_ currentTs =
  let newText = ciContentToText newContent
      edited' = itemEdited || edited
      live' = (live &&) <$> itemLive
      timed' = case timed_ of
        Just timed -> Just timed
        Nothing -> case (itemStatus, itemTimed, itemLive, live) of
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
    ((content, itemText, itemStatus, BI itemDeleted', itemDeletedTs', BI itemEdited, BI <$> itemLive, updatedAt) :. ciTimedRow itemTimed :. (userId, contactId, itemId))
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
    (itemId, MCText $ msgContentText msgContent, itemVersionTs)

deleteDirectChatItem :: DB.Connection -> User -> Contact -> ChatItem 'CTDirect d -> IO ()
deleteDirectChatItem db User {userId} Contact {contactId} ci = do
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
deleteChatItemMessages_ db itemId = DB.execute db deleteChatItemMessagesQuery (Only itemId)

deleteChatItemMessagesQuery :: Query
deleteChatItemMessagesQuery =
  [sql|
    DELETE FROM messages
    WHERE message_id IN (
      SELECT message_id
      FROM chat_item_messages
      WHERE chat_item_id = ?
    )
  |]

deleteChatItemVersions_ :: DB.Connection -> ChatItemId -> IO ()
deleteChatItemVersions_ db itemId =
  DB.execute db "DELETE FROM chat_item_versions WHERE chat_item_id = ?" (Only itemId)

markDirectChatItemDeleted :: DB.Connection -> User -> Contact -> ChatItem 'CTDirect d -> UTCTime -> IO (ChatItem 'CTDirect d)
markDirectChatItemDeleted db User {userId} Contact {contactId} ci@ChatItem {meta} deletedTs = do
  currentTs <- liftIO getCurrentTime
  let itemId = chatItemId' ci
  DB.execute
    db
    [sql|
      UPDATE chat_items
      SET item_deleted = ?, item_deleted_ts = ?, updated_at = ?
      WHERE user_id = ? AND contact_id = ? AND chat_item_id = ?
    |]
    (DBCIDeleted, deletedTs, currentTs, userId, contactId, itemId)
  pure ci {meta = meta {itemDeleted = Just $ CIDeleted $ Just deletedTs, editable = False, deletable = False}}

getDirectChatItemBySharedMsgId :: DB.Connection -> User -> ContactId -> SharedMsgId -> ExceptT StoreError IO (CChatItem 'CTDirect)
getDirectChatItemBySharedMsgId db user@User {userId} contactId sharedMsgId = do
  itemId <- getDirectChatItemIdBySharedMsgId_ db userId contactId sharedMsgId
  getDirectChatItem db user contactId itemId

getDirectChatItemsByAgentMsgId :: DB.Connection -> User -> ContactId -> Int64 -> AgentMsgId -> IO [CChatItem 'CTDirect]
getDirectChatItemsByAgentMsgId db user contactId connId msgId = do
  itemIds <- getChatItemIdsByAgentMsgId db connId msgId
  catMaybes <$> mapM (fmap eitherToMaybe . runExceptT . getDirectChatItem db user contactId) itemIds

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
  firstRow' (toDirectChatItem currentTs) (SEChatItemNotFound itemId) getItem
  where
    getItem =
      DB.query
        db
        [sql|
          SELECT
            -- ChatItem
            i.chat_item_id, i.item_ts, i.item_sent, i.item_content, i.item_text, i.item_status, i.via_proxy, i.shared_msg_id,
            i.item_deleted, i.item_deleted_ts, i.item_edited, i.created_at, i.updated_at,
            i.fwd_from_tag, i.fwd_from_chat_name, i.fwd_from_msg_dir, i.fwd_from_contact_id, i.fwd_from_group_id, i.fwd_from_chat_item_id,
            i.timed_ttl, i.timed_delete_at, i.item_live, i.user_mention,
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

updateGroupChatItemStatus :: MsgDirectionI d => DB.Connection -> User -> GroupInfo -> ChatItemId -> CIStatus d -> ExceptT StoreError IO (ChatItem 'CTGroup d)
updateGroupChatItemStatus db user@User {userId} g@GroupInfo {groupId} itemId itemStatus = do
  ci <- liftEither . correctDir =<< getGroupCIWithReactions db user g itemId
  currentTs <- liftIO getCurrentTime
  liftIO $ DB.execute db "UPDATE chat_items SET item_status = ?, updated_at = ? WHERE user_id = ? AND group_id = ? AND chat_item_id = ?" (itemStatus, currentTs, userId, groupId, itemId)
  pure ci {meta = (meta ci) {itemStatus}}

getGroupCIWithReactions :: DB.Connection -> User -> GroupInfo -> ChatItemId -> ExceptT StoreError IO (CChatItem 'CTGroup)
getGroupCIWithReactions db user g@GroupInfo {groupId} itemId = do
  liftIO . groupCIWithReactions db g =<< getGroupChatItem db user groupId itemId

groupCIWithReactions :: DB.Connection -> GroupInfo -> CChatItem 'CTGroup -> IO (CChatItem 'CTGroup)
groupCIWithReactions db g cci@(CChatItem md ci@ChatItem {meta = CIMeta {itemId, itemSharedMsgId}}) = do
  mentions <- getGroupCIMentions db itemId
  case itemSharedMsgId of
    Just sharedMsgId -> do
      let GroupMember {memberId} = chatItemMember g ci
      reactions <- getGroupCIReactions db g memberId sharedMsgId
      pure $ CChatItem md ci {reactions, mentions}
    Nothing -> pure $ if null mentions then cci else CChatItem md ci {mentions}

updateGroupChatItem :: MsgDirectionI d => DB.Connection -> User -> Int64 -> ChatItem 'CTGroup d -> CIContent d -> Bool -> Bool -> Maybe MessageId -> IO (ChatItem 'CTGroup d)
updateGroupChatItem db user groupId ci newContent edited live msgId_ = do
  currentTs <- liftIO getCurrentTime
  let ci' = updatedChatItem ci newContent edited live Nothing currentTs
  liftIO $ updateGroupChatItem_ db user groupId ci' msgId_
  pure ci'

-- this function assumes that the group item with correct chat direction already exists,
-- it should be checked before calling it
updateGroupChatItem_ :: MsgDirectionI d => DB.Connection -> User -> Int64 -> ChatItem 'CTGroup d -> Maybe MessageId -> IO ()
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
    ((content, itemText, itemStatus, BI itemDeleted', itemDeletedTs', BI itemEdited, BI <$> itemLive, updatedAt) :. ciTimedRow itemTimed :. (userId, groupId, itemId))
  forM_ msgId_ $ \msgId -> insertChatItemMessage_ db itemId msgId updatedAt

createGroupCIMentions :: forall d. DB.Connection -> GroupInfo -> ChatItem 'CTGroup d -> Map MemberName CIMention -> IO (ChatItem 'CTGroup d)
createGroupCIMentions db GroupInfo {groupId} ci mentions = do
  DB.executeMany db "INSERT INTO chat_item_mentions (chat_item_id, group_id, member_id, display_name) VALUES (?, ?, ?, ?)" rows
  pure (ci :: ChatItem 'CTGroup d) {mentions}
  where
    rows = map (\(name, CIMention {memberId}) -> (ciId, groupId, memberId, name)) $ M.assocs mentions
    ciId = chatItemId' ci

updateGroupCIMentions :: DB.Connection -> GroupInfo -> ChatItem 'CTGroup d -> Map MemberName CIMention -> IO (ChatItem 'CTGroup d)
updateGroupCIMentions db g ci@ChatItem {mentions} mentions'
  | mentions' == mentions = pure ci
  | otherwise = do
      unless (null mentions) $ deleteMentions
      if null mentions'
        then pure ci
        else -- This is a fallback for the error that should not happen in practice.
        -- In theory, it may happen in item mentions in database are different from item record.
          createMentions `E.catch` \e -> if constraintError e then deleteMentions >> createMentions else E.throwIO e
  where
    deleteMentions = DB.execute db "DELETE FROM chat_item_mentions WHERE chat_item_id = ?" (Only $ chatItemId' ci)
    createMentions = createGroupCIMentions db g ci mentions'

deleteGroupChatItem :: DB.Connection -> User -> GroupInfo -> ChatItem 'CTGroup d -> IO ()
deleteGroupChatItem db User {userId} g@GroupInfo {groupId} ci = do
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

updateGroupChatItemModerated :: forall d. MsgDirectionI d => DB.Connection -> User -> GroupInfo -> ChatItem 'CTGroup d -> GroupMember -> UTCTime -> IO (ChatItem 'CTGroup d)
updateGroupChatItemModerated db User {userId} GroupInfo {groupId} ci m@GroupMember {groupMemberId} deletedTs = do
  currentTs <- getCurrentTime
  let toContent = msgDirToModeratedContent_ $ msgDirection @d
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
  pure ci {content = toContent, meta = (meta ci) {itemText = toText, itemDeleted = Just (CIModerated (Just deletedTs) m), editable = False, deletable = False}, formattedText = Nothing}

updateMemberCIsModerated :: MsgDirectionI d => DB.Connection -> User -> GroupInfo -> GroupMember -> GroupMember -> SMsgDirection d -> UTCTime -> IO ()
updateMemberCIsModerated db User {userId} GroupInfo {groupId, membership} member byGroupMember md deletedTs = do
  itemIds <- updateCIs =<< getCurrentTime
  DB.executeMany db deleteChatItemMessagesQuery itemIds
  DB.executeMany db "DELETE FROM chat_item_versions WHERE chat_item_id = ?" itemIds
  where
    memId = groupMemberId' member
    updateQuery =
      [sql|
        UPDATE chat_items
        SET item_deleted = 1, item_deleted_ts = ?, item_deleted_by_group_member_id = ?, item_content = ?, item_text = ?, updated_at = ?
        WHERE user_id = ? AND group_id = ?
      |]
    updateCIs :: UTCTime -> IO [Only Int64]
    updateCIs currentTs
      | memId == groupMemberId' membership =
          DB.query
            db
            (updateQuery <> " AND group_member_id IS NULL AND item_sent = 1 RETURNING chat_item_id")
            (columns :. (userId, groupId))
      | otherwise =
          DB.query
            db
            (updateQuery <> " AND group_member_id = ? RETURNING chat_item_id")
            (columns :. (userId, groupId, memId))
      where
        columns = (deletedTs, groupMemberId' byGroupMember, msgDirToModeratedContent_ md, ciModeratedText, currentTs)

updateGroupCIBlockedByAdmin :: DB.Connection -> User -> GroupInfo -> ChatItem 'CTGroup d -> UTCTime -> IO (ChatItem 'CTGroup d)
updateGroupCIBlockedByAdmin db User {userId} GroupInfo {groupId} ci deletedTs = do
  currentTs <- getCurrentTime
  let itemId = chatItemId' ci
  deleteChatItemMessages_ db itemId
  deleteChatItemVersions_ db itemId
  liftIO $
    DB.execute
      db
      [sql|
        UPDATE chat_items
        SET item_deleted = ?, item_deleted_ts = ?, updated_at = ?
        WHERE user_id = ? AND group_id = ? AND chat_item_id = ?
      |]
      (DBCIBlockedByAdmin, deletedTs, currentTs, userId, groupId, itemId)
  pure $ ci {meta = (meta ci) {itemDeleted = Just (CIBlockedByAdmin $ Just deletedTs), editable = False, deletable = False}, formattedText = Nothing}

pattern DBCINotDeleted :: Int
pattern DBCINotDeleted = 0

pattern DBCIDeleted :: Int
pattern DBCIDeleted = 1

pattern DBCIBlocked :: Int
pattern DBCIBlocked = 2

pattern DBCIBlockedByAdmin :: Int
pattern DBCIBlockedByAdmin = 3

markGroupChatItemDeleted :: DB.Connection -> User -> GroupInfo -> ChatItem 'CTGroup d -> Maybe GroupMember -> UTCTime -> IO (ChatItem 'CTGroup d)
markGroupChatItemDeleted db User {userId} GroupInfo {groupId} ci@ChatItem {meta} byGroupMember_ deletedTs = do
  currentTs <- liftIO getCurrentTime
  let itemId = chatItemId' ci
      (deletedByGroupMemberId, itemDeleted) = case byGroupMember_ of
        Just m@GroupMember {groupMemberId} -> (Just groupMemberId, Just $ CIModerated (Just deletedTs) m)
        _ -> (Nothing, Just $ CIDeleted @'CTGroup (Just deletedTs))
  DB.execute
    db
    [sql|
      UPDATE chat_items
      SET item_deleted = ?, item_deleted_ts = ?, item_deleted_by_group_member_id = ?, updated_at = ?
      WHERE user_id = ? AND group_id = ? AND chat_item_id = ?
    |]
    (DBCIDeleted, deletedTs, deletedByGroupMemberId, currentTs, userId, groupId, itemId)
  pure ci {meta = meta {itemDeleted, editable = False, deletable = False}}

markMemberCIsDeleted :: DB.Connection -> User -> GroupInfo -> GroupMember -> GroupMember -> UTCTime -> IO ()
markMemberCIsDeleted db User {userId} GroupInfo {groupId, membership} member byGroupMember deletedTs =
  updateCIs =<< getCurrentTime
  where
    memId = groupMemberId' member
    updateQuery =
      [sql|
        UPDATE chat_items
        SET item_deleted = ?, item_deleted_ts = ?, item_deleted_by_group_member_id = ?, updated_at = ?
        WHERE user_id = ? AND group_id = ?
      |]
    updateCIs currentTs
      | memId == groupMemberId' membership =
          DB.execute
            db
            (updateQuery <> " AND group_member_id IS NULL AND item_sent = 1")
            (columns :. (userId, groupId))
      | otherwise =
          DB.execute
            db
            (updateQuery <> " AND group_member_id = ?")
            (columns :. (userId, groupId, memId))
      where
        columns = (DBCIDeleted, deletedTs, groupMemberId' byGroupMember, currentTs)

markGroupChatItemBlocked :: DB.Connection -> User -> GroupInfo -> ChatItem 'CTGroup 'MDRcv -> IO (ChatItem 'CTGroup 'MDRcv)
markGroupChatItemBlocked db User {userId} GroupInfo {groupId} ci@ChatItem {meta} = do
  deletedTs <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE chat_items
      SET item_deleted = ?, item_deleted_ts = ?, updated_at = ?
      WHERE user_id = ? AND group_id = ? AND chat_item_id = ?
    |]
    (DBCIBlocked, deletedTs, deletedTs, userId, groupId, chatItemId' ci)
  pure ci {meta = meta {itemDeleted = Just $ CIBlocked $ Just deletedTs, editable = False, deletable = False}}

markGroupCIBlockedByAdmin :: DB.Connection -> User -> GroupInfo -> ChatItem 'CTGroup 'MDRcv -> IO (ChatItem 'CTGroup 'MDRcv)
markGroupCIBlockedByAdmin db User {userId} GroupInfo {groupId} ci@ChatItem {meta} = do
  deletedTs <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE chat_items
      SET item_deleted = ?, item_deleted_ts = ?, updated_at = ?
      WHERE user_id = ? AND group_id = ? AND chat_item_id = ?
    |]
    (DBCIBlockedByAdmin, deletedTs, deletedTs, userId, groupId, chatItemId' ci)
  pure ci {meta = meta {itemDeleted = Just $ CIBlockedByAdmin $ Just deletedTs, editable = False, deletable = False}}

markMessageReportsDeleted :: DB.Connection -> User -> GroupInfo -> ChatItem 'CTGroup d -> GroupMember -> UTCTime -> IO [ChatItemId]
markMessageReportsDeleted db User {userId} GroupInfo {groupId} ChatItem {meta = CIMeta {itemSharedMsgId}} GroupMember {groupMemberId} deletedTs = do
  currentTs <- liftIO getCurrentTime
  map fromOnly
    <$> DB.query
      db
      [sql|
        UPDATE chat_items
        SET item_deleted = ?, item_deleted_ts = ?, item_deleted_by_group_member_id = ?, updated_at = ?
        WHERE user_id = ? AND group_id = ? AND msg_content_tag = ? AND quoted_shared_msg_id = ? AND item_deleted = ?
        RETURNING chat_item_id;
      |]
      (DBCIDeleted, deletedTs, groupMemberId, currentTs, userId, groupId, MCReport_, itemSharedMsgId, DBCINotDeleted)

markReceivedGroupReportsDeleted :: DB.Connection -> User -> GroupInfo -> UTCTime -> IO [ChatItemId]
markReceivedGroupReportsDeleted db User {userId} GroupInfo {groupId, membership} deletedTs = do
  currentTs <- liftIO getCurrentTime
  map fromOnly
    <$> DB.query
      db
      [sql|
        UPDATE chat_items
        SET item_deleted = ?, item_deleted_ts = ?, item_deleted_by_group_member_id = ?, updated_at = ?
        WHERE user_id = ? AND group_id = ? AND msg_content_tag = ? AND item_deleted = ? AND item_sent = 0
        RETURNING chat_item_id
      |]
      (DBCIDeleted, deletedTs, groupMemberId' membership, currentTs, userId, groupId, MCReport_, DBCINotDeleted)

getGroupChatItemBySharedMsgId :: DB.Connection -> User -> GroupInfo -> GroupMemberId -> SharedMsgId -> ExceptT StoreError IO (CChatItem 'CTGroup)
getGroupChatItemBySharedMsgId db user@User {userId} g@GroupInfo {groupId} groupMemberId sharedMsgId = do
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
  getGroupCIWithReactions db user g itemId

getGroupMemberCIBySharedMsgId :: DB.Connection -> User -> GroupInfo -> MemberId -> SharedMsgId -> ExceptT StoreError IO (CChatItem 'CTGroup)
getGroupMemberCIBySharedMsgId db user@User {userId} g@GroupInfo {groupId} memberId sharedMsgId = do
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
  getGroupCIWithReactions db user g itemId

getGroupChatItemsByAgentMsgId :: DB.Connection -> User -> GroupId -> Int64 -> AgentMsgId -> IO [CChatItem 'CTGroup]
getGroupChatItemsByAgentMsgId db user groupId connId msgId = do
  itemIds <- getChatItemIdsByAgentMsgId db connId msgId
  catMaybes <$> mapM (fmap eitherToMaybe . runExceptT . getGroupChatItem db user groupId) itemIds

getGroupChatItem :: DB.Connection -> User -> Int64 -> ChatItemId -> ExceptT StoreError IO (CChatItem 'CTGroup)
getGroupChatItem db User {userId, userContactId} groupId itemId = ExceptT $ do
  currentTs <- getCurrentTime
  firstRow' (toGroupChatItem currentTs userContactId) (SEChatItemNotFound itemId) getItem
  where
    getItem =
      DB.query
        db
        [sql|
          SELECT
            -- ChatItem
            i.chat_item_id, i.item_ts, i.item_sent, i.item_content, i.item_text, i.item_status, i.via_proxy, i.shared_msg_id,
            i.item_deleted, i.item_deleted_ts, i.item_edited, i.created_at, i.updated_at,
            i.fwd_from_tag, i.fwd_from_chat_name, i.fwd_from_msg_dir, i.fwd_from_contact_id, i.fwd_from_group_id, i.fwd_from_chat_item_id,
            i.timed_ttl, i.timed_delete_at, i.item_live, i.user_mention,
            -- CIFile
            f.file_id, f.file_name, f.file_size, f.file_path, f.file_crypto_key, f.file_crypto_nonce, f.ci_file_status, f.protocol,
            -- CIMeta forwardedByMember
            i.forwarded_by_group_member_id,
            -- GroupMember
            m.group_member_id, m.group_id, m.member_id, m.peer_chat_min_version, m.peer_chat_max_version, m.member_role, m.member_category,
            m.member_status, m.show_messages, m.member_restriction, m.invited_by, m.invited_by_group_member_id, m.local_display_name, m.contact_id, m.contact_profile_id, p.contact_profile_id,
            p.display_name, p.full_name, p.image, p.contact_link, p.local_alias, p.preferences,
            m.created_at, m.updated_at,
            -- quoted ChatItem
            ri.chat_item_id, i.quoted_shared_msg_id, i.quoted_sent_at, i.quoted_content, i.quoted_sent,
            -- quoted GroupMember
            rm.group_member_id, rm.group_id, rm.member_id, rm.peer_chat_min_version, rm.peer_chat_max_version, rm.member_role, rm.member_category,
            rm.member_status, rm.show_messages, rm.member_restriction, rm.invited_by, rm.invited_by_group_member_id, rm.local_display_name, rm.contact_id, rm.contact_profile_id, rp.contact_profile_id,
            rp.display_name, rp.full_name, rp.image, rp.contact_link, rp.local_alias, rp.preferences,
            rm.created_at, rm.updated_at,
            -- deleted by GroupMember
            dbm.group_member_id, dbm.group_id, dbm.member_id, dbm.peer_chat_min_version, dbm.peer_chat_max_version, dbm.member_role, dbm.member_category,
            dbm.member_status, dbm.show_messages, dbm.member_restriction, dbm.invited_by, dbm.invited_by_group_member_id, dbm.local_display_name, dbm.contact_id, dbm.contact_profile_id, dbp.contact_profile_id,
            dbp.display_name, dbp.full_name, dbp.image, dbp.contact_link, dbp.local_alias, dbp.preferences,
            dbm.created_at, dbm.updated_at
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
          WHERE i.user_id = ? AND i.group_id = ? AND m.local_display_name = ? AND i.item_text like ?
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

getLocalChatItem :: DB.Connection -> User -> Int64 -> ChatItemId -> ExceptT StoreError IO (CChatItem 'CTLocal)
getLocalChatItem db User {userId} folderId itemId = ExceptT $ do
  currentTs <- getCurrentTime
  firstRow' (toLocalChatItem currentTs) (SEChatItemNotFound itemId) getItem
  where
    getItem =
      DB.query
        db
        [sql|
          SELECT
            -- ChatItem
            i.chat_item_id, i.item_ts, i.item_sent, i.item_content, i.item_text, i.item_status, i.via_proxy, i.shared_msg_id,
            i.item_deleted, i.item_deleted_ts, i.item_edited, i.created_at, i.updated_at,
            i.fwd_from_tag, i.fwd_from_chat_name, i.fwd_from_msg_dir, i.fwd_from_contact_id, i.fwd_from_group_id, i.fwd_from_chat_item_id,
            i.timed_ttl, i.timed_delete_at, i.item_live, i.user_mention,
            -- CIFile
            f.file_id, f.file_name, f.file_size, f.file_path, f.file_crypto_key, f.file_crypto_nonce, f.ci_file_status, f.protocol
          FROM chat_items i
          LEFT JOIN files f ON f.chat_item_id = i.chat_item_id
          WHERE i.user_id = ? AND i.note_folder_id = ? AND i.chat_item_id = ?
        |]
        (userId, folderId, itemId)

getLocalChatItemIdByText :: DB.Connection -> User -> NoteFolderId -> SMsgDirection d -> Text -> ExceptT StoreError IO ChatItemId
getLocalChatItemIdByText db User {userId} noteFolderId msgDir quotedMsg =
  ExceptT . firstRow fromOnly (SEChatItemNotFoundByText quotedMsg) $
    DB.query
      db
      [sql|
        SELECT chat_item_id
        FROM chat_items
        WHERE user_id = ? AND note_folder_id = ? AND item_sent = ? AND item_text LIKE ?
        ORDER BY chat_item_id DESC
        LIMIT 1
      |]
      (userId, noteFolderId, msgDir, quotedMsg <> "%")

getLocalChatItemIdByText' :: DB.Connection -> User -> NoteFolderId -> Text -> ExceptT StoreError IO ChatItemId
getLocalChatItemIdByText' db User {userId} noteFolderId msg =
  ExceptT . firstRow fromOnly (SEChatItemNotFoundByText msg) $
    DB.query
      db
      [sql|
        SELECT chat_item_id
        FROM chat_items
        WHERE user_id = ? AND note_folder_id = ? AND item_text LIKE ?
        ORDER BY chat_item_id DESC
        LIMIT 1
      |]
      (userId, noteFolderId, msg <> "%")

updateLocalChatItem' :: forall d. MsgDirectionI d => DB.Connection -> User -> NoteFolderId -> ChatItem 'CTLocal d -> CIContent d -> Bool -> IO (ChatItem 'CTLocal d)
updateLocalChatItem' db User {userId} noteFolderId ci newContent edited = do
  currentTs <- liftIO getCurrentTime
  let ci' = updatedChatItem ci newContent edited False Nothing currentTs
  liftIO $ updateLocalChatItem_ db userId noteFolderId ci'
  pure ci'

-- this function assumes that local item with correct chat direction already exists,
-- it should be checked before calling it
updateLocalChatItem_ :: forall d. MsgDirectionI d => DB.Connection -> UserId -> NoteFolderId -> ChatItem 'CTLocal d -> IO ()
updateLocalChatItem_ db userId noteFolderId ChatItem {meta, content} = do
  let CIMeta {itemId, itemText, itemStatus, itemDeleted, itemEdited, updatedAt} = meta
      itemDeleted' = isJust itemDeleted
      itemDeletedTs' = itemDeletedTs =<< itemDeleted
  DB.execute
    db
    [sql|
      UPDATE chat_items
      SET item_content = ?, item_text = ?, item_status = ?, item_deleted = ?, item_deleted_ts = ?, item_edited = ?, updated_at = ?
      WHERE user_id = ? AND note_folder_id = ? AND chat_item_id = ?
    |]
    ((content, itemText, itemStatus, BI itemDeleted', itemDeletedTs', BI itemEdited, updatedAt) :. (userId, noteFolderId, itemId))

deleteLocalChatItem :: DB.Connection -> User -> NoteFolder -> ChatItem 'CTLocal d -> IO ()
deleteLocalChatItem db User {userId} NoteFolder {noteFolderId} ci = do
  let itemId = chatItemId' ci
  deleteChatItemVersions_ db itemId
  DB.execute
    db
    [sql|
      DELETE FROM chat_items
      WHERE user_id = ? AND note_folder_id = ? AND chat_item_id = ?
    |]
    (userId, noteFolderId, itemId)

getChatItemByFileId :: DB.Connection -> VersionRangeChat -> User -> Int64 -> ExceptT StoreError IO AChatItem
getChatItemByFileId db vr user@User {userId} fileId = do
  (chatRef, itemId) <-
    ExceptT . firstRow' toChatItemRef (SEChatItemNotFoundByFileId fileId) $
      DB.query
        db
        [sql|
            SELECT i.chat_item_id, i.contact_id, i.group_id, i.note_folder_id
            FROM chat_items i
            JOIN files f ON f.chat_item_id = i.chat_item_id
            WHERE f.user_id = ? AND f.file_id = ?
            LIMIT 1
          |]
        (userId, fileId)
  getAChatItem db vr user chatRef itemId

lookupChatItemByFileId :: DB.Connection -> VersionRangeChat -> User -> Int64 -> ExceptT StoreError IO (Maybe AChatItem)
lookupChatItemByFileId db vr user fileId = do
  fmap Just (getChatItemByFileId db vr user fileId) `catchError` \case
    SEChatItemNotFoundByFileId {} -> pure Nothing
    e -> throwError e

getChatItemByGroupId :: DB.Connection -> VersionRangeChat -> User -> GroupId -> ExceptT StoreError IO AChatItem
getChatItemByGroupId db vr user@User {userId} groupId = do
  (chatRef, itemId) <-
    ExceptT . firstRow' toChatItemRef (SEChatItemNotFoundByGroupId groupId) $
      DB.query
        db
        [sql|
          SELECT i.chat_item_id, i.contact_id, i.group_id, i.note_folder_id
          FROM chat_items i
          JOIN groups g ON g.chat_item_id = i.chat_item_id
          WHERE g.user_id = ? AND g.group_id = ?
          LIMIT 1
        |]
        (userId, groupId)
  getAChatItem db vr user chatRef itemId

getChatRefViaItemId :: DB.Connection -> User -> ChatItemId -> ExceptT StoreError IO ChatRef
getChatRefViaItemId db User {userId} itemId = do
  ExceptT . firstRow' toChatRef (SEChatItemNotFound itemId) $
    DB.query db "SELECT contact_id, group_id FROM chat_items WHERE user_id = ? AND chat_item_id = ?" (userId, itemId)
  where
    toChatRef = \case
      (Just contactId, Nothing) -> Right $ ChatRef CTDirect contactId
      (Nothing, Just groupId) -> Right $ ChatRef CTGroup groupId
      (_, _) -> Left $ SEBadChatItem itemId Nothing

getAChatItem :: DB.Connection -> VersionRangeChat -> User -> ChatRef -> ChatItemId -> ExceptT StoreError IO AChatItem
getAChatItem db vr user chatRef itemId = do
  aci <- case chatRef of
    ChatRef CTDirect contactId -> do
      ct <- getContact db vr user contactId
      (CChatItem msgDir ci) <- getDirectChatItem db user contactId itemId
      pure $ AChatItem SCTDirect msgDir (DirectChat ct) ci
    ChatRef CTGroup groupId -> do
      gInfo <- getGroupInfo db vr user groupId
      (CChatItem msgDir ci) <- getGroupChatItem db user groupId itemId
      pure $ AChatItem SCTGroup msgDir (GroupChat gInfo) ci
    ChatRef CTLocal folderId -> do
      nf <- getNoteFolder db user folderId
      CChatItem msgDir ci <- getLocalChatItem db user folderId itemId
      pure $ AChatItem SCTLocal msgDir (LocalChat nf) ci
    _ -> throwError $ SEChatItemNotFound itemId
  liftIO $ getACIReactions db aci

getAChatItemBySharedMsgId :: ChatTypeQuotable c => DB.Connection -> User -> ChatDirection c 'MDRcv -> SharedMsgId -> ExceptT StoreError IO AChatItem
getAChatItemBySharedMsgId db user cd sharedMsgId = case cd of
  CDDirectRcv ct@Contact {contactId} -> do
    (CChatItem msgDir ci) <- getDirectChatItemBySharedMsgId db user contactId sharedMsgId
    pure $ AChatItem SCTDirect msgDir (DirectChat ct) ci
  CDGroupRcv g GroupMember {groupMemberId} -> do
    (CChatItem msgDir ci) <- getGroupChatItemBySharedMsgId db user g groupMemberId sharedMsgId
    pure $ AChatItem SCTGroup msgDir (GroupChat g) ci

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

directCIWithReactions :: DB.Connection -> Contact -> CChatItem 'CTDirect -> IO (CChatItem 'CTDirect)
directCIWithReactions db ct cci@(CChatItem md ci@ChatItem {meta = CIMeta {itemSharedMsgId}}) = case itemSharedMsgId of
  Just sharedMsgId -> do
    reactions <- getDirectCIReactions db ct sharedMsgId
    pure $ CChatItem md ci {reactions}
  Nothing -> pure cci

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

getGroupCIMentions :: DB.Connection -> ChatItemId -> IO (Map MemberName CIMention)
getGroupCIMentions db ciId =
  M.fromList . map mentionedMember
    <$> DB.query
      db
      [sql|
        SELECT r.display_name, r.member_id, m.group_member_id, m.member_role, p.display_name, p.local_alias
        FROM chat_item_mentions r
        LEFT JOIN group_members m ON r.group_id = m.group_id AND r.member_id = m.member_id
        LEFT JOIN contact_profiles p ON p.contact_profile_id = COALESCE(m.member_profile_id, m.contact_profile_id) 
        WHERE r.chat_item_id = ?
      |]
      (Only ciId)
  where
    mentionedMember :: (ContactName, MemberId, Maybe GroupMemberId, Maybe GroupMemberRole, Maybe Text, Maybe Text) -> (ContactName, CIMention)
    mentionedMember (name, memberId, gmId_, mRole_, displayName_, localAlias) =
      let memberRef = case (gmId_, mRole_, displayName_) of
            (Just groupMemberId, Just memberRole, Just displayName) ->
              Just CIMentionMember {groupMemberId, displayName, localAlias, memberRole}
            _ -> Nothing
       in (name, CIMention {memberId, memberRef})

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

toCIReaction :: (MsgReaction, BoolInt, Int) -> CIReactionCount
toCIReaction (reaction, BI userReacted, totalReacted) = CIReactionCount {reaction, userReacted, totalReacted}

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
      (contactId' ct, itemSharedMId, BI sent)

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
        (contactId' ct, itemSharedMId, BI sent, reaction, msgId, reactionTs)
  | otherwise =
      DB.execute
        db
        [sql|
          DELETE FROM chat_item_reactions
          WHERE contact_id = ? AND shared_msg_id = ? AND reaction_sent = ? AND reaction = ?
        |]
        (contactId' ct, itemSharedMId, BI sent, reaction)

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
      (groupId, groupMemberId' m, itemMemberId, itemSharedMId, BI sent)

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
        (groupId, groupMemberId' m, itemMemberId, itemSharedMId, BI sent, reaction, msgId, reactionTs)
  | otherwise =
      DB.execute
        db
        [sql|
          DELETE FROM chat_item_reactions
          WHERE group_id = ? AND group_member_id = ? AND shared_msg_id = ? AND item_member_id = ? AND reaction_sent = ? AND reaction = ?
        |]
        (groupId, groupMemberId' m, itemSharedMId, itemMemberId, BI sent, reaction)

getReactionMembers :: DB.Connection -> VersionRangeChat -> User -> GroupId -> SharedMsgId -> MsgReaction -> IO [MemberReaction]
getReactionMembers db vr user groupId itemSharedMId reaction = do
  reactions <-
    DB.query
      db
      [sql|
        SELECT group_member_id, reaction_ts
        FROM chat_item_reactions
        WHERE group_id = ? AND shared_msg_id = ? AND reaction = ?
      |]
      (groupId, itemSharedMId, reaction)
  rights <$> mapM (runExceptT . toMemberReaction) reactions
  where
    toMemberReaction :: (GroupMemberId, UTCTime) -> ExceptT StoreError IO MemberReaction
    toMemberReaction (groupMemberId, reactionTs) = do
      groupMember <- getGroupMemberById db vr user groupMemberId
      pure MemberReaction {groupMember, reactionTs}

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

getChatItemTTL :: DB.Connection -> User -> IO Int64
getChatItemTTL db User {userId} =
  fmap (fromMaybe 0 . join) . maybeFirstRow fromOnly $
    DB.query db "SELECT chat_item_ttl FROM settings WHERE user_id = ? LIMIT 1" (Only userId)

setChatItemTTL :: DB.Connection -> User -> Int64 -> IO ()
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

getChatTTLCount :: DB.Connection -> User -> IO Int
getChatTTLCount db User {userId} = do
  contactCount <- getCount "SELECT COUNT(1) FROM contacts WHERE user_id = ? AND chat_item_ttl > 0"
  groupCount <- getCount "SELECT COUNT(1) FROM groups WHERE user_id = ? AND chat_item_ttl > 0"
  pure $ contactCount + groupCount
  where
    getCount q = fromOnly . head <$> DB.query db q (Only userId)

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

getCIModeration :: DB.Connection -> VersionRangeChat -> User -> GroupInfo -> MemberId -> Maybe SharedMsgId -> IO (Maybe CIModeration)
getCIModeration _ _ _ _ _ Nothing = pure Nothing
getCIModeration db vr user GroupInfo {groupId} itemMemberId (Just sharedMsgId) = do
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
      runExceptT (getGroupMember db vr user groupId moderatorId) >>= \case
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

createGroupSndStatus :: DB.Connection -> ChatItemId -> GroupMemberId -> GroupSndStatus -> IO ()
createGroupSndStatus db itemId memberId status =
  DB.execute
    db
    "INSERT INTO group_snd_item_statuses (chat_item_id, group_member_id, group_snd_item_status) VALUES (?,?,?)"
    (itemId, memberId, status)

getGroupSndStatus :: DB.Connection -> ChatItemId -> GroupMemberId -> ExceptT StoreError IO GroupSndStatus
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

updateGroupSndStatus :: DB.Connection -> ChatItemId -> GroupMemberId -> GroupSndStatus -> IO ()
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

setGroupSndViaProxy :: DB.Connection -> ChatItemId -> GroupMemberId -> Bool -> IO ()
setGroupSndViaProxy db itemId memberId viaProxy =
  DB.execute
    db
    [sql|
      UPDATE group_snd_item_statuses
      SET via_proxy = ?
      WHERE chat_item_id = ? AND group_member_id  = ?
    |]
    (BI viaProxy, itemId, memberId)

getGroupSndStatuses :: DB.Connection -> ChatItemId -> IO [MemberDeliveryStatus]
getGroupSndStatuses db itemId =
  map memStatus
    <$> DB.query
      db
      [sql|
        SELECT group_member_id, group_snd_item_status, via_proxy
        FROM group_snd_item_statuses
        WHERE chat_item_id = ?
      |]
      (Only itemId)
  where
    memStatus (groupMemberId, memberDeliveryStatus, sentViaProxy) =
      MemberDeliveryStatus {groupMemberId, memberDeliveryStatus, sentViaProxy = unBI <$> sentViaProxy}

getGroupSndStatusCounts :: DB.Connection -> ChatItemId -> IO [(GroupSndStatus, Int)]
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

-- TODO [knocking] filter out messages sent to member only
getGroupHistoryItems :: DB.Connection -> User -> GroupInfo -> GroupMember -> Int -> IO [Either StoreError (CChatItem 'CTGroup)]
getGroupHistoryItems db user@User {userId} g@GroupInfo {groupId} m count = do
  ciIds <- getLastItemIds_
  reverse <$> mapM (runExceptT . getGroupCIWithReactions db user g) ciIds
  where
    getLastItemIds_ :: IO [ChatItemId]
    getLastItemIds_ =
      map fromOnly
        <$> DB.query
          db
          [sql|
            SELECT i.chat_item_id
            FROM chat_items i
            LEFT JOIN group_snd_item_statuses s ON s.chat_item_id = i.chat_item_id AND s.group_member_id = ?
            WHERE s.group_snd_item_status_id IS NULL
              AND i.user_id = ? AND i.group_id = ?
              AND i.include_in_history = 1
              AND i.item_deleted = 0
            ORDER BY i.item_ts DESC, i.chat_item_id DESC
            LIMIT ?
          |]
          (groupMemberId' m, userId, groupId, count)
