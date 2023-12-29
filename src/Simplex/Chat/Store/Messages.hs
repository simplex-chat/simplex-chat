{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
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
    deleteGroupCIs,
    createNewSndMessage,
    createSndMsgDelivery,
    createNewMessageAndRcvMsgDelivery,
    createNewRcvMessage,
    updateSndMsgDeliveryStatus,
    updateRcvMsgDeliveryStatus,
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
    markGroupChatItemBlocked,
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
    getDirectCIWithReactions,
    getDirectChatItemBySharedMsgId,
    getDirectChatItemByAgentMsgId,
    getGroupChatItem,
    getGroupCIWithReactions,
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
    getGroupHistoryItems,
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
import Data.List (sortBy)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import Data.Time (addUTCTime)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Database.SQLite.Simple (NamedParam (..), Only (..), Query, (:.) (..))
import Database.SQLite.Simple.QQ (sql)
import Simplex.Chat.Controller (ChatListQuery (..), ChatPagination (..), PaginationByTime (..))
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
import Simplex.Messaging.Version (VersionRange)
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
          (MDSnd, toCMEventTag chatMsgEvent, msgBody, connId_, groupId_, sharedMsgId, Just True, createdAt, createdAt)
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
createNewMessageAndRcvMsgDelivery db connOrGroupId newMessage sharedMsgId_ RcvMsgDelivery {connId, agentMsgId, agentMsgMeta, agentAckCmdId} authorGroupMemberId_ = do
  msg@RcvMessage {msgId} <- createNewRcvMessage db connOrGroupId newMessage sharedMsgId_ authorGroupMemberId_ Nothing
  liftIO $ do
    currentTs <- getCurrentTime
    DB.execute
      db
      [sql|
        INSERT INTO msg_deliveries
          (message_id, connection_id, agent_msg_id, agent_msg_meta, agent_ack_cmd_id, chat_ts, created_at, updated_at, delivery_status)
        VALUES (?,?,?,?,?,?,?,?,?)
      |]
      (msgId, connId, agentMsgId, msgMetaJson agentMsgMeta, agentAckCmdId, snd $ broker agentMsgMeta, currentTs, currentTs, MDSRcvAgent)
  pure msg

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
        (MDRcv, toCMEventTag chatMsgEvent, msgBody, currentTs, currentTs, connId_, groupId_, sharedMsgId_, authorMember, forwardedByMember)
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

updateRcvMsgDeliveryStatus :: DB.Connection -> Int64 -> CommandId -> MsgDeliveryStatus 'MDRcv -> IO ()
updateRcvMsgDeliveryStatus db connId cmdId rcvMsgDeliveryStatus = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE msg_deliveries
      SET delivery_status = ?, updated_at = ?
      WHERE connection_id = ? AND agent_ack_cmd_id = ?
    |]
    (rcvMsgDeliveryStatus, currentTs, connId, cmdId)

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
  createNewChatItem_ db user chatDirection createdByMsgId (Just sharedMsgId) ciContent quoteRow timed live createdAt Nothing createdAt
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
createNewRcvChatItem db user chatDirection RcvMessage {msgId, chatMsgEvent, forwardedByMember} sharedMsgId_ ciContent timed live itemTs createdAt = do
  ciId <- createNewChatItem_ db user chatDirection (Just msgId) sharedMsgId_ ciContent quoteRow timed live itemTs forwardedByMember createdAt
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
createNewChatItemNoMsg db user chatDirection ciContent itemTs =
  createNewChatItem_ db user chatDirection Nothing Nothing ciContent quoteRow Nothing False itemTs Nothing
  where
    quoteRow :: NewQuoteRow
    quoteRow = (Nothing, Nothing, Nothing, Nothing, Nothing)

createNewChatItem_ :: forall c d. MsgDirectionI d => DB.Connection -> User -> ChatDirection c d -> Maybe MessageId -> Maybe SharedMsgId -> CIContent d -> NewQuoteRow -> Maybe CITimed -> Bool -> UTCTime -> Maybe GroupMemberId -> UTCTime -> IO ChatItemId
createNewChatItem_ db User {userId} chatDirection msgId_ sharedMsgId ciContent quoteRow timed live itemTs forwardedByMember createdAt = do
  DB.execute
    db
    [sql|
      INSERT INTO chat_items (
        -- user and IDs
        user_id, created_by_msg_id, contact_id, group_id, group_member_id,
        -- meta
        item_sent, item_ts, item_content, item_content_tag, item_text, item_status, shared_msg_id,
        forwarded_by_group_member_id, created_at, updated_at, item_live, timed_ttl, timed_delete_at,
        -- quote
        quoted_shared_msg_id, quoted_sent_at, quoted_content, quoted_sent, quoted_member_id
      ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
    |]
    ((userId, msgId_) :. idsRow :. itemRow :. quoteRow)
  ciId <- insertedRowId db
  forM_ msgId_ $ \msgId -> insertChatItemMessage_ db ciId msgId createdAt
  pure ciId
  where
    itemRow :: (SMsgDirection d, UTCTime, CIContent d, Text, Text, CIStatus d, Maybe SharedMsgId, Maybe GroupMemberId) :. (UTCTime, UTCTime, Maybe Bool) :. (Maybe Int, Maybe UTCTime)
    itemRow = (msgDirection @d, itemTs, ciContent, toCIContentTag ciContent, ciContentToText ciContent, ciCreateStatus ciContent, sharedMsgId, forwardedByMember) :. (createdAt, createdAt, justTrue live) :. ciTimedRow timed
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
              m.group_member_id, m.group_id, m.member_id, m.peer_chat_min_version, m.peer_chat_max_version, m.member_role, m.member_category,
              m.member_status, m.show_messages, m.invited_by, m.invited_by_group_member_id, m.local_display_name, m.contact_id, m.contact_profile_id, p.contact_profile_id,
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

getChatPreviews :: DB.Connection -> VersionRange -> User -> Bool -> PaginationByTime -> ChatListQuery -> IO [Either StoreError AChat]
getChatPreviews db vr user withPCC pagination query = do
  directChats <- findDirectChatPreviews_ db user pagination query
  groupChats <- findGroupChatPreviews_ db user pagination query
  cReqChats <- getContactRequestChatPreviews_ db user pagination query
  connChats <- if withPCC then getContactConnectionChatPreviews_ db user pagination query else pure []
  let refs = sortTake $ concat [directChats, groupChats, cReqChats, connChats]
  mapM (runExceptT <$> getChatPreview) refs
  where
    ts :: AChatPreviewData -> UTCTime
    ts (ACPD _ cpd) = case cpd of
      (DirectChatPD t _ _ _) -> t
      (GroupChatPD t _ _ _) -> t
      (ContactRequestPD t _) -> t
      (ContactConnectionPD t _) -> t
    sortTake = case pagination of
      PTLast count -> take count . sortBy (comparing $ Down . ts)
      PTAfter _ count -> reverse . take count . sortBy (comparing ts)
      PTBefore _ count -> take count . sortBy (comparing $ Down . ts)
    getChatPreview :: AChatPreviewData -> ExceptT StoreError IO AChat
    getChatPreview (ACPD cType cpd) = case cType of
      SCTDirect -> getDirectChatPreview_ db user cpd
      SCTGroup -> getGroupChatPreview_ db vr user cpd
      SCTContactRequest -> let (ContactRequestPD _ chat) = cpd in pure chat
      SCTContactConnection -> let (ContactConnectionPD _ chat) = cpd in pure chat

data ChatPreviewData (c :: ChatType) where
  DirectChatPD :: UTCTime -> ContactId -> Maybe ChatItemId -> ChatStats -> ChatPreviewData 'CTDirect
  GroupChatPD :: UTCTime -> GroupId -> Maybe ChatItemId -> ChatStats -> ChatPreviewData 'CTGroup
  ContactRequestPD :: UTCTime -> AChat -> ChatPreviewData 'CTContactRequest
  ContactConnectionPD :: UTCTime -> AChat -> ChatPreviewData 'CTContactConnection

data AChatPreviewData = forall c. ChatTypeI c => ACPD (SChatType c) (ChatPreviewData c)

paginationByTimeFilter :: PaginationByTime -> (Query, [NamedParam])
paginationByTimeFilter = \case
  PTLast count -> ("\nORDER BY ts DESC LIMIT :count", [":count" := count])
  PTAfter ts count -> ("\nAND ts > :ts ORDER BY ts ASC LIMIT :count", [":ts" := ts, ":count" := count])
  PTBefore ts count -> ("\nAND ts < :ts ORDER BY ts DESC LIMIT :count", [":ts" := ts, ":count" := count])

type ChatStatsRow = (Int, ChatItemId, Bool)

toChatStats :: ChatStatsRow -> ChatStats
toChatStats (unreadCount, minUnreadItemId, unreadChat) = ChatStats {unreadCount, minUnreadItemId, unreadChat}

findDirectChatPreviews_ :: DB.Connection -> User -> PaginationByTime -> ChatListQuery -> IO [AChatPreviewData]
findDirectChatPreviews_ db User {userId} pagination clq =
  map toPreview <$> getPreviews
  where
    toPreview :: (ContactId, UTCTime, Maybe ChatItemId) :. ChatStatsRow -> AChatPreviewData
    toPreview ((contactId, ts, lastItemId_) :. statsRow) =
      ACPD SCTDirect $ DirectChatPD ts contactId lastItemId_ (toChatStats statsRow)
    baseQuery =
      [sql|
        SELECT ct.contact_id, ct.chat_ts as ts, LastItems.chat_item_id, COALESCE(ChatStats.UnreadCount, 0), COALESCE(ChatStats.MinUnread, 0), ct.unread_chat
        FROM contacts ct
        LEFT JOIN (
          SELECT contact_id, chat_item_id, MAX(created_at)
          FROM chat_items
          GROUP BY contact_id
        ) LastItems ON LastItems.contact_id = ct.contact_id
        LEFT JOIN (
          SELECT contact_id, COUNT(1) AS UnreadCount, MIN(chat_item_id) AS MinUnread
          FROM chat_items
          WHERE item_status = :rcv_new
          GROUP BY contact_id
        ) ChatStats ON ChatStats.contact_id = ct.contact_id
      |]
    (pagQuery, pagParams) = paginationByTimeFilter pagination
    getPreviews = case clq of
      CLQFilters {favorite = False, unread = False} ->
        DB.queryNamed
          db
          ( baseQuery
              <> [sql|
                    WHERE ct.user_id = :user_id AND ct.is_user = 0 AND ct.deleted = 0 AND ct.contact_used
                  |]
              <> pagQuery
          )
          ([":user_id" := userId, ":rcv_new" := CISRcvNew] <> pagParams)
      CLQFilters {favorite = True, unread = False} ->
        DB.queryNamed
          db
          ( baseQuery
              <> [sql|
                    WHERE ct.user_id = :user_id AND ct.is_user = 0 AND ct.deleted = 0 AND ct.contact_used
                      AND ct.favorite = 1
                  |]
              <> pagQuery
          )
          ([":user_id" := userId, ":rcv_new" := CISRcvNew] <> pagParams)
      CLQFilters {favorite = False, unread = True} ->
        DB.queryNamed
          db
          ( baseQuery
              <> [sql|
                    WHERE ct.user_id = :user_id AND ct.is_user = 0 AND ct.deleted = 0 AND ct.contact_used
                      AND (ct.unread_chat = 1 OR ChatStats.UnreadCount > 0)
                  |]
              <> pagQuery
          )
          ([":user_id" := userId, ":rcv_new" := CISRcvNew] <> pagParams)
      CLQFilters {favorite = True, unread = True} ->
        DB.queryNamed
          db
          ( baseQuery
              <> [sql|
                    WHERE ct.user_id = :user_id AND ct.is_user = 0 AND ct.deleted = 0 AND ct.contact_used
                      AND (ct.favorite = 1
                        OR ct.unread_chat = 1 OR ChatStats.UnreadCount > 0)
                  |]
              <> pagQuery
          )
          ([":user_id" := userId, ":rcv_new" := CISRcvNew] <> pagParams)
      CLQSearch {search} ->
        DB.queryNamed
          db
          ( baseQuery
              <> [sql|
                    JOIN contact_profiles cp ON ct.contact_profile_id = cp.contact_profile_id
                    WHERE ct.user_id = :user_id AND ct.is_user = 0 AND ct.deleted = 0 AND ct.contact_used
                      AND (
                        ct.local_display_name LIKE '%' || :search || '%'
                        OR cp.display_name LIKE '%' || :search || '%'
                        OR cp.full_name LIKE '%' || :search || '%'
                        OR cp.local_alias LIKE '%' || :search || '%'
                      )
                  |]
              <> pagQuery
          )
          ([":user_id" := userId, ":rcv_new" := CISRcvNew, ":search" := search] <> pagParams)

getDirectChatPreview_ :: DB.Connection -> User -> ChatPreviewData 'CTDirect -> ExceptT StoreError IO AChat
getDirectChatPreview_ db user (DirectChatPD _ contactId lastItemId_ stats) = do
  contact <- getContact db user contactId
  lastItem <- case lastItemId_ of
    Just lastItemId -> (: []) <$> getDirectChatItem db user contactId lastItemId
    Nothing -> pure []
  pure $ AChat SCTDirect (Chat (DirectChat contact) lastItem stats)

findGroupChatPreviews_ :: DB.Connection -> User -> PaginationByTime -> ChatListQuery -> IO [AChatPreviewData]
findGroupChatPreviews_ db User {userId} pagination clq =
  map toPreview <$> getPreviews
  where
    toPreview :: (GroupId, UTCTime, Maybe ChatItemId) :. ChatStatsRow -> AChatPreviewData
    toPreview ((groupId, ts, lastItemId_) :. statsRow) =
      ACPD SCTGroup $ GroupChatPD ts groupId lastItemId_ (toChatStats statsRow)
    baseQuery =
      [sql|
        SELECT g.group_id, g.chat_ts as ts, LastItems.chat_item_id, COALESCE(ChatStats.UnreadCount, 0), COALESCE(ChatStats.MinUnread, 0), g.unread_chat
        FROM groups g
        LEFT JOIN (
          SELECT group_id, chat_item_id, MAX(item_ts)
          FROM chat_items
          GROUP BY group_id
        ) LastItems ON LastItems.group_id = g.group_id
        LEFT JOIN (
          SELECT group_id, COUNT(1) AS UnreadCount, MIN(chat_item_id) AS MinUnread
          FROM chat_items
          WHERE item_status = :rcv_new
          GROUP BY group_id
        ) ChatStats ON ChatStats.group_id = g.group_id
      |]
    (pagQuery, pagParams) = paginationByTimeFilter pagination
    getPreviews = case clq of
      CLQFilters {favorite = False, unread = False} ->
        DB.queryNamed
          db
          ( baseQuery
              <> [sql|
                    WHERE g.user_id = :user_id
                  |]
              <> pagQuery
          )
          ([":user_id" := userId, ":rcv_new" := CISRcvNew] <> pagParams)
      CLQFilters {favorite = True, unread = False} ->
        DB.queryNamed
          db
          ( baseQuery
              <> [sql|
                    WHERE g.user_id = :user_id
                      AND g.favorite = 1
                  |]
              <> pagQuery
          )
          ([":user_id" := userId, ":rcv_new" := CISRcvNew] <> pagParams)
      CLQFilters {favorite = False, unread = True} ->
        DB.queryNamed
          db
          ( baseQuery
              <> [sql|
                    WHERE g.user_id = :user_id
                      AND (g.unread_chat = 1 OR ChatStats.UnreadCount > 0)
                  |]
              <> pagQuery
          )
          ([":user_id" := userId, ":rcv_new" := CISRcvNew] <> pagParams)
      CLQFilters {favorite = True, unread = True} ->
        DB.queryNamed
          db
          ( baseQuery
              <> [sql|
                    WHERE g.user_id = :user_id
                      AND (g.favorite = 1
                        OR g.unread_chat = 1 OR ChatStats.UnreadCount > 0)
                  |]
              <> pagQuery
          )
          ([":user_id" := userId, ":rcv_new" := CISRcvNew] <> pagParams)
      CLQSearch {search} ->
        DB.queryNamed
          db
          ( baseQuery
              <> [sql|
                    JOIN group_profiles gp ON gp.group_profile_id = g.group_profile_id
                    WHERE g.user_id = :user_id
                      AND (
                        g.local_display_name LIKE '%' || :search || '%'
                        OR gp.display_name LIKE '%' || :search || '%'
                        OR gp.full_name LIKE '%' || :search || '%'
                        OR gp.description LIKE '%' || :search || '%'
                      )
                  |]
              <> pagQuery
          )
          ([":user_id" := userId, ":rcv_new" := CISRcvNew, ":search" := search] <> pagParams)

getGroupChatPreview_ :: DB.Connection -> VersionRange -> User -> ChatPreviewData 'CTGroup -> ExceptT StoreError IO AChat
getGroupChatPreview_ db vr user (GroupChatPD _ groupId lastItemId_ stats) = do
  groupInfo <- getGroupInfo db vr user groupId
  lastItem <- case lastItemId_ of
    Just lastItemId -> (: []) <$> getGroupChatItem db user groupId lastItemId
    Nothing -> pure []
  pure $ AChat SCTGroup (Chat (GroupChat groupInfo) lastItem stats)

getContactRequestChatPreviews_ :: DB.Connection -> User -> PaginationByTime -> ChatListQuery -> IO [AChatPreviewData]
getContactRequestChatPreviews_ db User {userId} pagination clq = case clq of
  CLQFilters {favorite = False, unread = False} -> query ""
  CLQFilters {favorite = True, unread = False} -> pure []
  CLQFilters {favorite = False, unread = True} -> query ""
  CLQFilters {favorite = True, unread = True} -> query ""
  CLQSearch {search} -> query search
  where
    (pagQuery, pagParams) = paginationByTimeFilter pagination
    query search =
      map toPreview
        <$> DB.queryNamed
          db
          ( [sql|
              SELECT
                cr.contact_request_id, cr.local_display_name, cr.agent_invitation_id, cr.user_contact_link_id,
                c.agent_conn_id, cr.contact_profile_id, p.display_name, p.full_name, p.image, p.contact_link, cr.xcontact_id, p.preferences,
                cr.created_at, cr.updated_at as ts,
                cr.peer_chat_min_version, cr.peer_chat_max_version
              FROM contact_requests cr
              JOIN connections c ON c.user_contact_link_id = cr.user_contact_link_id
              JOIN contact_profiles p ON p.contact_profile_id = cr.contact_profile_id
              JOIN user_contact_links uc ON uc.user_contact_link_id = cr.user_contact_link_id
              WHERE cr.user_id = :user_id
                AND uc.user_id = :user_id
                AND uc.local_display_name = ''
                AND uc.group_id IS NULL
                AND (
                  cr.local_display_name LIKE '%' || :search || '%'
                  OR p.display_name LIKE '%' || :search || '%'
                  OR p.full_name LIKE '%' || :search || '%'
                )
            |]
              <> pagQuery
          )
          ([":user_id" := userId, ":search" := search] <> pagParams)
    toPreview :: ContactRequestRow -> AChatPreviewData
    toPreview cReqRow =
      let cReq@UserContactRequest {updatedAt} = toContactRequest cReqRow
          stats = ChatStats {unreadCount = 0, minUnreadItemId = 0, unreadChat = False}
          aChat = AChat SCTContactRequest $ Chat (ContactRequest cReq) [] stats
       in ACPD SCTContactRequest $ ContactRequestPD updatedAt aChat

getContactConnectionChatPreviews_ :: DB.Connection -> User -> PaginationByTime -> ChatListQuery -> IO [AChatPreviewData]
getContactConnectionChatPreviews_ db User {userId} pagination clq = case clq of
  CLQFilters {favorite = False, unread = False} -> query ""
  CLQFilters {favorite = True, unread = False} -> pure []
  CLQFilters {favorite = False, unread = True} -> pure []
  CLQFilters {favorite = True, unread = True} -> pure []
  CLQSearch {search} -> query search
  where
    (pagQuery, pagParams) = paginationByTimeFilter pagination
    query search =
      map toPreview
        <$> DB.queryNamed
          db
          ( [sql|
              SELECT
                connection_id, agent_conn_id, conn_status, via_contact_uri_hash, via_user_contact_link, group_link_id,
                custom_user_profile_id, conn_req_inv, local_alias, created_at, updated_at as ts
              FROM connections
              WHERE user_id = :user_id
                AND conn_type = :conn_contact
                AND contact_id IS NULL
                AND conn_level = 0
                AND via_contact IS NULL
                AND (via_group_link = 0 || (via_group_link = 1 AND group_link_id IS NOT NULL))
                AND local_alias LIKE '%' || :search || '%'
            |]
              <> pagQuery
          )
          ([":user_id" := userId, ":conn_contact" := ConnContact, ":search" := search] <> pagParams)
    toPreview :: (Int64, ConnId, ConnStatus, Maybe ByteString, Maybe Int64, Maybe GroupLinkId, Maybe Int64, Maybe ConnReqInvitation, LocalAlias, UTCTime, UTCTime) -> AChatPreviewData
    toPreview connRow =
      let conn@PendingContactConnection {updatedAt} = toPendingContactConnection connRow
          stats = ChatStats {unreadCount = 0, minUnreadItemId = 0, unreadChat = False}
          aChat = AChat SCTContactConnection $ Chat (ContactConnection conn) [] stats
       in ACPD SCTContactConnection $ ContactConnectionPD updatedAt aChat

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
        ORDER BY i.created_at DESC, i.chat_item_id DESC
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
            ORDER BY i.created_at ASC, i.chat_item_id ASC
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
            ORDER BY i.created_at DESC, i.chat_item_id DESC
            LIMIT ?
          |]
          (userId, contactId, search, beforeChatItemId, count)

getGroupChat :: DB.Connection -> VersionRange -> User -> Int64 -> ChatPagination -> Maybe String -> ExceptT StoreError IO (Chat 'CTGroup)
getGroupChat db vr user groupId pagination search_ = do
  let search = fromMaybe "" search_
  g <- getGroupInfo db vr user groupId
  case pagination of
    CPLast count -> getGroupChatLast_ db user g count search
    CPAfter afterId count -> getGroupChatAfter_ db user g afterId count search
    CPBefore beforeId count -> getGroupChatBefore_ db user g beforeId count search

getGroupChatLast_ :: DB.Connection -> User -> GroupInfo -> Int -> String -> ExceptT StoreError IO (Chat 'CTGroup)
getGroupChatLast_ db user@User {userId} g@GroupInfo {groupId} count search = do
  let stats = ChatStats {unreadCount = 0, minUnreadItemId = 0, unreadChat = False}
  chatItemIds <- liftIO getGroupChatItemIdsLast_
  chatItems <- mapM (getGroupCIWithReactions db user g) chatItemIds
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
  chatItems <- mapM (getGroupCIWithReactions db user g) chatItemIds
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
  chatItems <- mapM (getGroupCIWithReactions db user g) chatItemIds
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

type MaybeCIFIleRow = (Maybe Int64, Maybe String, Maybe Integer, Maybe FilePath, Maybe C.SbKey, Maybe C.CbNonce, Maybe ACIFileStatus, Maybe FileProtocol)

type ChatItemModeRow = (Maybe Int, Maybe UTCTime, Maybe Bool)

type ChatItemRow = (Int64, ChatItemTs, AMsgDirection, Text, Text, ACIStatus, Maybe SharedMsgId) :. (Int, Maybe UTCTime, Maybe Bool, UTCTime, UTCTime) :. ChatItemModeRow :. MaybeCIFIleRow

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
      let itemDeleted' = case itemDeleted of
            DBCINotDeleted -> Nothing
            _ -> Just (CIDeleted @'CTDirect deletedTs)
          itemEdited' = fromMaybe False itemEdited
       in mkCIMeta itemId content itemText status sharedMsgId itemDeleted' itemEdited' ciTimed itemLive currentTs itemTs Nothing createdAt updatedAt
    ciTimed :: Maybe CITimed
    ciTimed = timedTTL >>= \ttl -> Just CITimed {ttl, deleteAt = timedDeleteAt}

type GroupQuoteRow = QuoteRow :. MaybeGroupMemberRow

toGroupQuote :: QuoteRow -> Maybe GroupMember -> Maybe (CIQuote 'CTGroup)
toGroupQuote qr@(_, _, _, _, quotedSent) quotedMember_ = toQuote qr $ direction quotedSent quotedMember_
  where
    direction (Just True) _ = Just CIQGroupSnd
    direction (Just False) (Just member) = Just . CIQGroupRcv $ Just member
    direction (Just False) Nothing = Just $ CIQGroupRcv Nothing
    direction _ _ = Nothing

-- this function can be changed so it never fails, not only avoid failure on invalid json
toGroupChatItem :: UTCTime -> Int64 -> ChatItemRow :. Only (Maybe GroupMemberId) :. MaybeGroupMemberRow :. GroupQuoteRow :. MaybeGroupMemberRow -> Either StoreError (CChatItem 'CTGroup)
toGroupChatItem currentTs userContactId (((itemId, itemTs, AMsgDirection msgDir, itemContentText, itemText, itemStatus, sharedMsgId) :. (itemDeleted, deletedTs, itemEdited, createdAt, updatedAt) :. (timedTTL, timedDeleteAt, itemLive) :. (fileId_, fileName_, fileSize_, filePath, fileKey, fileNonce, fileStatus_, fileProtocol_)) :. Only forwardedByMember :. memberRow_ :. (quoteRow :. quotedMemberRow_) :. deletedByGroupMemberRow_) = do
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
      let itemDeleted' = case itemDeleted of
            DBCINotDeleted -> Nothing
            DBCIBlocked -> Just (CIBlocked deletedTs)
            _ -> Just (maybe (CIDeleted @'CTGroup deletedTs) (CIModerated deletedTs) deletedByGroupMember_)
          itemEdited' = fromMaybe False itemEdited
       in mkCIMeta itemId content itemText status sharedMsgId itemDeleted' itemEdited' ciTimed itemLive currentTs itemTs forwardedByMember createdAt updatedAt
    ciTimed :: Maybe CITimed
    ciTimed = timedTTL >>= \ttl -> Just CITimed {ttl, deleteAt = timedDeleteAt}

getAllChatItems :: DB.Connection -> VersionRange -> User -> ChatPagination -> Maybe String -> ExceptT StoreError IO [AChatItem]
getAllChatItems db vr user@User {userId} pagination search_ = do
  itemRefs <-
    rights . map toChatItemRef <$> case pagination of
      CPLast count -> liftIO $ getAllChatItemsLast_ count
      CPAfter afterId count -> liftIO . getAllChatItemsAfter_ afterId count . aChatItemTs =<< getAChatItem_ afterId
      CPBefore beforeId count -> liftIO . getAllChatItemsBefore_ beforeId count . aChatItemTs =<< getAChatItem_ beforeId
  mapM (uncurry (getAChatItem db vr user) >=> liftIO . getACIReactions db) itemRefs
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

updateDirectChatItemStatus :: forall d. MsgDirectionI d => DB.Connection -> User -> Contact -> ChatItemId -> CIStatus d -> ExceptT StoreError IO (ChatItem 'CTDirect d)
updateDirectChatItemStatus db user@User {userId} ct@Contact {contactId} itemId itemStatus = do
  ci <- liftEither . correctDir =<< getDirectCIWithReactions db user ct itemId
  currentTs <- liftIO getCurrentTime
  liftIO $ DB.execute db "UPDATE chat_items SET item_status = ?, updated_at = ? WHERE user_id = ? AND contact_id = ? AND chat_item_id = ?" (itemStatus, currentTs, userId, contactId, itemId)
  pure ci {meta = (meta ci) {itemStatus}}

updateDirectChatItem :: MsgDirectionI d => DB.Connection -> User -> Contact -> ChatItemId -> CIContent d -> Bool -> Maybe MessageId -> ExceptT StoreError IO (ChatItem 'CTDirect d)
updateDirectChatItem db user ct@Contact {contactId} itemId newContent live msgId_ = do
  ci <- liftEither . correctDir =<< getDirectCIWithReactions db user ct itemId
  liftIO $ updateDirectChatItem' db user contactId ci newContent live msgId_

getDirectCIWithReactions :: DB.Connection -> User -> Contact -> ChatItemId -> ExceptT StoreError IO (CChatItem 'CTDirect)
getDirectCIWithReactions db user ct@Contact {contactId} itemId =
  liftIO . directCIWithReactions db ct =<< getDirectChatItem db user contactId itemId

correctDir :: MsgDirectionI d => CChatItem c -> Either StoreError (ChatItem c d)
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

markDirectChatItemDeleted :: DB.Connection -> User -> Contact -> ChatItem 'CTDirect d -> MessageId -> UTCTime -> IO (ChatItem 'CTDirect d)
markDirectChatItemDeleted db User {userId} Contact {contactId} ci@ChatItem {meta} msgId deletedTs = do
  currentTs <- liftIO getCurrentTime
  let itemId = chatItemId' ci
  insertChatItemMessage_ db itemId msgId currentTs
  DB.execute
    db
    [sql|
      UPDATE chat_items
      SET item_deleted = ?, item_deleted_ts = ?, updated_at = ?
      WHERE user_id = ? AND contact_id = ? AND chat_item_id = ?
    |]
    (DBCIDeleted, deletedTs, currentTs, userId, contactId, itemId)
  pure ci {meta = meta {itemDeleted = Just $ CIDeleted $ Just deletedTs}}

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
  firstRow' (toDirectChatItem currentTs) (SEChatItemNotFound itemId) getItem
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
groupCIWithReactions db g cci@(CChatItem md ci@ChatItem {meta = CIMeta {itemSharedMsgId}}) = case itemSharedMsgId of
  Just sharedMsgId -> do
    let GroupMember {memberId} = chatItemMember g ci
    reactions <- getGroupCIReactions db g memberId sharedMsgId
    pure $ CChatItem md ci {reactions}
  Nothing -> pure cci

updateGroupChatItem :: MsgDirectionI d => DB.Connection -> User -> Int64 -> ChatItem 'CTGroup d -> CIContent d -> Bool -> Maybe MessageId -> IO (ChatItem 'CTGroup d)
updateGroupChatItem db user groupId ci newContent live msgId_ = do
  currentTs <- liftIO getCurrentTime
  let ci' = updatedChatItem ci newContent live currentTs
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
    ((content, itemText, itemStatus, itemDeleted', itemDeletedTs', itemEdited, itemLive, updatedAt) :. ciTimedRow itemTimed :. (userId, groupId, itemId))
  forM_ msgId_ $ \msgId -> insertChatItemMessage_ db itemId msgId updatedAt

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
  pure $ ci {content = toContent, meta = (meta ci) {itemText = toText, itemDeleted = Just (CIModerated (Just currentTs) m), editable = False}, formattedText = Nothing}

pattern DBCINotDeleted :: Int
pattern DBCINotDeleted = 0

pattern DBCIDeleted :: Int
pattern DBCIDeleted = 1

pattern DBCIBlocked :: Int
pattern DBCIBlocked = 2

markGroupChatItemDeleted :: DB.Connection -> User -> GroupInfo -> ChatItem 'CTGroup d -> MessageId -> Maybe GroupMember -> UTCTime -> IO (ChatItem 'CTGroup d)
markGroupChatItemDeleted db User {userId} GroupInfo {groupId} ci@ChatItem {meta} msgId byGroupMember_ deletedTs = do
  currentTs <- liftIO getCurrentTime
  let itemId = chatItemId' ci
      (deletedByGroupMemberId, itemDeleted) = case byGroupMember_ of
        Just m@GroupMember {groupMemberId} -> (Just groupMemberId, Just $ CIModerated (Just deletedTs) m)
        _ -> (Nothing, Just $ CIDeleted @'CTGroup (Just deletedTs))
  insertChatItemMessage_ db itemId msgId currentTs
  DB.execute
    db
    [sql|
      UPDATE chat_items
      SET item_deleted = ?, item_deleted_ts = ?, item_deleted_by_group_member_id = ?, updated_at = ?
      WHERE user_id = ? AND group_id = ? AND chat_item_id = ?
    |]
    (DBCIDeleted, deletedTs, deletedByGroupMemberId, currentTs, userId, groupId, itemId)
  pure ci {meta = meta {itemDeleted}}

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
  pure ci {meta = meta {itemDeleted = Just $ CIBlocked $ Just deletedTs}}

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
  firstRow' (toGroupChatItem currentTs userContactId) (SEChatItemNotFound itemId) getItem
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
            -- CIMeta forwardedByMember
            i.forwarded_by_group_member_id,
            -- GroupMember
            m.group_member_id, m.group_id, m.member_id, m.peer_chat_min_version, m.peer_chat_max_version, m.member_role, m.member_category,
            m.member_status, m.show_messages, m.invited_by, m.invited_by_group_member_id, m.local_display_name, m.contact_id, m.contact_profile_id, p.contact_profile_id,
            p.display_name, p.full_name, p.image, p.contact_link, p.local_alias, p.preferences,
            -- quoted ChatItem
            ri.chat_item_id, i.quoted_shared_msg_id, i.quoted_sent_at, i.quoted_content, i.quoted_sent,
            -- quoted GroupMember
            rm.group_member_id, rm.group_id, rm.member_id, rm.peer_chat_min_version, rm.peer_chat_max_version, rm.member_role, rm.member_category,
            rm.member_status, rm.show_messages, rm.invited_by, rm.invited_by_group_member_id, rm.local_display_name, rm.contact_id, rm.contact_profile_id, rp.contact_profile_id,
            rp.display_name, rp.full_name, rp.image, rp.contact_link, rp.local_alias, rp.preferences,
            -- deleted by GroupMember
            dbm.group_member_id, dbm.group_id, dbm.member_id, dbm.peer_chat_min_version, dbm.peer_chat_max_version, dbm.member_role, dbm.member_category,
            dbm.member_status, dbm.show_messages, dbm.invited_by, dbm.invited_by_group_member_id, dbm.local_display_name, dbm.contact_id, dbm.contact_profile_id, dbp.contact_profile_id,
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

getChatItemByFileId :: DB.Connection -> VersionRange -> User -> Int64 -> ExceptT StoreError IO AChatItem
getChatItemByFileId db vr user@User {userId} fileId = do
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
  getAChatItem db vr user chatRef itemId

getChatItemByGroupId :: DB.Connection -> VersionRange -> User -> GroupId -> ExceptT StoreError IO AChatItem
getChatItemByGroupId db vr user@User {userId} groupId = do
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
  getAChatItem db vr user chatRef itemId

getChatRefViaItemId :: DB.Connection -> User -> ChatItemId -> ExceptT StoreError IO ChatRef
getChatRefViaItemId db User {userId} itemId = do
  ExceptT . firstRow' toChatRef (SEChatItemNotFound itemId) $
    DB.query db "SELECT contact_id, group_id FROM chat_items WHERE user_id = ? AND chat_item_id = ?" (userId, itemId)
  where
    toChatRef = \case
      (Just contactId, Nothing) -> Right $ ChatRef CTDirect contactId
      (Nothing, Just groupId) -> Right $ ChatRef CTGroup groupId
      (_, _) -> Left $ SEBadChatItem itemId

getAChatItem :: DB.Connection -> VersionRange -> User -> ChatRef -> ChatItemId -> ExceptT StoreError IO AChatItem
getAChatItem db vr user chatRef itemId = case chatRef of
  ChatRef CTDirect contactId -> do
    ct <- getContact db user contactId
    (CChatItem msgDir ci) <- getDirectChatItem db user contactId itemId
    pure $ AChatItem SCTDirect msgDir (DirectChat ct) ci
  ChatRef CTGroup groupId -> do
    gInfo <- getGroupInfo db vr user groupId
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
  chatItems' <- mapM (directCIWithReactions db ct) chatItems
  pure c {chatItems = chatItems'}

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

getGroupHistoryItems :: DB.Connection -> User -> GroupInfo -> Int -> IO [Either StoreError (CChatItem 'CTGroup)]
getGroupHistoryItems db user@User {userId} GroupInfo {groupId} count = do
  chatItemIds <- getLastItemIds_
  -- use getGroupCIWithReactions to read reactions data
  reverse <$> mapM (runExceptT . getGroupChatItem db user groupId) chatItemIds
  where
    getLastItemIds_ :: IO [ChatItemId]
    getLastItemIds_ =
      map fromOnly
        <$> DB.query
          db
          [sql|
            SELECT chat_item_id
            FROM chat_items
            WHERE user_id = ? AND group_id = ?
              AND item_content_tag IN (?,?)
              AND item_deleted = 0
            ORDER BY item_ts DESC, chat_item_id DESC
            LIMIT ?
          |]
          (userId, groupId, rcvMsgContentTag, sndMsgContentTag, count)
