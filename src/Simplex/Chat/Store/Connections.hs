{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Simplex.Chat.Store.Connections
  ( getChatLockEntity,
    getConnectionEntity,
    getConnectionEntityByConnReq,
    getConnectionEntityViaShortLink,
    getContactConnEntityByConnReqHash,
    getContactConnsToSub,
    getUCLConnsToSub,
    getMemberConnsToSub,
    getPendingConnsToSub,
    shouldSyncConnections,
    setConnectionsSyncTs,
  )
where

import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Bitraversable (bitraverse)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Simplex.Chat.Badges (rowToBadge)
import Simplex.Chat.Protocol
import Simplex.Chat.Store.Direct
import Simplex.Chat.Store.Groups
import Simplex.Chat.Store.Shared
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Protocol (ConnId)
import Simplex.Messaging.Agent.Store.AgentStore (firstRow, firstRow', fromOnlyBI, maybeFirstRow)
import Simplex.Messaging.Agent.Store.DB (BoolInt (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Util (eitherToMaybe)
#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..), (:.) (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..), (:.) (..))
import Database.SQLite.Simple.QQ (sql)
#endif

getChatLockEntity :: DB.Connection -> AgentConnId -> ExceptT StoreError IO ChatLockEntity
getChatLockEntity db agentConnId = do
  ((connId, connType) :. (contactId, groupMemberId, userContactLinkId)) <-
    ExceptT . firstRow id (SEConnectionNotFound agentConnId) $
      DB.query
        db
        [sql|
          SELECT connection_id, conn_type, contact_id, group_member_id, user_contact_link_id
          FROM connections
          WHERE agent_conn_id = ?
        |]
        (Only agentConnId)
  let err = throwError $ SEInternalError $ "connection " <> show connType <> " without entity"
  case connType of
    ConnMember -> maybe err (fmap CLGroup . getMemberGroupId) groupMemberId
    ConnContact -> pure $ maybe (CLConnection connId) CLContact contactId
    ConnUserContact -> maybe err (pure . CLUserContact) userContactLinkId
  where
    getMemberGroupId :: GroupMemberId -> ExceptT StoreError IO GroupId
    getMemberGroupId groupMemberId =
      ExceptT . firstRow fromOnly (SEInternalError "group member connection group_id not found") $
        DB.query db "SELECT group_id FROM group_members WHERE group_member_id = ?" (Only groupMemberId)

-- TODO consider whether ConnFailed connections should be excluded:
-- - from receiving: getConnectionEntity, getContactConnEntityByConnReqHash
-- - from subscribing: getContactConnsToSub, getUCLConnsToSub, getMemberConnsToSub, getPendingConnsToSub
getConnectionEntity :: DB.Connection -> StoreCxt -> User -> AgentConnId -> ExceptT StoreError IO ConnectionEntity
getConnectionEntity db cxt user@User {userId, userContactId} agentConnId = do
  c@Connection {connType, entityId} <- getConnection_
  case entityId of
    Nothing ->
      if connType == ConnContact
        then pure $ RcvDirectMsgConnection c Nothing
        else throwError $ SEInternalError $ "connection " <> show connType <> " without entity"
    Just entId ->
      case connType of
        ConnMember -> uncurry (RcvGroupMsgConnection c) <$> getGroupAndMember_ entId c
        ConnContact -> RcvDirectMsgConnection c . Just <$> getContactRec_ entId c
        ConnUserContact -> UserContactConnection c <$> getUserContact_ entId
  where
    getConnection_ :: ExceptT StoreError IO Connection
    getConnection_ = ExceptT $ do
      firstRow (toConnection cxt) (SEConnectionNotFound agentConnId) $
        DB.query
          db
          [sql|
            SELECT connection_id, agent_conn_id, conn_level, via_contact, via_user_contact_link, via_group_link, group_link_id, xcontact_id, custom_user_profile_id,
              conn_status, conn_type, contact_conn_initiated, local_alias, contact_id, group_member_id, user_contact_link_id,
              created_at, security_code, security_code_verified_at, pq_support, pq_encryption, pq_snd_enabled, pq_rcv_enabled, auth_err_counter, quota_err_counter,
              conn_chat_version, peer_chat_min_version, peer_chat_max_version
            FROM connections
            WHERE user_id = ? AND agent_conn_id = ? AND conn_status != ?
          |]
          (userId, agentConnId, ConnDeleted)
    getContactRec_ :: Int64 -> Connection -> ExceptT StoreError IO Contact
    getContactRec_ contactId c = ExceptT $ do
      currentTs <- getCurrentTime
      chatTags <- getDirectChatTags db contactId
      firstRow (toContact' currentTs contactId c chatTags) (SEInternalError "referenced contact not found") $
        DB.query
          db
          [sql|
            SELECT
              c.contact_profile_id, c.local_display_name, p.display_name, p.full_name, p.short_descr, p.description, p.image, p.contact_link, p.chat_peer_type, p.local_alias, c.contact_used, c.contact_status, c.enable_ntfs, c.send_rcpts, c.favorite,
              p.preferences, c.user_preferences, c.created_at, c.updated_at, c.chat_ts, c.conn_full_link_to_connect, c.conn_short_link_to_connect, c.welcome_shared_msg_id, c.request_shared_msg_id, c.contact_request_id, cr2.rejection_supported,
              c.contact_group_member_id, c.contact_grp_inv_sent, c.grp_direct_inv_link, c.grp_direct_inv_from_group_id, c.grp_direct_inv_from_group_member_id, c.grp_direct_inv_from_member_conn_id, c.grp_direct_inv_started_connection,
              c.ui_themes, c.chat_deleted, c.custom_data, c.chat_item_ttl,
              p.badge_proof, p.badge_pres_header, p.badge_expiry, p.badge_type, p.badge_verified, p.badge_extra, p.badge_master_key, p.badge_signature, p.badge_key_idx,
              p.contact_domain, p.contact_domain_proof, p.contact_domain_verified
            FROM contacts c
            JOIN contact_profiles p ON c.contact_profile_id = p.contact_profile_id
            LEFT JOIN contact_requests cr2 ON cr2.contact_request_id = c.contact_request_id
            WHERE c.user_id = ? AND c.contact_id = ? AND c.contact_status = ? AND c.deleted = 0
          |]
          (userId, contactId, CSActive)
    toContact' :: UTCTime -> Int64 -> Connection -> [ChatTagId] -> ContactRow' -> Contact
    toContact' currentTs contactId conn chatTags ((profileId, localDisplayName, displayName, fullName, shortDescr, description, image, contactLink, peerType, localAlias, BI contactUsed, contactStatus) :. (enableNtfs_, sendRcpts, BI favorite, preferences, userPreferences, createdAt, updatedAt, chatTs) :. preparedContactRow :. (contactRequestId, rejectionSupported_, contactGroupMemberId, BI contactGrpInvSent) :. groupDirectInvRow :. (uiThemes, BI chatDeleted, customData, chatItemTTL) :. badgeRow :. domainRow) =
      let profile = LocalProfile {profileId, displayName, fullName, shortDescr, description, image, contactLink, contactDomain = rowToContactDomain domainRow, contactDomainVerified = rowToDomainVerified domainRow, peerType, localBadge = rowToBadge currentTs badgeRow, preferences, localAlias}
          chatSettings = ChatSettings {enableNtfs = fromMaybe MFAll enableNtfs_, sendRcpts = unBI <$> sendRcpts, favorite}
          mergedPreferences = contactUserPreferences user userPreferences preferences $ connIncognito conn
          activeConn = Just conn
          preparedContact = toPreparedContact preparedContactRow
          contactRequest = UserContactRequestRef <$> contactRequestId <*> (unBI <$> rejectionSupported_)
          groupDirectInv = toGroupDirectInvitation groupDirectInvRow
       in Contact {contactId, localDisplayName, profile, activeConn, contactUsed, contactStatus, chatSettings, userPreferences, mergedPreferences, createdAt, updatedAt, chatTs, preparedContact, contactRequestId, contactRequest, contactGroupMemberId, contactGrpInvSent, groupDirectInv, chatTags, chatItemTTL, uiThemes, chatDeleted, customData}
    getGroupAndMember_ :: Int64 -> Connection -> ExceptT StoreError IO (GroupInfo, GroupMember)
    getGroupAndMember_ groupMemberId c = do
      currentTs <- liftIO getCurrentTime
      gm <-
        ExceptT $
          firstRow (toGroupAndMember currentTs c) (SEInternalError "referenced group member not found") $
            DB.query
              db
              [sql|
                SELECT
                  -- GroupInfo
                  g.group_id, g.local_display_name, gp.display_name, gp.full_name, gp.short_descr, g.local_alias, gp.description, gp.image, gp.group_type, gp.group_link, gp.public_group_id,
                  gp.group_web_page, gp.group_domain, gp.domain_web_page, gp.allow_embedding, gp.group_domain_proof,
                  g.enable_ntfs, g.send_rcpts, g.favorite, gp.preferences, gp.member_admission,
                  g.created_at, g.updated_at, g.chat_ts, g.user_member_profile_sent_at,
                  g.conn_full_link_to_connect, g.conn_short_link_to_connect, g.conn_link_prepared_connection, g.conn_link_started_connection, g.welcome_shared_msg_id, g.request_shared_msg_id,
                  g.business_chat, g.business_member_id, g.customer_member_id,
                  g.use_relays, g.relay_own_status,
                  g.ui_themes, g.summary_current_members_count, g.public_member_count, g.roster_version, g.custom_data, g.chat_item_ttl, g.members_require_attention, g.via_group_link_uri, g.group_domain_verified,
                  g.root_priv_key, g.root_pub_key, g.member_priv_key,
                  -- GroupInfo {membership}
                  mu.group_member_id, mu.group_id, mu.index_in_group, mu.member_id, mu.peer_chat_min_version, mu.peer_chat_max_version, mu.member_role, mu.member_category,
                  mu.member_status, mu.show_messages, mu.member_restriction, mu.invited_by, mu.invited_by_group_member_id, mu.local_display_name, mu.contact_id, mu.contact_profile_id, pu.contact_profile_id,
                  -- GroupInfo {membership = GroupMember {memberProfile}}
                  pu.display_name, pu.full_name, pu.short_descr, pu.description, pu.image, pu.contact_link, pu.chat_peer_type, pu.local_alias, pu.preferences,
                  pu.badge_proof, pu.badge_pres_header, pu.badge_expiry, pu.badge_type, pu.badge_verified, pu.badge_extra, pu.badge_master_key, pu.badge_signature, pu.badge_key_idx, pu.contact_domain, pu.contact_domain_proof, pu.contact_domain_verified,
                  mu.created_at, mu.updated_at,
                  mu.support_chat_ts, mu.support_chat_items_unread, mu.support_chat_items_member_attention, mu.support_chat_items_mentions, mu.support_chat_last_msg_from_member_ts, mu.member_pub_key, mu.relay_link, mu.member_security_code, mu.member_security_code_verified_at,
                  -- from GroupMember
                  m.group_member_id, m.group_id, m.index_in_group, m.member_id, m.peer_chat_min_version, m.peer_chat_max_version, m.member_role, m.member_category, m.member_status, m.show_messages, m.member_restriction,
                  m.invited_by, m.invited_by_group_member_id, m.local_display_name, m.contact_id, m.contact_profile_id, p.contact_profile_id, p.display_name, p.full_name, p.short_descr, p.description, p.image, p.contact_link, p.chat_peer_type, p.local_alias, p.preferences,
                  p.badge_proof, p.badge_pres_header, p.badge_expiry, p.badge_type, p.badge_verified, p.badge_extra, p.badge_master_key, p.badge_signature, p.badge_key_idx, p.contact_domain, p.contact_domain_proof, p.contact_domain_verified,
                  m.created_at, m.updated_at,
                  m.support_chat_ts, m.support_chat_items_unread, m.support_chat_items_member_attention, m.support_chat_items_mentions, m.support_chat_last_msg_from_member_ts, m.member_pub_key, m.relay_link, m.member_security_code, m.member_security_code_verified_at
                FROM group_members m
                JOIN contact_profiles p ON p.contact_profile_id = COALESCE(m.member_profile_id, m.contact_profile_id)
                JOIN groups g ON g.group_id = m.group_id
                JOIN group_profiles gp USING (group_profile_id)
                JOIN group_members mu ON g.group_id = mu.group_id
                JOIN contact_profiles pu ON pu.contact_profile_id = COALESCE(mu.member_profile_id, mu.contact_profile_id)
                WHERE m.group_member_id = ? AND g.user_id = ? AND mu.contact_id = ?
                  AND mu.member_status NOT IN (?,?,?)
              |]
              (groupMemberId, userId, userContactId, GSMemRemoved, GSMemLeft, GSMemGroupDeleted)
      liftIO $ bitraverse (addGroupChatTags db) pure gm
    toGroupAndMember :: UTCTime -> Connection -> GroupInfoRow :. GroupMemberRow -> (GroupInfo, GroupMember)
    toGroupAndMember currentTs c (groupInfoRow :. memberRow) =
      let groupInfo = toGroupInfo currentTs cxt userContactId [] groupInfoRow
          member = toGroupMember currentTs userContactId memberRow
       in (groupInfo, (member :: GroupMember) {activeConn = Just c})
    getUserContact_ :: Int64 -> ExceptT StoreError IO UserContact
    getUserContact_ userContactLinkId = ExceptT $ do
      userContact_
        <$> DB.query
          db
          [sql|
            SELECT conn_req_contact, group_id
            FROM user_contact_links
            WHERE user_id = ? AND user_contact_link_id = ?
          |]
          (userId, userContactLinkId)
      where
        userContact_ :: [(ConnReqContact, Maybe GroupId)] -> Either StoreError UserContact
        userContact_ [(cReq, groupId)] = Right UserContact {userContactLinkId, connReqContact = cReq, groupId}
        userContact_ _ = Left SEUserContactLinkNotFound

getConnectionEntityByConnReq :: DB.Connection -> StoreCxt -> User -> (ConnReqInvitation, ConnReqInvitation) -> IO (Maybe ConnectionEntity)
getConnectionEntityByConnReq db cxt user@User {userId} (cReqSchema1, cReqSchema2) = do
  connId_ <-
    maybeFirstRow fromOnly $
      DB.query db "SELECT agent_conn_id FROM connections WHERE user_id = ? AND conn_req_inv IN (?,?) LIMIT 1" (userId, cReqSchema1, cReqSchema2)
  maybe (pure Nothing) (fmap eitherToMaybe . runExceptT . getConnectionEntity db cxt user) connId_

getConnectionEntityViaShortLink :: DB.Connection -> StoreCxt -> User -> ShortLinkInvitation -> IO (Maybe (ConnReqInvitation, ConnectionEntity))
getConnectionEntityViaShortLink db cxt user@User {userId} shortLink = fmap eitherToMaybe $ runExceptT $ do
  (cReq, connId) <- ExceptT getConnReqConnId
  (cReq,) <$> getConnectionEntity db cxt user connId
  where
    getConnReqConnId =
      firstRow' toConnReqConnId (SEInternalError "connection not found") $
        DB.query
          db
          [sql|
            SELECT conn_req_inv, agent_conn_id
            FROM connections
            WHERE user_id = ? AND short_link_inv = ? LIMIT 1
          |]
          (userId, shortLink)
    -- cReq is Maybe - it is removed when connection is established
    toConnReqConnId = \case
      (Just cReq, connId) -> Right (cReq, connId)
      _ -> Left $ SEInternalError "no connection request"

-- search connection for connection plan:
-- multiple connections can have same via_contact_uri_hash if request was repeated;
-- this function searches for latest connection with contact so that "known contact" plan would be chosen;
-- deleted connections are filtered out to allow re-connecting via same contact address
getContactConnEntityByConnReqHash :: DB.Connection -> StoreCxt -> User -> (ConnReqUriHash, ConnReqUriHash) -> IO (Maybe ConnectionEntity)
getContactConnEntityByConnReqHash db cxt user@User {userId} (cReqHash1, cReqHash2) = do
  connId_ <-
    maybeFirstRow fromOnly $
      DB.query
        db
        [sql|
          SELECT agent_conn_id FROM (
            SELECT
              agent_conn_id,
              (CASE WHEN contact_id IS NOT NULL THEN 1 ELSE 0 END) AS conn_ord
            FROM connections
            WHERE user_id = ? AND via_contact_uri_hash IN (?,?) AND conn_status != ?
            ORDER BY conn_ord DESC, created_at DESC
            LIMIT 1
          ) c
        |]
        (userId, cReqHash1, cReqHash2, ConnDeleted)
  maybe (pure Nothing) (fmap eitherToMaybe . runExceptT . getConnectionEntity db cxt user) connId_

getContactConnsToSub :: DB.Connection -> User -> Bool -> IO [ConnId]
getContactConnsToSub db User {userId} filterToSubscribe =
  map fromOnly <$> DB.query db query (userId, ConnDeleted, CSActive)
  where
    query
      | filterToSubscribe = baseQuery <> " AND c.to_subscribe = 1 " <> cond
      | otherwise = baseQuery <> " " <> cond
    baseQuery =
      [sql|
        SELECT c.agent_conn_id
        FROM connections c
        JOIN contacts ct ON ct.contact_id = c.contact_id
        WHERE c.user_id = ?
      |]
    cond =
      [sql|
        AND c.conn_status != ?
        AND ct.contact_status = ? AND ct.deleted = 0
      |]

getUCLConnsToSub :: DB.Connection -> User -> Bool -> IO [ConnId]
getUCLConnsToSub db User {userId} filterToSubscribe =
  map fromOnly <$> DB.query db query (userId, ConnDeleted)
  where
    query
      | filterToSubscribe = baseQuery <> " AND c.to_subscribe = 1 " <> cond
      | otherwise = baseQuery <> " " <> cond
    baseQuery =
      [sql|
        SELECT c.agent_conn_id
        FROM connections c
        JOIN user_contact_links ucl ON ucl.user_contact_link_id = c.user_contact_link_id
        WHERE c.user_id = ?
      |]
    cond = " AND c.conn_status != ?"

getMemberConnsToSub :: DB.Connection -> User -> Bool -> IO [ConnId]
getMemberConnsToSub db User {userId, userContactId} filterToSubscribe =
  map fromOnly <$>
    DB.query
      db
      query
      ((userId, userContactId, GSMemRemoved, GSMemLeft, GSMemGroupDeleted)
        :. (userId, ConnDeleted, GSMemRemoved, GSMemLeft, GSMemGroupDeleted))
  where
    query
      | filterToSubscribe = baseQuery <> " AND c.to_subscribe = 1"
      | otherwise = baseQuery
    baseQuery =
      [sql|
        WITH user_groups AS MATERIALIZED (
          SELECT g.group_id
          FROM groups g
          JOIN group_members mu ON mu.group_id = g.group_id
          WHERE g.user_id = ?
            AND mu.contact_id = ?
            AND mu.member_status NOT IN (?,?,?)
        )
        SELECT c.agent_conn_id
        FROM connections c
        JOIN group_members m ON m.group_member_id = c.group_member_id
        JOIN user_groups ug ON ug.group_id = m.group_id
        WHERE c.user_id = ?
          AND c.conn_status != ?
          AND m.member_status NOT IN (?,?,?)
      |]

getPendingConnsToSub :: DB.Connection -> User -> Bool -> IO [ConnId]
getPendingConnsToSub db User {userId} filterToSubscribe =
  map fromOnly <$> DB.query db query (userId, ConnContact, ConnDeleted)
  where
    query
      | filterToSubscribe = baseQuery <> " AND to_subscribe = 1 " <> cond
      | otherwise = baseQuery <> " " <> cond
    baseQuery =
      [sql|
        SELECT agent_conn_id
        FROM connections
        WHERE user_id = ?
      |]
    cond =
      [sql|
        AND conn_type = ?
        AND contact_id IS NULL
        AND conn_status != ?
      |]

shouldSyncConnections :: DB.Connection -> IO Bool
shouldSyncConnections db =
  fromOnlyBI . head
    <$> DB.query_
      db
      "SELECT should_sync FROM connections_sync WHERE connections_sync_id = 1"

setConnectionsSyncTs :: DB.Connection -> IO ()
setConnectionsSyncTs db = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE connections_sync
      SET should_sync = 0, last_sync_ts = ?
      WHERE connections_sync_id = 1
    |]
    (Only currentTs)
