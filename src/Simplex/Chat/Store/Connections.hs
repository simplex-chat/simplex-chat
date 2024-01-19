{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Simplex.Chat.Store.Connections
  ( getConnectionEntity,
    getConnectionEntityByConnReq,
    getContactConnEntityByConnReqHash,
    getConnectionsToSubscribe,
    unsetConnectionToSubscribe,
  )
where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.Except
import Data.Int (Int64)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Time.Clock (UTCTime (..))
import Database.SQLite.Simple (Only (..), (:.) (..))
import Database.SQLite.Simple.QQ (sql)
import Simplex.Chat.Protocol
import Simplex.Chat.Store.Files
import Simplex.Chat.Store.Groups
import Simplex.Chat.Store.Profiles
import Simplex.Chat.Store.Shared
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences
import Simplex.Messaging.Agent.Protocol (ConnId)
import Simplex.Messaging.Agent.Store.SQLite (firstRow, firstRow', maybeFirstRow)
import qualified Simplex.Messaging.Agent.Store.SQLite.DB as DB
import Simplex.Messaging.Util (eitherToMaybe)
import Simplex.Messaging.Version (VersionRange)

getConnectionEntity :: DB.Connection -> VersionRange -> User -> AgentConnId -> ExceptT StoreError IO ConnectionEntity
getConnectionEntity db vr user@User {userId, userContactId} agentConnId = do
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
        ConnSndFile -> SndFileConnection c <$> getConnSndFileTransfer_ entId c
        ConnRcvFile -> RcvFileConnection c <$> getRcvFileTransfer db user entId
        ConnUserContact -> UserContactConnection c <$> getUserContact_ entId
  where
    getConnection_ :: ExceptT StoreError IO Connection
    getConnection_ = ExceptT $ do
      firstRow toConnection (SEConnectionNotFound agentConnId) $
        DB.query
          db
          [sql|
            SELECT connection_id, agent_conn_id, conn_level, via_contact, via_user_contact_link, via_group_link, group_link_id, custom_user_profile_id,
              conn_status, conn_type, contact_conn_initiated, local_alias, contact_id, group_member_id, snd_file_id, rcv_file_id, user_contact_link_id, created_at, security_code, security_code_verified_at, auth_err_counter,
              peer_chat_min_version, peer_chat_max_version
            FROM connections
            WHERE user_id = ? AND agent_conn_id = ?
          |]
          (userId, agentConnId)
    getContactRec_ :: Int64 -> Connection -> ExceptT StoreError IO Contact
    getContactRec_ contactId c = ExceptT $ do
      toContact' contactId c
        <$> DB.query
          db
          [sql|
            SELECT
              c.contact_profile_id, c.local_display_name, p.display_name, p.full_name, p.image, p.contact_link, p.local_alias, c.via_group, c.contact_used, c.contact_status, c.enable_ntfs, c.send_rcpts, c.favorite,
              p.preferences, c.user_preferences, c.created_at, c.updated_at, c.chat_ts, c.contact_group_member_id, c.contact_grp_inv_sent
            FROM contacts c
            JOIN contact_profiles p ON c.contact_profile_id = p.contact_profile_id
            WHERE c.user_id = ? AND c.contact_id = ? AND c.deleted = 0
          |]
          (userId, contactId)
    toContact' :: Int64 -> Connection -> [(ProfileId, ContactName, Text, Text, Maybe ImageData, Maybe ConnReqContact, LocalAlias, Maybe Int64, Bool, ContactStatus) :. (Maybe MsgFilter, Maybe Bool, Bool, Maybe Preferences, Preferences, UTCTime, UTCTime, Maybe UTCTime, Maybe GroupMemberId, Bool)] -> Either StoreError Contact
    toContact' contactId conn [(profileId, localDisplayName, displayName, fullName, image, contactLink, localAlias, viaGroup, contactUsed, contactStatus) :. (enableNtfs_, sendRcpts, favorite, preferences, userPreferences, createdAt, updatedAt, chatTs, contactGroupMemberId, contactGrpInvSent)] =
      let profile = LocalProfile {profileId, displayName, fullName, image, contactLink, preferences, localAlias}
          chatSettings = ChatSettings {enableNtfs = fromMaybe MFAll enableNtfs_, sendRcpts, favorite}
          mergedPreferences = contactUserPreferences user userPreferences preferences $ connIncognito conn
          activeConn = Just conn
       in Right Contact {contactId, localDisplayName, profile, activeConn, viaGroup, contactUsed, contactStatus, chatSettings, userPreferences, mergedPreferences, createdAt, updatedAt, chatTs, contactGroupMemberId, contactGrpInvSent}
    toContact' _ _ _ = Left $ SEInternalError "referenced contact not found"
    getGroupAndMember_ :: Int64 -> Connection -> ExceptT StoreError IO (GroupInfo, GroupMember)
    getGroupAndMember_ groupMemberId c = ExceptT $ do
      firstRow (toGroupAndMember c) (SEInternalError "referenced group member not found") $
        DB.query
          db
          [sql|
            SELECT
              -- GroupInfo
              g.group_id, g.local_display_name, gp.display_name, gp.full_name, gp.description, gp.image, g.host_conn_custom_user_profile_id, g.enable_ntfs, g.send_rcpts, g.favorite, gp.preferences, g.created_at, g.updated_at, g.chat_ts,
              -- GroupInfo {membership}
              mu.group_member_id, mu.group_id, mu.member_id, mu.peer_chat_min_version, mu.peer_chat_max_version, mu.member_role, mu.member_category,
              mu.member_status, mu.show_messages, mu.invited_by, mu.invited_by_group_member_id, mu.local_display_name, mu.contact_id, mu.contact_profile_id, pu.contact_profile_id,
              -- GroupInfo {membership = GroupMember {memberProfile}}
              pu.display_name, pu.full_name, pu.image, pu.contact_link, pu.local_alias, pu.preferences,
              -- from GroupMember
              m.group_member_id, m.group_id, m.member_id, m.peer_chat_min_version, m.peer_chat_max_version, m.member_role, m.member_category, m.member_status, m.show_messages,
              m.invited_by, m.invited_by_group_member_id, m.local_display_name, m.contact_id, m.contact_profile_id, p.contact_profile_id, p.display_name, p.full_name, p.image, p.contact_link, p.local_alias, p.preferences
            FROM group_members m
            JOIN contact_profiles p ON p.contact_profile_id = COALESCE(m.member_profile_id, m.contact_profile_id)
            JOIN groups g ON g.group_id = m.group_id
            JOIN group_profiles gp USING (group_profile_id)
            JOIN group_members mu ON g.group_id = mu.group_id
            JOIN contact_profiles pu ON pu.contact_profile_id = COALESCE(mu.member_profile_id, mu.contact_profile_id)
            WHERE m.group_member_id = ? AND g.user_id = ? AND mu.contact_id = ?
          |]
          (groupMemberId, userId, userContactId)
    toGroupAndMember :: Connection -> GroupInfoRow :. GroupMemberRow -> (GroupInfo, GroupMember)
    toGroupAndMember c (groupInfoRow :. memberRow) =
      let groupInfo = toGroupInfo vr userContactId groupInfoRow
          member = toGroupMember userContactId memberRow
       in (groupInfo, (member :: GroupMember) {activeConn = Just c})
    getConnSndFileTransfer_ :: Int64 -> Connection -> ExceptT StoreError IO SndFileTransfer
    getConnSndFileTransfer_ fileId Connection {connId} =
      ExceptT $
        firstRow' (sndFileTransfer_ fileId connId) (SESndFileNotFound fileId) $
          DB.query
            db
            [sql|
              SELECT s.file_status, f.file_name, f.file_size, f.chunk_size, f.file_path, s.file_descr_id, s.file_inline, s.group_member_id, cs.local_display_name, m.local_display_name
              FROM snd_files s
              JOIN files f USING (file_id)
              LEFT JOIN contacts cs USING (contact_id)
              LEFT JOIN group_members m USING (group_member_id)
              WHERE f.user_id = ? AND f.file_id = ? AND s.connection_id = ?
            |]
            (userId, fileId, connId)
    sndFileTransfer_ :: Int64 -> Int64 -> (FileStatus, String, Integer, Integer, FilePath, Maybe Int64, Maybe InlineFileMode, Maybe Int64, Maybe ContactName, Maybe ContactName) -> Either StoreError SndFileTransfer
    sndFileTransfer_ fileId connId (fileStatus, fileName, fileSize, chunkSize, filePath, fileDescrId, fileInline, groupMemberId, contactName_, memberName_) =
      case contactName_ <|> memberName_ of
        Just recipientDisplayName -> Right SndFileTransfer {fileId, fileStatus, fileName, fileSize, chunkSize, filePath, fileDescrId, fileInline, recipientDisplayName, connId, agentConnId, groupMemberId}
        Nothing -> Left $ SESndFileInvalid fileId
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

getConnectionEntityByConnReq :: DB.Connection -> VersionRange -> User -> (ConnReqInvitation, ConnReqInvitation) -> IO (Maybe ConnectionEntity)
getConnectionEntityByConnReq db vr user@User {userId} (cReqSchema1, cReqSchema2) = do
  connId_ <-
    maybeFirstRow fromOnly $
      DB.query db "SELECT agent_conn_id FROM connections WHERE user_id = ? AND conn_req_inv IN (?,?) LIMIT 1" (userId, cReqSchema1, cReqSchema2)
  maybe (pure Nothing) (fmap eitherToMaybe . runExceptT . getConnectionEntity db vr user) connId_

-- search connection for connection plan:
-- multiple connections can have same via_contact_uri_hash if request was repeated;
-- this function searches for latest connection with contact so that "known contact" plan would be chosen;
-- deleted connections are filtered out to allow re-connecting via same contact address
getContactConnEntityByConnReqHash :: DB.Connection -> VersionRange -> User -> (ConnReqUriHash, ConnReqUriHash) -> IO (Maybe ConnectionEntity)
getContactConnEntityByConnReqHash db vr user@User {userId} (cReqHash1, cReqHash2) = do
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
          )
        |]
        (userId, cReqHash1, cReqHash2, ConnDeleted)
  maybe (pure Nothing) (fmap eitherToMaybe . runExceptT . getConnectionEntity db vr user) connId_

getConnectionsToSubscribe :: DB.Connection -> VersionRange -> IO ([ConnId], [ConnectionEntity])
getConnectionsToSubscribe db vr = do
  aConnIds <- map fromOnly <$> DB.query_ db "SELECT agent_conn_id FROM connections where to_subscribe = 1"
  entities <- forM aConnIds $ \acId -> do
    getUserByAConnId db acId >>= \case
      Just user -> eitherToMaybe <$> runExceptT (getConnectionEntity db vr user acId)
      Nothing -> pure Nothing
  unsetConnectionToSubscribe db
  let connIds = map (\(AgentConnId connId) -> connId) aConnIds
  pure (connIds, catMaybes entities)

unsetConnectionToSubscribe :: DB.Connection -> IO ()
unsetConnectionToSubscribe db = DB.execute_ db "UPDATE connections SET to_subscribe = 0 WHERE to_subscribe = 1"
