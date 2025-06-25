{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Simplex.Chat.Store.ContactRequest
  ( createOrUpdateContactRequest,
    setContactRequestAccepted
  )
  where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Crypto.Random (ChaChaDRG)
import Data.Int (Int64)
import Data.Time.Clock (getCurrentTime)
import Simplex.Chat.Protocol (businessChatsVersion, MsgContent)
import Simplex.Chat.Store.Direct
import Simplex.Chat.Store.Groups
import Simplex.Chat.Store.Shared
import Simplex.Chat.Store.Profiles
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences
import Simplex.Messaging.Agent.Protocol (InvitationId)
import Simplex.Messaging.Agent.Store.AgentStore (maybeFirstRow)
import Simplex.Messaging.Agent.Store.DB (Binary (..), BoolInt (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Crypto.Ratchet (PQSupport)
import Simplex.Messaging.Version
import UnliftIO.STM
#if defined(dbPostgres)
import Database.PostgreSQL.Simple ((:.) (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple ((:.) (..))
import Database.SQLite.Simple.QQ (sql)
#endif

createOrUpdateContactRequest ::
  DB.Connection
  -> TVar ChaChaDRG
  -> VersionRangeChat
  -> User
  -> Int64
  -> UserContactLink
  -> Bool
  -> InvitationId
  -> VersionRangeChat
  -> Profile
  -> Maybe XContactId
  -> Maybe SharedMsgId
  -> Maybe (SharedMsgId, MsgContent)
  -> PQSupport
  -> ExceptT StoreError IO RequestStage
createOrUpdateContactRequest
  db
  gVar
  vr
  user@User {userId, userContactId}
  uclId
  UserContactLink {addressSettings = AddressSettings {businessAddress}}
  isSimplexTeam
  invId
  cReqChatVRange@(VersionRange minV maxV)
  profile@Profile {displayName, fullName, image, contactLink, preferences}
  xContactId_
  welcomeMsgId_
  requestMsg_
  pqSup =
    case xContactId_ of
      -- 0) this is very old legacy, when we didn't have xContactId at all (this should be deprecated)
      Nothing -> createContactRequest
      Just xContactId ->
        -- Here real logic starts:
        -- 1) first we try to find legacy accepted contact or business chat by xContactId
        --    (only legacy contacts and business chats may have xContactId set after contact request acceptance,
        --     while new contacts and business chats instead keep contact request record, which now has `accepted` field);
        --    this requires separate queries, because we were deleting contact request records after acceptance;
        --    this can also be deprecated in future releases: getLegacyAcceptedContact, getLegacyAcceptedBusinessChat
        liftIO (getLegacyAcceptedContact xContactId) >>= \case
          Just ct -> pure $ RSAcceptedRequest Nothing (REContact ct)
          Nothing -> liftIO (getLegacyAcceptedBusinessChat xContactId) >>= \case
            Just gInfo@GroupInfo {businessChat = Just BusinessChatInfo {customerId}} -> do
              clientMember <- getGroupMemberByMemberId db vr user gInfo customerId
              pure $ RSAcceptedRequest Nothing (REBusinessChat gInfo clientMember)
            Just GroupInfo {businessChat = Nothing} -> throwError SEInvalidBusinessChatContactRequest
            -- 2) if no legacy accepted contact or business chat was found, next we try to find an existing request
            Nothing ->
              liftIO (getContactRequestByXContactId xContactId) >>= \case
                Just cr@UserContactRequest {contactRequestId, accepted}
                  -- 3a) if accepted request was found, we return it as accepted request
                  | accepted ->
                      getRequestEntity cr >>= \case
                        Just re -> pure $ RSAcceptedRequest (Just cr) re
                        Nothing -> throwError $ SEInvalidContactRequestEntity contactRequestId
                  -- 3b) if request was found and it's not accepted, we update it
                  | otherwise -> updateContactRequest cr
                -- 3c) if no request was found, we create a new contact request
                Nothing -> createContactRequest
  where
    -- only legacy contacts and business chats may have xContactId set after contact request acceptance;
    -- new contacts and business chats instead keep contact request record, which has accepted field
    getLegacyAcceptedContact :: XContactId -> IO (Maybe Contact)
    getLegacyAcceptedContact xContactId = do
      ct_ <-
        maybeFirstRow (toContact vr user []) $
          DB.query
            db
            [sql|
              SELECT
                -- Contact
                ct.contact_id, ct.contact_profile_id, ct.local_display_name, ct.via_group, cp.display_name, cp.full_name, cp.image, cp.contact_link, cp.local_alias, ct.contact_used, ct.contact_status, ct.enable_ntfs, ct.send_rcpts, ct.favorite,
                cp.preferences, ct.user_preferences, ct.created_at, ct.updated_at, ct.chat_ts, ct.conn_full_link_to_connect, ct.conn_short_link_to_connect, ct.welcome_shared_msg_id, ct.contact_request_id,
                ct.contact_group_member_id, ct.contact_grp_inv_sent, ct.ui_themes, ct.chat_deleted, ct.custom_data, ct.chat_item_ttl,
                -- Connection
                c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id, c.conn_status, c.conn_type, c.contact_conn_initiated, c.local_alias,
                c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at, c.pq_support, c.pq_encryption, c.pq_snd_enabled, c.pq_rcv_enabled, c.auth_err_counter, c.quota_err_counter,
                c.conn_chat_version, c.peer_chat_min_version, c.peer_chat_max_version
              FROM contacts ct
              JOIN contact_profiles cp ON ct.contact_profile_id = cp.contact_profile_id
              LEFT JOIN connections c ON c.contact_id = ct.contact_id
              WHERE ct.user_id = ? AND ct.xcontact_id = ? AND ct.deleted = 0
              ORDER BY c.created_at DESC
              LIMIT 1
            |]
            (userId, xContactId)
      mapM (addDirectChatTags db) ct_
    getLegacyAcceptedBusinessChat :: XContactId -> IO (Maybe GroupInfo)
    getLegacyAcceptedBusinessChat xContactId = do
      g_ <-
        maybeFirstRow (toGroupInfo vr userContactId []) $
          DB.query
            db
            (groupInfoQuery <> " WHERE g.business_xcontact_id = ? AND g.user_id = ? AND mu.contact_id = ?")
            (xContactId, userId, userContactId)
      mapM (addGroupChatTags db) g_
    getContactRequestByXContactId :: XContactId -> IO (Maybe UserContactRequest)
    getContactRequestByXContactId xContactId =
      maybeFirstRow toContactRequest $
        DB.query
          db
          [sql|
            SELECT
              cr.contact_request_id, cr.local_display_name, cr.agent_invitation_id,
              cr.contact_id, cr.business_group_id, cr.user_contact_link_id,
              c.agent_conn_id, cr.contact_profile_id, p.display_name, p.full_name, p.image, p.contact_link, cr.xcontact_id,
              cr.pq_support, cr.accepted, cr.welcome_shared_msg_id, cr.request_shared_msg_id, p.preferences,
              cr.created_at, cr.updated_at,
              cr.peer_chat_min_version, cr.peer_chat_max_version
            FROM contact_requests cr
            JOIN connections c USING (user_contact_link_id)
            JOIN contact_profiles p USING (contact_profile_id)
            WHERE cr.user_id = ?
              AND cr.xcontact_id = ?
            LIMIT 1
          |]
          (userId, xContactId)
    createContactRequest :: ExceptT StoreError IO RequestStage
    createContactRequest = do
      currentTs <- liftIO $ getCurrentTime
      ExceptT $ withLocalDisplayName db userId displayName $ \ldn -> runExceptT $ do
        liftIO $
          DB.execute
            db
            "INSERT INTO contact_profiles (display_name, full_name, image, contact_link, user_id, preferences, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?)"
            (displayName, fullName, image, contactLink, userId, preferences, currentTs, currentTs)
        profileId <- liftIO $ insertedRowId db
        liftIO $
          DB.execute
            db
            [sql|
              INSERT INTO contact_requests
                (user_contact_link_id, agent_invitation_id, peer_chat_min_version, peer_chat_max_version, contact_profile_id, local_display_name, user_id,
                  created_at, updated_at, xcontact_id, welcome_shared_msg_id, request_shared_msg_id, pq_support)
              VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)
            |]
            ( (uclId, Binary invId, minV, maxV, profileId, ldn, userId)
                :. (currentTs, currentTs, xContactId_, welcomeMsgId_, fst <$> requestMsg_, pqSup)
            )
        contactRequestId <- liftIO $ insertedRowId db
        createRequestEntity ldn profileId contactRequestId currentTs
      where
        createRequestEntity ldn profileId contactRequestId currentTs
          | businessAddress =
              if isSimplexTeam && maxV < businessChatsVersion
                then createContact'
                else createBusinessChat
          | otherwise = createContact'
          where
            createContact' = do
              liftIO $
                DB.execute
                  db
                  "INSERT INTO contacts (contact_profile_id, local_display_name, user_id, created_at, updated_at, chat_ts, contact_used, contact_request_id) VALUES (?,?,?,?,?,?,?,?)"
                  (profileId, ldn, userId, currentTs, currentTs, currentTs, BI True, contactRequestId)
              contactId <- liftIO $ insertedRowId db
              liftIO $
                DB.execute
                  db
                  "UPDATE contact_requests SET contact_id = ? WHERE contact_request_id = ?"
                  (contactId, contactRequestId)
              ucr <- getContactRequest db user contactRequestId
              ct <- getContact db vr user contactId
              pure $ RSCurrentRequest ucr (Just $ REContact ct) False
            createBusinessChat = do
              let Profile {preferences = userPreferences} = profileToSendOnAccept user Nothing True
                  groupPreferences = maybe defaultBusinessGroupPrefs businessGroupPrefs userPreferences
              (gInfo@GroupInfo {groupId}, clientMember) <-
                createBusinessRequestGroup db vr gVar user cReqChatVRange profile groupPreferences
              liftIO $
                DB.execute
                  db
                  "UPDATE contact_requests SET business_group_id = ? WHERE contact_request_id = ?"
                  (groupId, contactRequestId)
              ucr <- getContactRequest db user contactRequestId
              pure $ RSCurrentRequest ucr (Just $ REBusinessChat gInfo clientMember) False
    updateContactRequest :: UserContactRequest -> ExceptT StoreError IO RequestStage
    updateContactRequest ucr@UserContactRequest {contactRequestId = cReqId, contactId_, localDisplayName = oldLdn, profile = Profile {displayName = oldDisplayName}} = do
      currentTs <- liftIO getCurrentTime
      liftIO $ updateProfile currentTs
      updateRequest currentTs
      re_ <- getRequestEntity ucr
      pure $ RSCurrentRequest ucr re_ True
      where
        updateProfile currentTs =
          DB.execute
            db
            [sql|
              UPDATE contact_profiles
              SET display_name = ?,
                  full_name = ?,
                  image = ?,
                  contact_link = ?,
                  updated_at = ?
              WHERE contact_profile_id IN (
                SELECT contact_profile_id
                FROM contact_requests
                WHERE user_id = ?
                  AND contact_request_id = ?
              )
            |]
            (displayName, fullName, image, contactLink, currentTs, userId, cReqId)
        updateRequest currentTs =
          if displayName == oldDisplayName
            then
              liftIO $
                DB.execute
                  db
                  [sql|
                    UPDATE contact_requests
                    SET agent_invitation_id = ?, pq_support = ?, peer_chat_min_version = ?, peer_chat_max_version = ?, updated_at = ?
                    WHERE user_id = ? AND contact_request_id = ?
                  |]
                  (Binary invId, pqSup, minV, maxV, currentTs, userId, cReqId)
            else
              ExceptT $ withLocalDisplayName db userId displayName $ \ldn -> runExceptT $ do
                liftIO $ do
                  DB.execute
                    db
                    [sql|
                      UPDATE contact_requests
                      SET agent_invitation_id = ?, pq_support = ?, peer_chat_min_version = ?, peer_chat_max_version = ?, local_display_name = ?, updated_at = ?
                      WHERE user_id = ? AND contact_request_id = ?
                    |]
                    (Binary invId, pqSup, minV, maxV, ldn, currentTs, userId, cReqId)
                  -- Here we could also update group member or business chat,
                  -- but they are synchronously auto-accepted so it's less of an issue
                  forM_ contactId_ $ \contactId ->
                    DB.execute
                      db
                      [sql|
                        UPDATE contacts
                        SET local_display_name = ?, updated_at = ?
                        WHERE contact_id = ?
                      |]
                      (ldn, currentTs, contactId)
                  safeDeleteLDN db user oldLdn
    getRequestEntity :: UserContactRequest -> ExceptT StoreError IO (Maybe RequestEntity)
    getRequestEntity UserContactRequest {contactRequestId, contactId_, businessGroupId_} =
      case (contactId_, businessGroupId_) of
        (Just contactId, Nothing) -> do
          ct <- getContact db vr user contactId
          pure $ Just (REContact ct)
        (Nothing, Just businessGroupId) -> do
          gInfo <- getGroupInfo db vr user businessGroupId
          case gInfo of
            GroupInfo {businessChat = Just BusinessChatInfo {customerId}} -> do
              clientMember <- getGroupMemberByMemberId db vr user gInfo customerId
              pure $ Just (REBusinessChat gInfo clientMember)
            _ -> throwError SEInvalidBusinessChatContactRequest
        (Nothing, Nothing) -> pure Nothing
        _ -> throwError $ SEInvalidContactRequestEntity contactRequestId

setContactRequestAccepted :: DB.Connection -> UserContactRequest -> IO ()
setContactRequestAccepted db UserContactRequest {contactRequestId} = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE contact_requests
      SET accepted = ?, updated_at = ?
      WHERE contact_request_id = ?
    |]
    (BI True, currentTs, contactRequestId)
