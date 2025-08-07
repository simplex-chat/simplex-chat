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
    setContactAcceptedXContactId,
    setBusinessChatAcceptedXContactId,
    setRequestSharedMsgIdForContact,
    setRequestSharedMsgIdForGroup
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
  profile@Profile {displayName, fullName, shortDescr, image, contactLink, preferences}
  xContactId_
  welcomeMsgId_
  requestMsg_
  pqSup =
    case xContactId_ of
      -- 0) this is very old legacy, when we didn't have xContactId at all (this should be deprecated)
      Nothing -> createContactRequest
      Just xContactId ->
        -- 1) first we try to find accepted contact or business chat by xContactId
        liftIO (getAcceptedContact xContactId) >>= \case
          Just ct -> do
            cr <- liftIO $ getContactRequestByXContactId xContactId
            pure $ RSAcceptedRequest cr (REContact ct)
          Nothing -> liftIO (getAcceptedBusinessChat xContactId) >>= \case
            Just gInfo@GroupInfo {businessChat = Just BusinessChatInfo {customerId}} -> do
              clientMember <- getGroupMemberByMemberId db vr user gInfo customerId
              cr <- liftIO $ getContactRequestByXContactId xContactId
              pure $ RSAcceptedRequest cr (REBusinessChat gInfo clientMember)
            Just GroupInfo {businessChat = Nothing} -> throwError SEInvalidBusinessChatContactRequest
            -- 2) if no legacy accepted contact or business chat was found, next we try to find an existing request
            Nothing ->
              liftIO (getContactRequestByXContactId xContactId) >>= \case
                -- 3a) if request was found, we update it
                Just cr -> updateContactRequest cr
                -- 3b) if no request was found, we create a new contact request
                Nothing -> createContactRequest
  where
    getAcceptedContact :: XContactId -> IO (Maybe Contact)
    getAcceptedContact xContactId = do
      ct_ <-
        maybeFirstRow (toContact vr user []) $
          DB.query
            db
            [sql|
              SELECT
                -- Contact
                ct.contact_id, ct.contact_profile_id, ct.local_display_name, ct.via_group, cp.display_name, cp.full_name, cp.short_descr, cp.image, cp.contact_link, cp.chat_peer_type, cp.local_alias, ct.contact_used, ct.contact_status, ct.enable_ntfs, ct.send_rcpts, ct.favorite,
                cp.preferences, ct.user_preferences, ct.created_at, ct.updated_at, ct.chat_ts, ct.conn_full_link_to_connect, ct.conn_short_link_to_connect, ct.welcome_shared_msg_id, ct.request_shared_msg_id, ct.contact_request_id,
                ct.contact_group_member_id, ct.contact_grp_inv_sent, ct.grp_direct_inv_link, ct.grp_direct_inv_from_group_id, ct.grp_direct_inv_from_group_member_id, ct.grp_direct_inv_from_member_conn_id, ct.grp_direct_inv_started_connection,
                ct.ui_themes, ct.chat_deleted, ct.custom_data, ct.chat_item_ttl,
                -- Connection
                c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.xcontact_id, c.custom_user_profile_id, c.conn_status, c.conn_type, c.contact_conn_initiated, c.local_alias,
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
    getAcceptedBusinessChat :: XContactId -> IO (Maybe GroupInfo)
    getAcceptedBusinessChat xContactId = do
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
              cr.contact_profile_id, p.display_name, p.full_name, p.short_descr, p.image, p.contact_link, p.chat_peer_type, cr.xcontact_id,
              cr.pq_support, cr.welcome_shared_msg_id, cr.request_shared_msg_id, p.preferences,
              cr.created_at, cr.updated_at,
              cr.peer_chat_min_version, cr.peer_chat_max_version
            FROM contact_requests cr
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
            "INSERT INTO contact_profiles (display_name, full_name, short_descr, image, contact_link, user_id, preferences, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?)"
            (displayName, fullName, shortDescr, image, contactLink, userId, preferences, currentTs, currentTs)
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
              let ctUserPreferences = newContactUserPrefs user profile
              liftIO $
                DB.execute
                  db
                  "INSERT INTO contacts (contact_profile_id, user_preferences, local_display_name, user_id, created_at, updated_at, chat_ts, contact_used, contact_request_id) VALUES (?,?,?,?,?,?,?,?,?)"
                  (profileId, ctUserPreferences, ldn, userId, currentTs, currentTs, currentTs, BI True, contactRequestId)
              contactId <- liftIO $ insertedRowId db
              liftIO $
                DB.execute
                  db
                  "UPDATE contact_requests SET contact_id = ? WHERE contact_request_id = ?"
                  (contactId, contactRequestId)
              ucr <- getContactRequest db user contactRequestId
              ct <- getContact db vr user contactId
              pure $ RSCurrentRequest Nothing ucr (Just $ REContact ct)
            createBusinessChat = do
              let groupPreferences = maybe defaultBusinessGroupPrefs businessGroupPrefs $ preferences' user
              (gInfo@GroupInfo {groupId}, clientMember) <-
                createBusinessRequestGroup db vr gVar user cReqChatVRange profile profileId ldn groupPreferences
              liftIO $
                DB.execute
                  db
                  "UPDATE contact_requests SET business_group_id = ? WHERE contact_request_id = ?"
                  (groupId, contactRequestId)
              ucr <- getContactRequest db user contactRequestId
              pure $ RSCurrentRequest Nothing ucr (Just $ REBusinessChat gInfo clientMember)
    updateContactRequest :: UserContactRequest -> ExceptT StoreError IO RequestStage
    updateContactRequest ucr@UserContactRequest {contactRequestId, contactId_, localDisplayName = oldLdn, profile = Profile {displayName = oldDisplayName}} = do
      currentTs <- liftIO getCurrentTime
      liftIO $ updateProfile currentTs
      updateRequest currentTs
      ucr' <- getContactRequest db user contactRequestId
      re_ <- getRequestEntity ucr'
      pure $ RSCurrentRequest (Just ucr) ucr' re_
      where
        updateProfile currentTs =
          DB.execute
            db
            [sql|
              UPDATE contact_profiles
              SET display_name = ?,
                  full_name = ?,
                  short_descr = ?,
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
            (displayName, fullName, shortDescr, image, contactLink, currentTs, userId, contactRequestId)
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
                  (Binary invId, pqSup, minV, maxV, currentTs, userId, contactRequestId)
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
                    (Binary invId, pqSup, minV, maxV, ldn, currentTs, userId, contactRequestId)
                  -- Here we could also update business chat, but is always synchronously auto-accepted so it's less of an issue
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

setContactAcceptedXContactId :: DB.Connection -> Contact -> XContactId -> IO ()
setContactAcceptedXContactId db Contact {contactId} xContactId =
  DB.execute db "UPDATE contacts SET xcontact_id = ? WHERE contact_id = ?" (xContactId, contactId)

setBusinessChatAcceptedXContactId :: DB.Connection -> GroupInfo -> XContactId -> IO ()
setBusinessChatAcceptedXContactId db GroupInfo {groupId} xContactId =
  DB.execute db "UPDATE groups SET business_xcontact_id = ? WHERE group_id = ?" (xContactId, groupId)

setRequestSharedMsgIdForContact :: DB.Connection -> ContactId -> SharedMsgId -> IO ()
setRequestSharedMsgIdForContact db contactId sharedMsgId = do
  DB.execute db "UPDATE contacts SET request_shared_msg_id = ? WHERE contact_id = ?" (sharedMsgId, contactId)

setRequestSharedMsgIdForGroup :: DB.Connection -> GroupId -> SharedMsgId -> IO ()
setRequestSharedMsgIdForGroup db groupId sharedMsgId = do
  DB.execute db "UPDATE groups SET request_shared_msg_id = ? WHERE group_id = ?" (sharedMsgId, groupId)
