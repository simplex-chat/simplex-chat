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

module Simplex.Chat.Store.Direct
  ( updateContactLDN_,
    updateContactProfile_,
    updateContactProfile_',
    updateMemberContactProfileReset_',
    updateMemberContactProfileReset_,
    updateMemberContactProfile_,
    updateMemberContactProfile_',
    deleteContactProfile_,
    deleteUnusedProfile_,

    -- * Contacts and connections functions
    getPendingContactConnection,
    deletePendingContactConnection,
    createDirectConnection,
    createIncognitoProfile,
    createConnReqConnection,
    createAddressContactConnection,
    getProfileById,
    getConnReqContactXContactId,
    getContactByConnReqHash,
    createDirectContact,
    deleteContactConnections,
    deleteContactFiles,
    deleteContact,
    deleteContactWithoutGroups,
    getDeletedContacts,
    getContactByName,
    getContact,
    getContactIdByName,
    updateContactProfile,
    updateContactUserPreferences,
    updateContactAlias,
    updateContactConnectionAlias,
    updatePCCIncognito,
    deletePCCIncognitoProfile,
    updateContactUnreadChat,
    setUserChatsRead,
    updateContactStatus,
    updateGroupUnreadChat,
    setConnectionVerified,
    incAuthErrCounter,
    setAuthErrCounter,
    incQuotaErrCounter,
    setQuotaErrCounter,
    getUserContacts,
    createOrUpdateContactRequest,
    getUserContactLinkIdByCReq,
    getContactRequest',
    getContactRequest,
    getContactRequestIdByName,
    deleteContactRequest,
    createAcceptedContact,
    deleteContactRequestRec,
    updateContactAccepted,
    getUserByContactRequestId,
    getPendingContactConnections,
    updatePCCUser,
    getContactConnections,
    getConnectionById,
    getConnectionsContacts,
    updateConnectionStatus,
    updateConnectionStatusFromTo,
    updateContactSettings,
    setConnConnReqInv,
    resetContactConnInitiated,
    setContactCustomData,
    setContactUIThemes,
    setContactChatDeleted,
    getDirectChatTags,
    updateDirectChatTags,
    setDirectChatTTL,
    getDirectChatTTL,
    getUserContactsToExpire
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Either (rights)
import Data.Functor (($>))
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Simplex.Chat.Messages
import Simplex.Chat.Store.Shared
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences
import Simplex.Chat.Types.UITheme
import Simplex.Messaging.Agent.Protocol (ConnId, CreatedConnLink (..), InvitationId, UserId)
import Simplex.Messaging.Agent.Store.AgentStore (firstRow, maybeFirstRow)
import Simplex.Messaging.Agent.Store.DB (Binary (..), BoolInt (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Crypto.Ratchet (PQSupport)
import Simplex.Messaging.Protocol (SubscriptionMode (..))
import Simplex.Messaging.Util ((<$$>))
import Simplex.Messaging.Version
#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..), (:.) (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..), (:.) (..))
import Database.SQLite.Simple.QQ (sql)
#endif

getPendingContactConnection :: DB.Connection -> UserId -> Int64 -> ExceptT StoreError IO PendingContactConnection
getPendingContactConnection db userId connId = do
  ExceptT . firstRow toPendingContactConnection (SEPendingConnectionNotFound connId) $
    DB.query
      db
      [sql|
        SELECT connection_id, agent_conn_id, conn_status, via_contact_uri_hash, via_user_contact_link, group_link_id, custom_user_profile_id, conn_req_inv, short_link_inv, local_alias, created_at, updated_at
        FROM connections
        WHERE user_id = ?
          AND connection_id = ?
          AND conn_type = ?
          AND contact_id IS NULL
          AND conn_level = 0
          AND via_contact IS NULL
      |]
      (userId, connId, ConnContact)

deletePendingContactConnection :: DB.Connection -> UserId -> Int64 -> IO ()
deletePendingContactConnection db userId connId =
  DB.execute
    db
    [sql|
      DELETE FROM connections
        WHERE user_id = ?
          AND connection_id = ?
          AND conn_type = ?
          AND contact_id IS NULL
          AND conn_level = 0
          AND via_contact IS NULL
    |]
    (userId, connId, ConnContact)

createAddressContactConnection :: DB.Connection -> VersionRangeChat -> User -> Contact -> ConnId -> ConnReqUriHash -> Maybe ShortLinkContact -> XContactId -> Maybe Profile -> SubscriptionMode -> VersionChat -> PQSupport -> ExceptT StoreError IO (Int64, Contact)
createAddressContactConnection db vr user@User {userId} Contact {contactId} acId cReqHash sLnk xContactId incognitoProfile subMode chatV pqSup = do
  PendingContactConnection {pccConnId} <- liftIO $ createConnReqConnection db userId acId cReqHash sLnk xContactId incognitoProfile Nothing subMode chatV pqSup
  liftIO $ DB.execute db "UPDATE connections SET contact_id = ? WHERE connection_id = ?" (contactId, pccConnId)
  (pccConnId,) <$> getContact db vr user contactId

createConnReqConnection :: DB.Connection -> UserId -> ConnId -> ConnReqUriHash -> Maybe ShortLinkContact -> XContactId -> Maybe Profile -> Maybe GroupLinkId -> SubscriptionMode -> VersionChat -> PQSupport -> IO PendingContactConnection
createConnReqConnection db userId acId cReqHash sLnk xContactId incognitoProfile groupLinkId subMode chatV pqSup = do
  createdAt <- getCurrentTime
  customUserProfileId <- mapM (createIncognitoProfile_ db userId createdAt) incognitoProfile
  let pccConnStatus = ConnJoined
  DB.execute
    db
    [sql|
      INSERT INTO connections (
        user_id, agent_conn_id, conn_status, conn_type, contact_conn_initiated,
        via_contact_uri_hash, via_short_link_contact, xcontact_id, custom_user_profile_id, via_group_link, group_link_id,
        created_at, updated_at, to_subscribe, conn_chat_version, pq_support, pq_encryption
      ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
    |]
    ( (userId, acId, pccConnStatus, ConnContact, BI True, cReqHash, sLnk, xContactId)
        :. (customUserProfileId, BI (isJust groupLinkId), groupLinkId)
        :. (createdAt, createdAt, BI (subMode == SMOnlyCreate), chatV, pqSup, pqSup)
    )
  pccConnId <- insertedRowId db
  pure PendingContactConnection {pccConnId, pccAgentConnId = AgentConnId acId, pccConnStatus, viaContactUri = True, viaUserContactLink = Nothing, groupLinkId, customUserProfileId, connLinkInv = Nothing, localAlias = "", createdAt, updatedAt = createdAt}

getConnReqContactXContactId :: DB.Connection -> VersionRangeChat -> User -> ConnReqUriHash -> IO (Maybe Contact, Maybe XContactId)
getConnReqContactXContactId db vr user@User {userId} cReqHash = do
  getContactByConnReqHash db vr user cReqHash >>= \case
    c@(Just _) -> pure (c, Nothing)
    Nothing -> (Nothing,) <$> getXContactId
  where
    getXContactId :: IO (Maybe XContactId)
    getXContactId =
      maybeFirstRow fromOnly $
        DB.query
          db
          "SELECT xcontact_id FROM connections WHERE user_id = ? AND via_contact_uri_hash = ? LIMIT 1"
          (userId, cReqHash)

getContactByConnReqHash :: DB.Connection -> VersionRangeChat -> User -> ConnReqUriHash -> IO (Maybe Contact)
getContactByConnReqHash db vr user@User {userId} cReqHash = do
  ct_ <-
    maybeFirstRow (toContact vr user []) $
      DB.query
        db
        [sql|
          SELECT
            -- Contact
            ct.contact_id, ct.contact_profile_id, ct.local_display_name, ct.via_group, cp.display_name, cp.full_name, cp.image, cp.contact_link, cp.local_alias, ct.contact_used, ct.contact_status, ct.enable_ntfs, ct.send_rcpts, ct.favorite,
            cp.preferences, ct.user_preferences, ct.created_at, ct.updated_at, ct.chat_ts, ct.contact_group_member_id, ct.contact_grp_inv_sent, ct.ui_themes, ct.chat_deleted, ct.custom_data, ct.chat_item_ttl,
            -- Connection
            c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id, c.conn_status, c.conn_type, c.contact_conn_initiated, c.local_alias,
            c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at, c.pq_support, c.pq_encryption, c.pq_snd_enabled, c.pq_rcv_enabled, c.auth_err_counter, c.quota_err_counter,
            c.conn_chat_version, c.peer_chat_min_version, c.peer_chat_max_version
          FROM contacts ct
          JOIN contact_profiles cp ON ct.contact_profile_id = cp.contact_profile_id
          JOIN connections c ON c.contact_id = ct.contact_id
          WHERE c.user_id = ? AND c.via_contact_uri_hash = ? AND ct.contact_status = ? AND ct.deleted = 0
          ORDER BY c.created_at DESC
          LIMIT 1
        |]
        (userId, cReqHash, CSActive)
  mapM (addDirectChatTags db) ct_

createDirectConnection :: DB.Connection -> User -> ConnId -> CreatedLinkInvitation -> ConnStatus -> Maybe Profile -> SubscriptionMode -> VersionChat -> PQSupport -> IO PendingContactConnection
createDirectConnection db User {userId} acId ccLink@(CCLink cReq shortLinkInv) pccConnStatus incognitoProfile subMode chatV pqSup = do
  createdAt <- getCurrentTime
  customUserProfileId <- mapM (createIncognitoProfile_ db userId createdAt) incognitoProfile
  let contactConnInitiated = pccConnStatus == ConnNew
  DB.execute
    db
    [sql|
      INSERT INTO connections
        (user_id, agent_conn_id, conn_req_inv, short_link_inv, conn_status, conn_type, contact_conn_initiated, custom_user_profile_id,
         created_at, updated_at, to_subscribe, conn_chat_version, pq_support, pq_encryption)
      VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?)
    |]
    ( (userId, acId, cReq, shortLinkInv, pccConnStatus, ConnContact, BI contactConnInitiated, customUserProfileId)
        :. (createdAt, createdAt, BI (subMode == SMOnlyCreate), chatV, pqSup, pqSup)
    )
  pccConnId <- insertedRowId db
  pure PendingContactConnection {pccConnId, pccAgentConnId = AgentConnId acId, pccConnStatus, viaContactUri = False, viaUserContactLink = Nothing, groupLinkId = Nothing, customUserProfileId, connLinkInv = Just ccLink, localAlias = "", createdAt, updatedAt = createdAt}

createIncognitoProfile :: DB.Connection -> User -> Profile -> IO Int64
createIncognitoProfile db User {userId} p = do
  createdAt <- getCurrentTime
  createIncognitoProfile_ db userId createdAt p

createDirectContact :: DB.Connection -> User -> Connection -> Profile -> ExceptT StoreError IO Contact
createDirectContact db user@User {userId} conn@Connection {connId, localAlias} p@Profile {preferences} = do
  currentTs <- liftIO getCurrentTime
  (localDisplayName, contactId, profileId) <- createContact_ db userId p localAlias Nothing currentTs
  liftIO $ DB.execute db "UPDATE connections SET contact_id = ?, updated_at = ? WHERE connection_id = ?" (contactId, currentTs, connId)
  let profile = toLocalProfile profileId p localAlias
      userPreferences = emptyChatPrefs
      mergedPreferences = contactUserPreferences user userPreferences preferences $ connIncognito conn
  pure $
    Contact
      { contactId,
        localDisplayName,
        profile,
        activeConn = Just conn,
        viaGroup = Nothing,
        contactUsed = True,
        contactStatus = CSActive,
        chatSettings = defaultChatSettings,
        userPreferences,
        mergedPreferences,
        createdAt = currentTs,
        updatedAt = currentTs,
        chatTs = Just currentTs,
        contactGroupMemberId = Nothing,
        contactGrpInvSent = False,
        chatTags = [],
        chatItemTTL = Nothing,
        uiThemes = Nothing,
        chatDeleted = False,
        customData = Nothing
      }

deleteContactConnections :: DB.Connection -> User -> Contact -> IO ()
deleteContactConnections db User {userId} Contact {contactId} = do
  DB.execute
    db
    [sql|
      DELETE FROM connections WHERE connection_id IN (
        SELECT connection_id
        FROM connections c
        JOIN contacts ct ON ct.contact_id = c.contact_id
        WHERE ct.user_id = ? AND ct.contact_id = ?
      )
    |]
    (userId, contactId)

deleteContactFiles :: DB.Connection -> User -> Contact -> IO ()
deleteContactFiles db User {userId} Contact {contactId} = do
  DB.execute db "DELETE FROM files WHERE user_id = ? AND contact_id = ?" (userId, contactId)

deleteContact :: DB.Connection -> User -> Contact -> ExceptT StoreError IO ()
deleteContact db user@User {userId} ct@Contact {contactId, localDisplayName, activeConn} = do
  assertNotUser db user ct
  liftIO $ do
    DB.execute db "DELETE FROM chat_items WHERE user_id = ? AND contact_id = ?" (userId, contactId)
    ctMember :: (Maybe ContactId) <- maybeFirstRow fromOnly $ DB.query db "SELECT contact_id FROM group_members WHERE contact_id = ? LIMIT 1" (Only contactId)
    if isNothing ctMember
      then do
        deleteContactProfile_ db userId contactId
        -- user's local display name already checked in assertNotUser
        DB.execute db "DELETE FROM display_names WHERE user_id = ? AND local_display_name = ?" (userId, localDisplayName)
      else do
        currentTs <- getCurrentTime
        DB.execute db "UPDATE group_members SET contact_id = NULL, updated_at = ? WHERE user_id = ? AND contact_id = ?" (currentTs, userId, contactId)
    DB.execute db "DELETE FROM contacts WHERE user_id = ? AND contact_id = ?" (userId, contactId)
    forM_ activeConn $ \Connection {customUserProfileId} ->
      forM_ customUserProfileId $ \profileId ->
        deleteUnusedIncognitoProfileById_ db user profileId

-- should only be used if contact is not member of any groups
deleteContactWithoutGroups :: DB.Connection -> User -> Contact -> ExceptT StoreError IO ()
deleteContactWithoutGroups db user@User {userId} ct@Contact {contactId, localDisplayName, activeConn} = do
  assertNotUser db user ct
  liftIO $ do
    DB.execute db "DELETE FROM chat_items WHERE user_id = ? AND contact_id = ?" (userId, contactId)
    deleteContactProfile_ db userId contactId
    -- user's local display name already checked in assertNotUser
    DB.execute db "DELETE FROM display_names WHERE user_id = ? AND local_display_name = ?" (userId, localDisplayName)
    DB.execute db "DELETE FROM contacts WHERE user_id = ? AND contact_id = ?" (userId, contactId)
    forM_ activeConn $ \Connection {customUserProfileId} ->
      forM_ customUserProfileId $ \profileId ->
        deleteUnusedIncognitoProfileById_ db user profileId

-- TODO remove in future versions: only used for legacy contact cleanup
getDeletedContacts :: DB.Connection -> VersionRangeChat -> User -> IO [Contact]
getDeletedContacts db vr user@User {userId} = do
  contactIds <- map fromOnly <$> DB.query db "SELECT contact_id FROM contacts WHERE user_id = ? AND deleted = 1" (Only userId)
  rights <$> mapM (runExceptT . getDeletedContact db vr user) contactIds

getDeletedContact :: DB.Connection -> VersionRangeChat -> User -> Int64 -> ExceptT StoreError IO Contact
getDeletedContact db vr user contactId = getContact_ db vr user contactId True

deleteContactProfile_ :: DB.Connection -> UserId -> ContactId -> IO ()
deleteContactProfile_ db userId contactId =
  DB.execute
    db
    [sql|
      DELETE FROM contact_profiles
      WHERE contact_profile_id in (
        SELECT contact_profile_id
        FROM contacts
        WHERE user_id = ? AND contact_id = ?
      )
    |]
    (userId, contactId)

deleteUnusedProfile_ :: DB.Connection -> UserId -> ProfileId -> IO ()
deleteUnusedProfile_ db userId profileId =
  DB.execute
    db
    [sql|
      DELETE FROM contact_profiles
      WHERE user_id = ? AND contact_profile_id = ?
        AND 1 NOT IN (
          SELECT 1 FROM connections
          WHERE user_id = ? AND custom_user_profile_id = ? LIMIT 1
        )
        AND 1 NOT IN (
          SELECT 1 FROM contacts
          WHERE user_id = ? AND contact_profile_id = ? LIMIT 1
        )
        AND 1 NOT IN (
          SELECT 1 FROM contact_requests
          WHERE user_id = ? AND contact_profile_id = ? LIMIT 1
        )
        AND 1 NOT IN (
          SELECT 1 FROM group_members
          WHERE user_id = ?
            AND (member_profile_id = ? OR contact_profile_id = ?)
          LIMIT 1
        )
    |]
    ( (userId, profileId, userId, profileId, userId, profileId)
        :. (userId, profileId, userId, profileId, profileId)
    )

updateContactProfile :: DB.Connection -> User -> Contact -> Profile -> ExceptT StoreError IO Contact
updateContactProfile db user@User {userId} c p'
  | displayName == newName = do
      liftIO $ updateContactProfile_ db userId profileId p'
      pure c {profile, mergedPreferences}
  | otherwise =
      ExceptT . withLocalDisplayName db userId newName $ \ldn -> do
        currentTs <- getCurrentTime
        updateContactProfile_' db userId profileId p' currentTs
        updateContactLDN_ db user contactId localDisplayName ldn currentTs
        pure $ Right c {localDisplayName = ldn, profile, mergedPreferences}
  where
    Contact {contactId, localDisplayName, profile = LocalProfile {profileId, displayName, localAlias}, userPreferences} = c
    Profile {displayName = newName, preferences} = p'
    profile = toLocalProfile profileId p' localAlias
    mergedPreferences = contactUserPreferences user userPreferences preferences $ contactConnIncognito c

updateContactUserPreferences :: DB.Connection -> User -> Contact -> Preferences -> IO Contact
updateContactUserPreferences db user@User {userId} c@Contact {contactId} userPreferences = do
  updatedAt <- getCurrentTime
  DB.execute
    db
    "UPDATE contacts SET user_preferences = ?, updated_at = ? WHERE user_id = ? AND contact_id = ?"
    (userPreferences, updatedAt, userId, contactId)
  let mergedPreferences = contactUserPreferences user userPreferences (preferences' c) $ contactConnIncognito c
  pure $ c {mergedPreferences, userPreferences}

updateContactAlias :: DB.Connection -> UserId -> Contact -> LocalAlias -> IO Contact
updateContactAlias db userId c@Contact {profile = lp@LocalProfile {profileId}} localAlias = do
  updatedAt <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE contact_profiles
      SET local_alias = ?, updated_at = ?
      WHERE user_id = ? AND contact_profile_id = ?
    |]
    (localAlias, updatedAt, userId, profileId)
  pure $ (c :: Contact) {profile = lp {localAlias}}

updateContactConnectionAlias :: DB.Connection -> UserId -> PendingContactConnection -> LocalAlias -> IO PendingContactConnection
updateContactConnectionAlias db userId conn localAlias = do
  updatedAt <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE connections
      SET local_alias = ?, updated_at = ?
      WHERE user_id = ? AND connection_id = ?
    |]
    (localAlias, updatedAt, userId, pccConnId conn)
  pure (conn :: PendingContactConnection) {localAlias, updatedAt}

updatePCCIncognito :: DB.Connection -> User -> PendingContactConnection -> Maybe ProfileId -> IO PendingContactConnection
updatePCCIncognito db User {userId} conn customUserProfileId = do
  updatedAt <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE connections
      SET custom_user_profile_id = ?, updated_at = ?
      WHERE user_id = ? AND connection_id = ?
    |]
    (customUserProfileId, updatedAt, userId, pccConnId conn)
  pure (conn :: PendingContactConnection) {customUserProfileId, updatedAt}

updatePCCUser :: DB.Connection -> UserId -> PendingContactConnection -> UserId -> IO PendingContactConnection
updatePCCUser db userId conn newUserId = do
  updatedAt <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE connections
      SET user_id = ?, custom_user_profile_id = NULL, updated_at = ?
      WHERE user_id = ? AND connection_id = ?
    |]
    (newUserId, updatedAt, userId, pccConnId conn)
  pure (conn :: PendingContactConnection) {customUserProfileId = Nothing, updatedAt}

deletePCCIncognitoProfile :: DB.Connection -> User -> ProfileId -> IO ()
deletePCCIncognitoProfile db User {userId} profileId =
  DB.execute
    db
    [sql|
      DELETE FROM contact_profiles
      WHERE user_id = ? AND contact_profile_id = ? AND incognito = 1
    |]
    (userId, profileId)

updateContactUnreadChat :: DB.Connection -> User -> Contact -> Bool -> IO ()
updateContactUnreadChat db User {userId} Contact {contactId} unreadChat = do
  updatedAt <- getCurrentTime
  DB.execute db "UPDATE contacts SET unread_chat = ?, updated_at = ? WHERE user_id = ? AND contact_id = ?" (BI unreadChat, updatedAt, userId, contactId)

setUserChatsRead :: DB.Connection -> User -> IO ()
setUserChatsRead db User {userId} = do
  updatedAt <- getCurrentTime
  DB.execute db "UPDATE contacts SET unread_chat = ?, updated_at = ? WHERE user_id = ? AND unread_chat = ?" (BI False, updatedAt, userId, BI True)
  DB.execute db "UPDATE groups SET unread_chat = ?, updated_at = ? WHERE user_id = ? AND unread_chat = ?" (BI False, updatedAt, userId, BI True)
  DB.execute db "UPDATE note_folders SET unread_chat = ?, updated_at = ? WHERE user_id = ? AND unread_chat = ?" (BI False, updatedAt, userId, BI True)
  DB.execute db "UPDATE chat_items SET item_status = ?, updated_at = ? WHERE user_id = ? AND item_status = ?" (CISRcvRead, updatedAt, userId, CISRcvNew)

updateContactStatus :: DB.Connection -> User -> Contact -> ContactStatus -> IO Contact
updateContactStatus db User {userId} ct@Contact {contactId} contactStatus = do
  currentTs <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE contacts
      SET contact_status = ?, updated_at = ?
      WHERE user_id = ? AND contact_id = ?
    |]
    (contactStatus, currentTs, userId, contactId)
  pure ct {contactStatus}

updateGroupUnreadChat :: DB.Connection -> User -> GroupInfo -> Bool -> IO ()
updateGroupUnreadChat db User {userId} GroupInfo {groupId} unreadChat = do
  updatedAt <- getCurrentTime
  DB.execute db "UPDATE groups SET unread_chat = ?, updated_at = ? WHERE user_id = ? AND group_id = ?" (BI unreadChat, updatedAt, userId, groupId)

setConnectionVerified :: DB.Connection -> User -> Int64 -> Maybe Text -> IO ()
setConnectionVerified db User {userId} connId code = do
  updatedAt <- getCurrentTime
  DB.execute db "UPDATE connections SET security_code = ?, security_code_verified_at = ?, updated_at = ? WHERE user_id = ? AND connection_id = ?" (code, code $> updatedAt, updatedAt, userId, connId)

incAuthErrCounter :: DB.Connection -> User -> Connection -> IO Int
incAuthErrCounter db User {userId} Connection {connId, authErrCounter} = do
  updatedAt <- getCurrentTime
  (counter_ :: Maybe Int) <- maybeFirstRow fromOnly $ DB.query db "SELECT auth_err_counter FROM connections WHERE user_id = ? AND connection_id = ?" (userId, connId)
  let counter' = fromMaybe authErrCounter counter_ + 1
  DB.execute db "UPDATE connections SET auth_err_counter = ?, updated_at = ? WHERE user_id = ? AND connection_id = ?" (counter', updatedAt, userId, connId)
  pure counter'

setAuthErrCounter :: DB.Connection -> User -> Connection -> Int -> IO ()
setAuthErrCounter db User {userId} Connection {connId} counter = do
  updatedAt <- getCurrentTime
  DB.execute db "UPDATE connections SET auth_err_counter = ?, updated_at = ? WHERE user_id = ? AND connection_id = ?" (counter, updatedAt, userId, connId)

incQuotaErrCounter :: DB.Connection -> User -> Connection -> IO Int
incQuotaErrCounter db User {userId} Connection {connId, quotaErrCounter} = do
  updatedAt <- getCurrentTime
  (counter_ :: Maybe Int) <- maybeFirstRow fromOnly $ DB.query db "SELECT quota_err_counter FROM connections WHERE user_id = ? AND connection_id = ?" (userId, connId)
  let counter' = fromMaybe quotaErrCounter counter_ + 1
  DB.execute db "UPDATE connections SET quota_err_counter = ?, updated_at = ? WHERE user_id = ? AND connection_id = ?" (counter', updatedAt, userId, connId)
  pure counter'

setQuotaErrCounter :: DB.Connection -> User -> Connection -> Int -> IO ()
setQuotaErrCounter db User {userId} Connection {connId} counter = do
  updatedAt <- getCurrentTime
  DB.execute db "UPDATE connections SET quota_err_counter = ?, updated_at = ? WHERE user_id = ? AND connection_id = ?" (counter, updatedAt, userId, connId)

updateContactProfile_ :: DB.Connection -> UserId -> ProfileId -> Profile -> IO ()
updateContactProfile_ db userId profileId profile = do
  currentTs <- getCurrentTime
  updateContactProfile_' db userId profileId profile currentTs

updateContactProfile_' :: DB.Connection -> UserId -> ProfileId -> Profile -> UTCTime -> IO ()
updateContactProfile_' db userId profileId Profile {displayName, fullName, image, contactLink, preferences} updatedAt = do
  DB.execute
    db
    [sql|
      UPDATE contact_profiles
      SET display_name = ?, full_name = ?, image = ?, contact_link = ?, preferences = ?, updated_at = ?
      WHERE user_id = ? AND contact_profile_id = ?
    |]
    (displayName, fullName, image, contactLink, preferences, updatedAt, userId, profileId)

-- update only member profile fields (when member doesn't have associated contact - we can reset contactLink and prefs)
updateMemberContactProfileReset_ :: DB.Connection -> UserId -> ProfileId -> Profile -> IO ()
updateMemberContactProfileReset_ db userId profileId profile = do
  currentTs <- getCurrentTime
  updateMemberContactProfileReset_' db userId profileId profile currentTs

updateMemberContactProfileReset_' :: DB.Connection -> UserId -> ProfileId -> Profile -> UTCTime -> IO ()
updateMemberContactProfileReset_' db userId profileId Profile {displayName, fullName, image} updatedAt = do
  DB.execute
    db
    [sql|
      UPDATE contact_profiles
      SET display_name = ?, full_name = ?, image = ?, contact_link = NULL, preferences = NULL, updated_at = ?
      WHERE user_id = ? AND contact_profile_id = ?
    |]
    (displayName, fullName, image, updatedAt, userId, profileId)

-- update only member profile fields (when member has associated contact - we keep contactLink and prefs)
updateMemberContactProfile_ :: DB.Connection -> UserId -> ProfileId -> Profile -> IO ()
updateMemberContactProfile_ db userId profileId profile = do
  currentTs <- getCurrentTime
  updateMemberContactProfile_' db userId profileId profile currentTs

updateMemberContactProfile_' :: DB.Connection -> UserId -> ProfileId -> Profile -> UTCTime -> IO ()
updateMemberContactProfile_' db userId profileId Profile {displayName, fullName, image} updatedAt = do
  DB.execute
    db
    [sql|
      UPDATE contact_profiles
      SET display_name = ?, full_name = ?, image = ?, updated_at = ?
      WHERE user_id = ? AND contact_profile_id = ?
    |]
    (displayName, fullName, image, updatedAt, userId, profileId)

updateContactLDN_ :: DB.Connection -> User -> Int64 -> ContactName -> ContactName -> UTCTime -> IO ()
updateContactLDN_ db user@User {userId} contactId displayName newName updatedAt = do
  DB.execute
    db
    "UPDATE contacts SET local_display_name = ?, updated_at = ? WHERE user_id = ? AND contact_id = ?"
    (newName, updatedAt, userId, contactId)
  DB.execute
    db
    "UPDATE group_members SET local_display_name = ?, updated_at = ? WHERE user_id = ? AND contact_id = ?"
    (newName, updatedAt, userId, contactId)
  safeDeleteLDN db user displayName

getContactByName :: DB.Connection -> VersionRangeChat -> User -> ContactName -> ExceptT StoreError IO Contact
getContactByName db vr user localDisplayName = do
  cId <- getContactIdByName db user localDisplayName
  getContact db vr user cId

getUserContacts :: DB.Connection -> VersionRangeChat -> User -> IO [Contact]
getUserContacts db vr user@User {userId} = do
  contactIds <- map fromOnly <$> DB.query db "SELECT contact_id FROM contacts WHERE user_id = ? AND deleted = 0" (Only userId)
  contacts <- rights <$> mapM (runExceptT . getContact db vr user) contactIds
  pure $ filter (\Contact {activeConn} -> isJust activeConn) contacts

createOrUpdateContactRequest :: DB.Connection -> VersionRangeChat -> User -> Int64 -> InvitationId -> VersionRangeChat -> Profile -> Maybe XContactId -> PQSupport -> ExceptT StoreError IO ChatOrRequest
createOrUpdateContactRequest db vr user@User {userId, userContactId} userContactLinkId invId (VersionRange minV maxV) Profile {displayName, fullName, image, contactLink, preferences} xContactId_ pqSup =
  liftIO (maybeM getContactOrGroup xContactId_) >>= \case
    Just cr -> pure cr
    Nothing -> CORRequest <$> createOrUpdate_
  where
    maybeM = maybe (pure Nothing)
    getContactOrGroup xContactId =
      getContact' xContactId >>= \case
        Just ct -> pure $ Just $ CORContact ct
        Nothing -> CORGroup <$$> getGroupInfo' xContactId
    createOrUpdate_ :: ExceptT StoreError IO UserContactRequest
    createOrUpdate_ = do
      cReqId <-
        ExceptT $
          maybeM getContactRequestByXContactId xContactId_ >>= \case
            Nothing -> createContactRequest
            Just cr@UserContactRequest {contactRequestId} -> updateContactRequest cr $> Right contactRequestId
      getContactRequest db user cReqId
    createContactRequest :: IO (Either StoreError Int64)
    createContactRequest = do
      currentTs <- getCurrentTime
      withLocalDisplayName db userId displayName (fmap Right . createContactRequest_ currentTs)
      where
        createContactRequest_ currentTs ldn = do
          DB.execute
            db
            "INSERT INTO contact_profiles (display_name, full_name, image, contact_link, user_id, preferences, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?)"
            (displayName, fullName, image, contactLink, userId, preferences, currentTs, currentTs)
          profileId <- insertedRowId db
          DB.execute
            db
            [sql|
              INSERT INTO contact_requests
                (user_contact_link_id, agent_invitation_id, peer_chat_min_version, peer_chat_max_version, contact_profile_id, local_display_name, user_id,
                 created_at, updated_at, xcontact_id, pq_support)
              VALUES (?,?,?,?,?,?,?,?,?,?,?)
            |]
            ( (userContactLinkId, Binary invId, minV, maxV, profileId, ldn, userId)
                :. (currentTs, currentTs, xContactId_, pqSup)
            )
          insertedRowId db
    getContact' :: XContactId -> IO (Maybe Contact)
    getContact' xContactId = do
      ct_ <-
        maybeFirstRow (toContact vr user []) $
          DB.query
            db
            [sql|
              SELECT
                -- Contact
                ct.contact_id, ct.contact_profile_id, ct.local_display_name, ct.via_group, cp.display_name, cp.full_name, cp.image, cp.contact_link, cp.local_alias, ct.contact_used, ct.contact_status, ct.enable_ntfs, ct.send_rcpts, ct.favorite,
                cp.preferences, ct.user_preferences, ct.created_at, ct.updated_at, ct.chat_ts, ct.contact_group_member_id, ct.contact_grp_inv_sent, ct.ui_themes, ct.chat_deleted, ct.custom_data, ct.chat_item_ttl,
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
    getGroupInfo' :: XContactId -> IO (Maybe GroupInfo)
    getGroupInfo' xContactId = do
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
              cr.contact_request_id, cr.local_display_name, cr.agent_invitation_id, cr.contact_id, cr.user_contact_link_id,
              c.agent_conn_id, cr.contact_profile_id, p.display_name, p.full_name, p.image, p.contact_link, cr.xcontact_id, cr.pq_support, p.preferences, cr.created_at, cr.updated_at,
              cr.peer_chat_min_version, cr.peer_chat_max_version
            FROM contact_requests cr
            JOIN connections c USING (user_contact_link_id)
            JOIN contact_profiles p USING (contact_profile_id)
            WHERE cr.user_id = ?
              AND cr.xcontact_id = ?
            LIMIT 1
          |]
          (userId, xContactId)
    updateContactRequest :: UserContactRequest -> IO (Either StoreError ())
    updateContactRequest UserContactRequest {contactRequestId = cReqId, localDisplayName = oldLdn, profile = Profile {displayName = oldDisplayName}} = do
      currentTs <- liftIO getCurrentTime
      updateProfile currentTs
      if displayName == oldDisplayName
        then
          Right
            <$> DB.execute
              db
              [sql|
                UPDATE contact_requests
                SET agent_invitation_id = ?, pq_support = ?, peer_chat_min_version = ?, peer_chat_max_version = ?, updated_at = ?
                WHERE user_id = ? AND contact_request_id = ?
              |]
              (Binary invId, pqSup, minV, maxV, currentTs, userId, cReqId)
        else withLocalDisplayName db userId displayName $ \ldn ->
          Right <$> do
            DB.execute
              db
              [sql|
                UPDATE contact_requests
                SET agent_invitation_id = ?, pq_support = ?, peer_chat_min_version = ?, peer_chat_max_version = ?, local_display_name = ?, updated_at = ?
                WHERE user_id = ? AND contact_request_id = ?
              |]
              (Binary invId, pqSup, minV, maxV, ldn, currentTs, userId, cReqId)
            safeDeleteLDN db user oldLdn
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

getUserContactLinkIdByCReq :: DB.Connection -> Int64 -> ExceptT StoreError IO Int64
getUserContactLinkIdByCReq db contactRequestId =
  ExceptT . firstRow fromOnly (SEContactRequestNotFound contactRequestId) $
    DB.query db "SELECT user_contact_link_id FROM contact_requests WHERE contact_request_id = ?" (Only contactRequestId)

getContactRequest' :: DB.Connection -> Int64 -> ExceptT StoreError IO (User, UserContactRequest)
getContactRequest' db contactRequestId = do
  user <- getUserByContactRequestId db contactRequestId
  (user,) <$> getContactRequest db user contactRequestId

getContactRequest :: DB.Connection -> User -> Int64 -> ExceptT StoreError IO UserContactRequest
getContactRequest db User {userId} contactRequestId =
  ExceptT . firstRow toContactRequest (SEContactRequestNotFound contactRequestId) $
    DB.query
      db
      [sql|
        SELECT
          cr.contact_request_id, cr.local_display_name, cr.agent_invitation_id, cr.contact_id, cr.user_contact_link_id,
          c.agent_conn_id, cr.contact_profile_id, p.display_name, p.full_name, p.image, p.contact_link, cr.xcontact_id, cr.pq_support, p.preferences, cr.created_at, cr.updated_at,
          cr.peer_chat_min_version, cr.peer_chat_max_version
        FROM contact_requests cr
        JOIN connections c USING (user_contact_link_id)
        JOIN contact_profiles p USING (contact_profile_id)
        WHERE cr.user_id = ?
          AND cr.contact_request_id = ?
      |]
      (userId, contactRequestId)

getContactRequestIdByName :: DB.Connection -> UserId -> ContactName -> ExceptT StoreError IO Int64
getContactRequestIdByName db userId cName =
  ExceptT . firstRow fromOnly (SEContactRequestNotFoundByName cName) $
    DB.query db "SELECT contact_request_id FROM contact_requests WHERE user_id = ? AND local_display_name = ?" (userId, cName)

deleteContactRequest :: DB.Connection -> User -> Int64 -> IO ()
deleteContactRequest db User {userId} contactRequestId = do
  DB.execute
    db
    [sql|
      DELETE FROM contact_profiles
      WHERE contact_profile_id in (
        SELECT contact_profile_id
        FROM contact_requests
        WHERE user_id = ? AND contact_request_id = ?
      )
    |]
    (userId, contactRequestId)
  DB.execute
    db
    [sql|
      DELETE FROM display_names
      WHERE user_id = ? AND local_display_name = (
        SELECT local_display_name FROM contact_requests
        WHERE user_id = ? AND contact_request_id = ?
      )
      AND local_display_name NOT IN (SELECT local_display_name FROM users WHERE user_id = ?)
    |]
    (userId, userId, contactRequestId, userId)
  DB.execute db "DELETE FROM contact_requests WHERE user_id = ? AND contact_request_id = ?" (userId, contactRequestId)

createAcceptedContact :: DB.Connection -> User -> ConnId -> VersionChat -> VersionRangeChat -> ContactName -> ProfileId -> Profile -> Int64 -> Maybe XContactId -> Maybe IncognitoProfile -> SubscriptionMode -> PQSupport -> Bool -> IO (Contact, Connection)
createAcceptedContact db user@User {userId, profile = LocalProfile {preferences}} agentConnId connChatVersion cReqChatVRange localDisplayName profileId profile userContactLinkId xContactId incognitoProfile subMode pqSup contactUsed = do
  createdAt <- getCurrentTime
  customUserProfileId <- forM incognitoProfile $ \case
    NewIncognito p -> createIncognitoProfile_ db userId createdAt p
    ExistingIncognito LocalProfile {profileId = pId} -> pure pId
  let userPreferences = fromMaybe emptyChatPrefs $ incognitoProfile >> preferences
  DB.execute
    db
    "INSERT INTO contacts (user_id, local_display_name, contact_profile_id, enable_ntfs, user_preferences, created_at, updated_at, chat_ts, xcontact_id, contact_used) VALUES (?,?,?,?,?,?,?,?,?,?)"
    (userId, localDisplayName, profileId, BI True, userPreferences, createdAt, createdAt, createdAt, xContactId, BI contactUsed)
  contactId <- insertedRowId db
  DB.execute db "UPDATE contact_requests SET contact_id = ? WHERE user_id = ? AND local_display_name = ?" (contactId, userId, localDisplayName)
  conn <- createConnection_ db userId ConnContact (Just contactId) agentConnId ConnNew connChatVersion cReqChatVRange Nothing (Just userContactLinkId) customUserProfileId 0 createdAt subMode pqSup
  let mergedPreferences = contactUserPreferences user userPreferences preferences $ connIncognito conn
      ct =
        Contact
          { contactId,
            localDisplayName,
            profile = toLocalProfile profileId profile "",
            activeConn = Just conn,
            viaGroup = Nothing,
            contactUsed,
            contactStatus = CSActive,
            chatSettings = defaultChatSettings,
            userPreferences,
            mergedPreferences,
            createdAt,
            updatedAt = createdAt,
            chatTs = Just createdAt,
            contactGroupMemberId = Nothing,
            contactGrpInvSent = False,
            chatTags = [],
            chatItemTTL = Nothing,
            uiThemes = Nothing,
            chatDeleted = False,
            customData = Nothing
          }
  pure (ct, conn)

deleteContactRequestRec :: DB.Connection -> User -> UserContactRequest -> IO ()
deleteContactRequestRec db User {userId} UserContactRequest {contactRequestId} =
  DB.execute db "DELETE FROM contact_requests WHERE user_id = ? AND contact_request_id = ?" (userId, contactRequestId)

updateContactAccepted :: DB.Connection -> User -> Contact -> Bool -> IO ()
updateContactAccepted db User {userId} Contact {contactId} contactUsed =
  DB.execute
    db
    "UPDATE contacts SET contact_used = ? WHERE user_id = ? AND contact_id = ?"
    (BI contactUsed, userId, contactId)

getContactIdByName :: DB.Connection -> User -> ContactName -> ExceptT StoreError IO Int64
getContactIdByName db User {userId} cName =
  ExceptT . firstRow fromOnly (SEContactNotFoundByName cName) $
    DB.query db "SELECT contact_id FROM contacts WHERE user_id = ? AND local_display_name = ? AND deleted = 0" (userId, cName)

getContact :: DB.Connection -> VersionRangeChat -> User -> Int64 -> ExceptT StoreError IO Contact
getContact db vr user contactId = getContact_ db vr user contactId False

getContact_ :: DB.Connection -> VersionRangeChat -> User -> Int64 -> Bool -> ExceptT StoreError IO Contact
getContact_ db vr user@User {userId} contactId deleted = do
  chatTags <- liftIO $ getDirectChatTags db contactId
  ExceptT . firstRow (toContact vr user chatTags) (SEContactNotFound contactId) $
    DB.query
      db
      [sql|
        SELECT
          -- Contact
          ct.contact_id, ct.contact_profile_id, ct.local_display_name, ct.via_group, cp.display_name, cp.full_name, cp.image, cp.contact_link, cp.local_alias, ct.contact_used, ct.contact_status, ct.enable_ntfs, ct.send_rcpts, ct.favorite,
          cp.preferences, ct.user_preferences, ct.created_at, ct.updated_at, ct.chat_ts, ct.contact_group_member_id, ct.contact_grp_inv_sent, ct.ui_themes, ct.chat_deleted, ct.custom_data, ct.chat_item_ttl,
          -- Connection
          c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id, c.conn_status, c.conn_type, c.contact_conn_initiated, c.local_alias,
          c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at, c.pq_support, c.pq_encryption, c.pq_snd_enabled, c.pq_rcv_enabled, c.auth_err_counter, c.quota_err_counter,
          c.conn_chat_version, c.peer_chat_min_version, c.peer_chat_max_version
        FROM contacts ct
        JOIN contact_profiles cp ON ct.contact_profile_id = cp.contact_profile_id
        LEFT JOIN connections c ON c.contact_id = ct.contact_id
        WHERE ct.user_id = ? AND ct.contact_id = ?
          AND ct.deleted = ?
          AND (
            c.connection_id = (
              SELECT cc_connection_id FROM (
                SELECT
                  cc.connection_id AS cc_connection_id,
                  cc.created_at AS cc_created_at,
                  (CASE WHEN cc.conn_status = ? OR cc.conn_status = ? THEN 1 ELSE 0 END) AS cc_conn_status_ord
                FROM connections cc
                WHERE cc.user_id = ct.user_id AND cc.contact_id = ct.contact_id
                ORDER BY cc_conn_status_ord DESC, cc_created_at DESC
                LIMIT 1
              ) cc
            )
            OR c.connection_id IS NULL
          )
      |]
      (userId, contactId, BI deleted, ConnReady, ConnSndReady)

getUserByContactRequestId :: DB.Connection -> Int64 -> ExceptT StoreError IO User
getUserByContactRequestId db contactRequestId =
  ExceptT . firstRow toUser (SEUserNotFoundByContactRequestId contactRequestId) $
    DB.query db (userQuery <> " JOIN contact_requests cr ON cr.user_id = u.user_id WHERE cr.contact_request_id = ?") (Only contactRequestId)

getPendingContactConnections :: DB.Connection -> User -> IO [PendingContactConnection]
getPendingContactConnections db User {userId} = do
  map toPendingContactConnection
    <$> DB.query
      db
      [sql|
        SELECT connection_id, agent_conn_id, conn_status, via_contact_uri_hash, via_user_contact_link, group_link_id, custom_user_profile_id, conn_req_inv, short_link_inv, local_alias, created_at, updated_at
        FROM connections
        WHERE user_id = ?
          AND conn_type = ?
          AND contact_id IS NULL
      |]
      (userId, ConnContact)

getContactConnections :: DB.Connection -> VersionRangeChat -> UserId -> Contact -> IO [Connection]
getContactConnections db vr userId Contact {contactId} =
  connections =<< liftIO getConnections_
  where
    getConnections_ =
      DB.query
        db
        [sql|
          SELECT c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id,
            c.conn_status, c.conn_type, c.contact_conn_initiated, c.local_alias, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id,
            c.created_at, c.security_code, c.security_code_verified_at, c.pq_support, c.pq_encryption, c.pq_snd_enabled, c.pq_rcv_enabled, c.auth_err_counter, c.quota_err_counter,
            c.conn_chat_version, c.peer_chat_min_version, c.peer_chat_max_version
          FROM connections c
          JOIN contacts ct ON ct.contact_id = c.contact_id
          WHERE c.user_id = ? AND ct.user_id = ? AND ct.contact_id = ?
        |]
        (userId, userId, contactId)
    connections [] = pure []
    connections rows = pure $ map (toConnection vr) rows

getConnectionById :: DB.Connection -> VersionRangeChat -> User -> Int64 -> ExceptT StoreError IO Connection
getConnectionById db vr User {userId} connId = ExceptT $ do
  firstRow (toConnection vr) (SEConnectionNotFoundById connId) $
    DB.query
      db
      [sql|
        SELECT connection_id, agent_conn_id, conn_level, via_contact, via_user_contact_link, via_group_link, group_link_id, custom_user_profile_id,
          conn_status, conn_type, contact_conn_initiated, local_alias, contact_id, group_member_id, snd_file_id, rcv_file_id, user_contact_link_id,
          created_at, security_code, security_code_verified_at, pq_support, pq_encryption, pq_snd_enabled, pq_rcv_enabled, auth_err_counter, quota_err_counter,
          conn_chat_version, peer_chat_min_version, peer_chat_max_version
        FROM connections
        WHERE user_id = ? AND connection_id = ?
      |]
      (userId, connId)

getConnectionsContacts :: DB.Connection -> [ConnId] -> IO [ContactRef]
getConnectionsContacts db agentConnIds = do
  DB.execute_ db "DROP TABLE IF EXISTS temp_conn_ids"
#if defined(dbPostgres)
  DB.execute_ db "CREATE TABLE temp_conn_ids (conn_id BYTEA)"
#else
  DB.execute_ db "CREATE TABLE temp_conn_ids (conn_id BLOB)"
#endif
  DB.executeMany db "INSERT INTO temp_conn_ids (conn_id) VALUES (?)" $ map Only agentConnIds
  conns <-
    map toContactRef
      <$> DB.query
        db
        [sql|
          SELECT ct.contact_id, c.connection_id, c.agent_conn_id, ct.local_display_name
          FROM contacts ct
          JOIN connections c ON c.contact_id = ct.contact_id
          WHERE c.agent_conn_id IN (SELECT conn_id FROM temp_conn_ids)
            AND c.conn_type = ?
            AND ct.deleted = 0
        |]
        (Only ConnContact)
  DB.execute_ db "DROP TABLE temp_conn_ids"
  pure conns
  where
    toContactRef :: (ContactId, Int64, ConnId, ContactName) -> ContactRef
    toContactRef (contactId, connId, acId, localDisplayName) = ContactRef {contactId, connId, agentConnId = AgentConnId acId, localDisplayName}

updateConnectionStatus :: DB.Connection -> Connection -> ConnStatus -> IO ()
updateConnectionStatus db Connection {connId} = updateConnectionStatus_ db connId
{-# INLINE updateConnectionStatus #-}

updateConnectionStatusFromTo :: DB.Connection -> Int64 -> ConnStatus -> ConnStatus -> IO ()
updateConnectionStatusFromTo db connId fromStatus toStatus = do
  maybeFirstRow fromOnly (DB.query db "SELECT conn_status FROM connections WHERE connection_id = ?" (Only connId)) >>= \case
    Just status | status == fromStatus -> updateConnectionStatus_ db connId toStatus
    _ -> pure ()

updateConnectionStatus_ :: DB.Connection -> Int64 -> ConnStatus -> IO ()
updateConnectionStatus_ db connId connStatus = do
  currentTs <- getCurrentTime
  if connStatus == ConnReady
    then DB.execute db "UPDATE connections SET conn_status = ?, updated_at = ?, conn_req_inv = NULL, short_link_inv = NULL WHERE connection_id = ?" (connStatus, currentTs, connId)
    else DB.execute db "UPDATE connections SET conn_status = ?, updated_at = ? WHERE connection_id = ?" (connStatus, currentTs, connId)

updateContactSettings :: DB.Connection -> User -> Int64 -> ChatSettings -> IO ()
updateContactSettings db User {userId} contactId ChatSettings {enableNtfs, sendRcpts, favorite} =
  DB.execute db "UPDATE contacts SET enable_ntfs = ?, send_rcpts = ?, favorite = ? WHERE user_id = ? AND contact_id = ?" (enableNtfs, BI <$> sendRcpts, BI favorite, userId, contactId)

setConnConnReqInv :: DB.Connection -> User -> Int64 -> ConnReqInvitation -> IO ()
setConnConnReqInv db User {userId} connId connReq = do
  updatedAt <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE connections
      SET conn_req_inv = ?, updated_at = ?
      WHERE user_id = ? AND connection_id = ?
    |]
    (connReq, updatedAt, userId, connId)

resetContactConnInitiated :: DB.Connection -> User -> Connection -> IO ()
resetContactConnInitiated db User {userId} Connection {connId} = do
  updatedAt <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE connections
      SET contact_conn_initiated = 0, updated_at = ?
      WHERE user_id = ? AND connection_id = ?
    |]
    (updatedAt, userId, connId)

setContactCustomData :: DB.Connection -> User -> Contact -> Maybe CustomData -> IO ()
setContactCustomData db User {userId} Contact {contactId} customData = do
  updatedAt <- getCurrentTime
  DB.execute db "UPDATE contacts SET custom_data = ?, updated_at = ? WHERE user_id = ? AND contact_id = ?" (customData, updatedAt, userId, contactId)

setContactUIThemes :: DB.Connection -> User -> Contact -> Maybe UIThemeEntityOverrides -> IO ()
setContactUIThemes db User {userId} Contact {contactId} uiThemes = do
  updatedAt <- getCurrentTime
  DB.execute db "UPDATE contacts SET ui_themes = ?, updated_at = ? WHERE user_id = ? AND contact_id = ?" (uiThemes, updatedAt, userId, contactId)

setContactChatDeleted :: DB.Connection -> User -> Contact -> Bool -> IO ()
setContactChatDeleted db User {userId} Contact {contactId} chatDeleted = do
  updatedAt <- getCurrentTime
  DB.execute db "UPDATE contacts SET chat_deleted = ?, updated_at = ? WHERE user_id = ? AND contact_id = ?" (BI chatDeleted, updatedAt, userId, contactId)

updateDirectChatTags :: DB.Connection -> ContactId -> [ChatTagId] -> IO ()
updateDirectChatTags db contactId tIds = do
  currentTags <- getDirectChatTags db contactId
  let tagsToAdd = filter (`notElem` currentTags) tIds
      tagsToDelete = filter (`notElem` tIds) currentTags
  forM_ tagsToDelete $ untagDirectChat db contactId
  forM_ tagsToAdd $ tagDirectChat db contactId

tagDirectChat :: DB.Connection -> ContactId -> ChatTagId -> IO ()
tagDirectChat db contactId tId =
  DB.execute
    db
    [sql|
      INSERT INTO chat_tags_chats (contact_id, chat_tag_id)
      VALUES (?,?)
    |]
    (contactId, tId)

untagDirectChat :: DB.Connection -> ContactId -> ChatTagId -> IO ()
untagDirectChat db contactId tId =
  DB.execute
    db
    [sql|
      DELETE FROM chat_tags_chats
      WHERE contact_id = ? AND chat_tag_id = ?
    |]
    (contactId, tId)

getDirectChatTags :: DB.Connection -> ContactId -> IO [ChatTagId]
getDirectChatTags db contactId = map fromOnly <$> DB.query db "SELECT chat_tag_id FROM chat_tags_chats WHERE contact_id = ?" (Only contactId)

addDirectChatTags :: DB.Connection -> Contact -> IO Contact
addDirectChatTags db ct = do
  chatTags <- getDirectChatTags db $ contactId' ct
  pure (ct :: Contact) {chatTags}

setDirectChatTTL :: DB.Connection -> ContactId -> Maybe Int64 -> IO ()
setDirectChatTTL db ctId ttl = do
  updatedAt <- getCurrentTime
  DB.execute db "UPDATE contacts SET chat_item_ttl = ?, updated_at = ? WHERE contact_id = ?" (ttl, updatedAt, ctId)

getDirectChatTTL :: DB.Connection -> ContactId -> IO (Maybe Int64)
getDirectChatTTL db ctId =
  fmap join . maybeFirstRow fromOnly $
    DB.query db "SELECT chat_item_ttl FROM contacts WHERE contact_id = ? LIMIT 1" (Only ctId)

getUserContactsToExpire :: DB.Connection -> User -> Int64 -> IO [ContactId]
getUserContactsToExpire db User {userId} globalTTL =
  map fromOnly <$> DB.query db ("SELECT contact_id FROM contacts WHERE user_id = ? AND chat_item_ttl > 0" <> cond) (Only userId)
  where
    cond = if globalTTL == 0 then "" else " OR chat_item_ttl IS NULL"
