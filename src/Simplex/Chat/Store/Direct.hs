{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Simplex.Chat.Store.Direct
  ( updateContact_,
    updateContactProfile_,
    updateContactProfile_',
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
    deleteContactConnectionsAndFiles,
    deleteContact,
    deleteContactWithoutGroups,
    setContactDeleted,
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
    updateContactUsed,
    updateContactUnreadChat,
    setUserChatsRead,
    updateContactStatus,
    updateGroupUnreadChat,
    setConnectionVerified,
    incConnectionAuthErrCounter,
    setConnectionAuthErrCounter,
    getUserContacts,
    createOrUpdateContactRequest,
    getContactRequest',
    getContactRequest,
    getContactRequestIdByName,
    deleteContactRequest,
    createAcceptedContact,
    getUserByContactRequestId,
    getPendingContactConnections,
    getContactConnections,
    getConnectionById,
    getConnectionsContacts,
    updateConnectionStatus,
    updateContactSettings,
    setConnConnReqInv,
    resetContactConnInitiated,
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
import Database.SQLite.Simple (NamedParam (..), Only (..), (:.) (..))
import Database.SQLite.Simple.QQ (sql)
import Simplex.Chat.Messages
import Simplex.Chat.Store.Shared
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences
import Simplex.Messaging.Agent.Protocol (ConnId, InvitationId, UserId)
import Simplex.Messaging.Agent.Store.SQLite (firstRow, maybeFirstRow)
import qualified Simplex.Messaging.Agent.Store.SQLite.DB as DB
import Simplex.Messaging.Protocol (SubscriptionMode (..))
import Simplex.Messaging.Version

getPendingContactConnection :: DB.Connection -> UserId -> Int64 -> ExceptT StoreError IO PendingContactConnection
getPendingContactConnection db userId connId = do
  ExceptT . firstRow toPendingContactConnection (SEPendingConnectionNotFound connId) $
    DB.query
      db
      [sql|
        SELECT connection_id, agent_conn_id, conn_status, via_contact_uri_hash, via_user_contact_link, group_link_id, custom_user_profile_id, conn_req_inv, local_alias, created_at, updated_at
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

createAddressContactConnection :: DB.Connection -> User -> Contact -> ConnId -> ConnReqUriHash -> XContactId -> Maybe Profile -> SubscriptionMode -> ExceptT StoreError IO Contact
createAddressContactConnection db user@User {userId} Contact {contactId} acId cReqHash xContactId incognitoProfile subMode = do
  PendingContactConnection {pccConnId} <- liftIO $ createConnReqConnection db userId acId cReqHash xContactId incognitoProfile Nothing subMode
  liftIO $ DB.execute db "UPDATE connections SET contact_id = ? WHERE connection_id = ?" (contactId, pccConnId)
  getContact db user contactId

createConnReqConnection :: DB.Connection -> UserId -> ConnId -> ConnReqUriHash -> XContactId -> Maybe Profile -> Maybe GroupLinkId -> SubscriptionMode -> IO PendingContactConnection
createConnReqConnection db userId acId cReqHash xContactId incognitoProfile groupLinkId subMode = do
  createdAt <- getCurrentTime
  customUserProfileId <- mapM (createIncognitoProfile_ db userId createdAt) incognitoProfile
  let pccConnStatus = ConnJoined
  DB.execute
    db
    [sql|
      INSERT INTO connections (
        user_id, agent_conn_id, conn_status, conn_type, contact_conn_initiated,
        via_contact_uri_hash, xcontact_id, custom_user_profile_id, via_group_link, group_link_id, created_at, updated_at, to_subscribe
      ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)
    |]
    ((userId, acId, pccConnStatus, ConnContact, True, cReqHash, xContactId) :. (customUserProfileId, isJust groupLinkId, groupLinkId, createdAt, createdAt, subMode == SMOnlyCreate))
  pccConnId <- insertedRowId db
  pure PendingContactConnection {pccConnId, pccAgentConnId = AgentConnId acId, pccConnStatus, viaContactUri = True, viaUserContactLink = Nothing, groupLinkId, customUserProfileId, connReqInv = Nothing, localAlias = "", createdAt, updatedAt = createdAt}

getConnReqContactXContactId :: DB.Connection -> User -> ConnReqUriHash -> IO (Maybe Contact, Maybe XContactId)
getConnReqContactXContactId db user@User {userId} cReqHash = do
  getContactByConnReqHash db user cReqHash >>= \case
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

getContactByConnReqHash :: DB.Connection -> User -> ConnReqUriHash -> IO (Maybe Contact)
getContactByConnReqHash db user@User {userId} cReqHash =
  maybeFirstRow (toContact user) $
    DB.query
      db
      [sql|
        SELECT
          -- Contact
          ct.contact_id, ct.contact_profile_id, ct.local_display_name, ct.via_group, cp.display_name, cp.full_name, cp.image, cp.contact_link, cp.local_alias, ct.contact_used, ct.contact_status, ct.enable_ntfs, ct.send_rcpts, ct.favorite,
          cp.preferences, ct.user_preferences, ct.created_at, ct.updated_at, ct.chat_ts, ct.contact_group_member_id, ct.contact_grp_inv_sent,
          -- Connection
          c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id, c.conn_status, c.conn_type, c.contact_conn_initiated, c.local_alias,
          c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at, c.auth_err_counter,
          c.peer_chat_min_version, c.peer_chat_max_version
        FROM contacts ct
        JOIN contact_profiles cp ON ct.contact_profile_id = cp.contact_profile_id
        JOIN connections c ON c.contact_id = ct.contact_id
        WHERE c.user_id = ? AND c.via_contact_uri_hash = ? AND ct.contact_status = ? AND ct.deleted = 0
        ORDER BY c.created_at DESC
        LIMIT 1
      |]
      (userId, cReqHash, CSActive)

createDirectConnection :: DB.Connection -> User -> ConnId -> ConnReqInvitation -> ConnStatus -> Maybe Profile -> SubscriptionMode -> IO PendingContactConnection
createDirectConnection db User {userId} acId cReq pccConnStatus incognitoProfile subMode = do
  createdAt <- getCurrentTime
  customUserProfileId <- mapM (createIncognitoProfile_ db userId createdAt) incognitoProfile
  let contactConnInitiated = pccConnStatus == ConnNew
  DB.execute
    db
    [sql|
      INSERT INTO connections
        (user_id, agent_conn_id, conn_req_inv, conn_status, conn_type, contact_conn_initiated, custom_user_profile_id, created_at, updated_at, to_subscribe) VALUES (?,?,?,?,?,?,?,?,?,?)
    |]
    (userId, acId, cReq, pccConnStatus, ConnContact, contactConnInitiated, customUserProfileId, createdAt, createdAt, subMode == SMOnlyCreate)
  pccConnId <- insertedRowId db
  pure PendingContactConnection {pccConnId, pccAgentConnId = AgentConnId acId, pccConnStatus, viaContactUri = False, viaUserContactLink = Nothing, groupLinkId = Nothing, customUserProfileId, connReqInv = Just cReq, localAlias = "", createdAt, updatedAt = createdAt}

createIncognitoProfile :: DB.Connection -> User -> Profile -> IO Int64
createIncognitoProfile db User {userId} p = do
  createdAt <- getCurrentTime
  createIncognitoProfile_ db userId createdAt p

createDirectContact :: DB.Connection -> User -> Connection -> Profile -> Bool -> ExceptT StoreError IO Contact
createDirectContact db user@User {userId} conn@Connection {connId, localAlias} p@Profile {preferences} contactUsed = do
  currentTs <- liftIO getCurrentTime
  (localDisplayName, contactId, profileId) <- createContact_ db userId p localAlias Nothing currentTs contactUsed
  liftIO $ DB.execute db "UPDATE connections SET contact_id = ?, updated_at = ? WHERE connection_id = ?" (contactId, currentTs, connId)
  let profile = toLocalProfile profileId p localAlias
      userPreferences = emptyChatPrefs
      mergedPreferences = contactUserPreferences user userPreferences preferences $ connIncognito conn
  pure $ Contact {contactId, localDisplayName, profile, activeConn = Just conn, viaGroup = Nothing, contactUsed, contactStatus = CSActive, chatSettings = defaultChatSettings, userPreferences, mergedPreferences, createdAt = currentTs, updatedAt = currentTs, chatTs = Just currentTs, contactGroupMemberId = Nothing, contactGrpInvSent = False}

deleteContactConnectionsAndFiles :: DB.Connection -> UserId -> Contact -> IO ()
deleteContactConnectionsAndFiles db userId Contact {contactId} = do
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
  DB.execute db "DELETE FROM files WHERE user_id = ? AND contact_id = ?" (userId, contactId)

deleteContact :: DB.Connection -> User -> Contact -> IO ()
deleteContact db user@User {userId} Contact {contactId, localDisplayName, activeConn} = do
  DB.execute db "DELETE FROM chat_items WHERE user_id = ? AND contact_id = ?" (userId, contactId)
  ctMember :: (Maybe ContactId) <- maybeFirstRow fromOnly $ DB.query db "SELECT contact_id FROM group_members WHERE user_id = ? AND contact_id = ? LIMIT 1" (userId, contactId)
  if isNothing ctMember
    then do
      deleteContactProfile_ db userId contactId
      DB.execute db "DELETE FROM display_names WHERE user_id = ? AND local_display_name = ?" (userId, localDisplayName)
    else do
      currentTs <- getCurrentTime
      DB.execute db "UPDATE group_members SET contact_id = NULL, updated_at = ? WHERE user_id = ? AND contact_id = ?" (currentTs, userId, contactId)
  DB.execute db "DELETE FROM contacts WHERE user_id = ? AND contact_id = ?" (userId, contactId)
  forM_ activeConn $ \Connection {customUserProfileId} ->
    forM_ customUserProfileId $ \profileId ->
      deleteUnusedIncognitoProfileById_ db user profileId

-- should only be used if contact is not member of any groups
deleteContactWithoutGroups :: DB.Connection -> User -> Contact -> IO ()
deleteContactWithoutGroups db user@User {userId} Contact {contactId, localDisplayName, activeConn} = do
  DB.execute db "DELETE FROM chat_items WHERE user_id = ? AND contact_id = ?" (userId, contactId)
  deleteContactProfile_ db userId contactId
  DB.execute db "DELETE FROM display_names WHERE user_id = ? AND local_display_name = ?" (userId, localDisplayName)
  DB.execute db "DELETE FROM contacts WHERE user_id = ? AND contact_id = ?" (userId, contactId)
  forM_ activeConn $ \Connection {customUserProfileId} ->
    forM_ customUserProfileId $ \profileId ->
      deleteUnusedIncognitoProfileById_ db user profileId

setContactDeleted :: DB.Connection -> User -> Contact -> IO ()
setContactDeleted db User {userId} Contact {contactId} = do
  currentTs <- getCurrentTime
  DB.execute db "UPDATE contacts SET deleted = 1, updated_at = ? WHERE user_id = ? AND contact_id = ?" (currentTs, userId, contactId)

getDeletedContacts :: DB.Connection -> User -> IO [Contact]
getDeletedContacts db user@User {userId} = do
  contactIds <- map fromOnly <$> DB.query db "SELECT contact_id FROM contacts WHERE user_id = ? AND deleted = 1" (Only userId)
  rights <$> mapM (runExceptT . getDeletedContact db user) contactIds

getDeletedContact :: DB.Connection -> User -> Int64 -> ExceptT StoreError IO Contact
getDeletedContact db user contactId = getContact_ db user contactId True

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
  DB.executeNamed
    db
    [sql|
      DELETE FROM contact_profiles
      WHERE user_id = :user_id AND contact_profile_id = :profile_id
        AND 1 NOT IN (
          SELECT 1 FROM connections
          WHERE user_id = :user_id AND custom_user_profile_id = :profile_id LIMIT 1
        )
        AND 1 NOT IN (
          SELECT 1 FROM contacts
          WHERE user_id = :user_id AND contact_profile_id = :profile_id LIMIT 1
        )
        AND 1 NOT IN (
          SELECT 1 FROM contact_requests
          WHERE user_id = :user_id AND contact_profile_id = :profile_id LIMIT 1
        )
        AND 1 NOT IN (
          SELECT 1 FROM group_members
          WHERE user_id = :user_id
            AND (member_profile_id = :profile_id OR contact_profile_id = :profile_id)
          LIMIT 1
        )
    |]
    [":user_id" := userId, ":profile_id" := profileId]

updateContactProfile :: DB.Connection -> User -> Contact -> Profile -> ExceptT StoreError IO Contact
updateContactProfile db user@User {userId} c p'
  | displayName == newName = do
      liftIO $ updateContactProfile_ db userId profileId p'
      pure c {profile, mergedPreferences}
  | otherwise =
      ExceptT . withLocalDisplayName db userId newName $ \ldn -> do
        currentTs <- getCurrentTime
        updateContactProfile_' db userId profileId p' currentTs
        updateContact_ db userId contactId localDisplayName ldn currentTs
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

deletePCCIncognitoProfile :: DB.Connection -> User -> ProfileId -> IO ()
deletePCCIncognitoProfile db User {userId} profileId =
  DB.execute
    db
    [sql|
      DELETE FROM contact_profiles
      WHERE user_id = ? AND contact_profile_id = ? AND incognito = 1
    |]
    (userId, profileId)

updateContactUsed :: DB.Connection -> User -> Contact -> IO ()
updateContactUsed db User {userId} Contact {contactId} = do
  updatedAt <- getCurrentTime
  DB.execute db "UPDATE contacts SET contact_used = 1, updated_at = ? WHERE user_id = ? AND contact_id = ?" (updatedAt, userId, contactId)

updateContactUnreadChat :: DB.Connection -> User -> Contact -> Bool -> IO ()
updateContactUnreadChat db User {userId} Contact {contactId} unreadChat = do
  updatedAt <- getCurrentTime
  DB.execute db "UPDATE contacts SET unread_chat = ?, updated_at = ? WHERE user_id = ? AND contact_id = ?" (unreadChat, updatedAt, userId, contactId)

setUserChatsRead :: DB.Connection -> User -> IO ()
setUserChatsRead db User {userId} = do
  updatedAt <- getCurrentTime
  DB.execute db "UPDATE contacts SET unread_chat = ?, updated_at = ? WHERE user_id = ? AND unread_chat = ?" (False, updatedAt, userId, True)
  DB.execute db "UPDATE groups SET unread_chat = ?, updated_at = ? WHERE user_id = ? AND unread_chat = ?" (False, updatedAt, userId, True)
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
  DB.execute db "UPDATE groups SET unread_chat = ?, updated_at = ? WHERE user_id = ? AND group_id = ?" (unreadChat, updatedAt, userId, groupId)

setConnectionVerified :: DB.Connection -> User -> Int64 -> Maybe Text -> IO ()
setConnectionVerified db User {userId} connId code = do
  updatedAt <- getCurrentTime
  DB.execute db "UPDATE connections SET security_code = ?, security_code_verified_at = ?, updated_at = ? WHERE user_id = ? AND connection_id = ?" (code, code $> updatedAt, updatedAt, userId, connId)

incConnectionAuthErrCounter :: DB.Connection -> User -> Connection -> IO Int
incConnectionAuthErrCounter db User {userId} Connection {connId, authErrCounter} = do
  updatedAt <- getCurrentTime
  (counter_ :: Maybe Int) <- maybeFirstRow fromOnly $ DB.query db "SELECT auth_err_counter FROM connections WHERE user_id = ? AND connection_id = ?" (userId, connId)
  let counter' = fromMaybe authErrCounter counter_ + 1
  DB.execute db "UPDATE connections SET auth_err_counter = ?, updated_at = ? WHERE user_id = ? AND connection_id = ?" (counter', updatedAt, userId, connId)
  pure counter'

setConnectionAuthErrCounter :: DB.Connection -> User -> Connection -> Int -> IO ()
setConnectionAuthErrCounter db User {userId} Connection {connId} counter = do
  updatedAt <- getCurrentTime
  DB.execute db "UPDATE connections SET auth_err_counter = ?, updated_at = ? WHERE user_id = ? AND connection_id = ?" (counter, updatedAt, userId, connId)

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

updateContact_ :: DB.Connection -> UserId -> Int64 -> ContactName -> ContactName -> UTCTime -> IO ()
updateContact_ db userId contactId displayName newName updatedAt = do
  DB.execute
    db
    "UPDATE contacts SET local_display_name = ?, updated_at = ? WHERE user_id = ? AND contact_id = ?"
    (newName, updatedAt, userId, contactId)
  DB.execute
    db
    "UPDATE group_members SET local_display_name = ?, updated_at = ? WHERE user_id = ? AND contact_id = ?"
    (newName, updatedAt, userId, contactId)
  DB.execute db "DELETE FROM display_names WHERE local_display_name = ? AND user_id = ?" (displayName, userId)

getContactByName :: DB.Connection -> User -> ContactName -> ExceptT StoreError IO Contact
getContactByName db user localDisplayName = do
  cId <- getContactIdByName db user localDisplayName
  getContact db user cId

getUserContacts :: DB.Connection -> User -> IO [Contact]
getUserContacts db user@User {userId} = do
  contactIds <- map fromOnly <$> DB.query db "SELECT contact_id FROM contacts WHERE user_id = ? AND deleted = 0" (Only userId)
  contacts <- rights <$> mapM (runExceptT . getContact db user) contactIds
  pure $ filter (\Contact {activeConn} -> isJust activeConn) contacts

createOrUpdateContactRequest :: DB.Connection -> User -> Int64 -> InvitationId -> VersionRange -> Profile -> Maybe XContactId -> ExceptT StoreError IO ContactOrRequest
createOrUpdateContactRequest db user@User {userId} userContactLinkId invId (VersionRange minV maxV) Profile {displayName, fullName, image, contactLink, preferences} xContactId_ =
  liftIO (maybeM getContact' xContactId_) >>= \case
    Just contact -> pure $ CORContact contact
    Nothing -> CORRequest <$> createOrUpdate_
  where
    maybeM = maybe (pure Nothing)
    createOrUpdate_ :: ExceptT StoreError IO UserContactRequest
    createOrUpdate_ = do
      cReqId <-
        ExceptT $
          maybeM getContactRequestByXContactId xContactId_ >>= \case
            Nothing -> createContactRequest
            Just cr -> updateContactRequest cr $> Right cr.contactRequestId
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
                (user_contact_link_id, agent_invitation_id, peer_chat_min_version, peer_chat_max_version, contact_profile_id, local_display_name, user_id, created_at, updated_at, xcontact_id)
              VALUES (?,?,?,?,?,?,?,?,?,?)
            |]
            (userContactLinkId, invId, minV, maxV, profileId, ldn, userId, currentTs, currentTs, xContactId_)
          insertedRowId db
    getContact' :: XContactId -> IO (Maybe Contact)
    getContact' xContactId =
      maybeFirstRow (toContact user) $
        DB.query
          db
          [sql|
            SELECT
              -- Contact
              ct.contact_id, ct.contact_profile_id, ct.local_display_name, ct.via_group, cp.display_name, cp.full_name, cp.image, cp.contact_link, cp.local_alias, ct.contact_used, ct.contact_status, ct.enable_ntfs, ct.send_rcpts, ct.favorite,
              cp.preferences, ct.user_preferences, ct.created_at, ct.updated_at, ct.chat_ts, ct.contact_group_member_id, ct.contact_grp_inv_sent,
              -- Connection
              c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id, c.conn_status, c.conn_type, c.contact_conn_initiated, c.local_alias,
              c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at, c.auth_err_counter,
              c.peer_chat_min_version, c.peer_chat_max_version
            FROM contacts ct
            JOIN contact_profiles cp ON ct.contact_profile_id = cp.contact_profile_id
            LEFT JOIN connections c ON c.contact_id = ct.contact_id
            WHERE ct.user_id = ? AND ct.xcontact_id = ? AND ct.deleted = 0
            ORDER BY c.created_at DESC
            LIMIT 1
          |]
          (userId, xContactId)
    getContactRequestByXContactId :: XContactId -> IO (Maybe UserContactRequest)
    getContactRequestByXContactId xContactId =
      maybeFirstRow toContactRequest $
        DB.query
          db
          [sql|
            SELECT
              cr.contact_request_id, cr.local_display_name, cr.agent_invitation_id, cr.user_contact_link_id,
              c.agent_conn_id, cr.contact_profile_id, p.display_name, p.full_name, p.image, p.contact_link, cr.xcontact_id, p.preferences, cr.created_at, cr.updated_at,
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
                SET agent_invitation_id = ?, peer_chat_min_version = ?, peer_chat_max_version = ?, updated_at = ?
                WHERE user_id = ? AND contact_request_id = ?
              |]
              (invId, minV, maxV, currentTs, userId, cReqId)
        else withLocalDisplayName db userId displayName $ \ldn ->
          Right <$> do
            DB.execute
              db
              [sql|
                UPDATE contact_requests
                SET agent_invitation_id = ?, peer_chat_min_version = ?, peer_chat_max_version = ?, local_display_name = ?, updated_at = ?
                WHERE user_id = ? AND contact_request_id = ?
              |]
              (invId, minV, maxV, ldn, currentTs, userId, cReqId)
            DB.execute db "DELETE FROM display_names WHERE local_display_name = ? AND user_id = ?" (oldLdn, userId)
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
          cr.contact_request_id, cr.local_display_name, cr.agent_invitation_id, cr.user_contact_link_id,
          c.agent_conn_id, cr.contact_profile_id, p.display_name, p.full_name, p.image, p.contact_link, cr.xcontact_id, p.preferences, cr.created_at, cr.updated_at,
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
    |]
    (userId, userId, contactRequestId)
  DB.execute db "DELETE FROM contact_requests WHERE user_id = ? AND contact_request_id = ?" (userId, contactRequestId)

createAcceptedContact :: DB.Connection -> User -> ConnId -> VersionRange -> ContactName -> ProfileId -> Profile -> Int64 -> Maybe XContactId -> Maybe IncognitoProfile -> SubscriptionMode -> Bool -> IO Contact
createAcceptedContact db user@User {userId, profile = LocalProfile {preferences}} agentConnId cReqChatVRange localDisplayName profileId profile userContactLinkId xContactId incognitoProfile subMode contactUsed = do
  DB.execute db "DELETE FROM contact_requests WHERE user_id = ? AND local_display_name = ?" (userId, localDisplayName)
  createdAt <- getCurrentTime
  customUserProfileId <- forM incognitoProfile $ \case
    NewIncognito p -> createIncognitoProfile_ db userId createdAt p
    ExistingIncognito LocalProfile {profileId = pId} -> pure pId
  let userPreferences = fromMaybe emptyChatPrefs $ incognitoProfile >> preferences
  DB.execute
    db
    "INSERT INTO contacts (user_id, local_display_name, contact_profile_id, enable_ntfs, user_preferences, created_at, updated_at, chat_ts, xcontact_id, contact_used) VALUES (?,?,?,?,?,?,?,?,?,?)"
    (userId, localDisplayName, profileId, True, userPreferences, createdAt, createdAt, createdAt, xContactId, contactUsed)
  contactId <- insertedRowId db
  conn <- createConnection_ db userId ConnContact (Just contactId) agentConnId cReqChatVRange Nothing (Just userContactLinkId) customUserProfileId 0 createdAt subMode
  let mergedPreferences = contactUserPreferences user userPreferences preferences $ connIncognito conn
  pure $ Contact {contactId, localDisplayName, profile = toLocalProfile profileId profile "", activeConn = Just conn, viaGroup = Nothing, contactUsed, contactStatus = CSActive, chatSettings = defaultChatSettings, userPreferences, mergedPreferences, createdAt = createdAt, updatedAt = createdAt, chatTs = Just createdAt, contactGroupMemberId = Nothing, contactGrpInvSent = False}

getContactIdByName :: DB.Connection -> User -> ContactName -> ExceptT StoreError IO Int64
getContactIdByName db User {userId} cName =
  ExceptT . firstRow fromOnly (SEContactNotFoundByName cName) $
    DB.query db "SELECT contact_id FROM contacts WHERE user_id = ? AND local_display_name = ? AND deleted = 0" (userId, cName)

getContact :: DB.Connection -> User -> Int64 -> ExceptT StoreError IO Contact
getContact db user contactId = getContact_ db user contactId False

getContact_ :: DB.Connection -> User -> Int64 -> Bool -> ExceptT StoreError IO Contact
getContact_ db user@User {userId} contactId deleted =
  ExceptT . firstRow (toContact user) (SEContactNotFound contactId) $
    DB.query
      db
      [sql|
        SELECT
          -- Contact
          ct.contact_id, ct.contact_profile_id, ct.local_display_name, ct.via_group, cp.display_name, cp.full_name, cp.image, cp.contact_link, cp.local_alias, ct.contact_used, ct.contact_status, ct.enable_ntfs, ct.send_rcpts, ct.favorite,
          cp.preferences, ct.user_preferences, ct.created_at, ct.updated_at, ct.chat_ts, ct.contact_group_member_id, ct.contact_grp_inv_sent,
          -- Connection
          c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id, c.conn_status, c.conn_type, c.contact_conn_initiated, c.local_alias,
          c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at, c.auth_err_counter,
          c.peer_chat_min_version, c.peer_chat_max_version
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
              )
            )
            OR c.connection_id IS NULL
          )
      |]
      (userId, contactId, deleted, ConnReady, ConnSndReady)

getUserByContactRequestId :: DB.Connection -> Int64 -> ExceptT StoreError IO User
getUserByContactRequestId db contactRequestId =
  ExceptT . firstRow toUser (SEUserNotFoundByContactRequestId contactRequestId) $
    DB.query db (userQuery <> " JOIN contact_requests cr ON cr.user_id = u.user_id WHERE cr.contact_request_id = ?") (Only contactRequestId)

getPendingContactConnections :: DB.Connection -> User -> IO [PendingContactConnection]
getPendingContactConnections db User {userId} = do
  map toPendingContactConnection
    <$> DB.queryNamed
      db
      [sql|
        SELECT connection_id, agent_conn_id, conn_status, via_contact_uri_hash, via_user_contact_link, group_link_id, custom_user_profile_id, conn_req_inv, local_alias, created_at, updated_at
        FROM connections
        WHERE user_id = :user_id
          AND conn_type = :conn_type
          AND contact_id IS NULL
      |]
      [":user_id" := userId, ":conn_type" := ConnContact]

getContactConnections :: DB.Connection -> UserId -> Contact -> IO [Connection]
getContactConnections db userId Contact {contactId} =
  connections =<< liftIO getConnections_
  where
    getConnections_ =
      DB.query
        db
        [sql|
          SELECT c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact, c.via_user_contact_link, c.via_group_link, c.group_link_id, c.custom_user_profile_id,
            c.conn_status, c.conn_type, c.contact_conn_initiated, c.local_alias, c.contact_id, c.group_member_id, c.snd_file_id, c.rcv_file_id, c.user_contact_link_id, c.created_at, c.security_code, c.security_code_verified_at, c.auth_err_counter,
            c.peer_chat_min_version, c.peer_chat_max_version
          FROM connections c
          JOIN contacts ct ON ct.contact_id = c.contact_id
          WHERE c.user_id = ? AND ct.user_id = ? AND ct.contact_id = ?
        |]
        (userId, userId, contactId)
    connections [] = pure []
    connections rows = pure $ map toConnection rows

getConnectionById :: DB.Connection -> User -> Int64 -> ExceptT StoreError IO Connection
getConnectionById db User {userId} connId = ExceptT $ do
  firstRow toConnection (SEConnectionNotFoundById connId) $
    DB.query
      db
      [sql|
        SELECT connection_id, agent_conn_id, conn_level, via_contact, via_user_contact_link, via_group_link, group_link_id, custom_user_profile_id,
          conn_status, conn_type, contact_conn_initiated, local_alias, contact_id, group_member_id, snd_file_id, rcv_file_id, user_contact_link_id, created_at, security_code, security_code_verified_at, auth_err_counter,
          peer_chat_min_version, peer_chat_max_version
        FROM connections
        WHERE user_id = ? AND connection_id = ?
      |]
      (userId, connId)

getConnectionsContacts :: DB.Connection -> [ConnId] -> IO [ContactRef]
getConnectionsContacts db agentConnIds = do
  DB.execute_ db "DROP TABLE IF EXISTS temp.conn_ids"
  DB.execute_ db "CREATE TABLE temp.conn_ids (conn_id BLOB)"
  DB.executeMany db "INSERT INTO temp.conn_ids (conn_id) VALUES (?)" $ map Only agentConnIds
  conns <-
    map toContactRef
      <$> DB.query
        db
        [sql|
          SELECT ct.contact_id, c.connection_id, c.agent_conn_id, ct.local_display_name
          FROM contacts ct
          JOIN connections c ON c.contact_id = ct.contact_id
          WHERE c.agent_conn_id IN (SELECT conn_id FROM temp.conn_ids)
            AND c.conn_type = ?
            AND ct.deleted = 0
        |]
        (Only ConnContact)
  DB.execute_ db "DROP TABLE temp.conn_ids"
  pure conns
  where
    toContactRef :: (ContactId, Int64, ConnId, ContactName) -> ContactRef
    toContactRef (contactId, connId, acId, localDisplayName) = ContactRef {contactId, connId, agentConnId = AgentConnId acId, localDisplayName}

updateConnectionStatus :: DB.Connection -> Connection -> ConnStatus -> IO ()
updateConnectionStatus db Connection {connId} connStatus = do
  currentTs <- getCurrentTime
  if connStatus == ConnReady
    then DB.execute db "UPDATE connections SET conn_status = ?, updated_at = ?, conn_req_inv = NULL WHERE connection_id = ?" (connStatus, currentTs, connId)
    else DB.execute db "UPDATE connections SET conn_status = ?, updated_at = ? WHERE connection_id = ?" (connStatus, currentTs, connId)

updateContactSettings :: DB.Connection -> User -> Int64 -> ChatSettings -> IO ()
updateContactSettings db User {userId} contactId ChatSettings {enableNtfs, sendRcpts, favorite} =
  DB.execute db "UPDATE contacts SET enable_ntfs = ?, send_rcpts = ?, favorite = ? WHERE user_id = ? AND contact_id = ?" (enableNtfs, sendRcpts, favorite, userId, contactId)

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
