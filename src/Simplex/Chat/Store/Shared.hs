{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Simplex.Chat.Store.Shared where

import Control.Exception (Exception)
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Crypto.Random (ChaChaDRG)
import qualified Data.Aeson.TH as J
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Char8 (ByteString)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Database.SQLite.Simple (NamedParam (..), Only (..), Query, SQLError, (:.) (..))
import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple.QQ (sql)
import Simplex.Chat.Messages
import Simplex.Chat.Protocol
import Simplex.Chat.Remote.Types
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences
import Simplex.Messaging.Agent.Protocol (ConnId, UserId)
import Simplex.Messaging.Agent.Store.SQLite (firstRow, maybeFirstRow)
import qualified Simplex.Messaging.Agent.Store.SQLite.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.Ratchet (PQEncryption (..), PQSupport (..))
import qualified Simplex.Messaging.Crypto.Ratchet as CR
import Simplex.Messaging.Parsers (dropPrefix, sumTypeJSON)
import Simplex.Messaging.Protocol (SubscriptionMode (..))
import Simplex.Messaging.Util (allFinally)
import Simplex.Messaging.Version
import UnliftIO.STM

-- These error type constructors must be added to mobile apps
data StoreError
  = SEDuplicateName
  | SEUserNotFound {userId :: UserId}
  | SEUserNotFoundByName {contactName :: ContactName}
  | SEUserNotFoundByContactId {contactId :: ContactId}
  | SEUserNotFoundByGroupId {groupId :: GroupId}
  | SEUserNotFoundByFileId {fileId :: FileTransferId}
  | SEUserNotFoundByContactRequestId {contactRequestId :: Int64}
  | SEContactNotFound {contactId :: ContactId}
  | SEContactNotFoundByName {contactName :: ContactName}
  | SEContactNotFoundByMemberId {groupMemberId :: GroupMemberId}
  | SEContactNotReady {contactName :: ContactName}
  | SEDuplicateContactLink
  | SEUserContactLinkNotFound
  | SEContactRequestNotFound {contactRequestId :: Int64}
  | SEContactRequestNotFoundByName {contactName :: ContactName}
  | SEGroupNotFound {groupId :: GroupId}
  | SEGroupNotFoundByName {groupName :: GroupName}
  | SEGroupMemberNameNotFound {groupId :: GroupId, groupMemberName :: ContactName}
  | SEGroupMemberNotFound {groupMemberId :: GroupMemberId}
  | SEGroupMemberNotFoundByMemberId {memberId :: MemberId}
  | SEMemberContactGroupMemberNotFound {contactId :: ContactId}
  | SEGroupWithoutUser
  | SEDuplicateGroupMember
  | SEGroupAlreadyJoined
  | SEGroupInvitationNotFound
  | SENoteFolderAlreadyExists {noteFolderId :: NoteFolderId}
  | SENoteFolderNotFound {noteFolderId :: NoteFolderId}
  | SEUserNoteFolderNotFound
  | SESndFileNotFound {fileId :: FileTransferId}
  | SESndFileInvalid {fileId :: FileTransferId}
  | SERcvFileNotFound {fileId :: FileTransferId}
  | SERcvFileDescrNotFound {fileId :: FileTransferId}
  | SEFileNotFound {fileId :: FileTransferId}
  | SERcvFileInvalid {fileId :: FileTransferId}
  | SERcvFileInvalidDescrPart
  | SELocalFileNoTransfer {fileId :: FileTransferId}
  | SESharedMsgIdNotFoundByFileId {fileId :: FileTransferId}
  | SEFileIdNotFoundBySharedMsgId {sharedMsgId :: SharedMsgId}
  | SESndFileNotFoundXFTP {agentSndFileId :: AgentSndFileId}
  | SERcvFileNotFoundXFTP {agentRcvFileId :: AgentRcvFileId}
  | SEConnectionNotFound {agentConnId :: AgentConnId}
  | SEConnectionNotFoundById {connId :: Int64}
  | SEConnectionNotFoundByMemberId {groupMemberId :: GroupMemberId}
  | SEPendingConnectionNotFound {connId :: Int64}
  | SEIntroNotFound
  | SEUniqueID
  | SELargeMsg
  | SEInternalError {message :: String}
  | SEDatabaseError {message :: String}
  | SEBadChatItem {itemId :: ChatItemId, itemTs :: Maybe ChatItemTs}
  | SEChatItemNotFound {itemId :: ChatItemId}
  | SEChatItemNotFoundByText {text :: Text}
  | SEChatItemSharedMsgIdNotFound {sharedMsgId :: SharedMsgId}
  | SEChatItemNotFoundByFileId {fileId :: FileTransferId}
  | SEChatItemNotFoundByContactId {contactId :: ContactId}
  | SEChatItemNotFoundByGroupId {groupId :: GroupId}
  | SEProfileNotFound {profileId :: Int64}
  | SEDuplicateGroupLink {groupInfo :: GroupInfo}
  | SEGroupLinkNotFound {groupInfo :: GroupInfo}
  | SEHostMemberIdNotFound {groupId :: Int64}
  | SEContactNotFoundByFileId {fileId :: FileTransferId}
  | SENoGroupSndStatus {itemId :: ChatItemId, groupMemberId :: GroupMemberId}
  | SEDuplicateGroupMessage {groupId :: Int64, sharedMsgId :: SharedMsgId, authorGroupMemberId :: Maybe GroupMemberId, forwardedByGroupMemberId :: Maybe GroupMemberId}
  | SERemoteHostNotFound {remoteHostId :: RemoteHostId}
  | SERemoteHostUnknown -- attempting to store KnownHost without a known fingerprint
  | SERemoteHostDuplicateCA
  | SERemoteCtrlNotFound {remoteCtrlId :: RemoteCtrlId}
  | SERemoteCtrlDuplicateCA
  | SEProhibitedDeleteUser {userId :: UserId, contactId :: ContactId}
  deriving (Show, Exception)

$(J.deriveJSON (sumTypeJSON $ dropPrefix "SE") ''StoreError)

insertedRowId :: DB.Connection -> IO Int64
insertedRowId db = fromOnly . head <$> DB.query_ db "SELECT last_insert_rowid()"

checkConstraint :: StoreError -> ExceptT StoreError IO a -> ExceptT StoreError IO a
checkConstraint err action = ExceptT $ runExceptT action `E.catch` (pure . Left . handleSQLError err)

handleSQLError :: StoreError -> SQLError -> StoreError
handleSQLError err e
  | SQL.sqlError e == SQL.ErrorConstraint = err
  | otherwise = SEInternalError $ show e

storeFinally :: ExceptT StoreError IO a -> ExceptT StoreError IO b -> ExceptT StoreError IO a
storeFinally = allFinally mkStoreError
{-# INLINE storeFinally #-}

mkStoreError :: E.SomeException -> StoreError
mkStoreError = SEInternalError . show
{-# INLINE mkStoreError #-}

fileInfoQuery :: Query
fileInfoQuery =
  [sql|
    SELECT f.file_id, f.ci_file_status, f.file_path
    FROM chat_items i
    JOIN files f ON f.chat_item_id = i.chat_item_id
  |]

toFileInfo :: (Int64, Maybe ACIFileStatus, Maybe FilePath) -> CIFileInfo
toFileInfo (fileId, fileStatus, filePath) = CIFileInfo {fileId, fileStatus, filePath}

type EntityIdsRow = (Maybe Int64, Maybe Int64, Maybe Int64, Maybe Int64, Maybe Int64)

type ConnectionRow = (Int64, ConnId, Int, Maybe Int64, Maybe Int64, Bool, Maybe GroupLinkId, Maybe Int64, ConnStatus, ConnType, Bool, LocalAlias) :. EntityIdsRow :. (UTCTime, Maybe Text, Maybe UTCTime, PQSupport, PQEncryption, Maybe PQEncryption, Maybe PQEncryption, Int, Maybe VersionChat, VersionChat, VersionChat)

type MaybeConnectionRow = (Maybe Int64, Maybe ConnId, Maybe Int, Maybe Int64, Maybe Int64, Maybe Bool, Maybe GroupLinkId, Maybe Int64, Maybe ConnStatus, Maybe ConnType, Maybe Bool, Maybe LocalAlias) :. EntityIdsRow :. (Maybe UTCTime, Maybe Text, Maybe UTCTime, Maybe PQSupport, Maybe PQEncryption, Maybe PQEncryption, Maybe PQEncryption, Maybe Int, Maybe VersionChat, Maybe VersionChat, Maybe VersionChat)

toConnection :: (PQSupport -> VersionRangeChat) -> ConnectionRow -> Connection
toConnection vr ((connId, acId, connLevel, viaContact, viaUserContactLink, viaGroupLink, groupLinkId, customUserProfileId, connStatus, connType, contactConnInitiated, localAlias) :. (contactId, groupMemberId, sndFileId, rcvFileId, userContactLinkId) :. (createdAt, code_, verifiedAt_, pqSupport, pqEncryption, pqSndEnabled, pqRcvEnabled, authErrCounter, chatV, minVer, maxVer)) =
  Connection
    { connId,
      agentConnId = AgentConnId acId,
      connChatVersion = fromMaybe (vr pqSupport `peerConnChatVersion` peerChatVRange) chatV,
      peerChatVRange = peerChatVRange,
      connLevel,
      viaContact,
      viaUserContactLink,
      viaGroupLink,
      groupLinkId,
      customUserProfileId,
      connStatus,
      connType,
      contactConnInitiated,
      localAlias,
      entityId = entityId_ connType,
      connectionCode = SecurityCode <$> code_ <*> verifiedAt_,
      pqSupport,
      pqEncryption,
      pqSndEnabled,
      pqRcvEnabled,
      authErrCounter,
      createdAt
    }
  where
    peerChatVRange = fromMaybe (versionToRange maxVer) $ safeVersionRange minVer maxVer
    entityId_ :: ConnType -> Maybe Int64
    entityId_ ConnContact = contactId
    entityId_ ConnMember = groupMemberId
    entityId_ ConnRcvFile = rcvFileId
    entityId_ ConnSndFile = sndFileId
    entityId_ ConnUserContact = userContactLinkId

toMaybeConnection :: (PQSupport -> VersionRangeChat) -> MaybeConnectionRow -> Maybe Connection
toMaybeConnection vr ((Just connId, Just agentConnId, Just connLevel, viaContact, viaUserContactLink, Just viaGroupLink, groupLinkId, customUserProfileId, Just connStatus, Just connType, Just contactConnInitiated, Just localAlias) :. (contactId, groupMemberId, sndFileId, rcvFileId, userContactLinkId) :. (Just createdAt, code_, verifiedAt_, Just pqSupport, Just pqEncryption, pqSndEnabled_, pqRcvEnabled_, Just authErrCounter, connChatVersion, Just minVer, Just maxVer)) =
  Just $ toConnection vr ((connId, agentConnId, connLevel, viaContact, viaUserContactLink, viaGroupLink, groupLinkId, customUserProfileId, connStatus, connType, contactConnInitiated, localAlias) :. (contactId, groupMemberId, sndFileId, rcvFileId, userContactLinkId) :. (createdAt, code_, verifiedAt_, pqSupport, pqEncryption, pqSndEnabled_, pqRcvEnabled_, authErrCounter, connChatVersion, minVer, maxVer))
toMaybeConnection _ _ = Nothing

createConnection_ :: DB.Connection -> UserId -> ConnType -> Maybe Int64 -> ConnId -> VersionChat -> VersionRangeChat -> Maybe ContactId -> Maybe Int64 -> Maybe ProfileId -> Int -> UTCTime -> SubscriptionMode -> PQSupport -> IO Connection
createConnection_ db userId connType entityId acId connChatVersion peerChatVRange@(VersionRange minV maxV) viaContact viaUserContactLink customUserProfileId connLevel currentTs subMode pqSup = do
  viaLinkGroupId :: Maybe Int64 <- fmap join . forM viaUserContactLink $ \ucLinkId ->
    maybeFirstRow fromOnly $ DB.query db "SELECT group_id FROM user_contact_links WHERE user_id = ? AND user_contact_link_id = ? AND group_id IS NOT NULL" (userId, ucLinkId)
  let viaGroupLink = isJust viaLinkGroupId
  DB.execute
    db
    [sql|
      INSERT INTO connections (
        user_id, agent_conn_id, conn_level, via_contact, via_user_contact_link, via_group_link, custom_user_profile_id, conn_status, conn_type,
        contact_id, group_member_id, snd_file_id, rcv_file_id, user_contact_link_id, created_at, updated_at,
        conn_chat_version, peer_chat_min_version, peer_chat_max_version, to_subscribe, pq_support, pq_encryption
      ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
    |]
    ( (userId, acId, connLevel, viaContact, viaUserContactLink, viaGroupLink, customUserProfileId, ConnNew, connType)
        :. (ent ConnContact, ent ConnMember, ent ConnSndFile, ent ConnRcvFile, ent ConnUserContact, currentTs, currentTs)
        :. (connChatVersion, minV, maxV, subMode == SMOnlyCreate, pqSup, pqSup)
    )
  connId <- insertedRowId db
  pure
    Connection
      { connId,
        agentConnId = AgentConnId acId,
        connChatVersion,
        peerChatVRange,
        connType,
        contactConnInitiated = False,
        entityId,
        viaContact,
        viaUserContactLink,
        viaGroupLink,
        groupLinkId = Nothing,
        customUserProfileId,
        connLevel,
        connStatus = ConnNew,
        localAlias = "",
        createdAt = currentTs,
        connectionCode = Nothing,
        pqSupport = pqSup,
        pqEncryption = CR.pqSupportToEnc pqSup,
        pqSndEnabled = Nothing,
        pqRcvEnabled = Nothing,
        authErrCounter = 0
      }
  where
    ent ct = if connType == ct then entityId else Nothing

createIncognitoProfile_ :: DB.Connection -> UserId -> UTCTime -> Profile -> IO Int64
createIncognitoProfile_ db userId createdAt Profile {displayName, fullName, image} = do
  DB.execute
    db
    [sql|
      INSERT INTO contact_profiles (display_name, full_name, image, user_id, incognito, created_at, updated_at)
      VALUES (?,?,?,?,?,?,?)
    |]
    (displayName, fullName, image, userId, Just True, createdAt, createdAt)
  insertedRowId db

updateConnSupportPQ :: DB.Connection -> Int64 -> PQSupport -> PQEncryption -> IO ()
updateConnSupportPQ db connId pqSup pqEnc =
  DB.execute
    db
    [sql|
      UPDATE connections
      SET pq_support = ?, pq_encryption = ?
      WHERE connection_id = ?
    |]
    (pqSup, pqEnc, connId)

updateConnPQSndEnabled :: DB.Connection -> Int64 -> PQEncryption -> IO ()
updateConnPQSndEnabled db connId pqSndEnabled =
  DB.execute
    db
    [sql|
      UPDATE connections
      SET pq_snd_enabled = ?
      WHERE connection_id = ?
    |]
    (pqSndEnabled, connId)

updateConnPQRcvEnabled :: DB.Connection -> Int64 -> PQEncryption -> IO ()
updateConnPQRcvEnabled db connId pqRcvEnabled =
  DB.execute
    db
    [sql|
      UPDATE connections
      SET pq_rcv_enabled = ?
      WHERE connection_id = ?
    |]
    (pqRcvEnabled, connId)

updateConnPQEnabledCON :: DB.Connection -> Int64 -> PQEncryption -> IO ()
updateConnPQEnabledCON db connId pqEnabled =
  DB.execute
    db
    [sql|
      UPDATE connections
      SET pq_snd_enabled = ?, pq_rcv_enabled = ?
      WHERE connection_id = ?
    |]
    (pqEnabled, pqEnabled, connId)

setPeerChatVRange :: DB.Connection -> Int64 -> VersionChat -> VersionRangeChat -> IO ()
setPeerChatVRange db connId chatV (VersionRange minVer maxVer) =
  DB.execute
    db
    [sql|
      UPDATE connections
      SET conn_chat_version = ?, peer_chat_min_version = ?, peer_chat_max_version = ?
      WHERE connection_id = ?
    |]
    (chatV, minVer, maxVer, connId)

setMemberChatVRange :: DB.Connection -> GroupMemberId -> VersionRangeChat -> IO ()
setMemberChatVRange db mId (VersionRange minVer maxVer) =
  DB.execute
    db
    [sql|
      UPDATE group_members
      SET peer_chat_min_version = ?, peer_chat_max_version = ?
      WHERE group_member_id = ?
    |]
    (minVer, maxVer, mId)

setCommandConnId :: DB.Connection -> User -> CommandId -> Int64 -> IO ()
setCommandConnId db User {userId} cmdId connId = do
  updatedAt <- getCurrentTime
  DB.execute
    db
    [sql|
      UPDATE commands
      SET connection_id = ?, updated_at = ?
      WHERE user_id = ? AND command_id = ?
    |]
    (connId, updatedAt, userId, cmdId)

createContact :: DB.Connection -> User -> Profile -> ExceptT StoreError IO ()
createContact db User {userId} profile = do
  currentTs <- liftIO getCurrentTime
  void $ createContact_ db userId profile "" Nothing currentTs True

createContact_ :: DB.Connection -> UserId -> Profile -> LocalAlias -> Maybe Int64 -> UTCTime -> Bool -> ExceptT StoreError IO (Text, ContactId, ProfileId)
createContact_ db userId Profile {displayName, fullName, image, contactLink, preferences} localAlias viaGroup currentTs contactUsed =
  ExceptT . withLocalDisplayName db userId displayName $ \ldn -> do
    DB.execute
      db
      "INSERT INTO contact_profiles (display_name, full_name, image, contact_link, user_id, local_alias, preferences, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?)"
      (displayName, fullName, image, contactLink, userId, localAlias, preferences, currentTs, currentTs)
    profileId <- insertedRowId db
    DB.execute
      db
      "INSERT INTO contacts (contact_profile_id, local_display_name, user_id, via_group, created_at, updated_at, chat_ts, contact_used) VALUES (?,?,?,?,?,?,?,?)"
      (profileId, ldn, userId, viaGroup, currentTs, currentTs, currentTs, contactUsed)
    contactId <- insertedRowId db
    pure $ Right (ldn, contactId, profileId)

deleteUnusedIncognitoProfileById_ :: DB.Connection -> User -> ProfileId -> IO ()
deleteUnusedIncognitoProfileById_ db User {userId} profileId =
  DB.executeNamed
    db
    [sql|
      DELETE FROM contact_profiles
      WHERE user_id = :user_id AND contact_profile_id = :profile_id AND incognito = 1
        AND 1 NOT IN (
          SELECT 1 FROM connections
          WHERE user_id = :user_id AND custom_user_profile_id = :profile_id LIMIT 1
        )
        AND 1 NOT IN (
          SELECT 1 FROM group_members
          WHERE user_id = :user_id AND member_profile_id = :profile_id LIMIT 1
        )
    |]
    [":user_id" := userId, ":profile_id" := profileId]

type ContactRow = (ContactId, ProfileId, ContactName, Maybe Int64, ContactName, Text, Maybe ImageData, Maybe ConnReqContact, LocalAlias, Bool, ContactStatus) :. (Maybe MsgFilter, Maybe Bool, Bool, Maybe Preferences, Preferences, UTCTime, UTCTime, Maybe UTCTime, Maybe GroupMemberId, Bool)

toContact :: (PQSupport -> VersionRangeChat) -> User -> ContactRow :. MaybeConnectionRow -> Contact
toContact vr user (((contactId, profileId, localDisplayName, viaGroup, displayName, fullName, image, contactLink, localAlias, contactUsed, contactStatus) :. (enableNtfs_, sendRcpts, favorite, preferences, userPreferences, createdAt, updatedAt, chatTs, contactGroupMemberId, contactGrpInvSent)) :. connRow) =
  let profile = LocalProfile {profileId, displayName, fullName, image, contactLink, preferences, localAlias}
      activeConn = toMaybeConnection vr connRow
      chatSettings = ChatSettings {enableNtfs = fromMaybe MFAll enableNtfs_, sendRcpts, favorite}
      incognito = maybe False connIncognito activeConn
      mergedPreferences = contactUserPreferences user userPreferences preferences incognito
   in Contact {contactId, localDisplayName, profile, activeConn, viaGroup, contactUsed, contactStatus, chatSettings, userPreferences, mergedPreferences, createdAt, updatedAt, chatTs, contactGroupMemberId, contactGrpInvSent}

getProfileById :: DB.Connection -> UserId -> Int64 -> ExceptT StoreError IO LocalProfile
getProfileById db userId profileId =
  ExceptT . firstRow toProfile (SEProfileNotFound profileId) $
    DB.query
      db
      [sql|
        SELECT cp.display_name, cp.full_name, cp.image, cp.contact_link, cp.local_alias, cp.preferences -- , ct.user_preferences
        FROM contact_profiles cp
        WHERE cp.user_id = ? AND cp.contact_profile_id = ?
      |]
      (userId, profileId)
  where
    toProfile :: (ContactName, Text, Maybe ImageData, Maybe ConnReqContact, LocalAlias, Maybe Preferences) -> LocalProfile
    toProfile (displayName, fullName, image, contactLink, localAlias, preferences) = LocalProfile {profileId, displayName, fullName, image, contactLink, preferences, localAlias}

type ContactRequestRow = (Int64, ContactName, AgentInvId, Int64, AgentConnId, Int64, ContactName, Text, Maybe ImageData, Maybe ConnReqContact) :. (Maybe XContactId, PQSupport, Maybe Preferences, UTCTime, UTCTime, VersionChat, VersionChat)

toContactRequest :: ContactRequestRow -> UserContactRequest
toContactRequest ((contactRequestId, localDisplayName, agentInvitationId, userContactLinkId, agentContactConnId, profileId, displayName, fullName, image, contactLink) :. (xContactId, pqSupport, preferences, createdAt, updatedAt, minVer, maxVer)) = do
  let profile = Profile {displayName, fullName, image, contactLink, preferences}
      cReqChatVRange = fromMaybe (versionToRange maxVer) $ safeVersionRange minVer maxVer
   in UserContactRequest {contactRequestId, agentInvitationId, userContactLinkId, agentContactConnId, cReqChatVRange, localDisplayName, profileId, profile, xContactId, pqSupport, createdAt, updatedAt}

userQuery :: Query
userQuery =
  [sql|
    SELECT u.user_id, u.agent_user_id, u.contact_id, ucp.contact_profile_id, u.active_user, u.local_display_name, ucp.full_name, ucp.image, ucp.contact_link, ucp.preferences,
      u.show_ntfs, u.send_rcpts_contacts, u.send_rcpts_small_groups, u.view_pwd_hash, u.view_pwd_salt, u.user_member_profile_updated_at
    FROM users u
    JOIN contacts uct ON uct.contact_id = u.contact_id
    JOIN contact_profiles ucp ON ucp.contact_profile_id = uct.contact_profile_id
  |]

toUser :: (UserId, UserId, ContactId, ProfileId, Bool, ContactName, Text, Maybe ImageData, Maybe ConnReqContact, Maybe Preferences) :. (Bool, Bool, Bool, Maybe B64UrlByteString, Maybe B64UrlByteString, Maybe UTCTime) -> User
toUser ((userId, auId, userContactId, profileId, activeUser, displayName, fullName, image, contactLink, userPreferences) :. (showNtfs, sendRcptsContacts, sendRcptsSmallGroups, viewPwdHash_, viewPwdSalt_, userMemberProfileUpdatedAt)) =
  User {userId, agentUserId = AgentUserId auId, userContactId, localDisplayName = displayName, profile, activeUser, fullPreferences, showNtfs, sendRcptsContacts, sendRcptsSmallGroups, viewPwdHash, userMemberProfileUpdatedAt}
  where
    profile = LocalProfile {profileId, displayName, fullName, image, contactLink, preferences = userPreferences, localAlias = ""}
    fullPreferences = mergePreferences Nothing userPreferences
    viewPwdHash = UserPwdHash <$> viewPwdHash_ <*> viewPwdSalt_

toPendingContactConnection :: (Int64, ConnId, ConnStatus, Maybe ByteString, Maybe Int64, Maybe GroupLinkId, Maybe Int64, Maybe ConnReqInvitation, LocalAlias, UTCTime, UTCTime) -> PendingContactConnection
toPendingContactConnection (pccConnId, acId, pccConnStatus, connReqHash, viaUserContactLink, groupLinkId, customUserProfileId, connReqInv, localAlias, createdAt, updatedAt) =
  PendingContactConnection {pccConnId, pccAgentConnId = AgentConnId acId, pccConnStatus, viaContactUri = isJust connReqHash, viaUserContactLink, groupLinkId, customUserProfileId, connReqInv, localAlias, createdAt, updatedAt}

getConnReqInv :: DB.Connection -> Int64 -> ExceptT StoreError IO ConnReqInvitation
getConnReqInv db connId =
  ExceptT . firstRow fromOnly (SEConnectionNotFoundById connId) $
    DB.query
      db
      "SELECT conn_req_inv FROM connections WHERE connection_id = ?"
      (Only connId)

-- | Saves unique local display name based on passed displayName, suffixed with _N if required.
-- This function should be called inside transaction.
withLocalDisplayName :: forall a. DB.Connection -> UserId -> Text -> (Text -> IO (Either StoreError a)) -> IO (Either StoreError a)
withLocalDisplayName db userId displayName action = getLdnSuffix >>= (`tryCreateName` 20)
  where
    getLdnSuffix :: IO Int
    getLdnSuffix =
      maybe 0 ((+ 1) . fromOnly) . listToMaybe
        <$> DB.queryNamed
          db
          [sql|
            SELECT ldn_suffix FROM display_names
            WHERE user_id = :user_id AND ldn_base = :display_name
            ORDER BY ldn_suffix DESC
            LIMIT 1
          |]
          [":user_id" := userId, ":display_name" := displayName]
    tryCreateName :: Int -> Int -> IO (Either StoreError a)
    tryCreateName _ 0 = pure $ Left SEDuplicateName
    tryCreateName ldnSuffix attempts = do
      currentTs <- getCurrentTime
      let ldn = displayName <> (if ldnSuffix == 0 then "" else T.pack $ '_' : show ldnSuffix)
      E.try (insertName ldn currentTs) >>= \case
        Right () -> action ldn
        Left e
          | SQL.sqlError e == SQL.ErrorConstraint -> tryCreateName (ldnSuffix + 1) (attempts - 1)
          | otherwise -> E.throwIO e
      where
        insertName ldn ts =
          DB.execute
            db
            [sql|
              INSERT INTO display_names
                (local_display_name, ldn_base, ldn_suffix, user_id, created_at, updated_at)
              VALUES (?,?,?,?,?,?)
            |]
            (ldn, displayName, ldnSuffix, userId, ts, ts)

createWithRandomId :: forall a. TVar ChaChaDRG -> (ByteString -> IO a) -> ExceptT StoreError IO a
createWithRandomId = createWithRandomBytes 12

createWithRandomId' :: forall a. TVar ChaChaDRG -> (ByteString -> IO (Either StoreError a)) -> ExceptT StoreError IO a
createWithRandomId' = createWithRandomBytes' 12

createWithRandomBytes :: forall a. Int -> TVar ChaChaDRG -> (ByteString -> IO a) -> ExceptT StoreError IO a
createWithRandomBytes size gVar create = createWithRandomBytes' size gVar (fmap Right . create)

createWithRandomBytes' :: forall a. Int -> TVar ChaChaDRG -> (ByteString -> IO (Either StoreError a)) -> ExceptT StoreError IO a
createWithRandomBytes' size gVar create = tryCreate 3
  where
    tryCreate :: Int -> ExceptT StoreError IO a
    tryCreate 0 = throwError SEUniqueID
    tryCreate n = do
      id' <- liftIO $ encodedRandomBytes gVar size
      liftIO (E.try $ create id') >>= \case
        Right x -> liftEither x
        Left e
          | SQL.sqlError e == SQL.ErrorConstraint -> tryCreate (n - 1)
          | otherwise -> throwError . SEInternalError $ show e

encodedRandomBytes :: TVar ChaChaDRG -> Int -> IO ByteString
encodedRandomBytes gVar n = atomically $ B64.encode <$> C.randomBytes n gVar

assertNotUser :: DB.Connection -> User -> Contact -> ExceptT StoreError IO ()
assertNotUser db User {userId} Contact {contactId, localDisplayName} = do
  r :: (Maybe Int64) <-
    -- This query checks that the foreign keys in the users table
    -- are not referencing the contact about to be deleted.
    -- With the current schema it would cause cascade delete of user,
    -- with mofified schema (in v5.6.0-beta.0) it would cause foreign key violation error.
    liftIO . maybeFirstRow fromOnly $
      DB.query
        db
        [sql|
          SELECT 1 FROM users
          WHERE (user_id = ? AND local_display_name = ?)
             OR contact_id = ?
          LIMIT 1
        |]
        (userId, localDisplayName, contactId)
  when (isJust r) $ throwError $ SEProhibitedDeleteUser userId contactId

safeDeleteLDN :: DB.Connection -> User -> ContactName -> IO ()
safeDeleteLDN db User {userId} localDisplayName = do
  DB.execute
    db
    [sql|
      DELETE FROM display_names
      WHERE user_id = ? AND local_display_name = ?
        AND local_display_name NOT IN (SELECT local_display_name FROM users WHERE user_id = ?)
    |]
    (userId, localDisplayName, userId)
