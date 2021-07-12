{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Simplex.Chat.Store
  ( SQLiteStore,
    StoreError (..),
    createStore,
    createUser,
    getUsers,
    setActiveUser,
    createDirectConnection,
    createDirectContact,
    deleteContact,
    getContact,
    getContactConnections,
    getConnectionChatDirection,
    createNewGroup,
    createGroupInvitation,
    getGroup,
    createGroupMember,
  )
where

import Control.Concurrent.STM (stateTVar)
import Control.Exception (Exception)
import qualified Control.Exception as E
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Crypto.Random (ChaChaDRG, randomBytesGenerate)
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Char8 (ByteString)
import Data.Either (isRight)
import Data.FileEmbed (embedDir, makeRelativeToProject)
import Data.Function (on)
import Data.Int (Int64)
import Data.List (sortBy)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (UTCTime)
import Database.SQLite.Simple (NamedParam (..), Only (..), SQLError, (:.) (..))
import qualified Database.SQLite.Simple as DB
import Database.SQLite.Simple.QQ (sql)
import Simplex.Chat.Protocol
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Protocol (AParty (..), ConnId)
import Simplex.Messaging.Agent.Store.SQLite (SQLiteStore (..), createSQLiteStore, withTransaction)
import Simplex.Messaging.Agent.Store.SQLite.Migrations (Migration (..))
import Simplex.Messaging.Util (bshow, liftIOEither, (<$$>))
import System.FilePath (takeBaseName, takeExtension)
import UnliftIO.STM

-- | The list of migrations in ascending order by date
migrations :: [Migration]
migrations =
  sortBy (compare `on` name) . map migration . filter sqlFile $
    $(makeRelativeToProject "migrations" >>= embedDir)
  where
    sqlFile (file, _) = takeExtension file == ".sql"
    migration (file, qStr) = Migration {name = takeBaseName file, up = decodeUtf8 qStr}

createStore :: FilePath -> Int -> IO SQLiteStore
createStore dbFilePath poolSize = createSQLiteStore dbFilePath poolSize migrations

checkConstraint :: StoreError -> IO (Either StoreError a) -> IO (Either StoreError a)
checkConstraint err action = action `E.catch` (pure . Left . handleSQLError err)

handleSQLError :: StoreError -> SQLError -> StoreError
handleSQLError err e
  | DB.sqlError e == DB.ErrorConstraint = err
  | otherwise = SEInternal $ bshow e

insertedRowId :: DB.Connection -> IO Int64
insertedRowId db = fromOnly . head <$> DB.query_ db "SELECT last_insert_rowid()"

createUser :: (MonadUnliftIO m, MonadError StoreError m) => SQLiteStore -> Profile -> Bool -> m User
createUser st Profile {displayName, fullName} activeUser =
  liftIOEither . checkConstraint SEDuplicateName . withTransaction st $ \db -> do
    DB.execute db "INSERT INTO users (local_display_name, active_user, contact_id) VALUES (?, ?, 0)" (displayName, activeUser)
    userId <- insertedRowId db
    DB.execute db "INSERT INTO display_names (local_display_name, ldn_base, user_id) VALUES (?, ?, ?)" (displayName, displayName, userId)
    DB.execute db "INSERT INTO contact_profiles (display_name, full_name) VALUES (?, ?)" (displayName, fullName)
    profileId <- insertedRowId db
    DB.execute db "INSERT INTO contacts (contact_profile_id, local_display_name, user_id, is_user) VALUES (?, ?, ?, ?)" (profileId, displayName, userId, True)
    contactId <- insertedRowId db
    DB.execute db "UPDATE users SET contact_id = ? WHERE user_id = ?" (contactId, userId)
    pure . Right $ toUser (userId, contactId, activeUser, displayName, fullName)

getUsers :: SQLiteStore -> IO [User]
getUsers st =
  withTransaction st $ \db ->
    map toUser
      <$> DB.query_
        db
        [sql|
          SELECT u.user_id, u.contact_id, u.active_user, u.local_display_name, p.full_name
          FROM users u
          JOIN contacts c ON u.contact_id = c.contact_id
          JOIN contact_profiles p ON c.contact_profile_id = p.contact_profile_id
        |]

toUser :: (UserId, Int64, Bool, ContactName, Text) -> User
toUser (userId, userContactId, activeUser, displayName, fullName) =
  let profile = Profile {displayName, fullName}
   in User {userId, userContactId, localDisplayName = displayName, profile, activeUser}

setActiveUser :: MonadUnliftIO m => SQLiteStore -> UserId -> m ()
setActiveUser st userId = do
  liftIO . withTransaction st $ \db -> do
    DB.execute_ db "UPDATE users SET active_user = 0"
    DB.execute db "UPDATE users SET active_user = 1 WHERE user_id = ?" (Only userId)

createDirectConnection :: MonadUnliftIO m => SQLiteStore -> UserId -> ConnId -> m ()
createDirectConnection st userId agentConnId =
  liftIO . withTransaction st $ \db ->
    DB.execute
      db
      [sql|
        INSERT INTO connections
          (user_id, agent_conn_id, conn_status, conn_type) VALUES (?,?,?,?);
      |]
      (userId, agentConnId, ConnNew, ConnContact)

createDirectContact ::
  (MonadUnliftIO m, MonadError StoreError m) => SQLiteStore -> UserId -> Connection -> Profile -> m ()
createDirectContact st userId Connection {connId} Profile {displayName, fullName} =
  liftIOEither . withTransaction st $ \db ->
    withLocalDisplayName db userId displayName $ \localDisplayName' -> do
      DB.execute db "INSERT INTO contact_profiles (display_name, full_name) VALUES (?, ?)" (displayName, fullName)
      profileId <- insertedRowId db
      DB.execute db "INSERT INTO contacts (contact_profile_id, local_display_name, user_id) VALUES (?, ?, ?)" (profileId, localDisplayName', userId)
      contactId <- insertedRowId db
      DB.execute db "UPDATE connections SET contact_id = ? WHERE connection_id = ?" (contactId, connId)

deleteContact :: MonadUnliftIO m => SQLiteStore -> UserId -> ContactName -> m ()
deleteContact st userId displayName =
  liftIO . withTransaction st $ \db -> do
    DB.executeNamed
      db
      [sql|
        DELETE FROM connections WHERE connection_id IN (
          SELECT connection_id
          FROM connections c
          JOIN contacts cs ON c.contact_id = cs.contact_id
          WHERE cs.user_id = :user_id AND cs.local_display_name = :display_name
        )
      |]
      [":user_id" := userId, ":display_name" := displayName]
    DB.executeNamed
      db
      [sql|
        DELETE FROM contacts
        WHERE user_id = :user_id AND local_display_name = :display_name
      |]
      [":user_id" := userId, ":display_name" := displayName]
    DB.executeNamed
      db
      [sql|
        DELETE FROM display_names
        WHERE user_id = :user_id AND local_display_name = :display_name
      |]
      [":user_id" := userId, ":display_name" := displayName]

-- TODO return the last connection that is ready, not any last connection
-- requires updating connection status
getContact ::
  (MonadUnliftIO m, MonadError StoreError m) => SQLiteStore -> UserId -> ContactName -> m Contact
getContact st userId localDisplayName =
  liftIOEither . withTransaction st $ \db -> runExceptT $ do
    c@Contact {contactId} <- getContact_ db
    activeConn <- getConnection_ db contactId
    pure $ (c :: Contact) {activeConn}
  where
    getContact_ db = ExceptT $ do
      toContact
        <$> DB.queryNamed
          db
          [sql|
            SELECT c.contact_id, p.display_name, p.full_name
            FROM contacts c
            JOIN contact_profiles p ON c.contact_profile_id = p.contact_profile_id
            WHERE c.user_id = :user_id AND c.local_display_name = :local_display_name AND c.is_user = :is_user
          |]
          [":user_id" := userId, ":local_display_name" := localDisplayName, ":is_user" := False]
    getConnection_ db contactId = ExceptT $ do
      connection
        <$> DB.queryNamed
          db
          [sql|
            SELECT c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact,
              c.conn_status, c.conn_type, c.contact_id, c.group_member_id, c.created_at
            FROM connections c
            WHERE c.user_id = :user_id AND c.contact_id == :contact_id
            ORDER BY c.connection_id DESC
            LIMIT 1
          |]
          [":user_id" := userId, ":contact_id" := contactId]
    toContact [(contactId, displayName, fullName)] =
      let profile = Profile {displayName, fullName}
       in Right Contact {contactId, localDisplayName, profile, activeConn = undefined}
    toContact _ = Left $ SEContactNotFound localDisplayName
    connection (connRow : _) = Right $ toConnection connRow
    connection _ = Left $ SEContactNotReady localDisplayName

getContactConnections :: (MonadUnliftIO m, MonadError StoreError m) => SQLiteStore -> UserId -> ContactName -> m [Connection]
getContactConnections st userId displayName =
  liftIOEither . withTransaction st $ \db ->
    connections
      <$> DB.queryNamed
        db
        [sql|
          SELECT c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact,
            c.conn_status, c.conn_type, c.contact_id, c.group_member_id, c.created_at
          FROM connections c
          JOIN contacts cs ON c.contact_id == cs.contact_id
          WHERE c.user_id = :user_id
            AND cs.user_id = :user_id
            AND cs.local_display_name == :display_name
        |]
        [":user_id" := userId, ":display_name" := displayName]
  where
    connections [] = Left $ SEContactNotFound displayName
    connections rows = Right $ map toConnection rows

type ConnectionRow = (Int64, ConnId, Int, Maybe Int64, ConnStatus, ConnType, Maybe Int64, Maybe Int64, UTCTime)

toConnection :: ConnectionRow -> Connection
toConnection (connId, agentConnId, connLevel, viaContact, connStatus, connType, contactId, groupMemberId, createdAt) =
  let entityId = entityId_ connType
   in Connection {connId, agentConnId, connLevel, viaContact, connStatus, connType, entityId, createdAt}
  where
    entityId_ :: ConnType -> Maybe Int64
    entityId_ ConnContact = contactId
    entityId_ ConnMember = groupMemberId

getConnectionChatDirection ::
  (MonadUnliftIO m, MonadError StoreError m) => SQLiteStore -> UserId -> ConnId -> m (ChatDirection 'Agent)
getConnectionChatDirection st userId agentConnId =
  liftIOEither . withTransaction st $ \db -> do
    getConnection_ db >>= \case
      Left e -> pure $ Left e
      Right c@Connection {connType, entityId} -> case connType of
        ConnMember -> pure . Left $ SEInternal "group members not supported yet"
        ConnContact ->
          ReceivedDirectMessage <$$> case entityId of
            Nothing -> pure . Right $ CConnection c
            Just cId -> getContact_ db cId c
  where
    getConnection_ db =
      connection
        <$> DB.query
          db
          [sql|
            SELECT connection_id, agent_conn_id, conn_level, via_contact,
              conn_status, conn_type, contact_id, group_member_id, created_at
            FROM connections
            WHERE user_id = ? AND agent_conn_id = ?
          |]
          (userId, agentConnId)
    connection (connRow : _) = Right $ toConnection connRow
    connection _ = Left $ SEConnectionNotFound agentConnId
    getContact_ db contactId c =
      toContact contactId c
        <$> DB.query
          db
          [sql|
            SELECT c.local_display_name, p.display_name, p.full_name
            FROM contacts c
            JOIN contact_profiles p ON c.contact_profile_id = p.contact_profile_id
            WHERE c.user_id = ? AND c.contact_id = ?
          |]
          (userId, contactId)
    toContact contactId c [(localDisplayName, displayName, fullName)] =
      let profile = Profile {displayName, fullName}
       in Right $ CContact Contact {contactId, localDisplayName, profile, activeConn = c}
    toContact _ _ _ = Left $ SEInternal "referenced contact not found"

-- | creates completely new group with a single member - the current user
createNewGroup :: (MonadUnliftIO m, MonadError StoreError m) => SQLiteStore -> TVar ChaChaDRG -> User -> GroupProfile -> m Group
createNewGroup st gVar user groupProfile =
  liftIOEither . checkConstraint SEDuplicateName . withTransaction st $ \db -> do
    let GroupProfile {displayName, fullName} = groupProfile
        uId = userId user
    DB.execute db "INSERT INTO display_names (local_display_name, ldn_base, user_id) VALUES (?, ?, ?)" (displayName, displayName, uId)
    DB.execute db "INSERT INTO group_profiles (display_name, full_name) VALUES (?, ?)" (displayName, fullName)
    profileId <- insertedRowId db
    DB.execute db "INSERT INTO groups (local_display_name, user_id, group_profile_id) VALUES (?, ?, ?)" (displayName, uId, profileId)
    groupId <- insertedRowId db
    memberId <- randomId gVar 12
    membership <- createContactMember_ db user groupId user (memberId, GROwner) GSMemReady IBUser
    pure $ Right Group {groupId, localDisplayName = displayName, groupProfile, members = [], membership}

-- | creates a new group record for the group the current user was invited to
createGroupInvitation ::
  (MonadUnliftIO m, MonadError StoreError m) => SQLiteStore -> User -> Contact -> GroupInvitation -> m Group
createGroupInvitation st user contact GroupInvitation {fromMember, invitedMember, queueInfo, groupProfile} =
  liftIOEither . withTransaction st $ \db -> do
    let GroupProfile {displayName, fullName} = groupProfile
        uId = userId user
    withLocalDisplayName db uId displayName $ \localDisplayName -> do
      DB.execute db "INSERT INTO group_profiles (display_name, full_name) VALUES (?, ?)" (displayName, fullName)
      profileId <- insertedRowId db
      DB.execute db "INSERT INTO groups (group_profile_id, local_display_name, inv_queue_info, user_id) VALUES (?, ?, ?, ?)" (profileId, localDisplayName, queueInfo, uId)
      groupId <- insertedRowId db
      member <- createContactMember_ db user groupId contact fromMember GSMemReady IBUnknown
      membership <- createContactMember_ db user groupId user invitedMember GSMemInvited (IBContact $ contactId contact)
      pure Group {groupId, localDisplayName, groupProfile, members = [(member, activeConn contact)], membership}

-- TODO return the last connection that is ready, not any last connection
-- requires updating connection status
getGroup :: (MonadUnliftIO m, MonadError StoreError m) => SQLiteStore -> User -> GroupName -> m Group
getGroup st User {userId, userContactId} localDisplayName =
  liftIOEither . withTransaction st $ \db -> runExceptT $ do
    g@Group {groupId} <- getGroup_ db
    members <- getMembers_ db groupId
    membership <- getUserMember_ db groupId
    pure g {members, membership}
  where
    getGroup_ :: DB.Connection -> ExceptT StoreError IO Group
    getGroup_ db = ExceptT $ do
      toGroup
        <$> DB.query
          db
          [sql|
            SELECT g.group_id, p.display_name, p.full_name
            FROM groups g
            JOIN group_profiles p ON p.group_profile_id = g.group_profile_id
            WHERE g.local_display_name = ? AND g.user_id = ?
          |]
          (localDisplayName, userId)
    toGroup :: [(Int64, GroupName, Text)] -> Either StoreError Group
    toGroup [(groupId, displayName, fullName)] =
      let groupProfile = GroupProfile {displayName, fullName}
       in Right Group {groupId, localDisplayName, groupProfile, members = undefined, membership = undefined}
    toGroup _ = Left $ SEGroupNotFound localDisplayName
    getMembers_ :: DB.Connection -> Int64 -> ExceptT StoreError IO [(GroupMember, Connection)]
    getMembers_ db groupId = ExceptT $ do
      Right . map toContactMember
        <$> DB.query
          db
          [sql|
            SELECT
              m.group_member_id, m.member_id, m.member_role, m.member_status,
              m.invited_by, m.contact_id, p.display_name, p.full_name,
              c.connection_id, c.agent_conn_id, c.conn_level, c.via_contact,
              c.conn_status, c.conn_type, c.contact_id, c.group_member_id, c.created_at
            FROM group_members m
            JOIN groups g ON g.group_id = m.group_id
            JOIN contact_profiles p ON p.contact_profile_id = m.contact_profile_id
            JOIN connections c ON c.group_member_id = m.group_member_id
            WHERE g.group_id = ?
            ORDER BY c.connection_id DESC
            LIMIT 1
          |]
          (Only groupId)
    getUserMember_ :: DB.Connection -> Int64 -> ExceptT StoreError IO GroupMember
    getUserMember_ db groupId = ExceptT $ do
      userMember
        <$> DB.query
          db
          [sql|
            SELECT
              m.group_member_id, m.member_id, m.member_role, m.member_status,
              m.invited_by, m.contact_id, p.display_name, p.full_name
            FROM group_members m
            JOIN groups g ON g.group_id = m.group_id
            JOIN contact_profiles p ON p.contact_profile_id = m.contact_profile_id
            WHERE g.group_id = ? AND m.contact_id = ?
          |]
          (groupId, userContactId)
    toContactMember :: (GroupMemberRow :. ConnectionRow) -> (GroupMember, Connection)
    toContactMember (memberRow :. connRow) = (toGroupMember memberRow, toConnection connRow)
    toGroupMember :: GroupMemberRow -> GroupMember
    toGroupMember (groupMemberId, memberId, memberRole, memberStatus, invitedById, memberContactId, displayName, fullName) =
      let memberProfile = Profile {displayName, fullName}
          invitedBy = toInvitedBy userContactId invitedById
       in GroupMember {groupMemberId, memberId, memberRole, memberStatus, invitedBy, memberProfile, memberContactId}
    userMember :: [GroupMemberRow] -> Either StoreError GroupMember
    userMember [memberRow] = Right $ toGroupMember memberRow
    userMember _ = Left SEGroupWithoutUser

type GroupMemberRow = (Int64, ByteString, GroupMemberRole, GroupMemberStatus, Maybe Int64, Maybe Int64, ContactName, Text)

createGroupMember :: (MonadUnliftIO m, MonadError StoreError m) => SQLiteStore -> TVar ChaChaDRG -> User -> Int64 -> Contact -> GroupMemberRole -> ConnId -> m GroupMember
createGroupMember st gVar user groupId contact memberRole agentConnId =
  liftIOEither . withTransaction st $ \db -> do
    member <- createWithRandomId gVar $ \memId ->
      createContactMember_ db user groupId contact (memId, memberRole) GSMemInvited IBUser
    when (isRight member) $ insertedRowId db >>= createMemberConnection_ db
    pure member
  where
    createMemberConnection_ :: DB.Connection -> Int64 -> IO ()
    createMemberConnection_ db groupMemberId =
      DB.execute
        db
        [sql|
          INSERT INTO connections
            (user_id, agent_conn_id, conn_status, conn_type, group_member_id) VALUES (?,?,?,?,?);
        |]
        (userId user, agentConnId, ConnNew, ConnMember, groupMemberId)

createContactMember_ :: IsContact a => DB.Connection -> User -> Int64 -> a -> MemberInfo -> GroupMemberStatus -> InvitedBy -> IO GroupMember
createContactMember_ db User {userContactId} groupId userOrContact (memberId, memberRole) memberStatus invitedBy = do
  insertMember_
  groupMemberId <- insertedRowId db
  let memberProfile = profile' userOrContact
      memberContactId = Just $ contactId' userOrContact
  pure GroupMember {groupMemberId, memberId, memberRole, memberStatus, invitedBy, memberProfile, memberContactId}
  where
    insertMember_ =
      DB.executeNamed
        db
        [sql|
          INSERT INTO group_members
            ( group_id, member_id, member_role, member_status, invited_by,
              contact_profile_id, contact_id)
          VALUES
            (:group_id,:member_id,:member_role,:member_status,:invited_by,
              (SELECT contact_profile_id FROM contacts WHERE contact_id = :contact_id),
              :contact_id)
        |]
        [ ":group_id" := groupId,
          ":member_id" := memberId,
          ":member_role" := memberRole,
          ":member_status" := memberStatus,
          ":invited_by" := fromInvitedBy userContactId invitedBy,
          ":contact_id" := contactId' userOrContact
        ]

-- | Saves unique local display name based on passed displayName, suffixed with _N if required.
-- This function should be called inside transaction.
withLocalDisplayName :: forall a. DB.Connection -> UserId -> Text -> (Text -> IO a) -> IO (Either StoreError a)
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
      let ldn = displayName <> (if ldnSuffix == 0 then "" else T.pack $ '_' : show ldnSuffix)
      E.try (insertName ldn) >>= \case
        Right () -> Right <$> action ldn
        Left e
          | DB.sqlError e == DB.ErrorConstraint -> tryCreateName (ldnSuffix + 1) (attempts - 1)
          | otherwise -> E.throwIO e
      where
        insertName ldn =
          DB.execute
            db
            [sql|
              INSERT INTO display_names
                (local_display_name, ldn_base, ldn_suffix, user_id) VALUES (?, ?, ?, ?)
            |]
            (ldn, displayName, ldnSuffix, userId)

createWithRandomId :: forall a. TVar ChaChaDRG -> (ByteString -> IO a) -> IO (Either StoreError a)
createWithRandomId gVar create = tryCreate 3
  where
    tryCreate :: Int -> IO (Either StoreError a)
    tryCreate 0 = pure $ Left SEUniqueID
    tryCreate n = do
      id' <- randomId gVar 12
      E.try (create id') >>= \case
        Right x -> pure $ Right x
        Left e
          | DB.sqlError e == DB.ErrorConstraint -> tryCreate (n - 1)
          | otherwise -> pure . Left . SEInternal $ bshow e

randomId :: TVar ChaChaDRG -> Int -> IO ByteString
randomId gVar n = B64.encode <$> (atomically . stateTVar gVar $ randomBytesGenerate n)

data StoreError
  = SEDuplicateName
  | SEContactNotFound ContactName
  | SEContactNotReady ContactName
  | SEGroupNotFound GroupName
  | SEGroupWithoutUser
  | SEDuplicateGroupMember
  | SEConnectionNotFound ConnId
  | SEUniqueID
  | SEInternal ByteString
  deriving (Show, Exception)
