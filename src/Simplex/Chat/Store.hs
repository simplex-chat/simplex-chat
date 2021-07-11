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
    createGroup,
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
import Data.FileEmbed (embedDir, makeRelativeToProject)
import Data.Function (on)
import Data.Int (Int64)
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as L
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (UTCTime)
import Database.SQLite.Simple (NamedParam (..), Only (..), SQLError)
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
insertedRowId db = fromOnly . head <$> DB.query_ db "SELECT last_insert_rowid();"

createUser :: (MonadUnliftIO m, MonadError StoreError m) => SQLiteStore -> Profile -> Bool -> m User
createUser st Profile {contactRef, displayName} activeUser =
  liftIOEither . checkConstraint SEDuplicateContactRef . withTransaction st $ \db -> do
    DB.execute db "INSERT INTO contact_profiles (contact_ref, display_name) VALUES (?, ?);" (contactRef, displayName)
    profileId <- insertedRowId db
    DB.execute db "INSERT INTO users (contact_id, active_user) VALUES (0, ?);" (Only activeUser)
    userId <- insertedRowId db
    DB.execute
      db
      "INSERT INTO contacts (contact_profile_id, local_contact_ref, lcr_base, user_id, user) VALUES (?, ?, ?, ?, 1);"
      (profileId, contactRef, contactRef, userId)
    contactId <- insertedRowId db
    DB.execute db "UPDATE users SET contact_id = ? WHERE user_id = ?;" (contactId, userId)
    pure . Right $ toUser (userId, contactId, activeUser, contactRef, displayName)

getUsers :: SQLiteStore -> IO [User]
getUsers st =
  withTransaction st $ \db ->
    map toUser
      <$> DB.query_
        db
        [sql|
          SELECT u.user_id, u.contact_id, u.active_user, c.local_contact_ref, p.display_name
          FROM users u
          JOIN contacts c ON u.contact_id = c.contact_id
          JOIN contact_profiles p ON c.contact_profile_id = p.contact_profile_id
        |]

toUser :: (UserId, Int64, Bool, ContactRef, Text) -> User
toUser (userId, userContactId, activeUser, contactRef, displayName) =
  let profile = Profile {contactRef, displayName}
   in User {userId, userContactId, localContactRef = contactRef, profile, activeUser}

setActiveUser :: MonadUnliftIO m => SQLiteStore -> UserId -> m ()
setActiveUser st userId = do
  liftIO . withTransaction st $ \db -> do
    DB.execute_ db "UPDATE users SET active_user = 0;"
    DB.execute db "UPDATE users SET active_user = 1 WHERE user_id = ?;" (Only userId)

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
createDirectContact st userId Connection {connId} Profile {contactRef, displayName} =
  liftIOEither . withTransaction st $ \db -> do
    DB.execute db "INSERT INTO contact_profiles (contact_ref, display_name) VALUES (?, ?);" (contactRef, displayName)
    profileId <- insertedRowId db
    lcrSuffix <- getLcrSuffix db
    create db profileId lcrSuffix 20
  where
    getLcrSuffix :: DB.Connection -> IO Int
    getLcrSuffix db =
      maybe 0 ((+ 1) . fromOnly) . listToMaybe
        <$> DB.queryNamed
          db
          [sql|
            SELECT lcr_suffix FROM contacts
            WHERE user_id = :user_id AND lcr_base = :contact_ref
            ORDER BY lcr_suffix DESC
            LIMIT 1;
          |]
          [":user_id" := userId, ":contact_ref" := contactRef]
    create :: DB.Connection -> Int64 -> Int -> Int -> IO (Either StoreError ())
    create _ _ _ 0 = pure $ Left SEDuplicateContactRef
    create db profileId lcrSuffix attempts = do
      let lcr = localContactRef' lcrSuffix
      E.try (insertUser lcr) >>= \case
        Right () -> do
          contactId <- insertedRowId db
          DB.execute db "UPDATE connections SET contact_id = ? WHERE connection_id = ?" (contactId, connId)
          pure $ Right ()
        Left e
          | DB.sqlError e == DB.ErrorConstraint -> create db profileId (lcrSuffix + 1) (attempts - 1)
          | otherwise -> E.throwIO e
      where
        localContactRef' 0 = contactRef
        localContactRef' n = contactRef <> T.pack ('_' : show n)
        insertUser lcr =
          DB.execute
            db
            [sql|
              INSERT INTO contacts
                (contact_profile_id, local_contact_ref, lcr_base, lcr_suffix, user_id) VALUES (?, ?, ?, ?, ?)
            |]
            (profileId, lcr, contactRef, lcrSuffix, userId)

deleteContact :: MonadUnliftIO m => SQLiteStore -> UserId -> ContactRef -> m ()
deleteContact st userId contactRef =
  liftIO . withTransaction st $ \db -> do
    DB.executeNamed
      db
      [sql|
        DELETE FROM connections WHERE connection_id IN (
          SELECT connection_id
          FROM connections c
          JOIN contacts cs ON c.contact_id = cs.contact_id
          WHERE cs.user_id = :user_id AND cs.local_contact_ref = :contact_ref
        );
      |]
      [":user_id" := userId, ":contact_ref" := contactRef]
    DB.executeNamed
      db
      [sql|
        DELETE FROM contacts
        WHERE user_id = :user_id AND local_contact_ref = :contact_ref;
      |]
      [":user_id" := userId, ":contact_ref" := contactRef]

-- TODO return the last connection that is ready, not any last connection
-- requires updating connection status
getContact ::
  (MonadUnliftIO m, MonadError StoreError m) => SQLiteStore -> UserId -> ContactRef -> m Contact
getContact st userId localContactRef =
  liftIOEither . withTransaction st $ \db -> runExceptT $ do
    c@Contact {contactId} <- getContact_ db
    activeConn <- getConnection_ db contactId
    pure $ c {activeConn}
  where
    getContact_ db = ExceptT $ do
      toContact
        <$> DB.query
          db
          [sql|
            SELECT c.contact_id, p.contact_ref, p.display_name
            FROM contacts c
            JOIN contact_profiles p ON c.contact_profile_id = p.contact_profile_id
            WHERE c.user_id = ? AND c.local_contact_ref = ? AND c.user IS NULL;
          |]
          (userId, localContactRef)
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
            LIMIT 1;
          |]
          [":user_id" := userId, ":contact_id" := contactId]
    toContact [(contactId, contactRef, displayName)] =
      let profile = Profile {contactRef, displayName}
       in Right Contact {contactId, localContactRef, profile, activeConn = undefined}
    toContact _ = Left $ SEContactNotFound localContactRef
    connection (connRow : _) = Right $ toConnection connRow
    connection _ = Left $ SEContactNotReady localContactRef

getContactConnections :: (MonadUnliftIO m, MonadError StoreError m) => SQLiteStore -> UserId -> ContactRef -> m [Connection]
getContactConnections st userId contactRef =
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
            AND cs.local_contact_ref == :contact_ref;
        |]
        [":user_id" := userId, ":contact_ref" := contactRef]
  where
    connections [] = Left $ SEContactNotFound contactRef
    connections rows = Right $ map toConnection rows

toConnection ::
  (Int64, ConnId, Int, Maybe Int64, ConnStatus, ConnType, Maybe Int64, Maybe Int64, UTCTime) -> Connection
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
            WHERE user_id = ? AND agent_conn_id = ?;
          |]
          (userId, agentConnId)
    connection (connRow : _) = Right $ toConnection connRow
    connection _ = Left $ SEConnectionNotFound agentConnId
    getContact_ db contactId c =
      toContact contactId c
        <$> DB.query
          db
          [sql|
            SELECT c.local_contact_ref, p.contact_ref, p.display_name
            FROM contacts c
            JOIN contact_profiles p ON c.contact_profile_id = p.contact_profile_id
            WHERE c.user_id = ? AND c.contact_id = ?
          |]
          (userId, contactId)
    toContact contactId c [(localContactRef, contactRef, displayName)] =
      let profile = Profile {contactRef, displayName}
       in Right $ CContact Contact {contactId, localContactRef, profile, activeConn = c}
    toContact _ _ _ = Left $ SEInternal "referenced contact not found"

createGroup :: (MonadUnliftIO m, MonadError StoreError m) => SQLiteStore -> TVar ChaChaDRG -> User -> GroupProfile -> m Group
createGroup st gVar User {userId, userContactId, profile} p@GroupProfile {groupRef, displayName} =
  liftIOEither . checkConstraint SEDuplicateGroupRef . withTransaction st $ \db -> do
    DB.execute db "INSERT INTO groups (local_group_ref, lgr_base, user_id) VALUES (?, ?, ?);" (groupRef, groupRef, userId)
    groupId <- insertedRowId db
    DB.execute db "INSERT INTO group_profiles (group_ref, display_name) VALUES (?, ?);" (groupRef, displayName)
    profileId <- insertedRowId db
    DB.execute db "UPDATE groups SET group_profile_id = ? WHERE group_id = ?;" (profileId, groupId)
    memberId <- randomId gVar 12
    createMember_ db groupId userContactId GROwner GSMemReady (Just userContactId) memberId
    groupMemberId <- insertedRowId db
    let membership =
          GroupMember
            { groupMemberId,
              memberId,
              memberRole = GROwner,
              memberStatus = GSMemReady,
              invitedBy = IBUser,
              memberProfile = profile,
              memberContactId = Just userContactId
            }
    pure $ Right Group {groupId, localGroupRef = groupRef, groupProfile = p, members = [], membership}

getGroup :: (MonadUnliftIO m, MonadError StoreError m) => SQLiteStore -> User -> GroupRef -> m Group
getGroup st User {userId, userContactId} localGroupRef =
  liftIOEither . withTransaction st $ \db -> runExceptT $ do
    g@Group {groupId} <- getGroup_ db
    allMembers <- getMembers_ db groupId
    (members, membership) <- liftEither $ separateUserMember allMembers
    pure g {members, membership}
  where
    getGroup_ :: DB.Connection -> ExceptT StoreError IO Group
    getGroup_ db = ExceptT $ do
      toGroup
        <$> DB.query
          db
          [sql|
              SELECT g.group_id, p.group_ref, p.display_name
              FROM groups g
              JOIN group_profiles p ON p.group_profile_id = g.group_profile_id
              WHERE g.local_group_ref = ? AND g.user_id = ?;
            |]
          (localGroupRef, userId)
    toGroup :: [(Int64, GroupRef, Text)] -> Either StoreError Group
    toGroup [(groupId, groupRef, displayName)] =
      let groupProfile = GroupProfile {groupRef, displayName}
       in Right Group {groupId, localGroupRef, groupProfile, members = undefined, membership = undefined}
    toGroup _ = Left $ SEGroupNotFound localGroupRef
    getMembers_ :: DB.Connection -> Int64 -> ExceptT StoreError IO (NonEmpty GroupMember)
    getMembers_ db groupId = ExceptT $ do
      maybe (Left SEGroupEmpty) Right . L.nonEmpty . map toGroupMember
        <$> DB.query
          db
          [sql|
            SELECT
              m.group_member_id, m.member_id, m.member_role, m.member_status,
              m.invited_by, m.contact_id, p.contact_ref, p.display_name
            FROM group_members m
            JOIN groups g ON g.group_id = m.group_id
            JOIN contact_profiles p ON p.contact_profile_id = m.contact_profile_id
            WHERE g.group_id = ?;
          |]
          (Only groupId)
    toGroupMember :: (Int64, ByteString, GroupMemberRole, GroupMemberStatus, Maybe Int64, Maybe Int64, ContactRef, Text) -> GroupMember
    toGroupMember (groupMemberId, memberId, memberRole, memberStatus, invitedById, memberContactId, contactRef, displayName) =
      let memberProfile = Profile {contactRef, displayName}
          invitedBy = maybe IBUnknown (\i -> if i == userContactId then IBUser else IBContact i) invitedById
       in GroupMember {groupMemberId, memberId, memberRole, memberStatus, invitedBy, memberProfile, memberContactId}
    separateUserMember :: NonEmpty GroupMember -> Either StoreError ([GroupMember], GroupMember)
    separateUserMember ms = case L.break ((== Just userContactId) . memberContactId) ms of
      (b, u : a) -> Right (b <> a, u)
      _ -> Left SEGroupWithoutUser

createGroupMember :: (MonadUnliftIO m, MonadError StoreError m) => SQLiteStore -> TVar ChaChaDRG -> UserId -> Int64 -> Int64 -> GroupMemberRole -> InvitedBy -> m ByteString
createGroupMember st gVar userId groupId contactId memberRole invitedBy =
  liftIOEither . withTransaction st $ \db ->
    let invitedById = case invitedBy of
          IBUnknown -> Nothing
          IBContact i -> Just i
          IBUser -> Just userId
     in createWithRandomId gVar $ createMember_ db groupId contactId memberRole GSMemInvited invitedById

createMember_ :: DB.Connection -> Int64 -> Int64 -> GroupMemberRole -> GroupMemberStatus -> Maybe Int64 -> ByteString -> IO ()
createMember_ db groupId contactId memberRole memberStatus invitedBy memberId =
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
      ":invited_by" := invitedBy,
      ":contact_id" := contactId
    ]

createWithRandomId :: TVar ChaChaDRG -> (ByteString -> IO ()) -> IO (Either StoreError ByteString)
createWithRandomId gVar create = tryCreate 3
  where
    tryCreate :: Int -> IO (Either StoreError ByteString)
    tryCreate 0 = pure $ Left SEUniqueID
    tryCreate n = do
      id' <- randomId gVar 12
      E.try (create id') >>= \case
        Right _ -> pure $ Right id'
        Left e
          | DB.sqlError e == DB.ErrorConstraint -> tryCreate (n - 1)
          | otherwise -> pure . Left . SEInternal $ bshow e

randomId :: TVar ChaChaDRG -> Int -> IO ByteString
randomId gVar n = B64.encode <$> (atomically . stateTVar gVar $ randomBytesGenerate n)

data StoreError
  = SEDuplicateContactRef
  | SEContactNotFound ContactRef
  | SEContactNotReady ContactRef
  | SEDuplicateGroupRef
  | SEGroupNotFound GroupRef
  | SEGroupEmpty
  | SEGroupWithoutUser
  | SEDuplicateGroupMember
  | SEConnectionNotFound ConnId
  | SEUniqueID
  | SEInternal ByteString
  deriving (Show, Exception)
