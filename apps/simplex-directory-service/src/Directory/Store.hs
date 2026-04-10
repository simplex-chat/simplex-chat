{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Directory.Store
  ( DirectoryLog (..),
    GroupReg (..),
    GroupRegStatus (..),
    UserGroupRegId,
    GroupApprovalId,
    DirectoryGroupData (..),
    DirectoryMemberAcceptance (..),
    DirectoryStatus (..),
    ProfileCondition (..),
    DirectoryLogRecord (..),
    openDirectoryLog,
    readDirectoryLogData,
    addGroupRegStore,
    insertGroupReg,
    delGroupReg,
    deleteGroupReg,
    setGroupStatusStore,
    setGroupStatusPromoStore,
    setGroupPromotedStore,
    grDirectoryStatus,
    setGroupRegOwner,
    getUserGroupReg,
    getUserGroupRegs,
    getAllGroupRegs_,
    getDuplicateGroupRegs,
    getGroupReg,
    getGroupAndReg,
    listLastGroups,
    listPendingGroups,
    getAllListedGroups,
    getAllListedGroups_,
    searchListedGroups,
    groupRegStatusText,
    pendingApproval,
    groupRemoved,
    fromCustomData,
    toCustomData,
    noJoinFilter,
    basicJoinFilter,
    moderateJoinFilter,
    strongJoinFilter,
    groupDBError,
    logGCreate,
    logGDelete,
    logGUpdateOwner,
    logGUpdateStatus,
    logGUpdatePromotion,
  )
where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson.KeyMap as JM
import qualified Data.Aeson.TH as JQ
import qualified Data.Aeson.Types as JT
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Data.Time.Clock.System (systemEpochDay)
import Directory.Search
import Directory.Util
import Simplex.Chat.Controller
import Simplex.Chat.Protocol (supportedChatVRange)
import Simplex.Chat.Options.DB (FromField (..), ToField (..))
import Simplex.Chat.Store
import Simplex.Chat.Store.Groups
import Simplex.Chat.Store.Shared (groupInfoQueryFields, groupInfoQueryFrom)
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Store.DB (BoolInt (..), fromTextField_)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON)
import Simplex.Messaging.Util (eitherToMaybe, firstRow, maybeFirstRow', safeDecodeUtf8)
import System.IO (BufferMode (..), Handle, IOMode (..), hSetBuffering, openFile)

#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..), Query, (:.) (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..), Query, (:.) (..))
import Database.SQLite.Simple.QQ (sql)
#endif

data DirectoryLog = DirectoryLog
  { directoryLogFile :: Maybe Handle
  }

data GroupReg = GroupReg
  { dbGroupId :: GroupId,
    userGroupRegId :: UserGroupRegId,
    dbContactId :: ContactId,
    dbOwnerMemberId :: Maybe GroupMemberId,
    groupRegStatus :: GroupRegStatus,
    promoted :: Bool,
    createdAt :: UTCTime
  }

data DirectoryGroupData = DirectoryGroupData
  { memberAcceptance :: DirectoryMemberAcceptance
  }

-- these filters are applied in the order of fields, depending on ProfileCondition:
-- Nothing - do not apply
-- Just
--   PCAll - apply to all profiles
--   PCNoImage - apply to profiles without images
data DirectoryMemberAcceptance = DirectoryMemberAcceptance
  { rejectNames :: Maybe ProfileCondition, -- reject long names and names with profanity
    passCaptcha :: Maybe ProfileCondition, -- run captcha challenge with joining members
    makeObserver :: Maybe ProfileCondition -- the role assigned in the end, after captcha challenge
  }
  deriving (Eq, Show)

data ProfileCondition = PCAll | PCNoImage deriving (Eq, Show)

noJoinFilter :: DirectoryMemberAcceptance
noJoinFilter = DirectoryMemberAcceptance Nothing Nothing Nothing

basicJoinFilter :: DirectoryMemberAcceptance
basicJoinFilter =
  DirectoryMemberAcceptance
    { rejectNames = Just PCNoImage,
      passCaptcha = Nothing,
      makeObserver = Nothing
    }

moderateJoinFilter :: DirectoryMemberAcceptance
moderateJoinFilter =
  DirectoryMemberAcceptance
    { rejectNames = Just PCAll,
      passCaptcha = Just PCNoImage,
      makeObserver = Nothing
    }

strongJoinFilter :: DirectoryMemberAcceptance
strongJoinFilter =
  DirectoryMemberAcceptance
    { rejectNames = Just PCAll,
      passCaptcha = Just PCAll,
      makeObserver = Nothing
    }

type UserGroupRegId = Int64

type GroupApprovalId = Int64

data GroupRegStatus
  = GRSPendingConfirmation
  | GRSProposed
  | GRSPendingUpdate
  | GRSPendingApproval GroupApprovalId
  | GRSActive
  | GRSSuspended
  | GRSSuspendedBadRoles
  | GRSRemoved
  deriving (Eq, Show)

pendingApproval :: GroupRegStatus -> Bool
pendingApproval = \case
  GRSPendingApproval _ -> True
  _ -> False

groupRemoved :: GroupRegStatus -> Bool
groupRemoved = \case
  GRSRemoved -> True
  _ -> False

data DirectoryStatus = DSListed | DSReserved | DSRegistered | DSRemoved
  deriving (Eq)

groupRegStatusText :: GroupRegStatus -> Text
groupRegStatusText = \case
  GRSPendingConfirmation -> "pending confirmation (duplicate names)"
  GRSProposed -> "proposed"
  GRSPendingUpdate -> "pending profile update"
  GRSPendingApproval _ -> "pending admin approval"
  GRSActive -> "active"
  GRSSuspended -> "suspended by admin"
  GRSSuspendedBadRoles -> "suspended because roles changed"
  GRSRemoved -> "removed"

grDirectoryStatus :: GroupRegStatus -> DirectoryStatus
grDirectoryStatus = \case
  GRSActive -> DSListed
  GRSSuspended -> DSReserved
  GRSSuspendedBadRoles -> DSReserved
  GRSRemoved -> DSRemoved
  _ -> DSRegistered

$(JQ.deriveJSON (enumJSON $ dropPrefix "PC") ''ProfileCondition)

$(JQ.deriveJSON defaultJSON ''DirectoryMemberAcceptance)

$(JQ.deriveJSON defaultJSON ''DirectoryGroupData)

fromCustomData :: Maybe CustomData -> DirectoryGroupData
fromCustomData cd_ =
  let memberAcceptance = fromMaybe noJoinFilter $ cd_ >>= \(CustomData o) -> JT.parseMaybe (.: "memberAcceptance") o
   in DirectoryGroupData {memberAcceptance}

toCustomData :: DirectoryGroupData -> CustomData
toCustomData DirectoryGroupData {memberAcceptance} =
  CustomData $ JM.fromList ["memberAcceptance" .= memberAcceptance]

addGroupRegStore :: ChatController -> Contact -> GroupInfo -> GroupRegStatus -> IO (Either String GroupReg)
addGroupRegStore cc Contact {contactId = dbContactId} GroupInfo {groupId = dbGroupId} groupRegStatus =
  withDB' "addGroupRegStore" cc $ \db -> do
    createdAt <- getCurrentTime
    maxUgrId <-
      maybeFirstRow' 0 (fromMaybe 0 . fromOnly) $
        DB.query db "SELECT MAX(user_group_reg_id) FROM sx_directory_group_regs WHERE contact_id = ?" (Only dbContactId)
    let gr = GroupReg {dbGroupId, userGroupRegId = maxUgrId + 1, dbContactId, dbOwnerMemberId = Nothing, groupRegStatus, promoted = False, createdAt}
    insertGroupReg db gr
    pure gr

insertGroupReg :: DB.Connection -> GroupReg -> IO ()
insertGroupReg db GroupReg {dbGroupId, userGroupRegId, dbContactId, dbOwnerMemberId, groupRegStatus, promoted, createdAt} = do
  DB.execute
    db
    [sql|
      INSERT INTO sx_directory_group_regs
        (group_id, user_group_reg_id, contact_id, owner_member_id, group_reg_status, group_promoted, created_at, updated_at)
      VALUES (?,?,?,?,?,?,?,?)
    |]
    (dbGroupId, userGroupRegId, dbContactId, dbOwnerMemberId, groupRegStatus, BI promoted, createdAt, createdAt)

delGroupReg :: ChatController -> GroupId -> IO (Either String ())
delGroupReg cc gId = withDB' "delGroupReg" cc (`deleteGroupReg` gId)

deleteGroupReg :: DB.Connection -> GroupId -> IO ()
deleteGroupReg db gId = DB.execute db "DELETE FROM sx_directory_group_regs WHERE group_id = ?" (Only gId)

setGroupStatusStore :: ChatController -> GroupId -> GroupRegStatus -> IO (Either String (GroupRegStatus, GroupReg))
setGroupStatusStore cc gId grStatus' =
  withDB "setGroupStatusStore" cc $ \db -> do
    gr <- getGroupReg_ db gId
    ts <- liftIO getCurrentTime
    liftIO $ DB.execute db "UPDATE sx_directory_group_regs SET group_reg_status = ?, updated_at = ? WHERE group_id = ?" (grStatus', ts, gId)
    pure (groupRegStatus gr, gr {groupRegStatus = grStatus'})

setGroupStatusPromoStore :: ChatController -> GroupId -> GroupRegStatus -> Bool -> IO (Either String (DirectoryStatus, Bool))
setGroupStatusPromoStore cc gId grStatus' grPromoted' =
  withDB "setGroupStatusPromoStore" cc $ \db -> do
    GroupReg {groupRegStatus, promoted} <- getGroupReg_ db gId
    ts <- liftIO getCurrentTime
    liftIO $ DB.execute db "UPDATE sx_directory_group_regs SET group_reg_status = ?, group_promoted = ?, updated_at = ? WHERE group_id = ?" (grStatus', BI grPromoted', ts, gId)
    pure (grDirectoryStatus groupRegStatus, promoted)

setGroupPromotedStore :: ChatController -> GroupId -> Bool -> IO (Either String (DirectoryStatus, Bool))
setGroupPromotedStore cc gId grPromoted' =
  withDB "setGroupPromotedStore" cc $ \db -> do
    GroupReg {groupRegStatus, promoted} <- getGroupReg_ db gId
    ts <- liftIO getCurrentTime
    liftIO $ DB.execute db "UPDATE sx_directory_group_regs SET group_promoted = ?, updated_at = ? WHERE group_id = ?" (BI grPromoted', ts, gId)
    pure (grDirectoryStatus groupRegStatus, promoted)

groupDBError :: StoreError -> String
groupDBError = \case
  SEGroupNotFound _ -> "group not found"
  e -> show e

setGroupRegOwner :: ChatController -> GroupId -> GroupMember -> IO (Either String ())
setGroupRegOwner cc gId owner = do
  ts <- getCurrentTime
  withDB' "setGroupRegOwner" cc $ \db ->
    DB.execute
      db
      [sql|
        UPDATE sx_directory_group_regs
        SET owner_member_id = ?, updated_at = ?
        WHERE group_id = ?
      |]
      (groupMemberId' owner, ts, gId)

getGroupReg :: ChatController -> GroupId -> IO (Either String GroupReg)
getGroupReg cc gId = withDB "getGroupReg" cc (`getGroupReg_` gId)

getGroupReg_ :: DB.Connection -> GroupId -> ExceptT String IO GroupReg
getGroupReg_ db gId =
  ExceptT $ firstRow rowToGroupReg "group registration not found" $
    DB.query
      db
      [sql|
        SELECT group_id, user_group_reg_id, contact_id, owner_member_id, group_reg_status, group_promoted, created_at
        FROM sx_directory_group_regs
        WHERE group_id = ?
      |]
      (Only gId)

getGroupAndReg :: ChatController -> User -> GroupId -> IO (Either String (GroupInfo, GroupReg))
getGroupAndReg cc user@User {userId, userContactId} gId =
  withDB "getGroupAndReg" cc $ \db ->
    ExceptT $ firstRow (toGroupInfoReg (vr cc) user) ("group " ++ show gId ++ " not found") $
    DB.query db (groupReqQuery <> " AND g.group_id = ?") (userId, userContactId, gId)

getUserGroupReg :: ChatController -> User -> ContactId -> UserGroupRegId -> IO (Either String (GroupInfo, GroupReg))
getUserGroupReg cc user@User {userId, userContactId} ctId ugrId =
  withDB "getUserGroupReg" cc $ \db ->
    ExceptT $ firstRow (toGroupInfoReg (vr cc) user) ("group " ++ show ugrId ++ " not found") $
      DB.query db (groupReqQuery <> " AND r.contact_id = ? AND r.user_group_reg_id = ?") (userId, userContactId, ctId, ugrId)

getUserGroupRegs :: ChatController -> User -> ContactId -> IO (Either String [(GroupInfo, GroupReg)])
getUserGroupRegs cc user@User {userId, userContactId} ctId =
  withDB' "getUserGroupRegs" cc $ \db ->
    map (toGroupInfoReg (vr cc) user)
      <$> DB.query db (groupReqQuery <> " AND r.contact_id = ? ORDER BY r.user_group_reg_id") (userId, userContactId, ctId)

getAllListedGroups :: ChatController -> User -> IO (Either String [(GroupInfo, GroupReg, Maybe GroupLink)])
getAllListedGroups cc user = withDB' "getAllListedGroups" cc $ \db -> getAllListedGroups_ db (vr cc) user

getAllListedGroups_ :: DB.Connection -> VersionRangeChat -> User -> IO [(GroupInfo, GroupReg, Maybe GroupLink)]
getAllListedGroups_ db vr' user@User {userId, userContactId} =
  DB.query db (groupReqQuery <> " AND r.group_reg_status = ?") (userId, userContactId, GRSActive)
    >>= mapM (withGroupLink . toGroupInfoReg vr' user)
  where
    withGroupLink (g, gr) = (g,gr,) . eitherToMaybe <$> runExceptT (getGroupLink db user g)

searchListedGroups :: ChatController -> User -> SearchType -> Maybe GroupId -> Int -> IO (Either String ([(GroupInfo, GroupReg)], Int))
searchListedGroups cc user@User {userId, userContactId} searchType lastGroup_ pageSize =
  withDB' "searchListedGroups" cc $ \db ->
    case searchType of
      STAll -> case lastGroup_ of
        Nothing -> do
          gs <- groups $ DB.query db (listedGroupQuery <> orderBy <> " LIMIT ?") (userId, userContactId, GRSActive, pageSize)
          n <- count $ DB.query db countQuery' (Only GRSActive)
          pure (gs, n)
        Just gId -> do
          gs <- groups $ DB.query db (listedGroupQuery <> " AND r.group_id > ? " <> orderBy <> " LIMIT ?") (userId, userContactId, GRSActive, gId, pageSize)
          n <- count $ DB.query db (countQuery' <> " AND r.group_id > ?") (GRSActive, gId)
          pure (gs, n)
        where
          countQuery' = countQuery <> " WHERE r.group_reg_status = ? "
          orderBy = " ORDER BY g.summary_current_members_count DESC, r.group_reg_id ASC "
      STRecent -> case lastGroup_ of
        Nothing -> do
          gs <- groups $ DB.query db (listedGroupQuery <> orderBy <> " LIMIT ?") (userId, userContactId, GRSActive, pageSize)
          n <- count $ DB.query db countQuery' (Only GRSActive)
          pure (gs, n)
        Just gId -> do
          gs <- groups $ DB.query db (listedGroupQuery <> " AND r.group_id > ? " <> orderBy <> " LIMIT ?") (userId, userContactId, GRSActive, gId, pageSize)
          n <- count $ DB.query db (countQuery' <> " AND r.group_id > ?") (GRSActive, gId)
          pure (gs, n)
        where
          countQuery' = countQuery <> " WHERE r.group_reg_status = ? "
          orderBy = " ORDER BY r.created_at DESC, r.group_reg_id ASC "
      STSearch search -> case lastGroup_ of
        Nothing -> do
          gs <- groups $ DB.query db (listedGroupQuery <> searchCond <> orderBy <> " LIMIT ?") (userId, userContactId, GRSActive, s, s, s, s, pageSize)
          n <- count $ DB.query db (countQuery' <> searchCond) (GRSActive, s, s, s, s)
          pure (gs, n)
        Just gId -> do
          gs <- groups $ DB.query db (listedGroupQuery <> " AND r.group_id > ? " <> searchCond <> orderBy <> " LIMIT ?") (userId, userContactId, GRSActive, gId, s, s, s, s, pageSize)
          n <- count $ DB.query db (countQuery' <> " AND r.group_id > ? " <> searchCond) (GRSActive, gId, s, s, s, s)
          pure (gs, n)
        where
          s = T.toLower search
          countQuery' = countQuery <> " JOIN group_profiles gp ON gp.group_profile_id = g.group_profile_id WHERE r.group_reg_status = ? "
          orderBy = " ORDER BY g.summary_current_members_count DESC, r.group_reg_id ASC "
  where
    groups = (map (toGroupInfoReg (vr cc) user) <$>)
    count = maybeFirstRow' 0 fromOnly
    listedGroupQuery = groupReqQuery <> " AND r.group_reg_status = ? "
    countQuery = "SELECT COUNT(1) FROM groups g JOIN sx_directory_group_regs r ON g.group_id = r.group_id "
    searchCond =
      [sql|
        AND (LOWER(gp.display_name) LIKE '%' || ? || '%'
          OR LOWER(gp.full_name) LIKE '%' || ? || '%'
          OR LOWER(gp.short_descr) LIKE '%' || ? || '%'
          OR LOWER(gp.description) LIKE '%' || ? || '%'
        )
      |]

getAllGroupRegs_ :: DB.Connection -> User -> IO [(GroupInfo, GroupReg)]
getAllGroupRegs_ db user@User {userId, userContactId} =
  map (toGroupInfoReg supportedChatVRange user)
    <$> DB.query db groupReqQuery (userId, userContactId)

getDuplicateGroupRegs :: ChatController -> User -> Text -> IO (Either String [(GroupInfo, GroupReg)])
getDuplicateGroupRegs cc user@User {userId, userContactId} displayName =
  withDB' "getDuplicateGroupRegs" cc $ \db ->
    map (toGroupInfoReg (vr cc) user)
      <$> DB.query db (groupReqQuery <> " AND gp.display_name = ?") (userId, userContactId, displayName)

listLastGroups :: ChatController -> User -> Int -> IO (Either String ([(GroupInfo, GroupReg)], Int))
listLastGroups cc user@User {userId, userContactId} count =
  withDB' "getUserGroupRegs" cc $ \db -> do
    gs <-
      map (toGroupInfoReg (vr cc) user)
        <$> DB.query db (groupReqQuery <> " ORDER BY group_reg_id DESC LIMIT ?") (userId, userContactId, count)
    n <- maybeFirstRow' 0 fromOnly $ DB.query_ db "SELECT COUNT(1) FROM sx_directory_group_regs"
    pure (gs, n)

listPendingGroups :: ChatController -> User -> Int -> IO (Either String ([(GroupInfo, GroupReg)], Int))
listPendingGroups cc user@User {userId, userContactId} count =
  withDB' "getUserGroupRegs" cc $ \db -> do
    gs <-
      map (toGroupInfoReg (vr cc) user)
        <$> DB.query db (groupReqQuery <> " AND r.group_reg_status LIKE 'pending_approval%' ORDER BY group_reg_id DESC LIMIT ?") (userId, userContactId, count)
    n <- maybeFirstRow' 0 fromOnly $ DB.query_ db "SELECT COUNT(1) FROM sx_directory_group_regs WHERE group_reg_status LIKE 'pending_approval%'"
    pure (gs, n)

toGroupInfoReg :: VersionRangeChat -> User -> (GroupInfoRow :. GroupRegRow) -> (GroupInfo, GroupReg)
toGroupInfoReg vr' User {userContactId} (groupRow :. grRow) =
  (toGroupInfo vr' userContactId [] groupRow, rowToGroupReg grRow)

type GroupRegRow = (GroupId, UserGroupRegId, ContactId, Maybe GroupMemberId, GroupRegStatus, BoolInt, UTCTime)

rowToGroupReg :: GroupRegRow -> GroupReg
rowToGroupReg (dbGroupId, userGroupRegId, dbContactId, dbOwnerMemberId, groupRegStatus, BI promoted, createdAt) =
  GroupReg {dbGroupId, userGroupRegId, dbContactId, dbOwnerMemberId, groupRegStatus, promoted, createdAt}

groupReqQuery :: Query
groupReqQuery = groupInfoQueryFields <> groupRegFields <> groupInfoQueryFrom <> groupRegFromCond
  where
    groupRegFields = ", r.group_id, r.user_group_reg_id, r.contact_id, r.owner_member_id, r.group_reg_status, r.group_promoted, r.created_at "
    groupRegFromCond = " JOIN sx_directory_group_regs r ON r.group_id = g.group_id WHERE g.user_id = ? AND mu.contact_id = ? "

data DirectoryLogRecord
  = GRCreate GroupReg
  | GRDelete GroupId
  | GRUpdateStatus GroupId GroupRegStatus
  | GRUpdatePromotion GroupId Bool
  | GRUpdateOwner GroupId GroupMemberId

data DLRTag
  = GRCreate_
  | GRDelete_
  | GRUpdateStatus_
  | GRUpdatePromotion_
  | GRUpdateOwner_

logDLR :: DirectoryLog -> DirectoryLogRecord -> IO ()
logDLR st r = forM_ (directoryLogFile st) $ \h -> B.hPutStrLn h (strEncode r)

logGCreate :: DirectoryLog -> GroupReg -> IO ()
logGCreate st = logDLR st . GRCreate

logGDelete :: DirectoryLog -> GroupId -> IO ()
logGDelete st = logDLR st . GRDelete

logGUpdateStatus :: DirectoryLog -> GroupId -> GroupRegStatus -> IO ()
logGUpdateStatus st gId = logDLR st . GRUpdateStatus gId

logGUpdatePromotion :: DirectoryLog -> GroupId -> Bool -> IO ()
logGUpdatePromotion st gId = logDLR st . GRUpdatePromotion gId

logGUpdateOwner :: DirectoryLog -> GroupId -> GroupMemberId -> IO ()
logGUpdateOwner st gId = logDLR st . GRUpdateOwner gId

instance StrEncoding DLRTag where
  strEncode = \case
    GRCreate_ -> "GCREATE"
    GRDelete_ -> "GDELETE"
    GRUpdateStatus_ -> "GSTATUS"
    GRUpdatePromotion_ -> "GPROMOTE"
    GRUpdateOwner_ -> "GOWNER"
  strP =
    A.takeTill (== ' ') >>= \case
      "GCREATE" -> pure GRCreate_
      "GDELETE" -> pure GRDelete_
      "GSTATUS" -> pure GRUpdateStatus_
      "GPROMOTE" -> pure GRUpdatePromotion_
      "GOWNER" -> pure GRUpdateOwner_
      _ -> fail "invalid DLRTag"

instance StrEncoding DirectoryLogRecord where
  strEncode = \case
    GRCreate gr -> strEncode (GRCreate_, gr)
    GRDelete gId -> strEncode (GRDelete_, gId)
    GRUpdateStatus gId grStatus -> strEncode (GRUpdateStatus_, gId, grStatus)
    GRUpdatePromotion gId promoted -> strEncode (GRUpdatePromotion_, gId, promoted)
    GRUpdateOwner gId grOwnerId -> strEncode (GRUpdateOwner_, gId, grOwnerId)
  strP =
    strP_ >>= \case
      GRCreate_ -> GRCreate <$> strP
      GRDelete_ -> GRDelete <$> strP
      GRUpdateStatus_ -> GRUpdateStatus <$> A.decimal <*> _strP
      GRUpdatePromotion_ -> GRUpdatePromotion <$> A.decimal <*> _strP
      GRUpdateOwner_ -> GRUpdateOwner <$> A.decimal <* A.space <*> A.decimal

instance StrEncoding GroupReg where
  strEncode GroupReg {dbGroupId, userGroupRegId, dbContactId, dbOwnerMemberId, groupRegStatus, promoted} =
    B.unwords $
      [ "group_id=" <> strEncode dbGroupId,
        "user_group_id=" <> strEncode userGroupRegId,
        "contact_id=" <> strEncode dbContactId,
        "owner_member_id=" <> strEncode dbOwnerMemberId,
        "status=" <> strEncode groupRegStatus
      ]
        <> ["promoted=" <> strEncode promoted | promoted]
  strP = do
    dbGroupId <- "group_id=" *> strP_
    userGroupRegId <- "user_group_id=" *> strP_
    dbContactId <- "contact_id=" *> strP_
    dbOwnerMemberId <- "owner_member_id=" *> strP_
    groupRegStatus <- "status=" *> strP
    promoted <- (" promoted=" *> strP) <|> pure False
    let createdAt = UTCTime systemEpochDay 0
    pure GroupReg {dbGroupId, userGroupRegId, dbContactId, dbOwnerMemberId, groupRegStatus, promoted, createdAt}

instance StrEncoding GroupRegStatus where
  strEncode = \case
    GRSPendingConfirmation -> "pending_confirmation"
    GRSProposed -> "proposed"
    GRSPendingUpdate -> "pending_update"
    GRSPendingApproval gaId -> "pending_approval:" <> strEncode gaId
    GRSActive -> "active"
    GRSSuspended -> "suspended"
    GRSSuspendedBadRoles -> "suspended_bad_roles"
    GRSRemoved -> "removed"
  strP =
    A.takeTill (\c -> c == ' ' || c == ':') >>= \case
      "pending_confirmation" -> pure GRSPendingConfirmation
      "proposed" -> pure GRSProposed
      "pending_update" -> pure GRSPendingUpdate
      "pending_approval" -> GRSPendingApproval <$> (A.char ':' *> A.decimal)
      "active" -> pure GRSActive
      "suspended" -> pure GRSSuspended
      "suspended_bad_roles" -> pure GRSSuspendedBadRoles
      "removed" -> pure GRSRemoved
      _ -> fail "invalid GroupRegStatus"

instance ToField GroupRegStatus where toField = toField . safeDecodeUtf8 . strEncode

instance FromField GroupRegStatus where fromField = fromTextField_ $ eitherToMaybe . strDecode . encodeUtf8

openDirectoryLog :: Maybe FilePath -> IO DirectoryLog
openDirectoryLog = \case
  Just f -> DirectoryLog . Just <$> openLogFile f
  Nothing -> pure $ DirectoryLog Nothing
  where
    openLogFile f = do
      h <- openFile f AppendMode
      hSetBuffering h LineBuffering
      pure h

readDirectoryLogData :: FilePath -> IO [GroupReg]
readDirectoryLogData f =
  sortOn dbGroupId . M.elems
    <$> (foldM processDLR M.empty . B.lines =<< B.readFile f)
  where
    processDLR :: Map GroupId GroupReg -> ByteString -> IO (Map GroupId GroupReg)
    processDLR m l = case strDecode l of
      Left e -> m <$ putStrLn ("Error parsing log record: " <> e <> ", " <> B.unpack (B.take 80 l))
      Right r -> case r of
        GRCreate gr@GroupReg {dbGroupId = gId} -> do
          when (isJust $ M.lookup gId m) $
            putStrLn $
              "Warning: duplicate group with ID " <> show gId <> ", group replaced."
          pure $ M.insert gId gr m
        GRDelete gId -> case M.lookup gId m of
          Just _ -> pure $ M.delete gId m
          Nothing -> m <$ putStrLn ("Warning: no group with ID " <> show gId <> ", deletion ignored.")
        GRUpdateStatus gId groupRegStatus -> case M.lookup gId m of
          Just gr -> pure $ M.insert gId gr {groupRegStatus} m
          Nothing -> m <$ putStrLn ("Warning: no group with ID " <> show gId <> ", status update ignored.")
        GRUpdatePromotion gId promoted -> case M.lookup gId m of
          Just gr -> pure $ M.insert gId gr {promoted} m
          Nothing -> m <$ putStrLn ("Warning: no group with ID " <> show gId <> ", promotion update ignored.")
        GRUpdateOwner gId grOwnerId -> case M.lookup gId m of
          Just gr -> pure $ M.insert gId gr {dbOwnerMemberId = Just grOwnerId} m
          Nothing -> m <$ putStrLn ("Warning: no group with ID " <> show gId <> ", owner update ignored.")
