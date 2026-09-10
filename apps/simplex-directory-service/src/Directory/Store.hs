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
  ( GroupReg (..),
    GroupRegStatus (..),
    UserGroupRegId,
    GroupApprovalId,
    DirectoryGroupData (..),
    DirectoryMemberAcceptance (..),
    DirectoryStatus (..),
    ProfileCondition (..),
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
    getGroupAndRegLink,
    listLastGroups,
    listPendingGroups,
    getAllListedGroups,
    getAllListedGroups_,
    searchListedGroups,
    verifiedGroupDomain,
    groupRegStatusText,
    pendingApproval,
    groupRemoved,
    fromCustomData,
    toCustomData,
    noJoinFilter,
    basicJoinFilter,
    moderateJoinFilter,
    strongJoinFilter,
    newGroupJoinFilter,
    groupDBError,
  )
where

import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson.KeyMap as JM
import qualified Data.Aeson.TH as JQ
import qualified Data.Aeson.Types as JT
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Directory.Search
import Directory.Util
import Simplex.Chat.Controller
import Simplex.Chat.Names (claimDomain)
import Simplex.Chat.Options.DB (FromField (..), ToField (..))
import Simplex.Chat.Store
import Simplex.Chat.Store.Groups
import Simplex.Chat.Store.Shared (groupInfoQueryFields, groupInfoQueryFrom)
import Simplex.Chat.Types
import Simplex.Chat.Types.Shared (GroupMemberRole (..))
import Simplex.Messaging.Agent.Protocol (CreatedConnLink (..), SimplexDomain)
import Simplex.Messaging.Agent.Store.DB (BoolInt (..), fromTextField_)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON)
import Simplex.Messaging.Util (eitherToMaybe, firstRow, maybeFirstRow', safeDecodeUtf8)

#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..), Query, (:.) (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..), Query, (:.) (..))
import Database.SQLite.Simple.QQ (sql)
#endif

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

-- Default applied to newly registered groups: a captcha challenge is required
-- from every joining member unless the owner changes it with /filter.
newGroupJoinFilter :: DirectoryMemberAcceptance
newGroupJoinFilter =
  DirectoryMemberAcceptance
    { rejectNames = Nothing,
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

verifiedGroupDomain :: GroupInfo -> Maybe SimplexDomain
verifiedGroupDomain GroupInfo {groupProfile = GroupProfile {publicGroup}, groupDomainVerified}
  | groupDomainVerified == Just True = claimDomain <$> (publicGroup >>= publicGroupAccess >>= groupDomainClaim)
  | otherwise = Nothing

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

getGroupAndRegLink :: ChatController -> User -> GroupId -> IO (Either String (GroupInfo, GroupReg, Maybe GroupLink))
getGroupAndRegLink cc user@User {userId, userContactId} gId =
  withDB "getGroupAndRegLink" cc $ \db -> do
    currentTs <- liftIO getCurrentTime
    ExceptT $ firstRow (toGroupInfoRegLink currentTs (storeCxt cc) user) ("group " ++ show gId ++ " not found") $
      DB.query db (groupReqQuery <> " AND g.group_id = ?") (userId, userContactId, gId)

getUserGroupReg :: ChatController -> User -> ContactId -> UserGroupRegId -> IO (Either String (GroupInfo, GroupReg))
getUserGroupReg cc user@User {userId, userContactId} ctId ugrId =
  withDB "getUserGroupReg" cc $ \db -> do
    currentTs <- liftIO getCurrentTime
    ExceptT $ firstRow (toGroupInfoReg currentTs (storeCxt cc) user) ("group " ++ show ugrId ++ " not found") $
      DB.query db (groupReqQuery <> " AND r.contact_id = ? AND r.user_group_reg_id = ?") (userId, userContactId, ctId, ugrId)

getUserGroupRegs :: ChatController -> User -> ContactId -> IO (Either String [(GroupInfo, GroupReg)])
getUserGroupRegs cc user@User {userId, userContactId} ctId =
  withDB' "getUserGroupRegs" cc $ \db -> do
    currentTs <- getCurrentTime
    map (toGroupInfoReg currentTs (storeCxt cc) user)
      <$> DB.query db (groupReqQuery <> " AND r.contact_id = ? ORDER BY r.user_group_reg_id") (userId, userContactId, ctId)

getAllListedGroups :: ChatController -> User -> IO (Either String [(GroupInfo, GroupReg, Maybe GroupLink)])
getAllListedGroups cc user = withDB' "getAllListedGroups" cc $ \db -> getAllListedGroups_ db (storeCxt cc) user

getAllListedGroups_ :: DB.Connection -> StoreCxt -> User -> IO [(GroupInfo, GroupReg, Maybe GroupLink)]
getAllListedGroups_ db cxt user@User {userId, userContactId} = do
  currentTs <- getCurrentTime
  map (toGroupInfoRegLink currentTs cxt user)
    <$> DB.query db (groupReqQuery <> " AND r.group_reg_status = ?") (userId, userContactId, GRSActive)

searchListedGroups :: ChatController -> User -> SearchType -> Maybe SearchCursor -> Int -> IO (Either String ([(GroupInfo, GroupReg, Maybe GroupLink)], Int))
searchListedGroups cc user@User {userId, userContactId} searchType cursor_ pageSize =
  withDB' "searchListedGroups" cc $ \db -> do
    currentTs <- getCurrentTime
    case searchType of
      STAll -> case cursor_ of
        Nothing -> do
          gs <- groups currentTs $ DB.query db (listedGroupQuery <> membersOrderBy <> " LIMIT ?") (userId, userContactId, GRSActive, pageSize)
          n <- count $ DB.query db countQuery' (Only GRSActive)
          pure (gs, n)
        Just SearchCursor {lastMembers, lastGroupId} -> do
          gs <- groups currentTs $ DB.query db (listedGroupQuery <> membersCond <> membersOrderBy <> " LIMIT ?") (userId, userContactId, GRSActive, lastMembers, lastMembers, lastGroupId, pageSize)
          n <- count $ DB.query db (countQuery' <> membersCond) (GRSActive, lastMembers, lastMembers, lastGroupId)
          pure (gs, n)
        where
          countQuery' = countQuery <> " WHERE r.group_reg_status = ? "
      STRecent -> case cursor_ of
        Nothing -> do
          gs <- groups currentTs $ DB.query db (listedGroupQuery <> recentOrderBy <> " LIMIT ?") (userId, userContactId, GRSActive, pageSize)
          n <- count $ DB.query db countQuery' (Only GRSActive)
          pure (gs, n)
        Just SearchCursor {lastCreatedAt, lastGroupId} -> do
          gs <- groups currentTs $ DB.query db (listedGroupQuery <> recentCond <> recentOrderBy <> " LIMIT ?") (userId, userContactId, GRSActive, lastCreatedAt, lastCreatedAt, lastGroupId, pageSize)
          n <- count $ DB.query db (countQuery' <> recentCond) (GRSActive, lastCreatedAt, lastCreatedAt, lastGroupId)
          pure (gs, n)
        where
          countQuery' = countQuery <> " WHERE r.group_reg_status = ? "
      STSearch search -> case cursor_ of
        Nothing -> do
          gs <- groups currentTs $ DB.query db (listedGroupQuery <> searchCond <> membersOrderBy <> " LIMIT ?") ((userId, userContactId, GRSActive, s, s, s, s) :. (sDomain, pageSize))
          n <- count $ DB.query db (countQuery' <> searchCond) (GRSActive, s, s, s, s, sDomain)
          pure (gs, n)
        Just SearchCursor {lastMembers, lastGroupId} -> do
          gs <- groups currentTs $ DB.query db (listedGroupQuery <> membersCond <> searchCond <> membersOrderBy <> " LIMIT ?") ((userId, userContactId, GRSActive, lastMembers, lastMembers, lastGroupId) :. (s, s, s, s, sDomain, pageSize))
          n <- count $ DB.query db (countQuery' <> membersCond <> searchCond) ((GRSActive, lastMembers, lastMembers, lastGroupId) :. (s, s, s, s, sDomain))
          pure (gs, n)
        where
          s = T.toLower search
          -- a bare "#"/"@" maps to "#", matching no stored domain (domains are stored unprefixed)
          sDomain = case T.uncons s of
            Just (c, rest) | c == '#' || c == '@' -> if T.null rest then "#" else rest
            _ -> s
          countQuery' = countQuery <> " JOIN group_profiles gp ON gp.group_profile_id = g.group_profile_id WHERE r.group_reg_status = ? "
  where
    groups currentTs = (map (toGroupInfoRegLink currentTs (storeCxt cc) user) <$>)
    count = maybeFirstRow' 0 fromOnly
    listedGroupQuery = groupReqQuery <> " AND r.group_reg_status = ? "
    countQuery = "SELECT COUNT(1) FROM groups g JOIN sx_directory_group_regs r ON g.group_id = r.group_id "
    -- the cursor conditions must stay paired with the order by of the same sort key:
    -- paging by any other column skips and repeats rows
    membersOrderBy = " ORDER BY g.summary_current_members_count DESC, r.group_id ASC "
    membersCond = " AND (g.summary_current_members_count < ? OR (g.summary_current_members_count = ? AND r.group_id > ?)) "
    recentOrderBy = " ORDER BY r.created_at DESC, r.group_id ASC "
    recentCond = " AND (r.created_at < ? OR (r.created_at = ? AND r.group_id > ?)) "
    searchCond =
      [sql|
        AND (LOWER(gp.display_name) LIKE '%' || ? || '%'
          OR LOWER(gp.full_name) LIKE '%' || ? || '%'
          OR LOWER(gp.short_descr) LIKE '%' || ? || '%'
          OR LOWER(gp.description) LIKE '%' || ? || '%'
          OR (LOWER(gp.group_domain) LIKE '%' || ? || '%' AND g.group_domain_verified = 1)
        )
      |]

getAllGroupRegs_ :: DB.Connection -> StoreCxt -> User -> IO [(GroupInfo, GroupReg)]
getAllGroupRegs_ db cxt user@User {userId, userContactId} = do
  currentTs <- getCurrentTime
  map (toGroupInfoReg currentTs cxt user)
    <$> DB.query db groupReqQuery (userId, userContactId)

getDuplicateGroupRegs :: ChatController -> User -> Text -> IO (Either String [(GroupInfo, GroupReg)])
getDuplicateGroupRegs cc user@User {userId, userContactId} displayName =
  withDB' "getDuplicateGroupRegs" cc $ \db -> do
    currentTs <- getCurrentTime
    map (toGroupInfoReg currentTs (storeCxt cc) user)
      <$> DB.query db (groupReqQuery <> " AND gp.display_name = ?") (userId, userContactId, displayName)

listLastGroups :: ChatController -> User -> Int -> IO (Either String ([(GroupInfo, GroupReg)], Int))
listLastGroups cc user@User {userId, userContactId} count =
  withDB' "getUserGroupRegs" cc $ \db -> do
    currentTs <- getCurrentTime
    gs <-
      map (toGroupInfoReg currentTs (storeCxt cc) user)
        <$> DB.query db (groupReqQuery <> " ORDER BY group_reg_id DESC LIMIT ?") (userId, userContactId, count)
    n <- maybeFirstRow' 0 fromOnly $ DB.query_ db "SELECT COUNT(1) FROM sx_directory_group_regs"
    pure (gs, n)

listPendingGroups :: ChatController -> User -> Int -> IO (Either String ([(GroupInfo, GroupReg)], Int))
listPendingGroups cc user@User {userId, userContactId} count =
  withDB' "getUserGroupRegs" cc $ \db -> do
    currentTs <- getCurrentTime
    gs <-
      map (toGroupInfoReg currentTs (storeCxt cc) user)
        <$> DB.query db (groupReqQuery <> " AND r.group_reg_status LIKE 'pending_approval%' ORDER BY group_reg_id DESC LIMIT ?") (userId, userContactId, count)
    n <- maybeFirstRow' 0 fromOnly $ DB.query_ db "SELECT COUNT(1) FROM sx_directory_group_regs WHERE group_reg_status LIKE 'pending_approval%'"
    pure (gs, n)

toGroupInfoReg :: UTCTime -> StoreCxt -> User -> (GroupInfoRow :. GroupRegRow :. GroupLinkRow) -> (GroupInfo, GroupReg)
toGroupInfoReg currentTs cxt user row = let (g, gr, _) = toGroupInfoRegLink currentTs cxt user row in (g, gr)

toGroupInfoRegLink :: UTCTime -> StoreCxt -> User -> (GroupInfoRow :. GroupRegRow :. GroupLinkRow) -> (GroupInfo, GroupReg, Maybe GroupLink)
toGroupInfoRegLink currentTs cxt User {userContactId} (groupRow :. grRow :. linkRow) =
  (toGroupInfo currentTs cxt userContactId [] groupRow, rowToGroupReg grRow, toMaybeGroupLink linkRow)

type GroupRegRow = (GroupId, UserGroupRegId, ContactId, Maybe GroupMemberId, GroupRegStatus, BoolInt, UTCTime)

rowToGroupReg :: GroupRegRow -> GroupReg
rowToGroupReg (dbGroupId, userGroupRegId, dbContactId, dbOwnerMemberId, groupRegStatus, BI promoted, createdAt) =
  GroupReg {dbGroupId, userGroupRegId, dbContactId, dbOwnerMemberId, groupRegStatus, promoted, createdAt}

type GroupLinkRow = (Maybe Int64, Maybe ConnReqContact, Maybe ShortLinkContact, Maybe BoolInt, Maybe BoolInt, Maybe GroupLinkId, Maybe GroupMemberRole)

toMaybeGroupLink :: GroupLinkRow -> Maybe GroupLink
toMaybeGroupLink (Just userContactLinkId, Just cReq, shortLink, slDataSet, slLarge, Just groupLinkId, mRole_) =
  Just
    GroupLink
      { userContactLinkId,
        connLinkContact = CCLink cReq shortLink,
        shortLinkDataSet = boolInt slDataSet,
        shortLinkLargeDataSet = BoolDef $ boolInt slLarge,
        groupLinkId,
        acceptMemberRole = fromMaybe GRMember mRole_
      }
  where
    boolInt = maybe False (\(BI b) -> b)
toMaybeGroupLink _ = Nothing

-- group with its registration and its join link (user_contact_links) in one query
groupReqQuery :: Query
groupReqQuery = groupInfoQueryFields <> groupRegFields <> groupLinkFields <> groupInfoQueryFrom <> groupLinkJoin <> groupRegFromCond
  where
    groupRegFields = ", r.group_id, r.user_group_reg_id, r.contact_id, r.owner_member_id, r.group_reg_status, r.group_promoted, r.created_at "
    groupLinkFields = ", uc.user_contact_link_id, uc.conn_req_contact, uc.short_link_contact, uc.short_link_data_set, uc.short_link_large_data_set, uc.group_link_id, uc.group_link_member_role "
    groupLinkJoin = " LEFT JOIN user_contact_links uc ON uc.group_id = g.group_id AND uc.user_id = g.user_id "
    groupRegFromCond = " JOIN sx_directory_group_regs r ON r.group_id = g.group_id WHERE g.user_id = ? AND mu.contact_id = ? "

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
