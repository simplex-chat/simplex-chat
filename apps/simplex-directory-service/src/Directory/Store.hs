{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Directory.Store
  ( DirectoryStore (..),
    LegacyGroupReg (..),
    GroupRegData (..),
    GroupRegStatus (..),
    UserGroupRegId,
    GroupApprovalId,
    GroupReg (..),
    LegacyDirectoryGroupData (..),
    DirectoryMemberAcceptance (..),
    DirectoryStatus (..),
    ProfileCondition (..),
    DirectoryLogRecord (..),
    restoreDirectoryStore,
    readDirectoryData,
    addGroupReg,
    insertGroupReg,
    delGroupReg,
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
    filterListedGroups',
    groupRegStatusText,
    pendingApproval,
    groupRemoved,
    legacyFromCustomData,
    legacyToCustomData,
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
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.KeyMap as JM
import qualified Data.Aeson.TH as JQ
import qualified Data.Aeson.Types as JT
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Bitraversable (bimapM)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Data.Time.Clock.System (systemEpochDay)
import Directory.Util
import Simplex.Chat.Controller
import Simplex.Chat.Protocol (supportedChatVRange)
import Simplex.Chat.Options.DB (FromField (..), ToField (..))
import Simplex.Chat.Store
import Simplex.Chat.Store.Groups
import Simplex.Chat.Store.Shared (groupInfoQueryFields, groupInfoQueryFrom)
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Store.DB (Binary (..), BoolInt (..), blobFieldDecoder, fromTextField_)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, taggedObjectJSON)
import Simplex.Messaging.Util (firstRow, ifM, maybeFirstRow')
import System.Directory
import System.IO (BufferMode (..), Handle, IOMode (..), hSetBuffering, openFile)

#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..), Query, (:.) (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..), Query, (:.) (..))
import Database.SQLite.Simple.QQ (sql)
#endif

data DirectoryStore = DirectoryStore
  { listedGroups :: TVar (Set GroupId), -- includes promoted
    promotedGroups :: TVar (Set GroupId),
    reservedGroups :: TVar (Set GroupId),
    directoryLogFile :: Maybe Handle
  }

data DirectoryStoreData = DirectoryStoreData
  { groupRegs_ :: [LegacyGroupReg],
    listedGroups_ :: Set GroupId,
    promotedGroups_ :: Set GroupId,
    reservedGroups_ :: Set GroupId
  }

data LegacyGroupReg = LegacyGroupReg
  { dbGroupId' :: GroupId,
    userGroupRegId' :: UserGroupRegId,
    dbContactId' :: ContactId,
    dbOwnerMemberId' :: TVar (Maybe GroupMemberId),
    groupRegStatus' :: TVar GroupRegStatus,
    promoted' :: TVar Bool
  }

data GroupRegData = GroupRegData
  { dbGroupId_ :: GroupId,
    userGroupRegId_ :: UserGroupRegId,
    dbContactId_ :: ContactId,
    dbOwnerMemberId_ :: Maybe GroupMemberId,
    groupRegStatus_ :: GroupRegStatus,
    promoted_ :: Bool
  }

data GroupReg = GroupReg
  { dbGroupId :: GroupId,
    userGroupRegId :: UserGroupRegId,
    dbContactId :: ContactId,
    dbOwnerMemberId :: Maybe GroupMemberId,
    groupRegStatus :: GroupRegStatus,
    promoted :: Bool,
    memberAcceptance :: DirectoryMemberAcceptance,
    createdAt :: UTCTime
  }

data LegacyDirectoryGroupData = LegacyDirectoryGroupData
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

instance TextEncoding ProfileCondition where
  textEncode = \case
    PCAll -> "all"
    PCNoImage -> "noImage"
  textDecode = \case
    "all" -> Just PCAll
    "noImage" -> Just PCNoImage
    _ -> Nothing

instance ToField ProfileCondition where toField = toField . textEncode

instance FromField ProfileCondition where fromField = fromTextField_ textDecode

instance ToJSON ProfileCondition where
  toJSON = J.String . textEncode
  toEncoding = JE.text . textEncode

instance FromJSON ProfileCondition where
  parseJSON = J.withText "ProfileCondition" $ maybe (fail "bad ProfileCondition") pure . textDecode

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

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "GRS") ''GroupRegStatus)

$(JQ.deriveJSON defaultJSON ''DirectoryMemberAcceptance)

$(JQ.deriveJSON defaultJSON ''LegacyDirectoryGroupData)

legacyFromCustomData :: Maybe CustomData -> LegacyDirectoryGroupData
legacyFromCustomData cd_ =
  let memberAcceptance = fromMaybe noJoinFilter $ cd_ >>= \(CustomData o) -> JT.parseMaybe (.: "memberAcceptance") o
   in LegacyDirectoryGroupData {memberAcceptance}

legacyToCustomData :: LegacyDirectoryGroupData -> CustomData
legacyToCustomData LegacyDirectoryGroupData {memberAcceptance} =
  CustomData $ JM.fromList ["memberAcceptance" .= memberAcceptance]

addGroupReg :: ChatController -> Contact -> GroupInfo -> GroupRegStatus -> IO (Either String GroupReg)
addGroupReg cc Contact {contactId = dbContactId} GroupInfo {groupId = dbGroupId} groupRegStatus =
  withDB' "addGroupReg" cc $ \db -> do
    createdAt <- getCurrentTime
    maxUgrId <-
      maybeFirstRow' 0 fromOnly $
        DB.query db "SELECT MAX(user_group_reg_id) FROM sx_directory_group_regs WHERE contact_id = ?" (Only dbContactId)
    let memberAcceptance = DirectoryMemberAcceptance Nothing Nothing Nothing
        gr = GroupReg {dbGroupId, userGroupRegId = maxUgrId + 1, dbContactId, dbOwnerMemberId = Nothing, groupRegStatus, promoted = False, memberAcceptance, createdAt}
    insertGroupReg db gr
    pure gr

insertGroupReg :: DB.Connection -> GroupReg -> IO ()
insertGroupReg db GroupReg {dbGroupId, userGroupRegId, dbContactId, dbOwnerMemberId, groupRegStatus, promoted, memberAcceptance, createdAt} = do
  let DirectoryMemberAcceptance {rejectNames, passCaptcha, makeObserver} = memberAcceptance
  DB.execute
    db
    [sql|
      INSERT INTO sx_directory_group_regs
        ( group_id, user_group_reg_id, contact_id, owner_member_id, group_reg_status, group_promoted,
          acceptance_reject_names, acceptance_pass_captcha, acceptance_make_observer, created_at, created_at
        )
      VALUES (?,?,?,?,?,?, ?,?,?,?,?)
    |]
    ( (dbGroupId, userGroupRegId, dbContactId, dbOwnerMemberId, groupRegStatus, promoted)
        :. (rejectNames, passCaptcha, makeObserver, createdAt, createdAt)
    )

delGroupReg :: ChatController -> GroupId -> IO (Either String ())
delGroupReg cc gId =
  withDB' "delGroupReg" cc $ \db ->
    DB.execute db "DELETE FROM sx_directory_group_regs WHERE group_id = ?" (Only gId)

setGroupStatusStore :: ChatController -> GroupId -> GroupRegStatus -> IO (Either String GroupReg)
setGroupStatusStore cc gId grStatus' =
  withDB "setGroupStatusStore" cc $ \db -> do
    gr <- getGroupReg_ db gId
    ts <- liftIO getCurrentTime
    liftIO $ DB.execute db "UPDATE sx_directory_group_regs SET group_reg_status = ?, updated_at = ? WHERE group_id = ?" (grStatus', ts, gId)
    pure gr {groupRegStatus = grStatus'}

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
        SELECT group_id, user_group_reg_id, contact_id, owner_member_id, group_reg_status, group_promoted,
          acceptance_reject_names, acceptance_pass_captcha, acceptance_make_observer, created_at
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
      <$> DB.query db (groupReqQuery <> " AND r.contact_id = ?") (userId, userContactId, ctId)

getAllListedGroups :: ChatController -> User -> IO (Either String [(GroupInfoSummary, GroupReg)])
getAllListedGroups cc user@User {userId, userContactId} =
  withDB' "getAllListedGroups" cc $ \db ->
    DB.query db (groupReqQuery <> " AND r.group_reg_status = ?") (userId, userContactId, GRSActive)
      >>= mapM (bimapM (getGroupInfoSummary db user) pure . toGroupInfoReg (vr cc) user)

getAllGroupRegs_ :: DB.Connection -> User -> IO [(GroupInfo, GroupReg)]
getAllGroupRegs_ db user@User {userId, userContactId} =
  map (toGroupInfoReg supportedChatVRange user)
    <$> DB.query db groupReqQuery (userId, userContactId)

getDuplicateGroupRegs :: ChatController -> User -> Text -> IO (Either String [(GroupInfo, GroupReg)])
getDuplicateGroupRegs cc user@User {userId, userContactId} displayName =
  withDB' "getDuplicateGroupRegs" cc $ \db ->
    map (toGroupInfoReg (vr cc) user)
      <$> DB.query db (groupReqQuery <> " AND gp.display_name = ?") (userId, userContactId, displayName)

filterListedGroups' :: DirectoryStore -> [GroupInfoSummary] -> IO [GroupInfoSummary]
filterListedGroups' st gs = do
  lgs <- readTVarIO $ listedGroups st
  pure $ filter (\(GIS GroupInfo {groupId} _ _) -> groupId `S.member` lgs) gs

listLastGroups :: ChatController -> User -> Int -> IO (Either String [(GroupInfo, GroupReg)])
listLastGroups cc user@User {userId, userContactId} count =
  withDB' "getUserGroupRegs" cc $ \db ->
    map (toGroupInfoReg (vr cc) user)
      <$> DB.query db (groupReqQuery <> " ORDER BY group_reg_id LIMIT ?") (userId, userContactId, count)

listPendingGroups :: ChatController -> User -> Int -> IO (Either String [(GroupInfo, GroupReg)])
listPendingGroups cc user@User {userId, userContactId} count =
  withDB' "getUserGroupRegs" cc $ \db ->
    map (toGroupInfoReg (vr cc) user)
      <$> DB.query db (groupReqQuery <> " r.group_reg_status LIKE 'pending_approval%' ORDER BY group_reg_id LIMIT ?") (userId, userContactId, count)

toGroupInfoReg :: VersionRangeChat -> User -> (GroupInfoRow :. GroupRegRow) -> (GroupInfo, GroupReg)
toGroupInfoReg vr' User {userContactId} (groupRow :. grRow) =
  (toGroupInfo vr' userContactId [] groupRow, rowToGroupReg grRow)

type GroupRegRow = (GroupId, UserGroupRegId, ContactId, Maybe GroupMemberId, GroupRegStatus, BoolInt, Maybe ProfileCondition, Maybe ProfileCondition, Maybe ProfileCondition, UTCTime)

rowToGroupReg :: GroupRegRow -> GroupReg
rowToGroupReg (dbGroupId, userGroupRegId, dbContactId, dbOwnerMemberId, groupRegStatus, BI promoted, rejectNames, passCaptcha, makeObserver, createdAt) =
  let memberAcceptance = DirectoryMemberAcceptance {rejectNames, passCaptcha, makeObserver}
   in GroupReg {dbGroupId, userGroupRegId, dbContactId, dbOwnerMemberId, groupRegStatus, promoted, memberAcceptance, createdAt}

groupReqQuery :: Query
groupReqQuery = groupInfoQueryFields <> groupRegFields <> groupInfoQueryFrom <> groupRegFromCond
  where
    groupRegFields =
      [sql|
        r.group_id, r.user_group_reg_id, r.contact_id, r.owner_member_id, r.group_reg_status, r.group_promoted,
        r.acceptance_reject_names, r.acceptance_pass_captcha, r.acceptance_make_observer, r.created_at
      |]
    groupRegFromCond =
      [sql|
        JOIN sx_directory_group_regs r ON r.group_id = g.group_id
        WHERE g.user_id = ? AND mu.contact_id = ?
      |]

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

logDLR :: DirectoryStore -> DirectoryLogRecord -> IO ()
logDLR st r = forM_ (directoryLogFile st) $ \h -> B.hPutStrLn h (strEncode r)

logGCreate :: DirectoryStore -> GroupReg -> IO ()
logGCreate st = logDLR st . GRCreate

logGDelete :: DirectoryStore -> GroupId -> IO ()
logGDelete st = logDLR st . GRDelete

logGUpdateStatus :: DirectoryStore -> GroupId -> GroupRegStatus -> IO ()
logGUpdateStatus st gId = logDLR st . GRUpdateStatus gId

logGUpdatePromotion :: DirectoryStore -> GroupId -> Bool -> IO ()
logGUpdatePromotion st gId = logDLR st . GRUpdatePromotion gId

logGUpdateOwner :: DirectoryStore -> GroupId -> GroupMemberId -> IO ()
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
    let memberAcceptance = DirectoryMemberAcceptance Nothing Nothing Nothing
        createdAt = UTCTime systemEpochDay 0
    pure GroupReg {dbGroupId, userGroupRegId, dbContactId, dbOwnerMemberId, groupRegStatus, promoted, memberAcceptance, createdAt}

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

instance ToField GroupRegStatus where toField = toField . Binary . strEncode

instance FromField GroupRegStatus where fromField = blobFieldDecoder strDecode

toLegacyGroupReg :: GroupReg -> IO LegacyGroupReg
toLegacyGroupReg GroupReg {dbGroupId, userGroupRegId, dbContactId, dbOwnerMemberId, groupRegStatus, promoted} = do
  dbOwnerMemberId' <- newTVarIO dbOwnerMemberId
  groupRegStatus' <- newTVarIO groupRegStatus
  promoted' <- newTVarIO promoted
  pure
    LegacyGroupReg
      { dbGroupId' = dbGroupId,
        userGroupRegId' = userGroupRegId,
        dbContactId' = dbContactId,
        dbOwnerMemberId',
        groupRegStatus',
        promoted'
      }

restoreDirectoryStore :: Maybe FilePath -> IO DirectoryStore
restoreDirectoryStore = \case
  Just f -> ifM (doesFileExist f) (restore f) (newFile f >>= newDirectoryStore . Just)
  Nothing -> newDirectoryStore Nothing
  where
    newFile f = do
      h <- openFile f WriteMode
      hSetBuffering h LineBuffering
      pure h
    restore f = do
      grs <- readDirectoryData f
      renameFile f (f <> ".bak")
      h <- writeDirectoryData f grs -- compact
      mkDirectoryStore h grs

emptyStoreData :: DirectoryStoreData
emptyStoreData = DirectoryStoreData [] S.empty S.empty S.empty

newDirectoryStore :: Maybe Handle -> IO DirectoryStore
newDirectoryStore = (`mkDirectoryStore_` emptyStoreData)

mkDirectoryStore :: Handle -> [GroupReg] -> IO DirectoryStore
mkDirectoryStore h groups =
  foldM addGroupRegData emptyStoreData groups >>= mkDirectoryStore_ (Just h)
  where
    addGroupRegData d gr@GroupReg {dbGroupId = gId} = do
      gr' <- toLegacyGroupReg gr
      let !grs' = gr' : groupRegs_ d
      pure $ case grDirectoryStatus $ groupRegStatus gr of
        DSListed ->
          let !listedGroups = S.insert gId $ listedGroups_ d
              !promotedGroups = (if promoted gr then S.insert gId else id) $ promotedGroups_ d
           in d {groupRegs_ = grs', listedGroups_ = listedGroups, promotedGroups_ = promotedGroups}
        DSReserved ->
          let !reservedGroups = S.insert gId $ reservedGroups_ d
           in d {groupRegs_ = grs', reservedGroups_ = reservedGroups}
        DSRegistered -> d {groupRegs_ = grs'}
        DSRemoved -> d

mkDirectoryStore_ :: Maybe Handle -> DirectoryStoreData -> IO DirectoryStore
mkDirectoryStore_ h d = do
  listedGroups <- newTVarIO $ listedGroups_ d
  promotedGroups <- newTVarIO $ promotedGroups_ d
  reservedGroups <- newTVarIO $ reservedGroups_ d
  pure DirectoryStore {listedGroups, promotedGroups, reservedGroups, directoryLogFile = h}

readDirectoryData :: FilePath -> IO [GroupReg]
readDirectoryData f =
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

writeDirectoryData :: FilePath -> [GroupReg] -> IO Handle
writeDirectoryData f grs = do
  h <- openFile f WriteMode
  hSetBuffering h LineBuffering
  forM_ grs $ B.hPutStrLn h . strEncode . GRCreate
  pure h
