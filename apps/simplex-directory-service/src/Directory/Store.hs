{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Directory.Store
  ( DirectoryStore (..),
    GroupReg (..),
    GroupRegStatus (..),
    UserGroupRegId,
    GroupApprovalId,
    restoreDirectoryStore,
    addGroupReg,
    setGroupStatus,
    setGroupRegOwner,
    getGroupReg,
    getUserGroupReg,
    getUserGroupRegs,
    filterListedGroups,
    groupRegStatusText,
  )
where

import Control.Concurrent.STM
import Control.Monad
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Composition ((.:))
import Data.Int (Int64)
import Data.List (find, foldl', sortOn)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Simplex.Chat.Types
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Util (ifM)
import System.Directory (doesFileExist, renameFile)
import System.IO (BufferMode (..), Handle, IOMode (..), hSetBuffering, openFile)

data DirectoryStore = DirectoryStore
  { groupRegs :: TVar [GroupReg],
    listedGroups :: TVar (Set GroupId),
    reservedGroups :: TVar (Set GroupId),
    directoryLogFile :: Maybe Handle
  }

data GroupReg = GroupReg
  { dbGroupId :: GroupId,
    userGroupRegId :: UserGroupRegId,
    dbContactId :: ContactId,
    dbOwnerMemberId :: TVar (Maybe GroupMemberId),
    groupRegStatus :: TVar GroupRegStatus
  }

data GroupRegData = GroupRegData
  { dbGroupId_ :: GroupId,
    userGroupRegId_ :: UserGroupRegId,
    dbContactId_ :: ContactId,
    dbOwnerMemberId_ :: Maybe GroupMemberId,
    groupRegStatus_ :: GroupRegStatus
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

data DirectoryStatus = DSListed | DSReserved | DSRegistered

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
  _ -> DSRegistered

addGroupReg :: DirectoryStore -> Contact -> GroupInfo -> GroupRegStatus -> IO UserGroupRegId
addGroupReg st ct GroupInfo {groupId} grStatus = do
  grData <- atomically addGroupReg_
  logGCreate st grData
  pure $ userGroupRegId_ grData
  where
    addGroupReg_ = do
      let grData = GroupRegData {dbGroupId_ = groupId, userGroupRegId_ = 1, dbContactId_ = ctId, dbOwnerMemberId_ = Nothing, groupRegStatus_ = grStatus}
      gr <- dataToGroupReg grData
      stateTVar (groupRegs st) $ \grs ->
        let ugrId = 1 + foldl' maxUgrId 0 grs
            grData' = grData {userGroupRegId_ = ugrId}
            gr' = gr {userGroupRegId = ugrId}
         in (grData', gr' : grs)
    ctId = contactId' ct
    maxUgrId mx GroupReg {dbContactId, userGroupRegId}
      | dbContactId == ctId && userGroupRegId > mx = userGroupRegId
      | otherwise = mx

setGroupStatus :: DirectoryStore -> GroupReg -> GroupRegStatus -> IO ()
setGroupStatus st gr grStatus = do
  logGUpdateStatus st (dbGroupId gr) grStatus
  atomically $ do
    writeTVar (groupRegStatus gr) grStatus
    updateListing st $ dbGroupId gr
  where
    updateListing = case grDirectoryStatus grStatus of
      DSListed -> listGroup
      DSReserved -> reserveGroup
      DSRegistered -> unlistGroup

setGroupRegOwner :: DirectoryStore -> GroupReg -> GroupMember -> IO ()
setGroupRegOwner st gr owner = do
  let memberId = groupMemberId' owner
  logGUpdateOwner st (dbGroupId gr) memberId
  atomically $ writeTVar (dbOwnerMemberId gr) (Just memberId)

getGroupReg :: DirectoryStore -> GroupId -> STM (Maybe GroupReg)
getGroupReg st gId = find ((gId ==) . dbGroupId) <$> readTVar (groupRegs st)

getUserGroupReg :: DirectoryStore -> ContactId -> UserGroupRegId -> STM (Maybe GroupReg)
getUserGroupReg st ctId ugrId = find (\r -> ctId == dbContactId r && ugrId == userGroupRegId r) <$> readTVar (groupRegs st)

getUserGroupRegs :: DirectoryStore -> ContactId -> STM [GroupReg]
getUserGroupRegs st ctId = filter ((ctId ==) . dbContactId) <$> readTVar (groupRegs st)

filterListedGroups :: DirectoryStore -> [(GroupInfo, GroupSummary)] -> STM [(GroupInfo, GroupSummary)]
filterListedGroups st gs = do
  lgs <- readTVar $ listedGroups st
  pure $ filter (\(GroupInfo {groupId}, _) -> groupId `S.member` lgs) gs

listGroup :: DirectoryStore -> GroupId -> STM ()
listGroup st gId = do
  modifyTVar' (listedGroups st) $ S.insert gId
  modifyTVar' (reservedGroups st) $ S.delete gId

reserveGroup :: DirectoryStore -> GroupId -> STM ()
reserveGroup st gId = do
  modifyTVar' (listedGroups st) $ S.delete gId
  modifyTVar' (reservedGroups st) $ S.insert gId

unlistGroup :: DirectoryStore -> GroupId -> STM ()
unlistGroup st gId = do
  modifyTVar' (listedGroups st) $ S.delete gId
  modifyTVar' (reservedGroups st) $ S.delete gId

data DirectoryLogRecord
  = GRCreate GroupRegData
  | GRUpdateStatus GroupId GroupRegStatus
  | GRUpdateOwner GroupId GroupMemberId

data DLRTag = GRCreate_ | GRUpdateStatus_ | GRUpdateOwner_

logDLR :: DirectoryStore -> DirectoryLogRecord -> IO ()
logDLR st r = forM_ (directoryLogFile st) $ \h -> B.hPutStrLn h (strEncode r)

logGCreate :: DirectoryStore -> GroupRegData -> IO ()
logGCreate st = logDLR st . GRCreate

logGUpdateStatus :: DirectoryStore -> GroupId -> GroupRegStatus -> IO ()
logGUpdateStatus st = logDLR st .: GRUpdateStatus

logGUpdateOwner :: DirectoryStore -> GroupId -> GroupMemberId -> IO ()
logGUpdateOwner st = logDLR st .: GRUpdateOwner

instance StrEncoding DLRTag where
  strEncode = \case
    GRCreate_ -> "GCREATE"
    GRUpdateStatus_ -> "GSTATUS"
    GRUpdateOwner_ -> "GOWNER"
  strP =
    A.takeTill (== ' ') >>= \case
      "GCREATE" -> pure GRCreate_
      "GSTATUS" -> pure GRUpdateStatus_
      "GOWNER" -> pure GRUpdateOwner_
      _ -> fail "invalid DLRTag"

instance StrEncoding DirectoryLogRecord where
  strEncode = \case
    GRCreate gr -> strEncode (GRCreate_, gr)
    GRUpdateStatus gId grStatus -> strEncode (GRUpdateStatus_, gId, grStatus)
    GRUpdateOwner gId grOwnerId -> strEncode (GRUpdateOwner_, gId, grOwnerId)
  strP =
    strP >>= \case
      GRCreate_ -> GRCreate <$> (A.space *> strP)
      GRUpdateStatus_ -> GRUpdateStatus <$> (A.space *> A.decimal) <*> (A.space *> strP)
      GRUpdateOwner_ -> GRUpdateOwner <$> (A.space *> A.decimal) <*> (A.space *> A.decimal)

instance StrEncoding GroupRegData where
  strEncode GroupRegData {dbGroupId_, userGroupRegId_, dbContactId_, dbOwnerMemberId_, groupRegStatus_} =
    B.unwords
      [ "group_id=" <> strEncode dbGroupId_,
        "user_group_id=" <> strEncode userGroupRegId_,
        "contact_id=" <> strEncode dbContactId_,
        "owner_member_id=" <> strEncode dbOwnerMemberId_,
        "status=" <> strEncode groupRegStatus_
      ]
  strP = do
    dbGroupId_ <- "group_id=" *> strP_
    userGroupRegId_ <- "user_group_id=" *> strP_
    dbContactId_ <- "contact_id=" *> strP_
    dbOwnerMemberId_ <- "owner_member_id=" *> strP_
    groupRegStatus_ <- "status=" *> strP
    pure GroupRegData {dbGroupId_, userGroupRegId_, dbContactId_, dbOwnerMemberId_, groupRegStatus_}

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

dataToGroupReg :: GroupRegData -> STM GroupReg
dataToGroupReg GroupRegData {dbGroupId_, userGroupRegId_, dbContactId_, dbOwnerMemberId_, groupRegStatus_} = do
  dbOwnerMemberId <- newTVar dbOwnerMemberId_
  groupRegStatus <- newTVar groupRegStatus_
  pure
    GroupReg
      { dbGroupId = dbGroupId_,
        userGroupRegId = userGroupRegId_,
        dbContactId = dbContactId_,
        dbOwnerMemberId,
        groupRegStatus
      }

restoreDirectoryStore :: Maybe FilePath -> IO DirectoryStore
restoreDirectoryStore = \case
  Just f -> ifM (doesFileExist f) (restore f) (newFile f >>= new . Just)
  Nothing -> new Nothing
  where
    new = atomically . newDirectoryStore
    newFile f = do
      h <- openFile f WriteMode
      hSetBuffering h LineBuffering
      pure h
    restore f = do
      grs <- readDirectoryData f
      renameFile f (f <> ".bak")
      h <- writeDirectoryData f grs -- compact
      atomically $ mkDirectoryStore h grs

emptyStoreData :: ([GroupReg], Set GroupId, Set GroupId)
emptyStoreData = ([], S.empty, S.empty)

newDirectoryStore :: Maybe Handle -> STM DirectoryStore
newDirectoryStore = (`mkDirectoryStore_` emptyStoreData)

mkDirectoryStore :: Handle -> [GroupRegData] -> STM DirectoryStore
mkDirectoryStore h groups =
  foldM addGroupRegData emptyStoreData groups >>= mkDirectoryStore_ (Just h)
  where
    addGroupRegData (!grs, !listed, !reserved) gr@GroupRegData {dbGroupId_ = gId} = do
      gr' <- dataToGroupReg gr
      let grs' = gr' : grs
      pure $ case grDirectoryStatus $ groupRegStatus_ gr of
        DSListed -> (grs', S.insert gId listed, reserved)
        DSReserved -> (grs', listed, S.insert gId reserved)
        DSRegistered -> (grs', listed, reserved)

mkDirectoryStore_ :: Maybe Handle -> ([GroupReg], Set GroupId, Set GroupId) -> STM DirectoryStore
mkDirectoryStore_ h (grs, listed, reserved) = do
  groupRegs <- newTVar grs
  listedGroups <- newTVar listed
  reservedGroups <- newTVar reserved
  pure DirectoryStore {groupRegs, listedGroups, reservedGroups, directoryLogFile = h}

readDirectoryData :: FilePath -> IO [GroupRegData]
readDirectoryData f =
  sortOn dbGroupId_ . M.elems
    <$> (foldM processDLR M.empty . B.lines =<< B.readFile f)
  where
    processDLR :: Map GroupId GroupRegData -> ByteString -> IO (Map GroupId GroupRegData)
    processDLR m l = case strDecode l of
      Left e -> m <$ putStrLn ("Error parsing log record: " <> e <> ", " <> B.unpack (B.take 80 l))
      Right r -> case r of
        GRCreate gr@GroupRegData {dbGroupId_ = gId} -> do
          when (isJust $ M.lookup gId m) $
            putStrLn $
              "Warning: duplicate group with ID " <> show gId <> ", group replaced."
          pure $ M.insert gId gr m
        GRUpdateStatus gId groupRegStatus_ -> case M.lookup gId m of
          Just gr -> pure $ M.insert gId gr {groupRegStatus_} m
          Nothing -> m <$ putStrLn ("Warning: no group with ID " <> show gId <> ", status update ignored.")
        GRUpdateOwner gId grOwnerId -> case M.lookup gId m of
          Just gr -> pure $ M.insert gId gr {dbOwnerMemberId_ = Just grOwnerId} m
          Nothing -> m <$ putStrLn ("Warning: no group with ID " <> show gId <> ", owner update ignored.")

writeDirectoryData :: FilePath -> [GroupRegData] -> IO Handle
writeDirectoryData f grs = do
  h <- openFile f WriteMode
  hSetBuffering h LineBuffering
  forM_ grs $ B.hPutStrLn h . strEncode . GRCreate
  pure h
