{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Directory.Store where

import Control.Concurrent.STM
import Data.Int (Int64)
import Data.Set (Set)
import Data.Text (Text)
import Simplex.Chat.Types
import Data.List (find, foldl')
import qualified Data.Set as S

data DirectoryStore = DirectoryStore
  { groupRegs :: TVar [GroupReg],
    listedGroups :: TVar (Set GroupId),
    reservedGroups :: TVar (Set GroupId)
  }

data GroupReg = GroupReg
  { userGroupRegId :: UserGroupRegId,
    dbGroupId :: GroupId,
    dbContactId :: ContactId,
    dbOwnerMemberId :: TVar (Maybe GroupMemberId),
    groupRegStatus :: TVar GroupRegStatus
  }

type GroupRegId = Int64

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

addGroupReg :: DirectoryStore -> Contact -> GroupInfo -> GroupRegStatus -> STM UserGroupRegId
addGroupReg st ct GroupInfo {groupId} grStatus = do
  dbOwnerMemberId <- newTVar Nothing
  groupRegStatus <- newTVar grStatus
  let gr = GroupReg {userGroupRegId = 1, dbGroupId = groupId, dbContactId = ctId, dbOwnerMemberId, groupRegStatus}
  stateTVar (groupRegs st) $ \grs ->
    let ugrId = 1 + foldl' maxUgrId 0 grs
     in (ugrId, gr {userGroupRegId = ugrId} : grs)
  where
    ctId = contactId' ct
    maxUgrId mx GroupReg {dbContactId, userGroupRegId}
      | dbContactId == ctId && userGroupRegId > mx = userGroupRegId
      | otherwise = mx

getGroupReg :: DirectoryStore -> GroupRegId -> STM (Maybe GroupReg)
getGroupReg st gId = find ((gId ==) . dbGroupId) <$> readTVar (groupRegs st)

getUserGroupReg :: DirectoryStore -> ContactId -> UserGroupRegId -> STM (Maybe GroupReg)
getUserGroupReg st ctId ugrId = find (\r -> ctId == dbContactId r && ugrId == userGroupRegId r) <$> readTVar (groupRegs st)

getUserGroupRegs :: DirectoryStore -> ContactId -> STM [GroupReg]
getUserGroupRegs st ctId = filter ((ctId ==) . dbContactId) <$> readTVar (groupRegs st)

filterListedGroups :: DirectoryStore -> [GroupInfo] -> STM [GroupInfo]
filterListedGroups st gs = do
  lgs <- readTVar $ listedGroups st
  pure $ filter (\GroupInfo {groupId} -> groupId `S.member` lgs) gs

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
  = CreateGroupReg GroupReg
  | UpdateGroupRegStatus GroupRegId GroupRegStatus

getDirectoryStore :: FilePath -> IO DirectoryStore
getDirectoryStore path = do
  groupRegs <- readDirectoryState path
  st <- atomically newDirectoryStore
  atomically $ mapM_ (add st) groupRegs
  pure st
  where
    add :: DirectoryStore -> GroupReg -> STM ()
    add st gr = modifyTVar' (groupRegs st) (gr :) -- TODO set listedGroups

newDirectoryStore :: STM DirectoryStore
newDirectoryStore = do
  groupRegs <- newTVar []
  listedGroups <- newTVar mempty
  reservedGroups <- newTVar mempty
  pure DirectoryStore {groupRegs, listedGroups, reservedGroups}

readDirectoryState :: FilePath -> IO [GroupReg]
readDirectoryState _ = pure []

writeDirectoryState :: FilePath -> [GroupReg] -> IO ()
writeDirectoryState _ _ = pure ()
