{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Directory.Store where

import Control.Concurrent.STM
import Data.Int (Int64)
import Data.Set (Set)
import Simplex.Chat.Types
import Data.List (find)
import qualified Data.Set as S

data DirectoryStore = DirectoryStore
  { groupRegs :: TVar [GroupReg],
    listedGroups :: TVar (Set GroupId)
  }

data GroupReg = GroupReg
  { userGroupRegId :: UserGroupRegId,
    dbGroupId :: GroupId,
    dbContactId :: ContactId,
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

addGroupReg :: DirectoryStore -> Contact -> GroupInfo -> STM ()
addGroupReg st ct GroupInfo {groupId} = do
  groupRegStatus <- newTVar GRSProposed
  let gr = GroupReg {userGroupRegId = groupId, dbGroupId = groupId, dbContactId = contactId' ct, groupRegStatus}
  modifyTVar' (groupRegs st) (gr :)

getGroupReg :: DirectoryStore -> GroupRegId -> STM (Maybe GroupReg)
getGroupReg st gId = find ((gId ==) . dbGroupId) <$> readTVar (groupRegs st)

getUserGroupRegId :: DirectoryStore -> ContactId -> UserGroupRegId -> STM (Maybe GroupReg)
getUserGroupRegId st ctId ugrId = find (\r -> ctId == dbContactId r && ugrId == userGroupRegId r) <$> readTVar (groupRegs st)

getContactGroupRegs :: DirectoryStore -> ContactId -> STM [GroupReg]
getContactGroupRegs st ctId = filter ((ctId ==) . dbContactId) <$> readTVar (groupRegs st)

filterListedGroups :: DirectoryStore -> [GroupInfo] -> STM [GroupInfo]
filterListedGroups _st _gs = undefined

listGroup :: DirectoryStore -> GroupId -> STM ()
listGroup st gId = modifyTVar' (listedGroups st) $ S.insert gId

unlistGroup :: DirectoryStore -> GroupId -> STM ()
unlistGroup st gId = modifyTVar' (listedGroups st) $ S.delete gId

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
  pure DirectoryStore {groupRegs, listedGroups}

readDirectoryState :: FilePath -> IO [GroupReg]
readDirectoryState _ = pure []

writeDirectoryState :: FilePath -> [GroupReg] -> IO ()
writeDirectoryState _ _ = pure ()
