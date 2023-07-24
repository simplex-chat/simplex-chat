{-# LANGUAGE NamedFieldPuns #-}

module Directory.Store where

import Control.Concurrent.STM
import Data.Set (Set)
import Directory.Events
import Simplex.Chat.Types

data DirectoryStore = DirectoryStore
  { groupRegs :: TVar [GroupReg],
    listedGroups :: TVar (Set GroupId)
  }

data GroupReg = GroupReg
  { groupRegId :: GroupRegId,
    userGroupRegId :: UserGroupRegId,
    dbGroupId :: GroupId,
    dbContactId :: ContactId,
    groupRegStatus :: TVar GroupRegStatus
  }

data GroupRegStatus
  = GRSProposed
  | GRSPendingConfirmation
  | GRSConfirmed
  | GRSPendingUpdate
  | GRSPendingApproval GroupApprovalId
  | GRSActive
  | GRSSuspended

data DirectoryLogRecord
  = CreateGroupReg GroupReg
  | UpdateGroupRegStatus GroupRegId GroupRegStatus

getDirectoryStore :: FilePath -> IO DirectoryStore
getDirectoryStore path = do
  groupRegs <- readDirectoryState path
  st <- atomically newDirectoryStore
  atomically $ mapM_ (addGroupReg st) groupRegs
  pure st
  where
    addGroupReg :: DirectoryStore -> GroupReg -> STM ()
    addGroupReg st gr = modifyTVar' (groupRegs st) (gr :) -- TODO set listedGroups

newDirectoryStore :: STM DirectoryStore
newDirectoryStore = do
  groupRegs <- newTVar []
  listedGroups <- newTVar mempty
  pure DirectoryStore {groupRegs, listedGroups}

readDirectoryState :: FilePath -> IO [GroupReg]
readDirectoryState _ = pure []

writeDirectoryState :: FilePath -> [GroupReg] -> IO ()
writeDirectoryState _ _ = pure ()
