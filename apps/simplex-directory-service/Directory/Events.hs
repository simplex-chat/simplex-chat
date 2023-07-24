{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Directory.Events where

import Data.Int (Int64)
import Data.Text (Text)
import Simplex.Chat.Protocol (MsgContent)
import Simplex.Chat.Types

data DirectoryEvent
  = DENewUserConnected Contact
  | DEGroupInvitation Contact GroupInfo GroupMemberRole
  | DEGroupUpdated Contact GroupInfo
  | DEGroupDeleted Contact GroupInfo
  | DEUserCommand Contact ADirectoryCmd

data DirectoryRole = DRUser | DRSuperUser

data SDirectoryRole (r :: DirectoryRole) where
  SDRUser :: SDirectoryRole 'DRUser
  SDRSuperUser :: SDirectoryRole 'DRSuperUser

type GroupRegId = Int64

type UserGroupRegId = Int64

type GroupApprovalId = Int64

data DirectoryCmdTag (r :: DirectoryRole) where
  DCHelp_ :: DirectoryCmdTag 'DRUser
  DCSearchGroup_ :: DirectoryCmdTag 'DRUser
  DCConfirmDuplicateGroup_ :: DirectoryCmdTag 'DRUser
  DCListUserGroups_ :: DirectoryCmdTag 'DRUser
  DCDeleteGroup_ :: DirectoryCmdTag 'DRUser
  DCApproveGroup_ :: DirectoryCmdTag 'DRSuperUser
  DCRejectGroup_ :: DirectoryCmdTag 'DRSuperUser
  DCSuspendGroup_ :: DirectoryCmdTag 'DRSuperUser
  DCResumeGroup_ :: DirectoryCmdTag 'DRSuperUser
  DCListGroups_ :: DirectoryCmdTag 'DRSuperUser
  DCUnsupportedMessage_ :: DirectoryCmdTag 'DRUser
  DCCommandSyntaxError_ :: DirectoryCmdTag 'DRUser

data DirectoryCmd (r :: DirectoryRole) where
  DCHelp :: DirectoryCmd 'DRUser
  DCSearchGroup :: Text -> DirectoryCmd 'DRUser
  DCConfirmDuplicateGroup :: UserGroupRegId -> DirectoryCmd 'DRUser
  DCListUserGroups :: DirectoryCmd 'DRUser
  DCDeleteGroup :: UserGroupRegId -> DirectoryCmd 'DRUser
  DCApproveGroup :: GroupApprovalId -> GroupName -> DirectoryCmd 'DRSuperUser
  DCRejectGroup :: GroupApprovalId -> GroupName -> DirectoryCmd 'DRSuperUser
  DCSuspendGroup :: GroupId -> GroupName -> DirectoryCmd 'DRSuperUser
  DCResumeGroup :: GroupId -> GroupName -> DirectoryCmd 'DRSuperUser
  DCListGroups :: DirectoryCmd 'DRSuperUser
  DCUnsupportedMessage :: MsgContent -> DirectoryCmd 'DRUser
  DCCommandSyntaxError :: DirectoryCmdTag r -> Text -> DirectoryCmd r

data ADirectoryCmd = forall r. ADC (SDirectoryRole r) (DirectoryCmd r)
