{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Directory.Events where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Simplex.Chat.Controller
import Simplex.Chat.Messages
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Protocol (MsgContent (..))
import Simplex.Chat.Types
import Data.Char (isSpace)
import Data.Either (fromRight)

data DirectoryEvent
  = DEContactConnected Contact
  | DEGroupInvitation {contact :: Contact, groupInfo :: GroupInfo, fromMemberRole :: GroupMemberRole, memberRole :: GroupMemberRole}
  | DEServiceJoinedGroup ContactId GroupInfo
  | DEGroupUpdated GroupInfo
  | DEContactRoleChanged ContactId GroupInfo GroupMemberRole
  | DEServiceRoleChanged GroupInfo GroupMemberRole
  | DEContactRemovedFromGroup ContactId GroupInfo
  | DEContactLeftGroup ContactId GroupInfo
  | DEServiceRemovedFromGroup GroupInfo
  | DEGroupDeleted GroupInfo
  | DEContactCommand Contact ADirectoryCmd
  | DEUnsupportedMessage Contact ChatItemId
  | DEItemEditIgnored Contact
  | DEItemDeleteIgnored Contact

crDirectoryEvent :: ChatResponse -> Maybe DirectoryEvent
crDirectoryEvent = \case
  CRContactConnected {contact} -> Just $ DEContactConnected contact
  CRReceivedGroupInvitation {contact, groupInfo, fromMemberRole, memberRole} -> Just $ DEGroupInvitation {contact, groupInfo, fromMemberRole, memberRole}
  CRUserJoinedGroup {groupInfo, hostMember} -> (`DEServiceJoinedGroup` groupInfo) <$> memberContactId hostMember
  CRGroupUpdated {toGroup} -> Just $ DEGroupUpdated toGroup
  CRMemberRole {groupInfo, member, toRole} -> (\ctId -> DEContactRoleChanged ctId groupInfo toRole) <$> memberContactId member
  CRMemberRoleUser {groupInfo, toRole} -> Just $ DEServiceRoleChanged groupInfo toRole
  CRDeletedMember {groupInfo, deletedMember} -> (`DEContactRemovedFromGroup` groupInfo) <$> memberContactId deletedMember
  CRLeftMember {groupInfo, member} -> (`DEContactLeftGroup` groupInfo) <$> memberContactId member
  CRDeletedMemberUser {groupInfo} -> Just $ DEServiceRemovedFromGroup groupInfo
  CRGroupDeleted {groupInfo} -> Just $ DEGroupDeleted groupInfo
  CRChatItemUpdated {chatItem = AChatItem _ SMDRcv (DirectChat ct) _} -> Just $ DEItemEditIgnored ct
  CRChatItemDeleted {deletedChatItem = AChatItem _ SMDRcv (DirectChat ct) _, byUser = False} -> Just $ DEItemDeleteIgnored ct
  CRNewChatItem {chatItem = AChatItem _ SMDRcv (DirectChat ct) ci@ChatItem {content = CIRcvMsgContent mc, meta = CIMeta {itemLive}}} ->
    Just $ case (mc, itemLive) of
      (MCText t, Nothing) -> DEContactCommand ct $ fromRight err $ A.parseOnly (directoryCmdP ciId) $ T.dropWhileEnd isSpace t
      _ -> DEUnsupportedMessage ct ciId
    where
      ciId = chatItemId' ci
      err = ADC SDRUser $ DCUnknownCommand ciId      
  _ -> Nothing

data DirectoryRole = DRUser | DRSuperUser

data SDirectoryRole (r :: DirectoryRole) where
  SDRUser :: SDirectoryRole 'DRUser
  SDRSuperUser :: SDirectoryRole 'DRSuperUser

type GroupRegId = Int64

type UserGroupRegId = Int64

type GroupApprovalId = Int64

data DirectoryCmdTag (r :: DirectoryRole) where
  DCHelp_ :: DirectoryCmdTag 'DRUser
  DCConfirmDuplicateGroup_ :: DirectoryCmdTag 'DRUser
  DCListUserGroups_ :: DirectoryCmdTag 'DRUser
  DCDeleteGroup_ :: DirectoryCmdTag 'DRUser
  DCApproveGroup_ :: DirectoryCmdTag 'DRSuperUser
  DCRejectGroup_ :: DirectoryCmdTag 'DRSuperUser
  DCSuspendGroup_ :: DirectoryCmdTag 'DRSuperUser
  DCResumeGroup_ :: DirectoryCmdTag 'DRSuperUser
  DCListGroups_ :: DirectoryCmdTag 'DRSuperUser

data ADirectoryCmdTag = forall r. ADCT (SDirectoryRole r) (DirectoryCmdTag r)

data DirectoryCmd (r :: DirectoryRole) where
  DCHelp :: DirectoryCmd 'DRUser
  DCSearchGroup :: Text -> DirectoryCmd 'DRUser
  DCConfirmDuplicateGroup :: UserGroupRegId -> GroupName -> DirectoryCmd 'DRUser
  DCListUserGroups :: DirectoryCmd 'DRUser
  DCDeleteGroup :: UserGroupRegId -> GroupName -> DirectoryCmd 'DRUser
  DCApproveGroup :: GroupApprovalId -> GroupName -> DirectoryCmd 'DRSuperUser
  DCRejectGroup :: GroupApprovalId -> GroupName -> DirectoryCmd 'DRSuperUser
  DCSuspendGroup :: GroupId -> GroupName -> DirectoryCmd 'DRSuperUser
  DCResumeGroup :: GroupId -> GroupName -> DirectoryCmd 'DRSuperUser
  DCListGroups :: DirectoryCmd 'DRSuperUser
  DCUnknownCommand :: ChatItemId -> DirectoryCmd 'DRUser
  DCCommandError :: ChatItemId -> DirectoryCmdTag r -> DirectoryCmd r

data ADirectoryCmd = forall r. ADC (SDirectoryRole r) (DirectoryCmd r)

directoryCmdP :: ChatItemId -> Parser ADirectoryCmd
directoryCmdP ciId =
  (A.char '/' *> A.choice [tagP >>= \t@(ADCT u t') -> A.choice [cmdP t, pure . ADC u $ DCCommandError ciId t'], pure . ADC SDRUser $ DCUnknownCommand ciId])
    <|> (ADC SDRUser . DCSearchGroup <$> A.takeText)
  where
    tagP = A.takeTill (== ' ') >>= \case
      "help" -> pure $ ADCT SDRUser DCHelp_
      "h" -> pure $ ADCT SDRUser DCHelp_
      "confim" -> pure $ ADCT SDRUser DCConfirmDuplicateGroup_
      "list" -> pure $ ADCT SDRUser DCListUserGroups_
      "delete" -> pure $ ADCT SDRUser DCDeleteGroup_
      "approve" -> pure $ ADCT SDRSuperUser DCApproveGroup_
      "reject" -> pure $ ADCT SDRSuperUser DCRejectGroup_
      "suspend" -> pure $ ADCT SDRSuperUser DCSuspendGroup_
      "resume" -> pure $ ADCT SDRSuperUser DCResumeGroup_
      "all" -> pure $ ADCT SDRSuperUser DCListGroups_
      _ -> fail "bad command tag"
    cmdP :: ADirectoryCmdTag -> Parser ADirectoryCmd
    cmdP = \case
      ADCT SDRUser tag ->
        ADC SDRUser <$> case tag of
          DCHelp_ -> pure DCHelp
          DCConfirmDuplicateGroup_ -> gc DCConfirmDuplicateGroup
          DCListUserGroups_ -> pure DCListUserGroups
          DCDeleteGroup_ -> gc DCDeleteGroup
      ADCT SDRSuperUser tag ->
        ADC SDRSuperUser <$> case tag of
          DCApproveGroup_ -> gc DCApproveGroup
          DCRejectGroup_ -> gc DCRejectGroup
          DCSuspendGroup_ -> gc DCSuspendGroup
          DCResumeGroup_ -> gc DCResumeGroup
          DCListGroups_ -> pure DCListGroups
      where
        gc f = f <$> A.decimal <* A.char ':' <*> A.takeText
