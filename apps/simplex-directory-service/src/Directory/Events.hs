{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Directory.Events
  ( DirectoryEvent (..),
    DirectoryCmd (..),
    ADirectoryCmd (..),
    DirectoryRole (..),
    SDirectoryRole (..),
    crDirectoryEvent,
    viewName,
  )
where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Directory.Store
import Simplex.Chat.Controller
import Simplex.Chat.Messages
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Protocol (MsgContent (..))
import Simplex.Chat.Types
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Util ((<$?>))
import Data.Char (isSpace)
import Data.Either (fromRight)

data DirectoryEvent
  = DEContactConnected Contact
  | DEGroupInvitation {contact :: Contact, groupInfo :: GroupInfo, fromMemberRole :: GroupMemberRole, memberRole :: GroupMemberRole}
  | DEServiceJoinedGroup {contactId :: ContactId, groupInfo ::  GroupInfo, hostMember :: GroupMember}
  | DEGroupUpdated {contactId :: ContactId, fromGroup :: GroupInfo, toGroup :: GroupInfo}
  | DEContactRoleChanged GroupInfo ContactId GroupMemberRole -- contactId here is the contact whose role changed
  | DEServiceRoleChanged GroupInfo GroupMemberRole
  | DEContactRemovedFromGroup ContactId GroupInfo
  | DEContactLeftGroup ContactId GroupInfo
  | DEServiceRemovedFromGroup GroupInfo
  | DEGroupDeleted GroupInfo
  | DEUnsupportedMessage Contact ChatItemId
  | DEItemEditIgnored Contact
  | DEItemDeleteIgnored Contact
  | DEContactCommand Contact ChatItemId ADirectoryCmd
  deriving (Show)

crDirectoryEvent :: ChatResponse -> Maybe DirectoryEvent
crDirectoryEvent = \case
  CRContactConnected {contact} -> Just $ DEContactConnected contact
  CRReceivedGroupInvitation {contact, groupInfo, fromMemberRole, memberRole} -> Just $ DEGroupInvitation {contact, groupInfo, fromMemberRole, memberRole}
  CRUserJoinedGroup {groupInfo, hostMember} -> (\contactId -> DEServiceJoinedGroup {contactId, groupInfo, hostMember}) <$> memberContactId hostMember
  CRGroupUpdated {fromGroup, toGroup, member_} -> (\contactId -> DEGroupUpdated {contactId, fromGroup, toGroup}) <$> (memberContactId =<< member_)
  CRMemberRole {groupInfo, member, toRole}
    | groupMemberId' member == groupMemberId' (membership groupInfo) -> Just $ DEServiceRoleChanged groupInfo toRole
    | otherwise -> (\ctId -> DEContactRoleChanged groupInfo ctId toRole) <$> memberContactId member
  CRDeletedMember {groupInfo, deletedMember} -> (`DEContactRemovedFromGroup` groupInfo) <$> memberContactId deletedMember
  CRLeftMember {groupInfo, member} -> (`DEContactLeftGroup` groupInfo) <$> memberContactId member
  CRDeletedMemberUser {groupInfo} -> Just $ DEServiceRemovedFromGroup groupInfo
  CRGroupDeleted {groupInfo} -> Just $ DEGroupDeleted groupInfo
  CRChatItemUpdated {chatItem = AChatItem _ SMDRcv (DirectChat ct) _} -> Just $ DEItemEditIgnored ct
  CRChatItemDeleted {deletedChatItem = AChatItem _ SMDRcv (DirectChat ct) _, byUser = False} -> Just $ DEItemDeleteIgnored ct
  CRNewChatItem {chatItem = AChatItem _ SMDRcv (DirectChat ct) ci@ChatItem {content = CIRcvMsgContent mc, meta = CIMeta {itemLive}}} ->
    Just $ case (mc, itemLive) of
      (MCText t, Nothing) -> DEContactCommand ct ciId $ fromRight err $ A.parseOnly directoryCmdP $ T.dropWhileEnd isSpace t
      _ -> DEUnsupportedMessage ct ciId
    where
      ciId = chatItemId' ci
      err = ADC SDRUser DCUnknownCommand
  _ -> Nothing

data DirectoryRole = DRUser | DRSuperUser

data SDirectoryRole (r :: DirectoryRole) where
  SDRUser :: SDirectoryRole 'DRUser
  SDRSuperUser :: SDirectoryRole 'DRSuperUser

deriving instance Show (SDirectoryRole r)

data DirectoryCmdTag (r :: DirectoryRole) where
  DCHelp_ :: DirectoryCmdTag 'DRUser
  DCSearchNext_ :: DirectoryCmdTag 'DRUser
  DCAllGroups_ :: DirectoryCmdTag 'DRUser
  DCRecentGroups_ :: DirectoryCmdTag 'DRUser
  DCSubmitGroup_ :: DirectoryCmdTag 'DRUser
  DCConfirmDuplicateGroup_ :: DirectoryCmdTag 'DRUser
  DCListUserGroups_ :: DirectoryCmdTag 'DRUser
  DCDeleteGroup_ :: DirectoryCmdTag 'DRUser
  DCApproveGroup_ :: DirectoryCmdTag 'DRSuperUser
  DCRejectGroup_ :: DirectoryCmdTag 'DRSuperUser
  DCSuspendGroup_ :: DirectoryCmdTag 'DRSuperUser
  DCResumeGroup_ :: DirectoryCmdTag 'DRSuperUser
  DCListLastGroups_ :: DirectoryCmdTag 'DRSuperUser
  DCExecuteCommand_ :: DirectoryCmdTag 'DRSuperUser

deriving instance Show (DirectoryCmdTag r)

data ADirectoryCmdTag = forall r. ADCT (SDirectoryRole r) (DirectoryCmdTag r)

data DirectoryCmd (r :: DirectoryRole) where
  DCHelp :: DirectoryCmd 'DRUser
  DCSearchGroup :: Text -> DirectoryCmd 'DRUser
  DCSearchNext :: DirectoryCmd 'DRUser
  DCAllGroups :: DirectoryCmd 'DRUser
  DCRecentGroups :: DirectoryCmd 'DRUser
  DCSubmitGroup :: ConnReqContact -> DirectoryCmd 'DRUser
  DCConfirmDuplicateGroup :: UserGroupRegId -> GroupName -> DirectoryCmd 'DRUser
  DCListUserGroups :: DirectoryCmd 'DRUser
  DCDeleteGroup :: UserGroupRegId -> GroupName -> DirectoryCmd 'DRUser
  DCApproveGroup :: {groupId :: GroupId, displayName :: GroupName, groupApprovalId :: GroupApprovalId} -> DirectoryCmd 'DRSuperUser
  DCRejectGroup :: GroupId -> GroupName -> DirectoryCmd 'DRSuperUser
  DCSuspendGroup :: GroupId -> GroupName -> DirectoryCmd 'DRSuperUser
  DCResumeGroup :: GroupId -> GroupName -> DirectoryCmd 'DRSuperUser
  DCListLastGroups :: Int -> DirectoryCmd 'DRSuperUser
  DCExecuteCommand :: String -> DirectoryCmd 'DRSuperUser
  DCUnknownCommand :: DirectoryCmd 'DRUser
  DCCommandError :: DirectoryCmdTag r -> DirectoryCmd r

deriving instance Show (DirectoryCmd r)

data ADirectoryCmd = forall r. ADC (SDirectoryRole r) (DirectoryCmd r)

deriving instance Show ADirectoryCmd

directoryCmdP :: Parser ADirectoryCmd
directoryCmdP =
  (A.char '/' *> cmdStrP)
    <|> (A.char '.' $> ADC SDRUser DCSearchNext)
    <|> (ADC SDRUser . DCSearchGroup <$> A.takeText)
  where
    cmdStrP =
      (tagP >>= \(ADCT u t) -> ADC u <$> (cmdP t <|> pure (DCCommandError t)))
        <|> pure (ADC SDRUser DCUnknownCommand)
    tagP = A.takeTill (== ' ') >>= \case
      "help" -> u DCHelp_
      "h" -> u DCHelp_
      "next" -> u DCSearchNext_
      "all" -> u DCAllGroups_
      "new" -> u DCRecentGroups_
      "submit" -> u DCSubmitGroup_
      "confirm" -> u DCConfirmDuplicateGroup_
      "list" -> u DCListUserGroups_
      "ls" -> u DCListUserGroups_
      "delete" -> u DCDeleteGroup_
      "approve" -> su DCApproveGroup_
      "reject" -> su DCRejectGroup_
      "suspend" -> su DCSuspendGroup_
      "resume" -> su DCResumeGroup_
      "last" -> su DCListLastGroups_
      "exec" -> su DCExecuteCommand_
      "x" -> su DCExecuteCommand_
      _ -> fail "bad command tag"
      where
        u = pure . ADCT SDRUser
        su = pure . ADCT SDRSuperUser
    cmdP :: DirectoryCmdTag r -> Parser (DirectoryCmd r)
    cmdP = \case
      DCHelp_ -> pure DCHelp
      DCSearchNext_ -> pure DCSearchNext
      DCAllGroups_ -> pure DCAllGroups
      DCRecentGroups_ -> pure DCRecentGroups
      DCSubmitGroup_ -> fmap DCSubmitGroup . strDecode . encodeUtf8 <$?> (A.takeWhile1 isSpace *> A.takeText)
      DCConfirmDuplicateGroup_ -> gc DCConfirmDuplicateGroup
      DCListUserGroups_ -> pure DCListUserGroups
      DCDeleteGroup_ -> gc DCDeleteGroup
      DCApproveGroup_ -> do
        (groupId, displayName) <- gc (,)
        groupApprovalId <- A.space *> A.decimal
        pure $ DCApproveGroup {groupId, displayName, groupApprovalId}
      DCRejectGroup_ -> gc DCRejectGroup
      DCSuspendGroup_ -> gc DCSuspendGroup
      DCResumeGroup_ -> gc DCResumeGroup
      DCListLastGroups_ -> DCListLastGroups <$> (A.space *> A.decimal <|> pure 10)
      DCExecuteCommand_ -> DCExecuteCommand . T.unpack <$> (A.space *> A.takeText)
      where
        gc f = f <$> (A.space *> A.decimal <* A.char ':') <*> displayNameP
        displayNameP = quoted '\'' <|> takeNameTill (== ' ')
        takeNameTill p =
          A.peekChar' >>= \c ->
            if refChar c then A.takeTill p else fail "invalid first character in display name"
        quoted c = A.char c *> takeNameTill (== c) <* A.char c
        refChar c = c > ' ' && c /= '#' && c /= '@'

viewName :: String -> String
viewName n = if ' ' `elem` n then "'" <> n <> "'" else n
