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
    DirectoryHelpSection (..),
    DirectoryRole (..),
    SDirectoryRole (..),
    crDirectoryEvent,
    directoryCmdTag,
  )
where

import Control.Applicative (optional, (<|>))
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.Char (isSpace)
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Directory.Store
import Simplex.Chat.Controller
import Simplex.Chat.Markdown (displayNameTextP)
import Simplex.Chat.Messages
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Protocol (MsgContent (..))
import Simplex.Chat.Types
import Simplex.Chat.Types.Shared
import Simplex.Messaging.Agent.Protocol (AgentErrorType (..))
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Protocol (BrokerErrorType (..))
import Simplex.Messaging.Util (tshow, (<$?>))

data DirectoryEvent
  = DEContactConnected Contact
  | DEGroupInvitation {contact :: Contact, groupInfo :: GroupInfo, fromMemberRole :: GroupMemberRole, memberRole :: GroupMemberRole}
  | DEServiceJoinedGroup {contactId :: ContactId, groupInfo :: GroupInfo, hostMember :: GroupMember}
  | DEGroupUpdated {member :: GroupMember, fromGroup :: GroupInfo, toGroup :: GroupInfo}
  | DEPendingMember GroupInfo GroupMember
  | DEPendingMemberMsg GroupInfo GroupMember ChatItemId Text
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
  | DELogChatResponse Text
  deriving (Show)

crDirectoryEvent :: Either ChatError ChatEvent -> Maybe DirectoryEvent
crDirectoryEvent = \case
  Right evt -> crDirectoryEvent_ evt
  Left e -> case e of
    ChatErrorAgent {agentError = BROKER _ NETWORK} -> Nothing
    ChatErrorAgent {agentError = BROKER _ TIMEOUT} -> Nothing
    _ -> Just $ DELogChatResponse $ "chat error: " <> tshow e

crDirectoryEvent_ :: ChatEvent -> Maybe DirectoryEvent
crDirectoryEvent_ = \case
  CEvtContactConnected {contact} -> Just $ DEContactConnected contact
  CEvtReceivedGroupInvitation {contact, groupInfo, fromMemberRole, memberRole} -> Just $ DEGroupInvitation {contact, groupInfo, fromMemberRole, memberRole}
  CEvtUserJoinedGroup {groupInfo, hostMember} -> (\contactId -> DEServiceJoinedGroup {contactId, groupInfo, hostMember}) <$> memberContactId hostMember
  CEvtGroupUpdated {fromGroup, toGroup, member_} -> (\member -> DEGroupUpdated {member, fromGroup, toGroup}) <$> member_
  CEvtJoinedGroupMember {groupInfo, member = m}
    | pending m -> Just $ DEPendingMember groupInfo m
    | otherwise -> Nothing
  CEvtNewChatItems {chatItems = AChatItem _ _ (GroupChat g) ci : _} -> case ci of
    ChatItem {chatDir = CIGroupRcv m, content = CIRcvMsgContent (MCText t)} | pending m -> Just $ DEPendingMemberMsg g m (chatItemId' ci) t
    _ -> Nothing
  CEvtMemberRole {groupInfo, member, toRole}
    | groupMemberId' member == groupMemberId' (membership groupInfo) -> Just $ DEServiceRoleChanged groupInfo toRole
    | otherwise -> (\ctId -> DEContactRoleChanged groupInfo ctId toRole) <$> memberContactId member
  CEvtDeletedMember {groupInfo, deletedMember} -> (`DEContactRemovedFromGroup` groupInfo) <$> memberContactId deletedMember
  CEvtLeftMember {groupInfo, member} -> (`DEContactLeftGroup` groupInfo) <$> memberContactId member
  CEvtDeletedMemberUser {groupInfo} -> Just $ DEServiceRemovedFromGroup groupInfo
  CEvtGroupDeleted {groupInfo} -> Just $ DEGroupDeleted groupInfo
  CEvtChatItemUpdated {chatItem = AChatItem _ SMDRcv (DirectChat ct) _} -> Just $ DEItemEditIgnored ct
  CEvtChatItemsDeleted {chatItemDeletions = ((ChatItemDeletion (AChatItem _ SMDRcv (DirectChat ct) _) _) : _), byUser = False} -> Just $ DEItemDeleteIgnored ct
  CEvtNewChatItems {chatItems = (AChatItem _ SMDRcv (DirectChat ct) ci@ChatItem {content = CIRcvMsgContent mc, meta = CIMeta {itemLive}}) : _} ->
    Just $ case (mc, itemLive) of
      (MCText t, Nothing) -> DEContactCommand ct ciId $ fromRight err $ A.parseOnly (directoryCmdP <* A.endOfInput) $ T.dropWhileEnd isSpace t
      _ -> DEUnsupportedMessage ct ciId
    where
      ciId = chatItemId' ci
      err = ADC SDRUser DCUnknownCommand
  CEvtMessageError {severity, errorMessage} -> Just $ DELogChatResponse $ "message error: " <> severity <> ", " <> errorMessage
  CEvtChatErrors {chatErrors} -> Just $ DELogChatResponse $ "chat errors: " <> T.intercalate ", " (map tshow chatErrors)
  _ -> Nothing
  where
    pending m = memberStatus m == GSMemPendingApproval

data DirectoryRole = DRUser | DRAdmin | DRSuperUser

data SDirectoryRole (r :: DirectoryRole) where
  SDRUser :: SDirectoryRole 'DRUser
  SDRAdmin :: SDirectoryRole 'DRAdmin
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
  DCMemberRole_ :: DirectoryCmdTag 'DRUser
  DCGroupFilter_ :: DirectoryCmdTag 'DRUser
  DCApproveGroup_ :: DirectoryCmdTag 'DRAdmin
  DCRejectGroup_ :: DirectoryCmdTag 'DRAdmin
  DCSuspendGroup_ :: DirectoryCmdTag 'DRAdmin
  DCResumeGroup_ :: DirectoryCmdTag 'DRAdmin
  DCListLastGroups_ :: DirectoryCmdTag 'DRAdmin
  DCListPendingGroups_ :: DirectoryCmdTag 'DRAdmin
  DCShowGroupLink_ :: DirectoryCmdTag 'DRAdmin
  DCSendToGroupOwner_ :: DirectoryCmdTag 'DRAdmin
  DCInviteOwnerToGroup_ :: DirectoryCmdTag 'DRAdmin
  -- DCAddBlockedWord_ :: DirectoryCmdTag 'DRAdmin
  -- DCRemoveBlockedWord_ :: DirectoryCmdTag 'DRAdmin
  DCExecuteCommand_ :: DirectoryCmdTag 'DRSuperUser

deriving instance Show (DirectoryCmdTag r)

data ADirectoryCmdTag = forall r. ADCT (SDirectoryRole r) (DirectoryCmdTag r)

data DirectoryHelpSection = DHSRegistration | DHSCommands
  deriving (Show)

data DirectoryCmd (r :: DirectoryRole) where
  DCHelp :: DirectoryHelpSection -> DirectoryCmd 'DRUser
  DCSearchGroup :: Text -> DirectoryCmd 'DRUser
  DCSearchNext :: DirectoryCmd 'DRUser
  DCAllGroups :: DirectoryCmd 'DRUser
  DCRecentGroups :: DirectoryCmd 'DRUser
  DCSubmitGroup :: ConnReqContact -> DirectoryCmd 'DRUser
  DCConfirmDuplicateGroup :: UserGroupRegId -> GroupName -> DirectoryCmd 'DRUser
  DCListUserGroups :: DirectoryCmd 'DRUser
  DCDeleteGroup :: UserGroupRegId -> GroupName -> DirectoryCmd 'DRUser
  DCMemberRole :: UserGroupRegId -> Maybe GroupName -> Maybe GroupMemberRole -> DirectoryCmd 'DRUser
  DCGroupFilter :: UserGroupRegId -> Maybe GroupName -> Maybe DirectoryMemberAcceptance -> DirectoryCmd 'DRUser
  DCApproveGroup :: {groupId :: GroupId, displayName :: GroupName, groupApprovalId :: GroupApprovalId} -> DirectoryCmd 'DRAdmin
  DCRejectGroup :: GroupId -> GroupName -> DirectoryCmd 'DRAdmin
  DCSuspendGroup :: GroupId -> GroupName -> DirectoryCmd 'DRAdmin
  DCResumeGroup :: GroupId -> GroupName -> DirectoryCmd 'DRAdmin
  DCListLastGroups :: Int -> DirectoryCmd 'DRAdmin
  DCListPendingGroups :: Int -> DirectoryCmd 'DRAdmin
  DCShowGroupLink :: GroupId -> GroupName -> DirectoryCmd 'DRAdmin
  DCSendToGroupOwner :: GroupId -> GroupName -> Text -> DirectoryCmd 'DRAdmin
  DCInviteOwnerToGroup :: GroupId -> GroupName -> DirectoryCmd 'DRAdmin
  -- DCAddBlockedWord :: Text -> DirectoryCmd 'DRAdmin
  -- DCRemoveBlockedWord :: Text -> DirectoryCmd 'DRAdmin
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
    tagP =
      A.takeTill isSpace >>= \case
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
        "role" -> u DCMemberRole_
        "filter" -> u DCGroupFilter_
        "approve" -> au DCApproveGroup_
        "reject" -> au DCRejectGroup_
        "suspend" -> au DCSuspendGroup_
        "resume" -> au DCResumeGroup_
        "last" -> au DCListLastGroups_
        "pending" -> au DCListPendingGroups_
        "link" -> au DCShowGroupLink_
        "owner" -> au DCSendToGroupOwner_
        "invite" -> au DCInviteOwnerToGroup_
        -- "block_word" -> au DCAddBlockedWord_
        -- "unblock_word" -> au DCRemoveBlockedWord_
        "exec" -> su DCExecuteCommand_
        "x" -> su DCExecuteCommand_
        _ -> fail "bad command tag"
      where
        u = pure . ADCT SDRUser
        au = pure . ADCT SDRAdmin
        su = pure . ADCT SDRSuperUser
    cmdP :: DirectoryCmdTag r -> Parser (DirectoryCmd r)
    cmdP = \case
      DCHelp_ -> DCHelp . fromMaybe DHSRegistration <$> optional (A.takeWhile isSpace *> helpSectionP)
        where
          helpSectionP =
            A.takeText >>= \case
              "registration" -> pure DHSRegistration
              "r" -> pure DHSRegistration
              "commands" -> pure DHSCommands
              "c" -> pure DHSCommands
              _ -> fail "bad help section"
      DCSearchNext_ -> pure DCSearchNext
      DCAllGroups_ -> pure DCAllGroups
      DCRecentGroups_ -> pure DCRecentGroups
      DCSubmitGroup_ -> fmap DCSubmitGroup . strDecode . encodeUtf8 <$?> (spacesP *> A.takeText)
      DCConfirmDuplicateGroup_ -> gc DCConfirmDuplicateGroup
      DCListUserGroups_ -> pure DCListUserGroups
      DCDeleteGroup_ -> gc DCDeleteGroup
      DCMemberRole_ -> do
        (groupId, displayName_) <- gc_ (,)
        memberRole_ <- optional $ spacesP *> ("member" $> GRMember <|> "observer" $> GRObserver)
        pure $ DCMemberRole groupId displayName_ memberRole_
      DCGroupFilter_ -> do
        (groupId, displayName_) <- gc_ (,)
        acceptance_ <-
          (A.takeWhile isSpace >> A.endOfInput) $> Nothing
            <|> Just <$> (acceptancePresetsP <|> acceptanceFiltersP)
        pure $ DCGroupFilter groupId displayName_ acceptance_
        where
          acceptancePresetsP =
            spacesP
              *> A.choice
                [ "off" $> noJoinFilter,
                  "basic" $> basicJoinFilter,
                  ("moderate" <|> "mod") $> moderateJoinFilter,
                  "strong" $> strongJoinFilter
                ]
          acceptanceFiltersP = do
            rejectNames <- filterP "name"
            passCaptcha <- filterP "captcha"
            makeObserver <- filterP "observer"
            pure DirectoryMemberAcceptance {rejectNames, passCaptcha, makeObserver}
          filterP :: Text -> Parser (Maybe ProfileCondition)
          filterP s = Just <$> (spacesP *> A.string s *> conditionP) <|> pure Nothing
          conditionP =
            "=all" $> PCAll
              <|> ("=noimage" <|> "=no_image" <|> "=no-image") $> PCNoImage
              <|> pure PCAll
      DCApproveGroup_ -> do
        (groupId, displayName) <- gc (,)
        groupApprovalId <- A.space *> A.decimal
        pure DCApproveGroup {groupId, displayName, groupApprovalId}
      DCRejectGroup_ -> gc DCRejectGroup
      DCSuspendGroup_ -> gc DCSuspendGroup
      DCResumeGroup_ -> gc DCResumeGroup
      DCListLastGroups_ -> DCListLastGroups <$> (A.space *> A.decimal <|> pure 10)
      DCListPendingGroups_ -> DCListPendingGroups <$> (A.space *> A.decimal <|> pure 10)
      DCShowGroupLink_ -> gc DCShowGroupLink
      DCSendToGroupOwner_ -> do
        (groupId, displayName) <- gc (,)
        msg <- A.space *> A.takeText
        pure $ DCSendToGroupOwner groupId displayName msg
      DCInviteOwnerToGroup_ -> gc DCInviteOwnerToGroup
      -- DCAddBlockedWord_ -> DCAddBlockedWord <$> wordP
      -- DCRemoveBlockedWord_ -> DCRemoveBlockedWord <$> wordP
      DCExecuteCommand_ -> DCExecuteCommand . T.unpack <$> (spacesP *> A.takeText)
      where
        gc f = f <$> (spacesP *> A.decimal) <*> (A.char ':' *> displayNameTextP)
        gc_ f = f <$> (spacesP *> A.decimal) <*> optional (A.char ':' *> displayNameTextP)
        -- wordP = spacesP *> A.takeTill isSpace
        spacesP = A.takeWhile1 isSpace

directoryCmdTag :: DirectoryCmd r -> Text
directoryCmdTag = \case
  DCHelp _ -> "help"
  DCSearchGroup _ -> "search"
  DCSearchNext -> "next"
  DCAllGroups -> "all"
  DCRecentGroups -> "new"
  DCSubmitGroup _ -> "submit"
  DCConfirmDuplicateGroup {} -> "confirm"
  DCListUserGroups -> "list" 
  DCDeleteGroup {} -> "delete"
  DCApproveGroup {} -> "approve"
  DCMemberRole {} -> "role"
  DCGroupFilter {} -> "filter"
  DCRejectGroup {} -> "reject"
  DCSuspendGroup {} -> "suspend"
  DCResumeGroup {} -> "resume"
  DCListLastGroups _ -> "last"
  DCListPendingGroups _ -> "pending"
  DCShowGroupLink {} -> "link"
  DCSendToGroupOwner {} -> "owner"
  DCInviteOwnerToGroup {} -> "invite"
  -- DCAddBlockedWord _ -> "block_word"
  -- DCRemoveBlockedWord _ -> "unblock_word"
  DCExecuteCommand _ -> "exec"
  DCUnknownCommand -> "unknown"
  DCCommandError _ -> "error"
