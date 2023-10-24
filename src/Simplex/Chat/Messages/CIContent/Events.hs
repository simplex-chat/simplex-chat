{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Messages.CIContent.Events where

import qualified Data.Aeson.TH as J
import Simplex.Chat.Messages.CIContent.Events.Prefix
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Protocol (RatchetSyncState (..), SwitchPhase (..))
import Simplex.Messaging.Parsers (sumTypeJSON)

data RcvGroupEvent
  = RGEMemberAdded {groupMemberId :: GroupMemberId, profile :: Profile} -- CRJoinedGroupMemberConnecting
  | RGEMemberConnected -- CRUserJoinedGroup, CRJoinedGroupMember, CRConnectedToGroupMember
  | RGEMemberLeft -- CRLeftMember
  | RGEMemberRole {groupMemberId :: GroupMemberId, profile :: Profile, role :: GroupMemberRole}
  | RGEUserRole {role :: GroupMemberRole}
  | RGEMemberDeleted {groupMemberId :: GroupMemberId, profile :: Profile} -- CRDeletedMember
  | RGEUserDeleted -- CRDeletedMemberUser
  | RGEGroupDeleted -- CRGroupDeleted
  | RGEGroupUpdated {groupProfile :: GroupProfile} -- CRGroupUpdated
  -- RGEInvitedViaGroupLink chat items are not received - they're created when sending group invitations,
  -- but being RcvGroupEvent allows them to be assigned to the respective member (and so enable "send direct message")
  -- and be created as unread without adding / working around new status for sent items
  | RGEInvitedViaGroupLink -- CRSentGroupInvitationViaLink
  | RGEMemberCreatedContact -- CRNewMemberContactReceivedInv
  deriving (Show)

data SndGroupEvent
  = SGEMemberRole {groupMemberId :: GroupMemberId, profile :: Profile, role :: GroupMemberRole}
  | SGEUserRole {role :: GroupMemberRole}
  | SGEMemberDeleted {groupMemberId :: GroupMemberId, profile :: Profile} -- CRUserDeletedMember
  | SGEUserLeft -- CRLeftMemberUser
  | SGEGroupUpdated {groupProfile :: GroupProfile} -- CRGroupUpdated
  deriving (Show)

data RcvConnEvent
  = RCESwitchQueue {phase :: SwitchPhase}
  | RCERatchetSync {syncStatus :: RatchetSyncState}
  | RCEVerificationCodeReset
  deriving (Show)

data SndConnEvent
  = SCESwitchQueue {phase :: SwitchPhase, member :: Maybe GroupMemberRef}
  | SCERatchetSync {syncStatus :: RatchetSyncState, member :: Maybe GroupMemberRef}
  deriving (Show)

data RcvDirectEvent =
  -- RDEProfileChanged {...}
  RDEContactDeleted
  deriving (Show)

$(J.deriveJSON (sumTypeJSON dropPrefixRGE) ''RcvGroupEvent)

$(J.deriveJSON (sumTypeJSON dropPrefixSGE) ''SndGroupEvent)

$(J.deriveJSON (sumTypeJSON dropPrefixRCE) ''RcvConnEvent)

$(J.deriveJSON (sumTypeJSON dropPrefixSCE) ''SndConnEvent)

$(J.deriveJSON (sumTypeJSON dropPrefixRDE) ''RcvDirectEvent)
