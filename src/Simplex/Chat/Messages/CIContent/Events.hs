{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Messages.CIContent.Events where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson.TH as J
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Protocol (RatchetSyncState (..), SwitchPhase (..))
import Simplex.Messaging.Parsers (dropPrefix, singleFieldJSON, sumTypeJSON)

data RcvGroupEvent
  = RGEMemberAdded {groupMemberId :: GroupMemberId, profile :: Profile} -- CRJoinedGroupMemberConnecting
  | RGEMemberConnected -- CRUserJoinedGroup, CRJoinedGroupMember, CRConnectedToGroupMember
  | RGEMemberLeft -- CRLeftMember
  | RGEMemberRole {groupMemberId :: GroupMemberId, profile :: Profile, role :: GroupMemberRole}
  | RGEMemberBlocked {groupMemberId :: GroupMemberId, profile :: Profile, blocked :: Bool} -- CRMemberBlockedForAll
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
  | RGEMemberProfileUpdated {fromProfile :: Profile, toProfile :: Profile} -- CRGroupMemberUpdated
  deriving (Show)

data SndGroupEvent
  = SGEMemberRole {groupMemberId :: GroupMemberId, profile :: Profile, role :: GroupMemberRole}
  | SGEMemberBlocked {groupMemberId :: GroupMemberId, profile :: Profile, blocked :: Bool} -- CRMemberBlockedForAllUser
  | SGEUserRole {role :: GroupMemberRole}
  | SGEMemberDeleted {groupMemberId :: GroupMemberId, profile :: Profile} -- CRUserDeletedMember
  | SGEUserLeft -- CRLeftMemberUser
  | SGEGroupUpdated {groupProfile :: GroupProfile} -- CRGroupUpdated
  deriving (Show)

data RcvConnEvent
  = RCESwitchQueue {phase :: SwitchPhase}
  | RCERatchetSync {syncStatus :: RatchetSyncState}
  | RCEVerificationCodeReset
  | RCEPQEnabled {enabled :: Bool}
  deriving (Show)

data SndConnEvent
  = SCESwitchQueue {phase :: SwitchPhase, member :: Maybe GroupMemberRef}
  | SCERatchetSync {syncStatus :: RatchetSyncState, member :: Maybe GroupMemberRef}
  deriving (Show)

data RcvDirectEvent
  = RDEContactDeleted
  | RDEProfileUpdated {fromProfile :: Profile, toProfile :: Profile} -- CRContactUpdated
  deriving (Show)

-- platform-specific JSON encoding (used in API)
$(J.deriveJSON (sumTypeJSON $ dropPrefix "RGE") ''RcvGroupEvent)

-- platform-independent JSON encoding (stored in DB)
newtype DBRcvGroupEvent = RGE RcvGroupEvent

instance FromJSON DBRcvGroupEvent where
  parseJSON v = RGE <$> $(J.mkParseJSON (singleFieldJSON $ dropPrefix "RGE") ''RcvGroupEvent) v

instance ToJSON DBRcvGroupEvent where
  toJSON (RGE v) = $(J.mkToJSON (singleFieldJSON $ dropPrefix "RGE") ''RcvGroupEvent) v
  toEncoding (RGE v) = $(J.mkToEncoding (singleFieldJSON $ dropPrefix "RGE") ''RcvGroupEvent) v

-- platform-specific JSON encoding (used in API)
$(J.deriveJSON (sumTypeJSON $ dropPrefix "SGE") ''SndGroupEvent)

-- platform-independent JSON encoding (stored in DB)
newtype DBSndGroupEvent = SGE SndGroupEvent

instance FromJSON DBSndGroupEvent where
  parseJSON v = SGE <$> $(J.mkParseJSON (singleFieldJSON $ dropPrefix "SGE") ''SndGroupEvent) v

instance ToJSON DBSndGroupEvent where
  toJSON (SGE v) = $(J.mkToJSON (singleFieldJSON $ dropPrefix "SGE") ''SndGroupEvent) v
  toEncoding (SGE v) = $(J.mkToEncoding (singleFieldJSON $ dropPrefix "SGE") ''SndGroupEvent) v

-- platform-specific JSON encoding (used in API)
$(J.deriveJSON (sumTypeJSON $ dropPrefix "RCE") ''RcvConnEvent)

-- platform-independent JSON encoding (stored in DB)
newtype DBRcvConnEvent = RCE RcvConnEvent

instance FromJSON DBRcvConnEvent where
  parseJSON v = RCE <$> $(J.mkParseJSON (singleFieldJSON $ dropPrefix "RCE") ''RcvConnEvent) v

instance ToJSON DBRcvConnEvent where
  toJSON (RCE v) = $(J.mkToJSON (singleFieldJSON $ dropPrefix "RCE") ''RcvConnEvent) v
  toEncoding (RCE v) = $(J.mkToEncoding (singleFieldJSON $ dropPrefix "RCE") ''RcvConnEvent) v

-- platform-specific JSON encoding (used in API)
$(J.deriveJSON (sumTypeJSON $ dropPrefix "SCE") ''SndConnEvent)

-- platform-independent JSON encoding (stored in DB)
newtype DBSndConnEvent = SCE SndConnEvent

instance FromJSON DBSndConnEvent where
  parseJSON v = SCE <$> $(J.mkParseJSON (singleFieldJSON $ dropPrefix "SCE") ''SndConnEvent) v

instance ToJSON DBSndConnEvent where
  toJSON (SCE v) = $(J.mkToJSON (singleFieldJSON $ dropPrefix "SCE") ''SndConnEvent) v
  toEncoding (SCE v) = $(J.mkToEncoding (singleFieldJSON $ dropPrefix "SCE") ''SndConnEvent) v

$(J.deriveJSON (sumTypeJSON $ dropPrefix "RDE") ''RcvDirectEvent)

-- platform-independent JSON encoding (stored in DB)
newtype DBRcvDirectEvent = RDE RcvDirectEvent

instance FromJSON DBRcvDirectEvent where
  parseJSON v = RDE <$> $(J.mkParseJSON (singleFieldJSON $ dropPrefix "RDE") ''RcvDirectEvent) v

instance ToJSON DBRcvDirectEvent where
  toJSON (RDE v) = $(J.mkToJSON (singleFieldJSON $ dropPrefix "RDE") ''RcvDirectEvent) v
  toEncoding (RDE v) = $(J.mkToEncoding (singleFieldJSON $ dropPrefix "RDE") ''RcvDirectEvent) v
