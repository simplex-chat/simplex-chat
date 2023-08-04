{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Simplex.Chat.Messages.CIContent.Types where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import Data.Int (Int64)
import Data.Text (Text)
import Data.Type.Equality
import Data.Typeable (Typeable)
import Database.SQLite.Simple (ResultError (..), SQLData (..))
import Database.SQLite.Simple.FromField (Field, FromField (..), returnError)
import Database.SQLite.Simple.Internal (Field (..))
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.Generics (Generic)
import Simplex.Chat.Protocol
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Protocol (RatchetSyncState (..), SwitchPhase (..))
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (dropPrefix, enumJSON, singleFieldJSON, sumTypeJSON)
import Simplex.Messaging.Util (safeDecodeUtf8)

data MsgDirection = MDRcv | MDSnd
  deriving (Eq, Show, Generic)

instance FromJSON MsgDirection where
  parseJSON = J.genericParseJSON . enumJSON $ dropPrefix "MD"

instance ToJSON MsgDirection where
  toJSON = J.genericToJSON . enumJSON $ dropPrefix "MD"
  toEncoding = J.genericToEncoding . enumJSON $ dropPrefix "MD"

instance FromField AMsgDirection where fromField = fromIntField_ $ fmap fromMsgDirection . msgDirectionIntP

instance ToField MsgDirection where toField = toField . msgDirectionInt

fromIntField_ :: (Typeable a) => (Int64 -> Maybe a) -> Field -> Ok a
fromIntField_ fromInt = \case
  f@(Field (SQLInteger i) _) ->
    case fromInt i of
      Just x -> Ok x
      _ -> returnError ConversionFailed f ("invalid integer: " <> show i)
  f -> returnError ConversionFailed f "expecting SQLInteger column type"

data SMsgDirection (d :: MsgDirection) where
  SMDRcv :: SMsgDirection 'MDRcv
  SMDSnd :: SMsgDirection 'MDSnd

deriving instance Show (SMsgDirection d)

instance TestEquality SMsgDirection where
  testEquality SMDRcv SMDRcv = Just Refl
  testEquality SMDSnd SMDSnd = Just Refl
  testEquality _ _ = Nothing

instance ToField (SMsgDirection d) where toField = toField . msgDirectionInt . toMsgDirection

data AMsgDirection = forall d. MsgDirectionI d => AMsgDirection (SMsgDirection d)

deriving instance Show AMsgDirection

toMsgDirection :: SMsgDirection d -> MsgDirection
toMsgDirection = \case
  SMDRcv -> MDRcv
  SMDSnd -> MDSnd

fromMsgDirection :: MsgDirection -> AMsgDirection
fromMsgDirection = \case
  MDRcv -> AMsgDirection SMDRcv
  MDSnd -> AMsgDirection SMDSnd

class MsgDirectionI (d :: MsgDirection) where
  msgDirection :: SMsgDirection d

instance MsgDirectionI 'MDRcv where msgDirection = SMDRcv

instance MsgDirectionI 'MDSnd where msgDirection = SMDSnd

msgDirectionInt :: MsgDirection -> Int
msgDirectionInt = \case
  MDRcv -> 0
  MDSnd -> 1

msgDirectionIntP :: Int64 -> Maybe MsgDirection
msgDirectionIntP = \case
  0 -> Just MDRcv
  1 -> Just MDSnd
  _ -> Nothing

data CIDeleteMode = CIDMBroadcast | CIDMInternal
  deriving (Show, Generic)

instance ToJSON CIDeleteMode where
  toJSON = J.genericToJSON . enumJSON $ dropPrefix "CIDM"
  toEncoding = J.genericToEncoding . enumJSON $ dropPrefix "CIDM"

instance FromJSON CIDeleteMode where
  parseJSON = J.genericParseJSON . enumJSON $ dropPrefix "CIDM"

ciDeleteModeToText :: CIDeleteMode -> Text
ciDeleteModeToText = \case
  CIDMBroadcast -> "this item is deleted (broadcast)"
  CIDMInternal -> "this item is deleted (internal)"

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
  deriving (Show, Generic)

instance FromJSON RcvGroupEvent where
  parseJSON = J.genericParseJSON . sumTypeJSON $ dropPrefix "RGE"

instance ToJSON RcvGroupEvent where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "RGE"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "RGE"

newtype DBRcvGroupEvent = RGE RcvGroupEvent

instance FromJSON DBRcvGroupEvent where
  parseJSON v = RGE <$> J.genericParseJSON (singleFieldJSON $ dropPrefix "RGE") v

instance ToJSON DBRcvGroupEvent where
  toJSON (RGE v) = J.genericToJSON (singleFieldJSON $ dropPrefix "RGE") v
  toEncoding (RGE v) = J.genericToEncoding (singleFieldJSON $ dropPrefix "RGE") v

data SndGroupEvent
  = SGEMemberRole {groupMemberId :: GroupMemberId, profile :: Profile, role :: GroupMemberRole}
  | SGEUserRole {role :: GroupMemberRole}
  | SGEMemberDeleted {groupMemberId :: GroupMemberId, profile :: Profile} -- CRUserDeletedMember
  | SGEUserLeft -- CRLeftMemberUser
  | SGEGroupUpdated {groupProfile :: GroupProfile} -- CRGroupUpdated
  deriving (Show, Generic)

instance FromJSON SndGroupEvent where
  parseJSON = J.genericParseJSON . sumTypeJSON $ dropPrefix "SGE"

instance ToJSON SndGroupEvent where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "SGE"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "SGE"

newtype DBSndGroupEvent = SGE SndGroupEvent

instance FromJSON DBSndGroupEvent where
  parseJSON v = SGE <$> J.genericParseJSON (singleFieldJSON $ dropPrefix "SGE") v

instance ToJSON DBSndGroupEvent where
  toJSON (SGE v) = J.genericToJSON (singleFieldJSON $ dropPrefix "SGE") v
  toEncoding (SGE v) = J.genericToEncoding (singleFieldJSON $ dropPrefix "SGE") v

data RcvConnEvent
  = RCESwitchQueue {phase :: SwitchPhase}
  | RCERatchetSync {syncStatus :: RatchetSyncState}
  | RCEVerificationCodeReset
  deriving (Show, Generic)

data SndConnEvent
  = SCESwitchQueue {phase :: SwitchPhase, member :: Maybe GroupMemberRef}
  | SCERatchetSync {syncStatus :: RatchetSyncState, member :: Maybe GroupMemberRef}
  deriving (Show, Generic)

instance FromJSON RcvConnEvent where
  parseJSON = J.genericParseJSON . sumTypeJSON $ dropPrefix "RCE"

instance ToJSON RcvConnEvent where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "RCE"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "RCE"

newtype DBRcvConnEvent = RCE RcvConnEvent

instance FromJSON DBRcvConnEvent where
  parseJSON v = RCE <$> J.genericParseJSON (singleFieldJSON $ dropPrefix "RCE") v

instance ToJSON DBRcvConnEvent where
  toJSON (RCE v) = J.genericToJSON (singleFieldJSON $ dropPrefix "RCE") v
  toEncoding (RCE v) = J.genericToEncoding (singleFieldJSON $ dropPrefix "RCE") v

instance FromJSON SndConnEvent where
  parseJSON = J.genericParseJSON . sumTypeJSON $ dropPrefix "SCE"

instance ToJSON SndConnEvent where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "SCE"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "SCE"

newtype DBSndConnEvent = SCE SndConnEvent

instance FromJSON DBSndConnEvent where
  parseJSON v = SCE <$> J.genericParseJSON (singleFieldJSON $ dropPrefix "SCE") v

instance ToJSON DBSndConnEvent where
  toJSON (SCE v) = J.genericToJSON (singleFieldJSON $ dropPrefix "SCE") v
  toEncoding (SCE v) = J.genericToEncoding (singleFieldJSON $ dropPrefix "SCE") v

rcvGroupEventToText :: RcvGroupEvent -> Text
rcvGroupEventToText = \case
  RGEMemberAdded _ p -> "added " <> profileToText p
  RGEMemberConnected -> "connected"
  RGEMemberLeft -> "left"
  RGEMemberRole _ p r -> "changed role of " <> profileToText p <> " to " <> safeDecodeUtf8 (strEncode r)
  RGEUserRole r -> "changed your role to " <> safeDecodeUtf8 (strEncode r)
  RGEMemberDeleted _ p -> "removed " <> profileToText p
  RGEUserDeleted -> "removed you"
  RGEGroupDeleted -> "deleted group"
  RGEGroupUpdated _ -> "group profile updated"
  RGEInvitedViaGroupLink -> "invited via your group link"

sndGroupEventToText :: SndGroupEvent -> Text
sndGroupEventToText = \case
  SGEMemberRole _ p r -> "changed role of " <> profileToText p <> " to " <> safeDecodeUtf8 (strEncode r)
  SGEUserRole r -> "changed role for yourself to " <> safeDecodeUtf8 (strEncode r)
  SGEMemberDeleted _ p -> "removed " <> profileToText p
  SGEUserLeft -> "left"
  SGEGroupUpdated _ -> "group profile updated"

rcvConnEventToText :: RcvConnEvent -> Text
rcvConnEventToText = \case
  RCESwitchQueue phase -> case phase of
    SPStarted -> "started changing address for you..."
    SPConfirmed -> "confirmed changing address for you..."
    SPSecured -> "secured new address for you..."
    SPCompleted -> "changed address for you"
  RCERatchetSync syncStatus -> ratchetSyncStatusToText syncStatus
  RCEVerificationCodeReset -> "security code changed"

ratchetSyncStatusToText :: RatchetSyncState -> Text
ratchetSyncStatusToText = \case
  RSOk -> "connection synchronized"
  RSAllowed -> "decryption error (connection may be out of sync), synchronization allowed"
  RSRequired -> "decryption error (connection out of sync), synchronization required"
  RSStarted -> "connection synchronization started"
  RSAgreed -> "connection synchronization agreed"

sndConnEventToText :: SndConnEvent -> Text
sndConnEventToText = \case
  SCESwitchQueue phase m -> case phase of
    SPStarted -> "started changing address" <> forMember m <> "..."
    SPConfirmed -> "confirmed changing address" <> forMember m <> "..."
    SPSecured -> "secured new address" <> forMember m <> "..."
    SPCompleted -> "you changed address" <> forMember m
  SCERatchetSync syncStatus m -> ratchetSyncStatusToText syncStatus <> forMember m
  where
    forMember member_ =
      maybe "" (\GroupMemberRef {profile = Profile {displayName}} -> " for " <> displayName) member_

profileToText :: Profile -> Text
profileToText Profile {displayName, fullName} = displayName <> optionalFullName displayName fullName

data CICallStatus
  = CISCallPending
  | CISCallMissed
  | CISCallRejected -- only possible for received calls, not on type level
  | CISCallAccepted
  | CISCallNegotiated
  | CISCallProgress
  | CISCallEnded
  | CISCallError
  deriving (Show, Generic)

instance FromJSON CICallStatus where
  parseJSON = J.genericParseJSON . enumJSON $ dropPrefix "CISCall"

instance ToJSON CICallStatus where
  toJSON = J.genericToJSON . enumJSON $ dropPrefix "CISCall"
  toEncoding = J.genericToEncoding . enumJSON $ dropPrefix "CISCall"

ciCallInfoText :: CICallStatus -> Int -> Text
ciCallInfoText status duration = case status of
  CISCallPending -> "calling..."
  CISCallMissed -> "missed"
  CISCallRejected -> "rejected"
  CISCallAccepted -> "accepted"
  CISCallNegotiated -> "connecting..."
  CISCallProgress -> "in progress " <> durationText duration
  CISCallEnded -> "ended " <> durationText duration
  CISCallError -> "error"
