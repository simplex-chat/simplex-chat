{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Simplex.Chat.Messages.CIContent where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Type.Equality
import Data.Word (Word32)
import Simplex.Chat.Messages.CIContent.Events
import Simplex.Chat.Options.DB (FromField (..), ToField (..))
import Simplex.Chat.Protocol
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences
import Simplex.Chat.Types.Shared
import Simplex.Messaging.Agent.Protocol (MsgErrorType (..), RatchetSyncState (..), SwitchPhase (..))
import Simplex.Messaging.Crypto.Ratchet (PQEncryption, pattern PQEncOff, pattern PQEncOn)
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON, fstToLower, singleFieldJSON, sumTypeJSON)
import Simplex.Messaging.Util (encodeJSON, safeDecodeUtf8, tshow, (<$?>))

data MsgDirection = MDRcv | MDSnd
  deriving (Eq, Show)

$(JQ.deriveJSON (enumJSON $ dropPrefix "MD") ''MsgDirection)

instance FromField AMsgDirection where fromField = fromIntField_ $ fmap fromMsgDirection . msgDirectionIntP

instance FromField MsgDirection where fromField = fromIntField_ msgDirectionIntP

instance ToField MsgDirection where toField = toField . msgDirectionInt

data SMsgDirection (d :: MsgDirection) where
  SMDRcv :: SMsgDirection 'MDRcv
  SMDSnd :: SMsgDirection 'MDSnd

deriving instance Show (SMsgDirection d)

instance TestEquality SMsgDirection where
  testEquality SMDRcv SMDRcv = Just Refl
  testEquality SMDSnd SMDSnd = Just Refl
  testEquality _ _ = Nothing

instance MsgDirectionI d => FromJSON (SMsgDirection d) where
  parseJSON v = (\(AMsgDirection d) -> checkDirection d) . fromMsgDirection <$?> J.parseJSON v

instance ToJSON (SMsgDirection d) where
  toJSON = J.toJSON . toMsgDirection
  toEncoding = J.toEncoding . toMsgDirection

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

checkDirection :: forall t d d'. (MsgDirectionI d, MsgDirectionI d') => t d' -> Either String (t d)
checkDirection x = case testEquality (msgDirection @d) (msgDirection @d') of
  Just Refl -> Right x
  Nothing -> Left "bad direction"

msgDirectionInt :: MsgDirection -> Int
msgDirectionInt = \case
  MDRcv -> 0
  MDSnd -> 1

msgDirectionIntP :: Int64 -> Maybe MsgDirection
msgDirectionIntP = \case
  0 -> Just MDRcv
  1 -> Just MDSnd
  _ -> Nothing

data CIDeleteMode = CIDMBroadcast | CIDMInternal | CIDMInternalMark
  deriving (Show)

instance StrEncoding CIDeleteMode where
  strEncode = \case
    CIDMBroadcast -> "broadcast"
    CIDMInternal -> "internal"
    CIDMInternalMark -> "internalMark"
  strP =
    A.takeTill (== ' ') >>= \case
      "broadcast" -> pure CIDMBroadcast
      "internal" -> pure CIDMInternal
      "internalMark" -> pure CIDMInternalMark
      _ -> fail "bad CIDeleteMode"

instance ToJSON CIDeleteMode where
  toJSON = strToJSON
  toEncoding = strToJEncoding

instance FromJSON CIDeleteMode where
  parseJSON = strParseJSON "CIDeleteMode"

ciDeleteModeToText :: CIDeleteMode -> Text
ciDeleteModeToText = \case
  CIDMBroadcast -> "this item is deleted (broadcast)"
  CIDMInternal -> "this item is deleted (locally)"
  CIDMInternalMark -> "this item is deleted (locally)"

-- This type is used both in API and in DB, so we use different JSON encodings for the database and for the API
-- ! Nested sum types also have to use different encodings for database and API
-- ! to avoid breaking cross-platform compatibility, see RcvGroupEvent and SndGroupEvent
data CIContent (d :: MsgDirection) where
  CISndMsgContent :: MsgContent -> CIContent 'MDSnd
  CIRcvMsgContent :: MsgContent -> CIContent 'MDRcv
  CISndDeleted :: CIDeleteMode -> CIContent 'MDSnd -- legacy - since v4.3.0 item_deleted field is used
  CIRcvDeleted :: CIDeleteMode -> CIContent 'MDRcv -- legacy - since v4.3.0 item_deleted field is used
  CISndCall :: CICallStatus -> Int -> CIContent 'MDSnd
  CIRcvCall :: CICallStatus -> Int -> CIContent 'MDRcv
  CIRcvIntegrityError :: MsgErrorType -> CIContent 'MDRcv
  CIRcvDecryptionError :: MsgDecryptError -> Word32 -> CIContent 'MDRcv
  CIRcvGroupInvitation :: CIGroupInvitation -> GroupMemberRole -> CIContent 'MDRcv
  CISndGroupInvitation :: CIGroupInvitation -> GroupMemberRole -> CIContent 'MDSnd
  CIRcvDirectEvent :: RcvDirectEvent -> CIContent 'MDRcv
  CIRcvGroupEvent :: RcvGroupEvent -> CIContent 'MDRcv
  CISndGroupEvent :: SndGroupEvent -> CIContent 'MDSnd
  CIRcvConnEvent :: RcvConnEvent -> CIContent 'MDRcv
  CISndConnEvent :: SndConnEvent -> CIContent 'MDSnd
  CIRcvChatFeature :: ChatFeature -> PrefEnabled -> Maybe Int -> CIContent 'MDRcv
  CISndChatFeature :: ChatFeature -> PrefEnabled -> Maybe Int -> CIContent 'MDSnd
  CIRcvChatPreference :: ChatFeature -> FeatureAllowed -> Maybe Int -> CIContent 'MDRcv
  CISndChatPreference :: ChatFeature -> FeatureAllowed -> Maybe Int -> CIContent 'MDSnd
  CIRcvGroupFeature :: GroupFeature -> GroupPreference -> Maybe Int -> Maybe GroupMemberRole -> CIContent 'MDRcv
  CISndGroupFeature :: GroupFeature -> GroupPreference -> Maybe Int -> Maybe GroupMemberRole -> CIContent 'MDSnd
  CIRcvChatFeatureRejected :: ChatFeature -> CIContent 'MDRcv
  CIRcvGroupFeatureRejected :: GroupFeature -> CIContent 'MDRcv
  CISndModerated :: CIContent 'MDSnd
  CIRcvModerated :: CIContent 'MDRcv
  CIRcvBlocked :: CIContent 'MDRcv
  CISndDirectE2EEInfo :: E2EInfo -> CIContent 'MDSnd
  CIRcvDirectE2EEInfo :: E2EInfo -> CIContent 'MDRcv
  CISndGroupE2EEInfo :: E2EInfo -> CIContent 'MDSnd -- when new group is created
  CIRcvGroupE2EEInfo :: E2EInfo -> CIContent 'MDRcv -- when enabled with some member
  CIInvalidJSON :: Text -> CIContent d -- this is also used for logical database errors, e.g. SEBadChatItem

-- ^ This type is used both in API and in DB, so we use different JSON encodings for the database and for the API
-- ! ^ Nested sum types also have to use different encodings for database and API
-- ! ^ to avoid breaking cross-platform compatibility, see RcvGroupEvent and SndGroupEvent

deriving instance Show (CIContent d)

data E2EInfo = E2EInfo {pqEnabled :: PQEncryption}
  deriving (Eq, Show)

ciMsgContent :: CIContent d -> Maybe MsgContent
ciMsgContent = \case
  CISndMsgContent mc -> Just mc
  CIRcvMsgContent mc -> Just mc
  _ -> Nothing

data MsgDecryptError
  = MDERatchetHeader
  | MDETooManySkipped
  | MDERatchetEarlier
  | MDEOther
  | MDERatchetSync
  deriving (Eq, Show)

ciRequiresAttention :: forall d. MsgDirectionI d => CIContent d -> Bool
ciRequiresAttention content = case msgDirection @d of
  SMDSnd -> True
  SMDRcv -> case content of
    CIRcvMsgContent _ -> True
    CIRcvDeleted _ -> True
    CIRcvCall {} -> True
    CIRcvIntegrityError _ -> True
    CIRcvDecryptionError {} -> True
    CIRcvGroupInvitation {} -> True
    CIRcvDirectEvent rde -> case rde of
      RDEContactDeleted -> False
      RDEProfileUpdated {} -> False
    CIRcvGroupEvent rge -> case rge of
      RGEMemberAdded {} -> False
      RGEMemberConnected -> False
      RGEMemberLeft -> False
      RGEMemberRole {} -> False
      RGEMemberBlocked {} -> False
      RGEUserRole _ -> True
      RGEMemberDeleted {} -> False
      RGEUserDeleted -> True
      RGEGroupDeleted -> True
      RGEGroupUpdated _ -> False
      RGEInvitedViaGroupLink -> False
      RGEMemberCreatedContact -> False
      RGEMemberProfileUpdated {} -> False
    CIRcvConnEvent _ -> True
    CIRcvChatFeature {} -> False
    CIRcvChatPreference {} -> False
    CIRcvGroupFeature {} -> False
    CIRcvChatFeatureRejected _ -> True
    CIRcvGroupFeatureRejected _ -> True
    CIRcvModerated -> True
    CIRcvBlocked -> False
    CIRcvDirectE2EEInfo _ -> False
    CIRcvGroupE2EEInfo _ -> False
    CIInvalidJSON _ -> False

newtype DBMsgErrorType = DBME MsgErrorType

instance FromJSON DBMsgErrorType where
  parseJSON v = DBME <$> $(JQ.mkParseJSON (singleFieldJSON fstToLower) ''MsgErrorType) v

instance ToJSON DBMsgErrorType where
  toJSON (DBME v) = $(JQ.mkToJSON (singleFieldJSON fstToLower) ''MsgErrorType) v
  toEncoding (DBME v) = $(JQ.mkToEncoding (singleFieldJSON fstToLower) ''MsgErrorType) v

data CIGroupInvitation = CIGroupInvitation
  { groupId :: GroupId,
    groupMemberId :: GroupMemberId,
    localDisplayName :: GroupName,
    groupProfile :: GroupProfile,
    status :: CIGroupInvitationStatus
  }
  deriving (Eq, Show)

data CIGroupInvitationStatus
  = CIGISPending
  | CIGISAccepted
  | CIGISRejected
  | CIGISExpired
  deriving (Eq, Show)

ciContentToText :: CIContent d -> Text
ciContentToText = \case
  CISndMsgContent mc -> msgContentText mc
  CIRcvMsgContent mc -> msgContentText mc
  CISndDeleted cidm -> ciDeleteModeToText cidm
  CIRcvDeleted cidm -> ciDeleteModeToText cidm
  CISndCall status duration -> "outgoing call: " <> ciCallInfoText status duration
  CIRcvCall status duration -> "incoming call: " <> ciCallInfoText status duration
  CIRcvIntegrityError err -> msgIntegrityError err
  CIRcvDecryptionError err n -> msgDecryptErrorText err n
  CIRcvGroupInvitation groupInvitation memberRole -> "received " <> ciGroupInvitationToText groupInvitation memberRole
  CISndGroupInvitation groupInvitation memberRole -> "sent " <> ciGroupInvitationToText groupInvitation memberRole
  CIRcvDirectEvent event -> rcvDirectEventToText event
  CIRcvGroupEvent event -> rcvGroupEventToText event
  CISndGroupEvent event -> sndGroupEventToText event
  CIRcvConnEvent event -> rcvConnEventToText event
  CISndConnEvent event -> sndConnEventToText event
  CIRcvChatFeature feature enabled param -> featureStateText feature enabled param
  CISndChatFeature feature enabled param -> featureStateText feature enabled param
  CIRcvChatPreference feature allowed param -> prefStateText feature allowed param
  CISndChatPreference feature allowed param -> "you " <> prefStateText feature allowed param
  CIRcvGroupFeature feature pref param role -> groupPrefStateText feature pref param role
  CISndGroupFeature feature pref param role -> groupPrefStateText feature pref param role
  CIRcvChatFeatureRejected feature -> chatFeatureNameText feature <> ": received, prohibited"
  CIRcvGroupFeatureRejected feature -> groupFeatureNameText feature <> ": received, prohibited"
  CISndModerated -> ciModeratedText
  CIRcvModerated -> ciModeratedText
  CIRcvBlocked -> "blocked"
  CISndDirectE2EEInfo e2eeInfo -> directE2EInfoToText e2eeInfo
  CIRcvDirectE2EEInfo e2eeInfo -> directE2EInfoToText e2eeInfo
  CISndGroupE2EEInfo e2eeInfo -> groupE2EInfoToText e2eeInfo
  CIRcvGroupE2EEInfo e2eeInfo -> groupE2EInfoToText e2eeInfo
  CIInvalidJSON _ -> "invalid content JSON"

directE2EInfoToText :: E2EInfo -> Text
directE2EInfoToText E2EInfo {pqEnabled} = case pqEnabled of
  PQEncOn -> e2eInfoPQText
  PQEncOff -> e2eInfoNoPQText

groupE2EInfoToText :: E2EInfo -> Text
groupE2EInfoToText _e2eeInfo = e2eInfoNoPQText

e2eInfoNoPQText :: Text
e2eInfoNoPQText =
  "This conversation is protected by end-to-end encryption with perfect forward secrecy, repudiation and break-in recovery."

e2eInfoPQText :: Text
e2eInfoPQText =
  "This conversation is protected by quantum resistant end-to-end encryption. It has perfect forward secrecy, repudiation and quantum resistant break-in recovery."

ciGroupInvitationToText :: CIGroupInvitation -> GroupMemberRole -> Text
ciGroupInvitationToText CIGroupInvitation {groupProfile = GroupProfile {displayName, fullName}} role =
  "invitation to join group " <> displayName <> optionalFullName displayName fullName <> " as " <> (decodeLatin1 . strEncode $ role)

rcvDirectEventToText :: RcvDirectEvent -> Text
rcvDirectEventToText = \case
  RDEContactDeleted -> "contact deleted"
  RDEProfileUpdated {} -> "updated profile"

rcvGroupEventToText :: RcvGroupEvent -> Text
rcvGroupEventToText = \case
  RGEMemberAdded _ p -> "added " <> profileToText p
  RGEMemberConnected -> "connected"
  RGEMemberLeft -> "left"
  RGEMemberRole _ p r -> "changed role of " <> profileToText p <> " to " <> safeDecodeUtf8 (strEncode r)
  RGEMemberBlocked _ p blocked -> (if blocked then "blocked" else "unblocked") <> " " <> profileToText p
  RGEUserRole r -> "changed your role to " <> safeDecodeUtf8 (strEncode r)
  RGEMemberDeleted _ p -> "removed " <> profileToText p
  RGEUserDeleted -> "removed you"
  RGEGroupDeleted -> "deleted group"
  RGEGroupUpdated _ -> "group profile updated"
  RGEInvitedViaGroupLink -> "invited via your group link"
  RGEMemberCreatedContact -> "started direct connection with you"
  RGEMemberProfileUpdated {} -> "updated profile"

sndGroupEventToText :: SndGroupEvent -> Text
sndGroupEventToText = \case
  SGEMemberRole _ p r -> "changed role of " <> profileToText p <> " to " <> safeDecodeUtf8 (strEncode r)
  SGEMemberBlocked _ p blocked -> (if blocked then "blocked" else "unblocked") <> " " <> profileToText p
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
  RCEPqEnabled pqEnc -> case pqEnc of
    PQEncOn -> "quantum resistant e2e encryption"
    PQEncOff -> "standard end-to-end encryption"

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
  SCEPqEnabled pqEnc -> case pqEnc of
    PQEncOn -> "quantum resistant e2e encryption"
    PQEncOff -> "standard end-to-end encryption"
  where
    forMember member_ =
      maybe "" (\GroupMemberRef {profile = Profile {displayName}} -> " for " <> displayName) member_

profileToText :: Profile -> Text
profileToText Profile {displayName, fullName} = displayName <> optionalFullName displayName fullName

msgIntegrityError :: MsgErrorType -> Text
msgIntegrityError = \case
  MsgSkipped fromId toId ->
    ("skipped message ID " <> tshow fromId)
      <> if fromId == toId then "" else ".." <> tshow toId
  MsgBadId msgId -> "unexpected message ID " <> tshow msgId
  MsgBadHash -> "incorrect message hash"
  MsgDuplicate -> "duplicate message ID"

msgDecryptErrorText :: MsgDecryptError -> Word32 -> Text
msgDecryptErrorText err n =
  "decryption error, possibly due to the device change"
    <> maybe "" (\ed -> " (" <> ed <> ")") errDesc
  where
    errDesc = case err of
      MDERatchetHeader -> Just $ "header" <> counter
      MDETooManySkipped -> Just $ "too many skipped messages" <> counter
      MDERatchetEarlier -> Just $ "earlier message" <> counter
      MDEOther -> counter_
      MDERatchetSync -> Just "synchronization error"
    counter_ = if n == 1 then Nothing else Just $ tshow n <> " messages"
    counter = maybe "" (", " <>) counter_

msgDirToModeratedContent_ :: SMsgDirection d -> CIContent d
msgDirToModeratedContent_ = \case
  SMDRcv -> CIRcvModerated
  SMDSnd -> CISndModerated

ciModeratedText :: Text
ciModeratedText = "moderated"

data ACIContent = forall d. MsgDirectionI d => ACIContent (SMsgDirection d) (CIContent d)

deriving instance Show ACIContent

-- platform specific
data JSONCIContent
  = JCISndMsgContent {msgContent :: MsgContent}
  | JCIRcvMsgContent {msgContent :: MsgContent}
  | JCISndDeleted {deleteMode :: CIDeleteMode}
  | JCIRcvDeleted {deleteMode :: CIDeleteMode}
  | JCISndCall {status :: CICallStatus, duration :: Int} -- duration in seconds
  | JCIRcvCall {status :: CICallStatus, duration :: Int}
  | JCIRcvIntegrityError {msgError :: MsgErrorType}
  | JCIRcvDecryptionError {msgDecryptError :: MsgDecryptError, msgCount :: Word32}
  | JCIRcvGroupInvitation {groupInvitation :: CIGroupInvitation, memberRole :: GroupMemberRole}
  | JCISndGroupInvitation {groupInvitation :: CIGroupInvitation, memberRole :: GroupMemberRole}
  | JCIRcvDirectEvent {rcvDirectEvent :: RcvDirectEvent}
  | JCIRcvGroupEvent {rcvGroupEvent :: RcvGroupEvent}
  | JCISndGroupEvent {sndGroupEvent :: SndGroupEvent}
  | JCIRcvConnEvent {rcvConnEvent :: RcvConnEvent}
  | JCISndConnEvent {sndConnEvent :: SndConnEvent}
  | JCIRcvChatFeature {feature :: ChatFeature, enabled :: PrefEnabled, param :: Maybe Int}
  | JCISndChatFeature {feature :: ChatFeature, enabled :: PrefEnabled, param :: Maybe Int}
  | JCIRcvChatPreference {feature :: ChatFeature, allowed :: FeatureAllowed, param :: Maybe Int}
  | JCISndChatPreference {feature :: ChatFeature, allowed :: FeatureAllowed, param :: Maybe Int}
  | JCIRcvGroupFeature {groupFeature :: GroupFeature, preference :: GroupPreference, param :: Maybe Int, memberRole_ :: Maybe GroupMemberRole}
  | JCISndGroupFeature {groupFeature :: GroupFeature, preference :: GroupPreference, param :: Maybe Int, memberRole_ :: Maybe GroupMemberRole}
  | JCIRcvChatFeatureRejected {feature :: ChatFeature}
  | JCIRcvGroupFeatureRejected {groupFeature :: GroupFeature}
  | JCISndModerated
  | JCIRcvModerated
  | JCIRcvBlocked
  | JCISndDirectE2EEInfo {e2eeInfo :: E2EInfo}
  | JCIRcvDirectE2EEInfo {e2eeInfo :: E2EInfo}
  | JCISndGroupE2EEInfo {e2eeInfo :: E2EInfo}
  | JCIRcvGroupE2EEInfo {e2eeInfo :: E2EInfo}
  | JCIInvalidJSON {direction :: MsgDirection, json :: Text}

jsonCIContent :: forall d. MsgDirectionI d => CIContent d -> JSONCIContent
jsonCIContent = \case
  CISndMsgContent mc -> JCISndMsgContent mc
  CIRcvMsgContent mc -> JCIRcvMsgContent mc
  CISndDeleted cidm -> JCISndDeleted cidm
  CIRcvDeleted cidm -> JCIRcvDeleted cidm
  CISndCall status duration -> JCISndCall {status, duration}
  CIRcvCall status duration -> JCIRcvCall {status, duration}
  CIRcvIntegrityError err -> JCIRcvIntegrityError err
  CIRcvDecryptionError err n -> JCIRcvDecryptionError err n
  CIRcvGroupInvitation groupInvitation memberRole -> JCIRcvGroupInvitation {groupInvitation, memberRole}
  CISndGroupInvitation groupInvitation memberRole -> JCISndGroupInvitation {groupInvitation, memberRole}
  CIRcvDirectEvent rcvDirectEvent -> JCIRcvDirectEvent {rcvDirectEvent}
  CIRcvGroupEvent rcvGroupEvent -> JCIRcvGroupEvent {rcvGroupEvent}
  CISndGroupEvent sndGroupEvent -> JCISndGroupEvent {sndGroupEvent}
  CIRcvConnEvent rcvConnEvent -> JCIRcvConnEvent {rcvConnEvent}
  CISndConnEvent sndConnEvent -> JCISndConnEvent {sndConnEvent}
  CIRcvChatFeature feature enabled param -> JCIRcvChatFeature {feature, enabled, param}
  CISndChatFeature feature enabled param -> JCISndChatFeature {feature, enabled, param}
  CIRcvChatPreference feature allowed param -> JCIRcvChatPreference {feature, allowed, param}
  CISndChatPreference feature allowed param -> JCISndChatPreference {feature, allowed, param}
  CIRcvGroupFeature groupFeature preference param memberRole_ -> JCIRcvGroupFeature {groupFeature, preference, param, memberRole_}
  CISndGroupFeature groupFeature preference param memberRole_ -> JCISndGroupFeature {groupFeature, preference, param, memberRole_}
  CIRcvChatFeatureRejected feature -> JCIRcvChatFeatureRejected {feature}
  CIRcvGroupFeatureRejected groupFeature -> JCIRcvGroupFeatureRejected {groupFeature}
  CISndModerated -> JCISndModerated
  CIRcvModerated -> JCIRcvModerated
  CIRcvBlocked -> JCIRcvBlocked
  CISndDirectE2EEInfo e2eeInfo -> JCISndDirectE2EEInfo e2eeInfo
  CIRcvDirectE2EEInfo e2eeInfo -> JCIRcvDirectE2EEInfo e2eeInfo
  CISndGroupE2EEInfo e2eeInfo -> JCISndGroupE2EEInfo e2eeInfo
  CIRcvGroupE2EEInfo e2eeInfo -> JCIRcvGroupE2EEInfo e2eeInfo
  CIInvalidJSON json -> JCIInvalidJSON (toMsgDirection $ msgDirection @d) json

aciContentJSON :: JSONCIContent -> ACIContent
aciContentJSON = \case
  JCISndMsgContent mc -> ACIContent SMDSnd $ CISndMsgContent mc
  JCIRcvMsgContent mc -> ACIContent SMDRcv $ CIRcvMsgContent mc
  JCISndDeleted cidm -> ACIContent SMDSnd $ CISndDeleted cidm
  JCIRcvDeleted cidm -> ACIContent SMDRcv $ CIRcvDeleted cidm
  JCISndCall {status, duration} -> ACIContent SMDSnd $ CISndCall status duration
  JCIRcvCall {status, duration} -> ACIContent SMDRcv $ CIRcvCall status duration
  JCIRcvIntegrityError err -> ACIContent SMDRcv $ CIRcvIntegrityError err
  JCIRcvDecryptionError err n -> ACIContent SMDRcv $ CIRcvDecryptionError err n
  JCIRcvGroupInvitation {groupInvitation, memberRole} -> ACIContent SMDRcv $ CIRcvGroupInvitation groupInvitation memberRole
  JCISndGroupInvitation {groupInvitation, memberRole} -> ACIContent SMDSnd $ CISndGroupInvitation groupInvitation memberRole
  JCIRcvDirectEvent {rcvDirectEvent} -> ACIContent SMDRcv $ CIRcvDirectEvent rcvDirectEvent
  JCIRcvGroupEvent {rcvGroupEvent} -> ACIContent SMDRcv $ CIRcvGroupEvent rcvGroupEvent
  JCISndGroupEvent {sndGroupEvent} -> ACIContent SMDSnd $ CISndGroupEvent sndGroupEvent
  JCIRcvConnEvent {rcvConnEvent} -> ACIContent SMDRcv $ CIRcvConnEvent rcvConnEvent
  JCISndConnEvent {sndConnEvent} -> ACIContent SMDSnd $ CISndConnEvent sndConnEvent
  JCIRcvChatFeature {feature, enabled, param} -> ACIContent SMDRcv $ CIRcvChatFeature feature enabled param
  JCISndChatFeature {feature, enabled, param} -> ACIContent SMDSnd $ CISndChatFeature feature enabled param
  JCIRcvChatPreference {feature, allowed, param} -> ACIContent SMDRcv $ CIRcvChatPreference feature allowed param
  JCISndChatPreference {feature, allowed, param} -> ACIContent SMDSnd $ CISndChatPreference feature allowed param
  JCIRcvGroupFeature {groupFeature, preference, param, memberRole_} -> ACIContent SMDRcv $ CIRcvGroupFeature groupFeature preference param memberRole_
  JCISndGroupFeature {groupFeature, preference, param, memberRole_} -> ACIContent SMDSnd $ CISndGroupFeature groupFeature preference param memberRole_
  JCIRcvChatFeatureRejected {feature} -> ACIContent SMDRcv $ CIRcvChatFeatureRejected feature
  JCIRcvGroupFeatureRejected {groupFeature} -> ACIContent SMDRcv $ CIRcvGroupFeatureRejected groupFeature
  JCISndModerated -> ACIContent SMDSnd CISndModerated
  JCIRcvModerated -> ACIContent SMDRcv CIRcvModerated
  JCIRcvBlocked -> ACIContent SMDRcv CIRcvBlocked
  JCISndDirectE2EEInfo {e2eeInfo} -> ACIContent SMDSnd $ CISndDirectE2EEInfo e2eeInfo
  JCIRcvDirectE2EEInfo {e2eeInfo} -> ACIContent SMDRcv $ CIRcvDirectE2EEInfo e2eeInfo
  JCISndGroupE2EEInfo {e2eeInfo} -> ACIContent SMDSnd $ CISndGroupE2EEInfo e2eeInfo
  JCIRcvGroupE2EEInfo {e2eeInfo} -> ACIContent SMDRcv $ CIRcvGroupE2EEInfo e2eeInfo
  JCIInvalidJSON dir json -> case fromMsgDirection dir of
    AMsgDirection d -> ACIContent d $ CIInvalidJSON json

-- platform independent
data DBJSONCIContent
  = DBJCISndMsgContent {msgContent :: MsgContent}
  | DBJCIRcvMsgContent {msgContent :: MsgContent}
  | DBJCISndDeleted {deleteMode :: CIDeleteMode}
  | DBJCIRcvDeleted {deleteMode :: CIDeleteMode}
  | DBJCISndCall {status :: CICallStatus, duration :: Int}
  | DBJCIRcvCall {status :: CICallStatus, duration :: Int}
  | DBJCIRcvIntegrityError {msgError :: DBMsgErrorType}
  | DBJCIRcvDecryptionError {msgDecryptError :: MsgDecryptError, msgCount :: Word32}
  | DBJCIRcvGroupInvitation {groupInvitation :: CIGroupInvitation, memberRole :: GroupMemberRole}
  | DBJCISndGroupInvitation {groupInvitation :: CIGroupInvitation, memberRole :: GroupMemberRole}
  | DBJCIRcvDirectEvent {rcvDirectEvent :: DBRcvDirectEvent}
  | DBJCIRcvGroupEvent {rcvGroupEvent :: DBRcvGroupEvent}
  | DBJCISndGroupEvent {sndGroupEvent :: DBSndGroupEvent}
  | DBJCIRcvConnEvent {rcvConnEvent :: DBRcvConnEvent}
  | DBJCISndConnEvent {sndConnEvent :: DBSndConnEvent}
  | DBJCIRcvChatFeature {feature :: ChatFeature, enabled :: PrefEnabled, param :: Maybe Int}
  | DBJCISndChatFeature {feature :: ChatFeature, enabled :: PrefEnabled, param :: Maybe Int}
  | DBJCIRcvChatPreference {feature :: ChatFeature, allowed :: FeatureAllowed, param :: Maybe Int}
  | DBJCISndChatPreference {feature :: ChatFeature, allowed :: FeatureAllowed, param :: Maybe Int}
  | DBJCIRcvGroupFeature {groupFeature :: GroupFeature, preference :: GroupPreference, param :: Maybe Int, memberRole_ :: Maybe GroupMemberRole}
  | DBJCISndGroupFeature {groupFeature :: GroupFeature, preference :: GroupPreference, param :: Maybe Int, memberRole_ :: Maybe GroupMemberRole}
  | DBJCIRcvChatFeatureRejected {feature :: ChatFeature}
  | DBJCIRcvGroupFeatureRejected {groupFeature :: GroupFeature}
  | DBJCISndModerated
  | DBJCIRcvModerated
  | DBJCIRcvBlocked
  | DBJCISndDirectE2EEInfo {e2eeInfo :: E2EInfo}
  | DBJCIRcvDirectE2EEInfo {e2eeInfo :: E2EInfo}
  | DBJCISndGroupE2EEInfo {e2eeInfo :: E2EInfo}
  | DBJCIRcvGroupE2EEInfo {e2eeInfo :: E2EInfo}
  | DBJCIInvalidJSON {direction :: MsgDirection, json :: Text}

dbJsonCIContent :: forall d. MsgDirectionI d => CIContent d -> DBJSONCIContent
dbJsonCIContent = \case
  CISndMsgContent mc -> DBJCISndMsgContent mc
  CIRcvMsgContent mc -> DBJCIRcvMsgContent mc
  CISndDeleted cidm -> DBJCISndDeleted cidm
  CIRcvDeleted cidm -> DBJCIRcvDeleted cidm
  CISndCall status duration -> DBJCISndCall {status, duration}
  CIRcvCall status duration -> DBJCIRcvCall {status, duration}
  CIRcvIntegrityError err -> DBJCIRcvIntegrityError $ DBME err
  CIRcvDecryptionError err n -> DBJCIRcvDecryptionError err n
  CIRcvGroupInvitation groupInvitation memberRole -> DBJCIRcvGroupInvitation {groupInvitation, memberRole}
  CISndGroupInvitation groupInvitation memberRole -> DBJCISndGroupInvitation {groupInvitation, memberRole}
  CIRcvDirectEvent rde -> DBJCIRcvDirectEvent $ RDE rde
  CIRcvGroupEvent rge -> DBJCIRcvGroupEvent $ RGE rge
  CISndGroupEvent sge -> DBJCISndGroupEvent $ SGE sge
  CIRcvConnEvent rce -> DBJCIRcvConnEvent $ RCE rce
  CISndConnEvent sce -> DBJCISndConnEvent $ SCE sce
  CIRcvChatFeature feature enabled param -> DBJCIRcvChatFeature {feature, enabled, param}
  CISndChatFeature feature enabled param -> DBJCISndChatFeature {feature, enabled, param}
  CIRcvChatPreference feature allowed param -> DBJCIRcvChatPreference {feature, allowed, param}
  CISndChatPreference feature allowed param -> DBJCISndChatPreference {feature, allowed, param}
  CIRcvGroupFeature groupFeature preference param memberRole_ -> DBJCIRcvGroupFeature {groupFeature, preference, param, memberRole_}
  CISndGroupFeature groupFeature preference param memberRole_ -> DBJCISndGroupFeature {groupFeature, preference, param, memberRole_}
  CIRcvChatFeatureRejected feature -> DBJCIRcvChatFeatureRejected {feature}
  CIRcvGroupFeatureRejected groupFeature -> DBJCIRcvGroupFeatureRejected {groupFeature}
  CISndModerated -> DBJCISndModerated
  CIRcvModerated -> DBJCIRcvModerated
  CIRcvBlocked -> DBJCIRcvBlocked
  CISndDirectE2EEInfo e2eeInfo -> DBJCISndDirectE2EEInfo e2eeInfo
  CIRcvDirectE2EEInfo e2eeInfo -> DBJCIRcvDirectE2EEInfo e2eeInfo
  CISndGroupE2EEInfo e2eeInfo -> DBJCISndGroupE2EEInfo e2eeInfo
  CIRcvGroupE2EEInfo e2eeInfo -> DBJCIRcvGroupE2EEInfo e2eeInfo
  CIInvalidJSON json -> DBJCIInvalidJSON (toMsgDirection $ msgDirection @d) json

aciContentDBJSON :: DBJSONCIContent -> ACIContent
aciContentDBJSON = \case
  DBJCISndMsgContent mc -> ACIContent SMDSnd $ CISndMsgContent mc
  DBJCIRcvMsgContent mc -> ACIContent SMDRcv $ CIRcvMsgContent mc
  DBJCISndDeleted cidm -> ACIContent SMDSnd $ CISndDeleted cidm
  DBJCIRcvDeleted cidm -> ACIContent SMDRcv $ CIRcvDeleted cidm
  DBJCISndCall {status, duration} -> ACIContent SMDSnd $ CISndCall status duration
  DBJCIRcvCall {status, duration} -> ACIContent SMDRcv $ CIRcvCall status duration
  DBJCIRcvIntegrityError (DBME err) -> ACIContent SMDRcv $ CIRcvIntegrityError err
  DBJCIRcvDecryptionError err n -> ACIContent SMDRcv $ CIRcvDecryptionError err n
  DBJCIRcvGroupInvitation {groupInvitation, memberRole} -> ACIContent SMDRcv $ CIRcvGroupInvitation groupInvitation memberRole
  DBJCISndGroupInvitation {groupInvitation, memberRole} -> ACIContent SMDSnd $ CISndGroupInvitation groupInvitation memberRole
  DBJCIRcvDirectEvent (RDE rde) -> ACIContent SMDRcv $ CIRcvDirectEvent rde
  DBJCIRcvGroupEvent (RGE rge) -> ACIContent SMDRcv $ CIRcvGroupEvent rge
  DBJCISndGroupEvent (SGE sge) -> ACIContent SMDSnd $ CISndGroupEvent sge
  DBJCIRcvConnEvent (RCE rce) -> ACIContent SMDRcv $ CIRcvConnEvent rce
  DBJCISndConnEvent (SCE sce) -> ACIContent SMDSnd $ CISndConnEvent sce
  DBJCIRcvChatFeature {feature, enabled, param} -> ACIContent SMDRcv $ CIRcvChatFeature feature enabled param
  DBJCISndChatFeature {feature, enabled, param} -> ACIContent SMDSnd $ CISndChatFeature feature enabled param
  DBJCIRcvChatPreference {feature, allowed, param} -> ACIContent SMDRcv $ CIRcvChatPreference feature allowed param
  DBJCISndChatPreference {feature, allowed, param} -> ACIContent SMDSnd $ CISndChatPreference feature allowed param
  DBJCIRcvGroupFeature {groupFeature, preference, param, memberRole_} -> ACIContent SMDRcv $ CIRcvGroupFeature groupFeature preference param memberRole_
  DBJCISndGroupFeature {groupFeature, preference, param, memberRole_} -> ACIContent SMDSnd $ CISndGroupFeature groupFeature preference param memberRole_
  DBJCIRcvChatFeatureRejected {feature} -> ACIContent SMDRcv $ CIRcvChatFeatureRejected feature
  DBJCIRcvGroupFeatureRejected {groupFeature} -> ACIContent SMDRcv $ CIRcvGroupFeatureRejected groupFeature
  DBJCISndModerated -> ACIContent SMDSnd CISndModerated
  DBJCIRcvModerated -> ACIContent SMDRcv CIRcvModerated
  DBJCIRcvBlocked -> ACIContent SMDRcv CIRcvBlocked
  DBJCISndDirectE2EEInfo e2eeInfo -> ACIContent SMDSnd $ CISndDirectE2EEInfo e2eeInfo
  DBJCIRcvDirectE2EEInfo e2eeInfo -> ACIContent SMDRcv $ CIRcvDirectE2EEInfo e2eeInfo
  DBJCISndGroupE2EEInfo e2eeInfo -> ACIContent SMDSnd $ CISndGroupE2EEInfo e2eeInfo
  DBJCIRcvGroupE2EEInfo e2eeInfo -> ACIContent SMDRcv $ CIRcvGroupE2EEInfo e2eeInfo
  DBJCIInvalidJSON dir json -> case fromMsgDirection dir of
    AMsgDirection d -> ACIContent d $ CIInvalidJSON json

data CICallStatus
  = CISCallPending
  | CISCallMissed
  | CISCallRejected -- only possible for received calls, not on type level
  | CISCallAccepted
  | CISCallNegotiated
  | CISCallProgress
  | CISCallEnded
  | CISCallError
  deriving (Show)

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

callComplete :: CICallStatus -> Bool
callComplete = \case
  CISCallMissed -> True
  CISCallRejected -> True
  CISCallEnded -> True
  CISCallError -> True
  _ -> False

$(JQ.deriveJSON defaultJSON ''E2EInfo)

$(JQ.deriveJSON (enumJSON $ dropPrefix "MDE") ''MsgDecryptError)

$(JQ.deriveJSON (enumJSON $ dropPrefix "CIGIS") ''CIGroupInvitationStatus)

$(JQ.deriveJSON defaultJSON ''CIGroupInvitation)

$(JQ.deriveJSON (enumJSON $ dropPrefix "CISCall") ''CICallStatus)

-- platform specific
$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "JCI") ''JSONCIContent)

-- platform independent
$(JQ.deriveJSON (singleFieldJSON $ dropPrefix "DBJCI") ''DBJSONCIContent)

-- platform independent
instance MsgDirectionI d => ToField (CIContent d) where
  toField = toField . encodeJSON . dbJsonCIContent

-- platform specific
instance MsgDirectionI d => ToJSON (CIContent d) where
  toJSON = J.toJSON . jsonCIContent
  toEncoding = J.toEncoding . jsonCIContent

instance MsgDirectionI d => FromJSON (CIContent d) where
  parseJSON v = (\(ACIContent _ c) -> checkDirection c) <$?> J.parseJSON v

-- platform independent
dbParseACIContent :: Text -> Either String ACIContent
dbParseACIContent = fmap aciContentDBJSON . J.eitherDecodeStrict' . encodeUtf8

-- platform specific
instance FromJSON ACIContent where
  parseJSON = fmap aciContentJSON . J.parseJSON

sndMsgContentTag :: Text
sndMsgContentTag = "sndMsgContent"

rcvMsgContentTag :: Text
rcvMsgContentTag = "rcvMsgContent"

toCIContentTag :: CIContent e -> Text
toCIContentTag ciContent = case ciContent of
  CISndMsgContent _ -> sndMsgContentTag
  CIRcvMsgContent _ -> rcvMsgContentTag
  CISndDeleted _ -> "sndDeleted"
  CIRcvDeleted _ -> "rcvDeleted"
  CISndCall {} -> "sndCall"
  CIRcvCall {} -> "rcvCall"
  CIRcvIntegrityError _ -> "rcvIntegrityError"
  CIRcvDecryptionError {} -> "rcvDecryptionError"
  CIRcvGroupInvitation {} -> "rcvGroupInvitation"
  CISndGroupInvitation {} -> "sndGroupInvitation"
  CIRcvDirectEvent _ -> "rcvDirectEvent"
  CIRcvGroupEvent _ -> "rcvGroupEvent"
  CISndGroupEvent _ -> "sndGroupEvent"
  CIRcvConnEvent _ -> "rcvConnEvent"
  CISndConnEvent _ -> "sndConnEvent"
  CIRcvChatFeature {} -> "rcvChatFeature"
  CISndChatFeature {} -> "sndChatFeature"
  CIRcvChatPreference {} -> "rcvChatPreference"
  CISndChatPreference {} -> "sndChatPreference"
  CIRcvGroupFeature {} -> "rcvGroupFeature"
  CISndGroupFeature {} -> "sndGroupFeature"
  CIRcvChatFeatureRejected _ -> "rcvChatFeatureRejected"
  CIRcvGroupFeatureRejected _ -> "rcvGroupFeatureRejected"
  CISndModerated -> "sndModerated"
  CIRcvModerated -> "rcvModerated"
  CIRcvBlocked -> "rcvBlocked"
  CISndDirectE2EEInfo _ -> "sndDirectE2EEInfo"
  CIRcvDirectE2EEInfo _ -> "rcvDirectE2EEInfo"
  CISndGroupE2EEInfo _ -> "sndGroupE2EEInfo"
  CIRcvGroupE2EEInfo _ -> "rcvGroupE2EEInfo"
  CIInvalidJSON _ -> "invalidJSON"
