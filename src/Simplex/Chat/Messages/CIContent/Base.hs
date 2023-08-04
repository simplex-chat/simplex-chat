{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Simplex.Chat.Messages.CIContent.Base where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1)
import Data.Word (Word32)
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.Generics (Generic)
import Simplex.Chat.Messages.CIContent.Types
import Simplex.Chat.Protocol
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences
import Simplex.Chat.Types.Util
import Simplex.Messaging.Agent.Protocol (MsgErrorType (..))
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (dropPrefix, enumJSON, fstToLower, singleFieldJSON, sumTypeJSON)
import Simplex.Messaging.Util (tshow)

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
  CIRcvGroupEvent :: RcvGroupEvent -> CIContent 'MDRcv
  CISndGroupEvent :: SndGroupEvent -> CIContent 'MDSnd
  CIRcvConnEvent :: RcvConnEvent -> CIContent 'MDRcv
  CISndConnEvent :: SndConnEvent -> CIContent 'MDSnd
  CIRcvChatFeature :: ChatFeature -> PrefEnabled -> Maybe Int -> CIContent 'MDRcv
  CISndChatFeature :: ChatFeature -> PrefEnabled -> Maybe Int -> CIContent 'MDSnd
  CIRcvChatPreference :: ChatFeature -> FeatureAllowed -> Maybe Int -> CIContent 'MDRcv
  CISndChatPreference :: ChatFeature -> FeatureAllowed -> Maybe Int -> CIContent 'MDSnd
  CIRcvGroupFeature :: GroupFeature -> GroupPreference -> Maybe Int -> CIContent 'MDRcv
  CISndGroupFeature :: GroupFeature -> GroupPreference -> Maybe Int -> CIContent 'MDSnd
  CIRcvChatFeatureRejected :: ChatFeature -> CIContent 'MDRcv
  CIRcvGroupFeatureRejected :: GroupFeature -> CIContent 'MDRcv
  CISndModerated :: CIContent 'MDSnd
  CIRcvModerated :: CIContent 'MDRcv
  CIInvalidJSON :: Text -> CIContent d
-- ^ This type is used both in API and in DB, so we use different JSON encodings for the database and for the API
-- ! ^ Nested sum types also have to use different encodings for database and API
-- ! ^ to avoid breaking cross-platform compatibility, see RcvGroupEvent and SndGroupEvent

deriving instance Show (CIContent d)

ciMsgContent :: CIContent d -> Maybe MsgContent
ciMsgContent = \case
  CISndMsgContent mc -> Just mc
  CIRcvMsgContent mc -> Just mc
  _ -> Nothing

data MsgDecryptError = MDERatchetHeader | MDETooManySkipped | MDERatchetEarlier | MDEOther
  deriving (Eq, Show, Generic)

instance ToJSON MsgDecryptError where
  toJSON = J.genericToJSON . enumJSON $ dropPrefix "MDE"
  toEncoding = J.genericToEncoding . enumJSON $ dropPrefix "MDE"

instance FromJSON MsgDecryptError where
  parseJSON = J.genericParseJSON . enumJSON $ dropPrefix "MDE"

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
    CIRcvGroupEvent rge -> case rge of
      RGEMemberAdded {} -> False
      RGEMemberConnected -> False
      RGEMemberLeft -> False
      RGEMemberRole {} -> False
      RGEUserRole _ -> True
      RGEMemberDeleted {} -> False
      RGEUserDeleted -> True
      RGEGroupDeleted -> True
      RGEGroupUpdated _ -> False
      RGEInvitedViaGroupLink -> False
    CIRcvConnEvent _ -> True
    CIRcvChatFeature {} -> False
    CIRcvChatPreference {} -> False
    CIRcvGroupFeature {} -> False
    CIRcvChatFeatureRejected _ -> True
    CIRcvGroupFeatureRejected _ -> True
    CIRcvModerated -> True
    CIInvalidJSON _ -> False

newtype DBMsgErrorType = DBME MsgErrorType

instance FromJSON DBMsgErrorType where
  parseJSON v = DBME <$> J.genericParseJSON (singleFieldJSON fstToLower) v

instance ToJSON DBMsgErrorType where
  toJSON (DBME v) = J.genericToJSON (singleFieldJSON fstToLower) v
  toEncoding (DBME v) = J.genericToEncoding (singleFieldJSON fstToLower) v

data CIGroupInvitation = CIGroupInvitation
  { groupId :: GroupId,
    groupMemberId :: GroupMemberId,
    localDisplayName :: GroupName,
    groupProfile :: GroupProfile,
    status :: CIGroupInvitationStatus
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON CIGroupInvitation where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

data CIGroupInvitationStatus
  = CIGISPending
  | CIGISAccepted
  | CIGISRejected
  | CIGISExpired
  deriving (Eq, Show, Generic)

instance FromJSON CIGroupInvitationStatus where
  parseJSON = J.genericParseJSON . enumJSON $ dropPrefix "CIGIS"

instance ToJSON CIGroupInvitationStatus where
  toJSON = J.genericToJSON . enumJSON $ dropPrefix "CIGIS"
  toEncoding = J.genericToEncoding . enumJSON $ dropPrefix "CIGIS"

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
  CIRcvGroupEvent event -> rcvGroupEventToText event
  CISndGroupEvent event -> sndGroupEventToText event
  CIRcvConnEvent event -> rcvConnEventToText event
  CISndConnEvent event -> sndConnEventToText event
  CIRcvChatFeature feature enabled param -> featureStateText feature enabled param
  CISndChatFeature feature enabled param -> featureStateText feature enabled param
  CIRcvChatPreference feature allowed param -> prefStateText feature allowed param
  CISndChatPreference feature allowed param -> "you " <> prefStateText feature allowed param
  CIRcvGroupFeature feature pref param -> groupPrefStateText feature pref param
  CISndGroupFeature feature pref param -> groupPrefStateText feature pref param
  CIRcvChatFeatureRejected feature -> chatFeatureNameText feature <> ": received, prohibited"
  CIRcvGroupFeatureRejected feature -> groupFeatureNameText feature <> ": received, prohibited"
  CISndModerated -> ciModeratedText
  CIRcvModerated -> ciModeratedText
  CIInvalidJSON _ -> "invalid content JSON"

ciGroupInvitationToText :: CIGroupInvitation -> GroupMemberRole -> Text
ciGroupInvitationToText CIGroupInvitation {groupProfile = GroupProfile {displayName, fullName}} role =
  "invitation to join group " <> displayName <> optionalFullName displayName fullName <> " as " <> (decodeLatin1 . strEncode $ role)

msgIntegrityError :: MsgErrorType -> Text
msgIntegrityError = \case
  MsgSkipped fromId toId ->
    "skipped message ID " <> tshow fromId
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
    counter_ = if n == 1 then Nothing else Just $ tshow n <> " messages"
    counter = maybe "" (", " <>) counter_

msgDirToModeratedContent_ :: SMsgDirection d -> CIContent d
msgDirToModeratedContent_ = \case
  SMDRcv -> CIRcvModerated
  SMDSnd -> CISndModerated

ciModeratedText :: Text
ciModeratedText = "moderated"

-- platform independent
instance MsgDirectionI d => ToField (CIContent d) where
  toField = toField . encodeJSON . dbJsonCIContent

-- platform specific
instance MsgDirectionI d => ToJSON (CIContent d) where
  toJSON = J.toJSON . jsonCIContent
  toEncoding = J.toEncoding . jsonCIContent

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
  | JCIRcvGroupEvent {rcvGroupEvent :: RcvGroupEvent}
  | JCISndGroupEvent {sndGroupEvent :: SndGroupEvent}
  | JCIRcvConnEvent {rcvConnEvent :: RcvConnEvent}
  | JCISndConnEvent {sndConnEvent :: SndConnEvent}
  | JCIRcvChatFeature {feature :: ChatFeature, enabled :: PrefEnabled, param :: Maybe Int}
  | JCISndChatFeature {feature :: ChatFeature, enabled :: PrefEnabled, param :: Maybe Int}
  | JCIRcvChatPreference {feature :: ChatFeature, allowed :: FeatureAllowed, param :: Maybe Int}
  | JCISndChatPreference {feature :: ChatFeature, allowed :: FeatureAllowed, param :: Maybe Int}
  | JCIRcvGroupFeature {groupFeature :: GroupFeature, preference :: GroupPreference, param :: Maybe Int}
  | JCISndGroupFeature {groupFeature :: GroupFeature, preference :: GroupPreference, param :: Maybe Int}
  | JCIRcvChatFeatureRejected {feature :: ChatFeature}
  | JCIRcvGroupFeatureRejected {groupFeature :: GroupFeature}
  | JCISndModerated
  | JCIRcvModerated
  | JCIInvalidJSON {direction :: MsgDirection, json :: Text}
  deriving (Generic)

instance FromJSON JSONCIContent where
  parseJSON = J.genericParseJSON . sumTypeJSON $ dropPrefix "JCI"

instance ToJSON JSONCIContent where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "JCI"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "JCI"

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
  CIRcvGroupEvent rcvGroupEvent -> JCIRcvGroupEvent {rcvGroupEvent}
  CISndGroupEvent sndGroupEvent -> JCISndGroupEvent {sndGroupEvent}
  CIRcvConnEvent rcvConnEvent -> JCIRcvConnEvent {rcvConnEvent}
  CISndConnEvent sndConnEvent -> JCISndConnEvent {sndConnEvent}
  CIRcvChatFeature feature enabled param -> JCIRcvChatFeature {feature, enabled, param}
  CISndChatFeature feature enabled param -> JCISndChatFeature {feature, enabled, param}
  CIRcvChatPreference feature allowed param -> JCIRcvChatPreference {feature, allowed, param}
  CISndChatPreference feature allowed param -> JCISndChatPreference {feature, allowed, param}
  CIRcvGroupFeature groupFeature preference param -> JCIRcvGroupFeature {groupFeature, preference, param}
  CISndGroupFeature groupFeature preference param -> JCISndGroupFeature {groupFeature, preference, param}
  CIRcvChatFeatureRejected feature -> JCIRcvChatFeatureRejected {feature}
  CIRcvGroupFeatureRejected groupFeature -> JCIRcvGroupFeatureRejected {groupFeature}
  CISndModerated -> JCISndModerated
  CIRcvModerated -> JCISndModerated
  CIInvalidJSON json -> JCIInvalidJSON (toMsgDirection $ msgDirection @d) json

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
  | DBJCIRcvGroupEvent {rcvGroupEvent :: DBRcvGroupEvent}
  | DBJCISndGroupEvent {sndGroupEvent :: DBSndGroupEvent}
  | DBJCIRcvConnEvent {rcvConnEvent :: DBRcvConnEvent}
  | DBJCISndConnEvent {sndConnEvent :: DBSndConnEvent}
  | DBJCIRcvChatFeature {feature :: ChatFeature, enabled :: PrefEnabled, param :: Maybe Int}
  | DBJCISndChatFeature {feature :: ChatFeature, enabled :: PrefEnabled, param :: Maybe Int}
  | DBJCIRcvChatPreference {feature :: ChatFeature, allowed :: FeatureAllowed, param :: Maybe Int}
  | DBJCISndChatPreference {feature :: ChatFeature, allowed :: FeatureAllowed, param :: Maybe Int}
  | DBJCIRcvGroupFeature {groupFeature :: GroupFeature, preference :: GroupPreference, param :: Maybe Int}
  | DBJCISndGroupFeature {groupFeature :: GroupFeature, preference :: GroupPreference, param :: Maybe Int}
  | DBJCIRcvChatFeatureRejected {feature :: ChatFeature}
  | DBJCIRcvGroupFeatureRejected {groupFeature :: GroupFeature}
  | DBJCISndModerated
  | DBJCIRcvModerated
  | DBJCIInvalidJSON {direction :: MsgDirection, json :: Text}
  deriving (Generic)

instance FromJSON DBJSONCIContent where
  parseJSON = J.genericParseJSON . singleFieldJSON $ dropPrefix "DBJCI"

instance ToJSON DBJSONCIContent where
  toJSON = J.genericToJSON . singleFieldJSON $ dropPrefix "DBJCI"
  toEncoding = J.genericToEncoding . singleFieldJSON $ dropPrefix "DBJCI"

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
  CIRcvGroupEvent rge -> DBJCIRcvGroupEvent $ RGE rge
  CISndGroupEvent sge -> DBJCISndGroupEvent $ SGE sge
  CIRcvConnEvent rce -> DBJCIRcvConnEvent $ RCE rce
  CISndConnEvent sce -> DBJCISndConnEvent $ SCE sce
  CIRcvChatFeature feature enabled param -> DBJCIRcvChatFeature {feature, enabled, param}
  CISndChatFeature feature enabled param -> DBJCISndChatFeature {feature, enabled, param}
  CIRcvChatPreference feature allowed param -> DBJCIRcvChatPreference {feature, allowed, param}
  CISndChatPreference feature allowed param -> DBJCISndChatPreference {feature, allowed, param}
  CIRcvGroupFeature groupFeature preference param -> DBJCIRcvGroupFeature {groupFeature, preference, param}
  CISndGroupFeature groupFeature preference param -> DBJCISndGroupFeature {groupFeature, preference, param}
  CIRcvChatFeatureRejected feature -> DBJCIRcvChatFeatureRejected {feature}
  CIRcvGroupFeatureRejected groupFeature -> DBJCIRcvGroupFeatureRejected {groupFeature}
  CISndModerated -> DBJCISndModerated
  CIRcvModerated -> DBJCIRcvModerated
  CIInvalidJSON json -> DBJCIInvalidJSON (toMsgDirection $ msgDirection @d) json
