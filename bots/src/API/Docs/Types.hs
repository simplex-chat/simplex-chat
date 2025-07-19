{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module API.Docs.Types where

import API.TypeInfo
import Data.Text (Text)
import GHC.Generics
import Simplex.Chat.Controller
import Simplex.Chat.Markdown
import Simplex.Chat.Messages
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Messages.CIContent.Events
import Simplex.Chat.Protocol
import Simplex.Chat.Store.Groups
import Simplex.Chat.Store.Profiles
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences
import Simplex.Chat.Types.Shared
import Simplex.Chat.Types.UITheme
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Crypto.File
import Simplex.Messaging.Parsers (dropPrefix, fstToLower)
import Simplex.Messaging.Protocol (BlockingInfo (..), BlockingReason (..))

data CTDoc = CTDoc
  { typeInfo :: SumTypeInfo,
    jsonEncoding :: Maybe SumTypeJsonEncoding,
    consPrefix :: String,
    typeDescr :: Text
  }

docTypeName :: CTDoc -> String
docTypeName CTDoc {typeInfo = STI name _} = name

chatTypesDocs :: [CTDoc]
chatTypesDocs = map toCTDoc chatTypesDocsData
  where
    toCTDoc (typeInfo, jsonEncoding, consPrefix, typeDescr) = CTDoc {typeInfo, jsonEncoding, consPrefix, typeDescr}

primitiveTypes :: [ConsName]
primitiveTypes = ["Bool", "Int", "Int64", "Word32", "Double", "String", "UTCTime", "JSONObject"]

data SumTypeJsonEncoding = STRecord | STUnion | STEnum | STEnum' (RecordTypeInfo -> Maybe String)

enumEnc :: String -> String -> String -> RecordTypeInfo -> Maybe String
enumEnc unknown pfx sfx RecordTypeInfo {consName}
  | consName == unknown = Nothing
  | otherwise = Just $ dropSuffix sfx $ dropPrefix pfx consName

dropSuffix :: String -> String -> String
dropSuffix sfx s =
  let (s', sfx') = splitAt (length s - length sfx) s
   in fstToLower $ if sfx' == sfx then s' else s

chatTypesDocsData :: [(SumTypeInfo, Maybe SumTypeJsonEncoding, String, Text)]
chatTypesDocsData =
  [ ((sti @JSONChatInfo) {typeName = "ChatInfo"}, Just STUnion, "JCInfo", ""),
    ((sti @JSONCIContent) {typeName = "CIContent"}, Just STUnion, "JCI", ""),
    ((sti @JSONCIDeleted) {typeName = "CIDeleted"}, Just STUnion, "JCID", ""),
    ((sti @JSONCIDirection) {typeName = "CIDirection"}, Just STUnion, "JCI", ""),
    ((sti @JSONCIDirection) {typeName = "CIQDirection"}, Just STUnion, "JCI", ""), -- this is incorrect, because of how JSON instances for CIQDirection is defined
    ((sti @JSONCIFileStatus) {typeName = "CIFileStatus"}, Just STUnion, "JCIFS", ""),
    ((sti @JSONCIStatus) {typeName = "CIStatus"}, Just STUnion, "JCIS", ""),
    (STI "AChatItem" [RecordTypeInfo "AChatItem" [FieldInfo "chatInfo" (ti "ChatInfo"), FieldInfo "chatItem" (ti "ChatItem")]], Just STRecord, "", ""),
    (STI "ACIReaction" [RecordTypeInfo "ACIReaction" [FieldInfo "chatInfo" (ti "ChatInfo"), FieldInfo "chatReaction" (ti "CIReaction")]], Just STRecord, "", ""),
    (STI "ChatError" [], Just STUnion, "Chat", ""),
    (STI "ConnStatus" [], Just STRecord, "", ""),
    (STI "FormatColor" [], Just STRecord, "", ""),
    (STI "VersionRange" [RecordTypeInfo "VersionRange" [FieldInfo "minVersion" (ti "Int"), FieldInfo "maxVersion" (ti "Int")]], Just STRecord, "", ""),
    (sti @(ChatItem 'CTDirect 'MDSnd), Just STRecord, "", ""),
    (sti @(CIFile 'MDSnd), Just STRecord, "", ""),
    (sti @(CIMeta 'CTDirect 'MDSnd), Just STRecord, "", ""),
    (sti @(CIQuote 'CTDirect), Just STRecord, "", ""), -- this is incorrect, because chatDir should be optional, because of how CIQDirection JSON instance is defined
    (sti @(CIReaction 'CTDirect 'MDSnd), Just STRecord, "", ""),
    (sti @(ContactUserPref SimplePreference), Just STUnion, "CUP", ""),
    (sti @(ContactUserPreference SimplePreference), Just STRecord, "", ""),
    (sti @(CreatedConnLink 'CMContact), Just STRecord, "", ""),
    (sti @AddressSettings, Just STRecord, "", ""),
    (sti @AutoAccept, Just STRecord, "", ""),
    (sti @BlockingInfo, Just STRecord, "", ""),
    (sti @BlockingReason, Just STEnum, "BR", ""),
    (sti @BusinessChatInfo, Just STRecord, "", ""),
    (sti @BusinessChatType, Just STEnum, "BC", ""),
    (sti @ChatDeleteMode, Just STUnion, "CDM", ""),
    (sti @ChatFeature, Just STEnum, "CF", ""),
    (sti @ChatItemDeletion, Just STRecord, "", "Message deletion result."),
    (sti @ChatRef, Nothing, "", ""),
    (sti @ChatSettings, Just STRecord, "", ""),
    (sti @ChatType, Just STEnum, "CT", ""),
    (sti @ChatWallpaper, Just STRecord, "", ""),
    (sti @ChatWallpaperScale, Just STEnum, "CWS", ""),
    (sti @CICallStatus, Just STEnum, "CISCall", ""),
    (sti @CIDeleteMode, Just STEnum, "CIDM", ""),
    (sti @CIForwardedFrom, Just STUnion, "CIFF", ""),
    (sti @CIGroupInvitation, Just STRecord, "", ""),
    (sti @CIGroupInvitationStatus, Just STEnum, "CIGIS", ""),
    (sti @CIMention, Just STRecord, "", ""),
    (sti @CIMentionMember, Just STRecord, "", ""),
    (sti @CIReactionCount, Just STRecord, "", ""),
    (sti @CITimed, Just STRecord, "", ""),
    (sti @ComposedMessage, Just STRecord, "", ""),
    (sti @Connection, Just STRecord, "", ""),
    (sti @ConnectionMode, Just STEnum, "CM", ""), -- incorrect, should be inv/con
    (sti @ConnectionPlan, Just STUnion, "CP", ""),
    (sti @ConnType, Just STEnum, "", ""), -- incorrect
    (sti @Contact, Just STRecord, "", ""),
    (sti @ContactAddressPlan, Just STUnion, "CAP", ""),
    (sti @ContactShortLinkData, Just STRecord, "", ""),
    (sti @ContactStatus, Just STEnum, "CS", ""),
    (sti @ContactUserPreferences, Just STRecord, "", ""),
    (sti @CryptoFile, Just STRecord, "", ""),
    (sti @CryptoFileArgs, Just STRecord, "", ""),
    (sti @E2EInfo, Just STRecord, "", ""),
    (sti @FeatureAllowed, Just STEnum, "FA", ""),
    (sti @FileDescr, Just STRecord, "", ""),
    (sti @FileError, Just STUnion, "FileErr", ""),
    (sti @FileInvitation, Just STRecord, "", ""),
    (sti @FileProtocol, Just STEnum, "FP", ""), -- incorrect, should be lowercase
    (sti @FileStatus, Just STEnum, "FS", ""),
    (sti @FileTransferMeta, Just STRecord, "", ""),
    (sti @Format, Just STUnion, "", ""),
    (sti @FormattedText, Just STRecord, "", ""),
    (sti @FullGroupPreferences, Just STRecord, "", ""),
    (sti @FullPreferences, Just STRecord, "", ""),
    (sti @GroupChatScope, Just STUnion, "GCS", ""),
    (sti @GroupChatScopeInfo, Just STUnion, "GCSI", ""),
    (sti @GroupFeature, Just STEnum, "GF", ""),
    (sti @GroupFeatureEnabled, Just STEnum, "FE", ""),
    (sti @GroupInfo, Just STRecord, "", ""),
    (sti @GroupInfoSummary, Just STRecord, "", ""),
    (sti @GroupLink, Just STRecord, "", ""),
    (sti @GroupLinkPlan, Just STUnion, "GLP", ""),
    (sti @GroupMember, Just STRecord, "", ""),
    (sti @GroupMemberAdmission, Just STRecord, "", ""),
    (sti @GroupMemberCategory, Just (STEnum' $ enumEnc "" "GC" "Member"), "", ""),
    (sti @GroupMemberRef, Just STRecord, "", ""),
    (sti @GroupMemberRole, Just STEnum, "GR", ""),
    (sti @GroupMemberSettings, Just STRecord, "", ""),
    (sti @GroupMemberStatus, Just STEnum, "", ""), -- incorrect
    (sti @GroupPreference, Just STRecord, "", ""),
    (sti @GroupPreferences, Just STRecord, "", ""),
    (sti @GroupProfile, Just STRecord, "", ""),
    (sti @GroupShortLinkData, Just STRecord, "", ""),
    (sti @GroupSummary, Just STRecord, "", ""),
    (sti @GroupSupportChat, Just STRecord, "", ""),
    (sti @InlineFileMode, Just STEnum, "IFM", ""),
    (sti @InvitationLinkPlan, Just STUnion, "ILP", ""),
    (sti @InvitedBy, Just STUnion, "IB", ""),
    (sti @LinkContent, Just STUnion, "LC", ""),
    (sti @LinkPreview, Just STRecord, "", ""),
    (sti @LocalProfile, Just STRecord, "", ""),
    (sti @MemberCriteria, Just STEnum, "MC", ""),
    (sti @MsgChatLink, Just STUnion, "MCL", "Connection link sent in a message - only short links are allowed."),
    (sti @MsgContent, Just STUnion, "MC", ""),
    (sti @MsgDecryptError, Just STEnum, "MDE", ""),
    (sti @MsgDirection, Just STEnum, "MD", ""),
    (sti @MsgErrorType, Just STUnion, "", ""), -- check, may be correct?
    (sti @MsgFilter, Just STEnum, "MF", ""),
    (sti @MsgReaction, Just STUnion, "MR", ""),
    (sti @MsgReceiptStatus, Just STEnum, "MR", ""),
    (sti @NewUser, Just STRecord, "", ""),
    (sti @NoteFolder, Just STRecord, "", ""),
    (sti @PendingContactConnection, Just STRecord, "", ""),
    (sti @PrefEnabled, Just STRecord, "", ""),
    (sti @Preferences, Just STRecord, "", ""),
    (sti @PreparedContact, Just STRecord, "", ""),
    (sti @PreparedGroup, Just STRecord, "", ""),
    (sti @Profile, Just STRecord, "", ""),
    (sti @RatchetSyncState, Just STEnum, "RS", ""),
    (sti @RcvConnEvent, Just STUnion, "RCE", ""),
    (sti @RcvDirectEvent, Just STUnion, "RDE", ""),
    (sti @RcvFileDescr, Just STRecord, "", ""),
    (sti @RcvFileInfo, Just STRecord, "", ""),
    (sti @RcvFileStatus, Just STUnion, "RFS", ""),
    (sti @RcvFileTransfer, Just STRecord, "", ""),
    (sti @RcvGroupEvent, Just STUnion, "RGE", ""),
    (sti @ReportReason, Just (STEnum' $ enumEnc "RRUnknown" "RR" ""), "RR", ""),
    (sti @RoleGroupPreference, Just STRecord, "", ""),
    (sti @SecurityCode, Just STRecord, "", ""),
    (sti @SendRef, Nothing, "", ""),
    (sti @SimplePreference, Just STRecord, "", ""),
    (sti @SimplexLinkType, Just STEnum, "XL", ""),
    (sti @SndCIStatusProgress, Just STEnum, "SSP", ""),
    (sti @SndConnEvent, Just STUnion, "SCE", ""),
    (sti @SndError, Just STUnion, "SndErr", ""),
    (sti @SndFileTransfer, Just STRecord, "", ""),
    (sti @SndGroupEvent, Just STUnion, "SGE", ""),
    (sti @SrvError, Just STUnion, "SrvErr", ""),
    (sti @SwitchPhase, Just STEnum, "SP", ""),
    (sti @TimedMessagesGroupPreference, Just STRecord, "", ""),
    (sti @TimedMessagesPreference, Just STRecord, "", ""),
    (sti @UIColorMode, Just STEnum, "UCM", ""),
    (sti @UIColors, Just STRecord, "", ""),
    (sti @UIThemeEntityOverride, Just STRecord, "", ""),
    (sti @UIThemeEntityOverrides, Just STRecord, "", ""),
    (sti @UpdatedMessage, Just STRecord, "", ""),
    (sti @User, Just STRecord, "", ""),
    (sti @UserContactLink, Just STRecord, "", ""),
    (sti @UserContactRequest, Just STRecord, "", ""),
    (sti @UserInfo, Just STRecord, "", ""),
    (sti @UserProfileUpdateSummary, Just STRecord, "", ""),
    (sti @UserPwdHash, Just STRecord, "", ""),
    (sti @XFTPRcvFile, Just STRecord, "", ""),
    (sti @XFTPSndFile, Just STRecord, "", "")
    -- ((sti @(Chat 'CTDirect)) {typeName = "AChat"} , Just STRecord, "", ""),
    -- (sti @ChatError, Just STUnion, "Chat", ""),
    -- (sti @ChatItemInfo, Just STRecord, "", ""),
    -- (sti @ChatItemVersion, Just STRecord, "", ""),
    -- (sti @ChatListQuery, Just STUnion, "CLQ", ""),
    -- (sti @ChatName, Just STRecord, "", ""),
    -- (sti @ChatPagination, Nothing, "CP", ""),
    -- (sti @ChatStats, Just STRecord, "", ""),
    -- (sti @ConnectionStats, Just STRecord, "", ""),
    -- (sti @Group, Just STRecord, "", ""),
    -- (sti @GroupSndStatus, Just STUnion, "GSS", ""),
    -- (sti @MemberDeliveryStatus, Just STRecord, "", ""),
    -- (sti @MemberReaction, Just STRecord, "", ""),
    -- (sti @MsgContentTag, Just (STEnum' $ enumEnc "MCUnknown_" "MC" "_"), "", ""),
    -- (sti @NavigationInfo, Just STRecord, "", ""),
    -- (sti @PaginationByTime, Nothing, "", ""),
    -- (sti @RcvQueueInfo, Just STRecord, "", ""),
    -- (sti @RcvSwitchStatus, Just STEnum, "", ""), -- incorrect
    -- (sti @SndQueueInfo, Just STRecord, "", ""),
    -- (sti @SndSwitchStatus, Just STEnum, "", ""), -- incorrect
 ]

data SimplePreference = SimplePreference {allow :: FeatureAllowed} deriving (Generic)

data RoleGroupPreference = RoleGroupPreference {enable :: GroupFeatureEnabled, role :: Maybe GroupMemberRole} deriving (Generic)

deriving instance Generic (ChatItem c d)
deriving instance Generic (CIFile d)
deriving instance Generic (CIMeta c d)
deriving instance Generic (CIQuote d)
deriving instance Generic (CIReaction c d)
deriving instance Generic (ContactUserPref p)
deriving instance Generic (ContactUserPreference p)
deriving instance Generic (CreatedConnLink m)
deriving instance Generic AddressSettings
deriving instance Generic AutoAccept
deriving instance Generic BlockingInfo
deriving instance Generic BlockingReason
deriving instance Generic BusinessChatInfo
deriving instance Generic BusinessChatType
deriving instance Generic ChatDeleteMode
deriving instance Generic ChatFeature
deriving instance Generic ChatItemDeletion
deriving instance Generic ChatRef
deriving instance Generic ChatSettings
deriving instance Generic ChatType
deriving instance Generic ChatWallpaper
deriving instance Generic ChatWallpaperScale
deriving instance Generic CICallStatus
deriving instance Generic CIDeleteMode
deriving instance Generic CIForwardedFrom
deriving instance Generic CIGroupInvitation
deriving instance Generic CIGroupInvitationStatus
deriving instance Generic CIMention
deriving instance Generic CIMentionMember
deriving instance Generic CIReactionCount
deriving instance Generic CITimed
deriving instance Generic ComposedMessage
deriving instance Generic Connection
deriving instance Generic ConnectionMode
deriving instance Generic ConnectionPlan
deriving instance Generic ConnType
deriving instance Generic Contact
deriving instance Generic ContactAddressPlan
deriving instance Generic ContactShortLinkData
deriving instance Generic ContactStatus
deriving instance Generic ContactUserPreferences
deriving instance Generic CryptoFile
deriving instance Generic CryptoFileArgs
deriving instance Generic E2EInfo
deriving instance Generic FeatureAllowed
deriving instance Generic FileDescr
deriving instance Generic FileError
deriving instance Generic FileInvitation
deriving instance Generic FileProtocol
deriving instance Generic FileStatus
deriving instance Generic FileTransferMeta
deriving instance Generic Format
deriving instance Generic FormattedText
deriving instance Generic FullGroupPreferences
deriving instance Generic FullPreferences
deriving instance Generic GroupChatScope
deriving instance Generic GroupChatScopeInfo
deriving instance Generic GroupFeature
deriving instance Generic GroupFeatureEnabled
deriving instance Generic GroupInfo
deriving instance Generic GroupInfoSummary
deriving instance Generic GroupLink
deriving instance Generic GroupLinkPlan
deriving instance Generic GroupMember
deriving instance Generic GroupMemberAdmission
deriving instance Generic GroupMemberCategory
deriving instance Generic GroupMemberRef
deriving instance Generic GroupMemberRole
deriving instance Generic GroupMemberSettings
deriving instance Generic GroupMemberStatus
deriving instance Generic GroupPreference
deriving instance Generic GroupPreferences
deriving instance Generic GroupProfile
deriving instance Generic GroupShortLinkData
deriving instance Generic GroupSummary
deriving instance Generic GroupSupportChat
deriving instance Generic InlineFileMode
deriving instance Generic InvitationLinkPlan
deriving instance Generic InvitedBy
deriving instance Generic JSONChatInfo
deriving instance Generic JSONCIContent
deriving instance Generic JSONCIDeleted
deriving instance Generic JSONCIDirection -- possibly CIQDirection needs another JSON instance adapter
deriving instance Generic JSONCIFileStatus
deriving instance Generic JSONCIStatus
deriving instance Generic LinkContent
deriving instance Generic LinkPreview
deriving instance Generic LocalProfile
deriving instance Generic MemberCriteria
deriving instance Generic MsgChatLink
deriving instance Generic MsgContent
deriving instance Generic MsgDecryptError
deriving instance Generic MsgDirection
deriving instance Generic MsgErrorType
deriving instance Generic MsgFilter
deriving instance Generic MsgReaction
deriving instance Generic MsgReceiptStatus
deriving instance Generic NewUser
deriving instance Generic NoteFolder
deriving instance Generic PendingContactConnection
deriving instance Generic PrefEnabled
deriving instance Generic Preferences
deriving instance Generic PreparedContact
deriving instance Generic PreparedGroup
deriving instance Generic Profile
deriving instance Generic RatchetSyncState
deriving instance Generic RcvConnEvent
deriving instance Generic RcvDirectEvent
deriving instance Generic RcvFileDescr
deriving instance Generic RcvFileInfo
deriving instance Generic RcvFileStatus
deriving instance Generic RcvFileTransfer
deriving instance Generic RcvGroupEvent
deriving instance Generic ReportReason
deriving instance Generic SecurityCode
deriving instance Generic SendRef
deriving instance Generic SimplexLinkType
deriving instance Generic SndCIStatusProgress
deriving instance Generic SndConnEvent
deriving instance Generic SndError
deriving instance Generic SndFileTransfer
deriving instance Generic SndGroupEvent
deriving instance Generic SrvError
deriving instance Generic SwitchPhase
deriving instance Generic TimedMessagesGroupPreference
deriving instance Generic TimedMessagesPreference
deriving instance Generic UIColorMode
deriving instance Generic UIColors
deriving instance Generic UIThemeEntityOverride
deriving instance Generic UIThemeEntityOverrides
deriving instance Generic UpdatedMessage
deriving instance Generic User
deriving instance Generic UserContactLink
deriving instance Generic UserContactRequest
deriving instance Generic UserInfo
deriving instance Generic UserProfileUpdateSummary
deriving instance Generic UserPwdHash
deriving instance Generic XFTPRcvFile
deriving instance Generic XFTPSndFile

-- deriving instance Generic (Chat c)
-- deriving instance Generic ChatError
-- deriving instance Generic ChatItemInfo
-- deriving instance Generic ChatItemVersion
-- deriving instance Generic ChatListQuery
-- deriving instance Generic ChatName
-- deriving instance Generic ChatPagination
-- deriving instance Generic ChatStats
-- deriving instance Generic ConnectionStats
-- deriving instance Generic Group
-- deriving instance Generic GroupSndStatus
-- deriving instance Generic MemberDeliveryStatus
-- deriving instance Generic MemberReaction
-- deriving instance Generic MsgContentTag
-- deriving instance Generic NavigationInfo
-- deriving instance Generic PaginationByTime
-- deriving instance Generic RcvQueueInfo
-- deriving instance Generic RcvSwitchStatus
-- deriving instance Generic SndQueueInfo
-- deriving instance Generic SndSwitchStatus
