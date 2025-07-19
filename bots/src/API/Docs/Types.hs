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
docTypeName CTDoc {typeInfo = SumTypeInfo name _} = name

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
  [ ((sumTypeInfo @JSONChatInfo) {typeName = "ChatInfo"}, Just STUnion, "JCInfo", ""),
    ((sumTypeInfo @JSONCIContent) {typeName = "CIContent"}, Just STUnion, "JCI", ""),
    ((sumTypeInfo @JSONCIDeleted) {typeName = "CIDeleted"}, Just STUnion, "JCID", ""),
    ((sumTypeInfo @JSONCIDirection) {typeName = "CIDirection"}, Just STUnion, "JCI", ""),
    ((sumTypeInfo @JSONCIDirection) {typeName = "CIQDirection"}, Just STUnion, "JCI", ""), -- this is incorrect, because of how JSON instances for CIQDirection is defined
    ((sumTypeInfo @JSONCIFileStatus) {typeName = "CIFileStatus"}, Just STUnion, "JCIFS", ""),
    ((sumTypeInfo @JSONCIStatus) {typeName = "CIStatus"}, Just STUnion, "JCIS", ""),
    (SumTypeInfo "AChatItem" [RecordTypeInfo "AChatItem" [FieldInfo "chatInfo" (ti "ChatInfo"), FieldInfo "chatItem" (ti "ChatItem")]], Just STRecord, "", ""),
    (SumTypeInfo "ACIReaction" [RecordTypeInfo "ACIReaction" [FieldInfo "chatInfo" (ti "ChatInfo"), FieldInfo "chatReaction" (ti "CIReaction")]], Just STRecord, "", ""),
    (SumTypeInfo "ChatError" [], Just STUnion, "Chat", ""),
    (SumTypeInfo "ConnStatus" [], Just STRecord, "", ""),
    (SumTypeInfo "FormatColor" [], Just STRecord, "", ""),
    (SumTypeInfo "VersionRange" [RecordTypeInfo "VersionRange" [FieldInfo "minVersion" (ti "Int"), FieldInfo "maxVersion" (ti "Int")]], Just STRecord, "", ""),
    (sumTypeInfo @(ChatItem 'CTDirect 'MDSnd), Just STRecord, "", ""),
    (sumTypeInfo @(CIFile 'MDSnd), Just STRecord, "", ""),
    (sumTypeInfo @(CIMeta 'CTDirect 'MDSnd), Just STRecord, "", ""),
    (sumTypeInfo @(CIQuote 'CTDirect), Just STRecord, "", ""), -- this is incorrect, because chatDir should be optional, because of how CIQDirection JSON instance is defined
    (sumTypeInfo @(CIReaction 'CTDirect 'MDSnd), Just STRecord, "", ""),
    (sumTypeInfo @(ContactUserPref SimplePreference), Just STUnion, "CUP", ""),
    (sumTypeInfo @(ContactUserPreference SimplePreference), Just STRecord, "", ""),
    (sumTypeInfo @(CreatedConnLink 'CMContact), Just STRecord, "", ""),
    (sumTypeInfo @AddressSettings, Just STRecord, "", ""),
    (sumTypeInfo @AutoAccept, Just STRecord, "", ""),
    (sumTypeInfo @BlockingInfo, Just STRecord, "", ""),
    (sumTypeInfo @BlockingReason, Just STEnum, "BR", ""),
    (sumTypeInfo @BusinessChatInfo, Just STRecord, "", ""),
    (sumTypeInfo @BusinessChatType, Just STEnum, "BC", ""),
    (sumTypeInfo @ChatDeleteMode, Just STUnion, "CDM", ""),
    (sumTypeInfo @ChatFeature, Just STEnum, "CF", ""),
    (sumTypeInfo @ChatItemDeletion, Just STRecord, "", "Message deletion result."),
    (sumTypeInfo @ChatRef, Nothing, "", ""),
    (sumTypeInfo @ChatSettings, Just STRecord, "", ""),
    (sumTypeInfo @ChatType, Just STEnum, "CT", ""),
    (sumTypeInfo @ChatWallpaper, Just STRecord, "", ""),
    (sumTypeInfo @ChatWallpaperScale, Just STEnum, "CWS", ""),
    (sumTypeInfo @CICallStatus, Just STEnum, "CISCall", ""),
    (sumTypeInfo @CIDeleteMode, Just STEnum, "CIDM", ""),
    (sumTypeInfo @CIForwardedFrom, Just STUnion, "CIFF", ""),
    (sumTypeInfo @CIGroupInvitation, Just STRecord, "", ""),
    (sumTypeInfo @CIGroupInvitationStatus, Just STEnum, "CIGIS", ""),
    (sumTypeInfo @CIMention, Just STRecord, "", ""),
    (sumTypeInfo @CIMentionMember, Just STRecord, "", ""),
    (sumTypeInfo @CIReactionCount, Just STRecord, "", ""),
    (sumTypeInfo @CITimed, Just STRecord, "", ""),
    (sumTypeInfo @ComposedMessage, Just STRecord, "", ""),
    (sumTypeInfo @Connection, Just STRecord, "", ""),
    (sumTypeInfo @ConnectionMode, Just STEnum, "CM", ""), -- incorrect, should be inv/con
    (sumTypeInfo @ConnectionPlan, Just STUnion, "CP", ""),
    (sumTypeInfo @ConnType, Just STEnum, "", ""), -- incorrect
    (sumTypeInfo @Contact, Just STRecord, "", ""),
    (sumTypeInfo @ContactAddressPlan, Just STUnion, "CAP", ""),
    (sumTypeInfo @ContactShortLinkData, Just STRecord, "", ""),
    (sumTypeInfo @ContactStatus, Just STEnum, "CS", ""),
    (sumTypeInfo @ContactUserPreferences, Just STRecord, "", ""),
    (sumTypeInfo @CryptoFile, Just STRecord, "", ""),
    (sumTypeInfo @CryptoFileArgs, Just STRecord, "", ""),
    (sumTypeInfo @E2EInfo, Just STRecord, "", ""),
    (sumTypeInfo @FeatureAllowed, Just STEnum, "FA", ""),
    (sumTypeInfo @FileDescr, Just STRecord, "", ""),
    (sumTypeInfo @FileError, Just STUnion, "FileErr", ""),
    (sumTypeInfo @FileInvitation, Just STRecord, "", ""),
    (sumTypeInfo @FileProtocol, Just STEnum, "FP", ""), -- incorrect, should be lowercase
    (sumTypeInfo @FileStatus, Just STEnum, "FS", ""),
    (sumTypeInfo @FileTransferMeta, Just STRecord, "", ""),
    (sumTypeInfo @Format, Just STUnion, "", ""),
    (sumTypeInfo @FormattedText, Just STRecord, "", ""),
    (sumTypeInfo @FullGroupPreferences, Just STRecord, "", ""),
    (sumTypeInfo @FullPreferences, Just STRecord, "", ""),
    (sumTypeInfo @GroupChatScope, Just STUnion, "GCS", ""),
    (sumTypeInfo @GroupChatScopeInfo, Just STUnion, "GCSI", ""),
    (sumTypeInfo @GroupFeature, Just STEnum, "GF", ""),
    (sumTypeInfo @GroupFeatureEnabled, Just STEnum, "FE", ""),
    (sumTypeInfo @GroupInfo, Just STRecord, "", ""),
    (sumTypeInfo @GroupInfoSummary, Just STRecord, "", ""),
    (sumTypeInfo @GroupLink, Just STRecord, "", ""),
    (sumTypeInfo @GroupLinkPlan, Just STUnion, "GLP", ""),
    (sumTypeInfo @GroupMember, Just STRecord, "", ""),
    (sumTypeInfo @GroupMemberAdmission, Just STRecord, "", ""),
    (sumTypeInfo @GroupMemberCategory, Just (STEnum' $ enumEnc "" "GC" "Member"), "", ""),
    (sumTypeInfo @GroupMemberRef, Just STRecord, "", ""),
    (sumTypeInfo @GroupMemberRole, Just STEnum, "GR", ""),
    (sumTypeInfo @GroupMemberSettings, Just STRecord, "", ""),
    (sumTypeInfo @GroupMemberStatus, Just STEnum, "", ""), -- incorrect
    (sumTypeInfo @GroupPreference, Just STRecord, "", ""),
    (sumTypeInfo @GroupPreferences, Just STRecord, "", ""),
    (sumTypeInfo @GroupProfile, Just STRecord, "", ""),
    (sumTypeInfo @GroupShortLinkData, Just STRecord, "", ""),
    (sumTypeInfo @GroupSummary, Just STRecord, "", ""),
    (sumTypeInfo @GroupSupportChat, Just STRecord, "", ""),
    (sumTypeInfo @InlineFileMode, Just STEnum, "IFM", ""),
    (sumTypeInfo @InvitationLinkPlan, Just STUnion, "ILP", ""),
    (sumTypeInfo @InvitedBy, Just STUnion, "IB", ""),
    (sumTypeInfo @LinkContent, Just STUnion, "LC", ""),
    (sumTypeInfo @LinkPreview, Just STRecord, "", ""),
    (sumTypeInfo @LocalProfile, Just STRecord, "", ""),
    (sumTypeInfo @MemberCriteria, Just STEnum, "MC", ""),
    (sumTypeInfo @MsgChatLink, Just STUnion, "MCL", "Connection link sent in a message - only short links are allowed."),
    (sumTypeInfo @MsgContent, Just STUnion, "MC", ""),
    (sumTypeInfo @MsgDecryptError, Just STEnum, "MDE", ""),
    (sumTypeInfo @MsgDirection, Just STEnum, "MD", ""),
    (sumTypeInfo @MsgErrorType, Just STUnion, "", ""), -- check, may be correct?
    (sumTypeInfo @MsgFilter, Just STEnum, "MF", ""),
    (sumTypeInfo @MsgReaction, Just STUnion, "MR", ""),
    (sumTypeInfo @MsgReceiptStatus, Just STEnum, "MR", ""),
    (sumTypeInfo @NewUser, Just STRecord, "", ""),
    (sumTypeInfo @NoteFolder, Just STRecord, "", ""),
    (sumTypeInfo @PendingContactConnection, Just STRecord, "", ""),
    (sumTypeInfo @PrefEnabled, Just STRecord, "", ""),
    (sumTypeInfo @Preferences, Just STRecord, "", ""),
    (sumTypeInfo @PreparedContact, Just STRecord, "", ""),
    (sumTypeInfo @PreparedGroup, Just STRecord, "", ""),
    (sumTypeInfo @Profile, Just STRecord, "", ""),
    (sumTypeInfo @RatchetSyncState, Just STEnum, "RS", ""),
    (sumTypeInfo @RcvConnEvent, Just STUnion, "RCE", ""),
    (sumTypeInfo @RcvDirectEvent, Just STUnion, "RDE", ""),
    (sumTypeInfo @RcvFileDescr, Just STRecord, "", ""),
    (sumTypeInfo @RcvFileInfo, Just STRecord, "", ""),
    (sumTypeInfo @RcvFileStatus, Just STUnion, "RFS", ""),
    (sumTypeInfo @RcvFileTransfer, Just STRecord, "", ""),
    (sumTypeInfo @RcvGroupEvent, Just STUnion, "RGE", ""),
    (sumTypeInfo @ReportReason, Just (STEnum' $ enumEnc "RRUnknown" "RR" ""), "RR", ""),
    (sumTypeInfo @RoleGroupPreference, Just STRecord, "", ""),
    (sumTypeInfo @SecurityCode, Just STRecord, "", ""),
    (sumTypeInfo @SendRef, Nothing, "", ""),
    (sumTypeInfo @SimplePreference, Just STRecord, "", ""),
    (sumTypeInfo @SimplexLinkType, Just STEnum, "XL", ""),
    (sumTypeInfo @SndCIStatusProgress, Just STEnum, "SSP", ""),
    (sumTypeInfo @SndConnEvent, Just STUnion, "SCE", ""),
    (sumTypeInfo @SndError, Just STUnion, "SndErr", ""),
    (sumTypeInfo @SndFileTransfer, Just STRecord, "", ""),
    (sumTypeInfo @SndGroupEvent, Just STUnion, "SGE", ""),
    (sumTypeInfo @SrvError, Just STUnion, "SrvErr", ""),
    (sumTypeInfo @SwitchPhase, Just STEnum, "SP", ""),
    (sumTypeInfo @TimedMessagesGroupPreference, Just STRecord, "", ""),
    (sumTypeInfo @TimedMessagesPreference, Just STRecord, "", ""),
    (sumTypeInfo @UIColorMode, Just STEnum, "UCM", ""),
    (sumTypeInfo @UIColors, Just STRecord, "", ""),
    (sumTypeInfo @UIThemeEntityOverride, Just STRecord, "", ""),
    (sumTypeInfo @UIThemeEntityOverrides, Just STRecord, "", ""),
    (sumTypeInfo @UpdatedMessage, Just STRecord, "", ""),
    (sumTypeInfo @User, Just STRecord, "", ""),
    (sumTypeInfo @UserContactLink, Just STRecord, "", ""),
    (sumTypeInfo @UserContactRequest, Just STRecord, "", ""),
    (sumTypeInfo @UserInfo, Just STRecord, "", ""),
    (sumTypeInfo @UserProfileUpdateSummary, Just STRecord, "", ""),
    (sumTypeInfo @UserPwdHash, Just STRecord, "", ""),
    (sumTypeInfo @XFTPRcvFile, Just STRecord, "", ""),
    (sumTypeInfo @XFTPSndFile, Just STRecord, "", "")
    -- ((sumTypeInfo @(Chat 'CTDirect)) {typeName = "AChat"} , Just STRecord, "", ""),
    -- (sumTypeInfo @ChatError, Just STUnion, "Chat", ""),
    -- (sumTypeInfo @ChatItemInfo, Just STRecord, "", ""),
    -- (sumTypeInfo @ChatItemVersion, Just STRecord, "", ""),
    -- (sumTypeInfo @ChatListQuery, Just STUnion, "CLQ", ""),
    -- (sumTypeInfo @ChatName, Just STRecord, "", ""),
    -- (sumTypeInfo @ChatPagination, Nothing, "CP", ""),
    -- (sumTypeInfo @ChatStats, Just STRecord, "", ""),
    -- (sumTypeInfo @ConnectionStats, Just STRecord, "", ""),
    -- (sumTypeInfo @Group, Just STRecord, "", ""),
    -- (sumTypeInfo @GroupSndStatus, Just STUnion, "GSS", ""),
    -- (sumTypeInfo @MemberDeliveryStatus, Just STRecord, "", ""),
    -- (sumTypeInfo @MemberReaction, Just STRecord, "", ""),
    -- (sumTypeInfo @MsgContentTag, Just (STEnum' $ enumEnc "MCUnknown_" "MC" "_"), "", ""),
    -- (sumTypeInfo @NavigationInfo, Just STRecord, "", ""),
    -- (sumTypeInfo @PaginationByTime, Nothing, "", ""),
    -- (sumTypeInfo @RcvQueueInfo, Just STRecord, "", ""),
    -- (sumTypeInfo @RcvSwitchStatus, Just STEnum, "", ""), -- incorrect
    -- (sumTypeInfo @SndQueueInfo, Just STRecord, "", ""),
    -- (sumTypeInfo @SndSwitchStatus, Just STEnum, "", ""), -- incorrect
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
