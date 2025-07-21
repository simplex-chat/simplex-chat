{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module API.Docs.Types where

import API.TypeInfo
import Data.Char (isUpper, toLower)
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
import Simplex.Chat.Store.Shared
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences
import Simplex.Chat.Types.Shared
import Simplex.Chat.Types.UITheme
import Simplex.FileTransfer.Transport
import Simplex.FileTransfer.Types hiding (RcvFileStatus) -- the type with the same name is used in simplex-chat.
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Client
import Simplex.Messaging.Crypto.File
import Simplex.Messaging.Parsers (dropPrefix, fstToLower)
import Simplex.Messaging.Protocol (BlockingInfo (..), BlockingReason (..), CommandError (..), ErrorType (..), ProxyError (..))
import Simplex.Messaging.Transport
import Simplex.RemoteControl.Types
import System.Console.ANSI.Types (Color (..))

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
    toCTDoc (STI consName records, jsonEncoding, consPrefix, hideConstrs, typeDescr) =
      let records' = filter ((`notElem` hideConstrs) . consName') records
       in CTDoc {typeInfo = STI consName records', jsonEncoding, consPrefix, typeDescr}

data SumTypeJsonEncoding = STRecord | STUnion | STEnum | STEnum' (ConsName -> String)

dropPfxSfx :: String -> String -> ConsName -> String
dropPfxSfx pfx sfx = dropSuffix sfx . dropPrefix pfx

consLower :: String -> ConsName -> String
consLower pfx = map toLower . dropPrefix pfx

consSep :: String -> Char -> ConsName -> String
consSep pfx sep = foldr (\c s -> if isUpper c then sep : toLower c : s else c : s) "" . dropPrefix pfx

dropSuffix :: String -> String -> String
dropSuffix sfx s =
  let (s', sfx') = splitAt (length s - length sfx) s
   in fstToLower $ if sfx' == sfx then s' else s

-- making chatDir optional because clients use CIDirection? instead of CIQDirection (the type is replaced in Types.hs)
ciQuoteType :: SumTypeInfo
ciQuoteType =
  let st@(STI _ records) = sti @(CIQuote 'CTDirect)
      optChatDir f@(FieldInfo n t) = if n == "chatDir" then FieldInfo n (TIOptional t) else f
      updateRecord (RecordTypeInfo name fields) = RecordTypeInfo name $ map optChatDir fields
   in st {recordTypes = map updateRecord records} -- need to map even though there is one constructor in this type

chatTypesDocsData :: [(SumTypeInfo, Maybe SumTypeJsonEncoding, String, [ConsName], Text)]
chatTypesDocsData =
  [ ((sti @(Chat 'CTDirect)) {typeName = "AChat"} , Just STRecord, "", [], ""),
    ((sti @JSONChatInfo) {typeName = "ChatInfo"}, Just STUnion, "JCInfo", [], ""),
    ((sti @JSONCIContent) {typeName = "CIContent"}, Just STUnion, "JCI", [], ""),
    ((sti @JSONCIDeleted) {typeName = "CIDeleted"}, Just STUnion, "JCID", [], ""),
    ((sti @JSONCIDirection) {typeName = "CIDirection"}, Just STUnion, "JCI", [], ""),
    ((sti @JSONCIFileStatus) {typeName = "CIFileStatus"}, Just STUnion, "JCIFS", [], ""),
    ((sti @JSONCIStatus) {typeName = "CIStatus"}, Just STUnion, "JCIS", [], ""),
    (ciQuoteType, Just STRecord, "", [], ""),
    (STI "AChatItem" [RecordTypeInfo "AChatItem" [FieldInfo "chatInfo" (ti "ChatInfo"), FieldInfo "chatItem" (ti "ChatItem")]], Just STRecord, "", [], ""),
    (STI "ACIReaction" [RecordTypeInfo "ACIReaction" [FieldInfo "chatInfo" (ti "ChatInfo"), FieldInfo "chatReaction" (ti "CIReaction")]], Just STRecord, "", [], ""),
    (STI "JSONObject" [], Just STRecord, "", [], "Arbitrary JSON object."),
    (STI "UTCTime" [], Just STRecord, "", [], "Timestampe in ISO8601 format as string."),
    (STI "VersionRange" [RecordTypeInfo "VersionRange" [FieldInfo "minVersion" (ti TInt), FieldInfo "maxVersion" (ti TInt)]], Just STRecord, "", [], ""),
    (sti @(ChatItem 'CTDirect 'MDSnd), Just STRecord, "", [], ""),
    (sti @(CIFile 'MDSnd), Just STRecord, "", [], ""),
    (sti @(CIMeta 'CTDirect 'MDSnd), Just STRecord, "", [], ""),
    (sti @(CIReaction 'CTDirect 'MDSnd), Just STRecord, "", [], ""),
    (sti @(ContactUserPref SimplePreference), Just STUnion, "CUP", [], ""),
    (sti @(ContactUserPreference SimplePreference), Just STRecord, "", [], ""),
    (sti @(CreatedConnLink 'CMContact), Just STRecord, "", [], ""),
    (sti @AddressSettings, Just STRecord, "", [], ""),
    (sti @AgentCryptoError, Just STUnion, "", [], ""),
    (sti @AgentErrorType, Just STUnion, "", [], ""),
    (sti @AutoAccept, Just STRecord, "", [], ""),
    (sti @BlockingInfo, Just STRecord, "", [], ""),
    (sti @BlockingReason, Just STEnum, "BR", [], ""),
    (sti @BrokerErrorType, Just STUnion, "", [], ""),
    (sti @BusinessChatInfo, Just STRecord, "", [], ""),
    (sti @BusinessChatType, Just STEnum, "BC", [], ""),
    (sti @ChatDeleteMode, Just STUnion, "CDM", [], ""),
    (sti @ChatError, Just STUnion, "Chat", ["ChatErrorDatabase", "ChatErrorRemoteHost", "ChatErrorRemoteCtrl"], ""),
    (sti @ChatErrorType, Just STUnion, "CE", ["CEContactNotFound", "CEServerProtocol", "CECallState", "CEInvalidChatMessage"], ""),
    (sti @ChatFeature, Just STEnum, "CF", [], ""),
    (sti @ChatItemDeletion, Just STRecord, "", [], "Message deletion result."),
    (sti @ChatRef, Nothing, "", [], ""),
    (sti @ChatSettings, Just STRecord, "", [], ""),
    (sti @ChatStats, Just STRecord, "", [], ""),
    (sti @ChatType, Just STEnum, "CT", [], ""),
    (sti @ChatWallpaper, Just STRecord, "", [], ""),
    (sti @ChatWallpaperScale, Just STEnum, "CWS", [], ""),
    (sti @CICallStatus, Just STEnum, "CISCall", [], ""),
    (sti @CIDeleteMode, Just STEnum, "CIDM", [], ""),
    (sti @CIForwardedFrom, Just STUnion, "CIFF", [], ""),
    (sti @CIGroupInvitation, Just STRecord, "", [], ""),
    (sti @CIGroupInvitationStatus, Just STEnum, "CIGIS", [], ""),
    (sti @CIMention, Just STRecord, "", [], ""),
    (sti @CIMentionMember, Just STRecord, "", [], ""),
    (sti @CIReactionCount, Just STRecord, "", [], ""),
    (sti @CITimed, Just STRecord, "", [], ""),
    (sti @Color, Just STEnum, "", [], ""),
    (sti @CommandError, Just STUnion, "", [], ""),
    (sti @CommandErrorType, Just STUnion, "", [], ""),
    (sti @ComposedMessage, Just STRecord, "", [], ""),
    (sti @Connection, Just STRecord, "", [], ""),
    (sti @ConnectionEntity, Just STUnion, "", [], ""),
    (sti @ConnectionErrorType, Just STUnion, "", [], ""),
    (sti @ConnectionMode, Just (STEnum' $ take 3 . consLower "CM"), "", [], ""),
    (sti @ConnectionPlan, Just STUnion, "CP", [], ""),
    (sti @ConnStatus, Just (STEnum' $ consSep "Conn" '-'), "", [], ""),
    (sti @ConnType, Just (STEnum' $ consSep "Conn" '_'), "", ["ConnSndFile", "ConnRcvFile"], ""),
    (sti @Contact, Just STRecord, "", [], ""),
    (sti @ContactAddressPlan, Just STUnion, "CAP", [], ""),
    (sti @ContactShortLinkData, Just STRecord, "", [], ""),
    (sti @ContactStatus, Just STEnum, "CS", [], ""),
    (sti @ContactUserPreferences, Just STRecord, "", [], ""),
    (sti @CryptoFile, Just STRecord, "", [], ""),
    (sti @CryptoFileArgs, Just STRecord, "", [], ""),
    (sti @E2EInfo, Just STRecord, "", [], ""),
    (sti @ErrorType, Just STUnion, "", [], ""),
    (sti @FeatureAllowed, Just STEnum, "FA", [], ""),
    (sti @FileDescr, Just STRecord, "", [], ""),
    (sti @FileError, Just STUnion, "FileErr", [], ""),
    (sti @FileErrorType, Just STUnion, "", [], ""),
    (sti @FileInvitation, Just STRecord, "", [], ""),
    (sti @FileProtocol, Just (STEnum' $ consLower "FP"), "", [], ""),
    (sti @FileStatus, Just STEnum, "FS", [], ""),
    (sti @FileTransferMeta, Just STRecord, "", [], ""),
    (sti @Format, Just STUnion, "", [], ""),
    (sti @FormattedText, Just STRecord, "", [], ""),
    (sti @FullGroupPreferences, Just STRecord, "", [], ""),
    (sti @FullPreferences, Just STRecord, "", [], ""),
    (sti @GroupChatScope, Just STUnion, "GCS", [], ""),
    (sti @GroupChatScopeInfo, Just STUnion, "GCSI", [], ""),
    (sti @GroupFeature, Just STEnum, "GF", [], ""),
    (sti @GroupFeatureEnabled, Just STEnum, "FE", [], ""),
    (sti @GroupInfo, Just STRecord, "", [], ""),
    (sti @GroupInfoSummary, Just STRecord, "", [], ""),
    (sti @GroupLink, Just STRecord, "", [], ""),
    (sti @GroupLinkPlan, Just STUnion, "GLP", [], ""),
    (sti @GroupMember, Just STRecord, "", [], ""),
    (sti @GroupMemberAdmission, Just STRecord, "", [], ""),
    (sti @GroupMemberCategory, Just (STEnum' $ dropPfxSfx "GC" "Member"), "", [], ""),
    (sti @GroupMemberRef, Just STRecord, "", [], ""),
    (sti @GroupMemberRole, Just STEnum, "GR", [], ""),
    (sti @GroupMemberSettings, Just STRecord, "", [], ""),
    (sti @GroupMemberStatus, Just (STEnum' $ (\case "group_deleted" -> "deleted"; "intro_invited" -> "intro-inv"; s -> s) . consSep "GSMem" '_'), "", [], ""),
    (sti @GroupPreference, Just STRecord, "", [], ""),
    (sti @GroupPreferences, Just STRecord, "", [], ""),
    (sti @GroupProfile, Just STRecord, "", [], ""),
    (sti @GroupShortLinkData, Just STRecord, "", [], ""),
    (sti @GroupSummary, Just STRecord, "", [], ""),
    (sti @GroupSupportChat, Just STRecord, "", [], ""),
    (sti @HandshakeError, Just STEnum, "", [], ""),
    (sti @InlineFileMode, Just STEnum, "IFM", [], ""),
    (sti @InvitationLinkPlan, Just STUnion, "ILP", [], ""),
    (sti @InvitedBy, Just STUnion, "IB", [], ""),
    (sti @LinkContent, Just STUnion, "LC", [], ""),
    (sti @LinkPreview, Just STRecord, "", [], ""),
    (sti @LocalProfile, Just STRecord, "", [], ""),
    (sti @MemberCriteria, Just STEnum, "MC", [], ""),
    (sti @MsgChatLink, Just STUnion, "MCL", [], "Connection link sent in a message - only short links are allowed."),
    (sti @MsgContent, Just STUnion, "MC", [], ""),
    (sti @MsgDecryptError, Just STEnum, "MDE", [], ""),
    (sti @MsgDirection, Just STEnum, "MD", [], ""),
    (sti @MsgErrorType, Just STUnion, "", [], ""), -- check, may be correct?
    (sti @MsgFilter, Just STEnum, "MF", [], ""),
    (sti @MsgReaction, Just STUnion, "MR", [], ""),
    (sti @MsgReceiptStatus, Just STEnum, "MR", [], ""),
    (sti @NewUser, Just STRecord, "", [], ""),
    (sti @NoteFolder, Just STRecord, "", [], ""),
    (sti @PendingContactConnection, Just STRecord, "", [], ""),
    (sti @PrefEnabled, Just STRecord, "", [], ""),
    (sti @Preferences, Just STRecord, "", [], ""),
    (sti @PreparedContact, Just STRecord, "", [], ""),
    (sti @PreparedGroup, Just STRecord, "", [], ""),
    (sti @Profile, Just STRecord, "", [], ""),
    (sti @ProxyClientError, Just STUnion, "Proxy", [], ""),
    (sti @ProxyError, Just STUnion, "", [], ""),
    (sti @RatchetSyncState, Just STEnum, "RS", [], ""),
    (sti @RCErrorType, Just STUnion, "RCE", [], ""),
    (sti @RcvConnEvent, Just STUnion, "RCE", [], ""),
    (sti @RcvDirectEvent, Just STUnion, "RDE", [], ""),
    (sti @RcvFileDescr, Just STRecord, "", [], ""),
    (sti @RcvFileInfo, Just STRecord, "", [], ""),
    (sti @RcvFileStatus, Just STUnion, "RFS", [], ""),
    (sti @RcvFileTransfer, Just STRecord, "", [], ""),
    (sti @RcvGroupEvent, Just STUnion, "RGE", [], ""),
    (sti @ReportReason, Just (STEnum' $ dropPfxSfx "RR" ""), "", ["RRUnknown"], ""),
    (sti @RoleGroupPreference, Just STRecord, "", [], ""),
    (sti @SecurityCode, Just STRecord, "", [], ""),
    (sti @SendRef, Nothing, "", [], ""),
    (sti @SimplePreference, Just STRecord, "", [], ""),
    (sti @SimplexLinkType, Just STEnum, "XL", [], ""),
    (sti @SMPAgentError, Just STUnion, "", [], ""),
    (sti @SndCIStatusProgress, Just STEnum, "SSP", [], ""),
    (sti @SndConnEvent, Just STUnion, "SCE", [], ""),
    (sti @SndError, Just STUnion, "SndErr", [], ""),
    (sti @SndFileTransfer, Just STRecord, "", [], ""),
    (sti @SndGroupEvent, Just STUnion, "SGE", [], ""),
    (sti @SrvError, Just STUnion, "SrvErr", [], ""),
    (sti @StoreError, Just STUnion, "SE", [], ""),
    (sti @SwitchPhase, Just STEnum, "SP", [], ""),
    (sti @TimedMessagesGroupPreference, Just STRecord, "", [], ""),
    (sti @TimedMessagesPreference, Just STRecord, "", [], ""),
    (sti @TransportError, Just STUnion, "TE", [], ""),
    (sti @UIColorMode, Just STEnum, "UCM", [], ""),
    (sti @UIColors, Just STRecord, "", [], ""),
    (sti @UIThemeEntityOverride, Just STRecord, "", [], ""),
    (sti @UIThemeEntityOverrides, Just STRecord, "", [], ""),
    (sti @UpdatedMessage, Just STRecord, "", [], ""),
    (sti @User, Just STRecord, "", [], ""),
    (sti @UserContact, Just STRecord, "", [], ""),
    (sti @UserContactLink, Just STRecord, "", [], ""),
    (sti @UserContactRequest, Just STRecord, "", [], ""),
    (sti @UserInfo, Just STRecord, "", [], ""),
    (sti @UserProfileUpdateSummary, Just STRecord, "", [], ""),
    (sti @UserPwdHash, Just STRecord, "", [], ""),
    (sti @XFTPErrorType, Just STUnion, "", [], ""),
    (sti @XFTPRcvFile, Just STRecord, "", [], ""),
    (sti @XFTPSndFile, Just STRecord, "", [], "")

    -- (sti @DatabaseError, Just STUnion, "DB", [], ""),
    -- (sti @ChatItemInfo, Just STRecord, "", [], ""),
    -- (sti @ChatItemVersion, Just STRecord, "", [], ""),
    -- (sti @ChatListQuery, Just STUnion, "CLQ", [], ""),
    -- (sti @ChatName, Just STRecord, "", [], ""),
    -- (sti @ChatPagination, Nothing, "CP", [], ""),
    -- (sti @ConnectionStats, Just STRecord, "", [], ""),
    -- (sti @Group, Just STRecord, "", [], ""),
    -- (sti @GroupSndStatus, Just STUnion, "GSS", [], ""),
    -- (sti @MemberDeliveryStatus, Just STRecord, "", [], ""),
    -- (sti @MemberReaction, Just STRecord, "", [], ""),
    -- (sti @MsgContentTag, Just (STEnum' $ dropPfxSfx "MC" '_'), "", ["MCUnknown_"], ""),
    -- (sti @NavigationInfo, Just STRecord, "", [], ""),
    -- (sti @PaginationByTime, Nothing, "", [], ""),
    -- (sti @RcvQueueInfo, Just STRecord, "", [], ""),
    -- (sti @RcvSwitchStatus, Just STEnum, "", [], ""), -- incorrect
    -- (sti @SndQueueInfo, Just STRecord, "", [], ""),
    -- (sti @SndSwitchStatus, Just STEnum, "", [], ""), -- incorrect
 ]

data SimplePreference = SimplePreference {allow :: FeatureAllowed} deriving (Generic)

data RoleGroupPreference = RoleGroupPreference {enable :: GroupFeatureEnabled, role :: Maybe GroupMemberRole} deriving (Generic)

deriving instance Generic (Chat c)
deriving instance Generic (ChatItem c d)
deriving instance Generic (CIFile d)
deriving instance Generic (CIMeta c d)
deriving instance Generic (CIQuote d)
deriving instance Generic (CIReaction c d)
deriving instance Generic (ContactUserPref p)
deriving instance Generic (ContactUserPreference p)
deriving instance Generic (CreatedConnLink m)
deriving instance Generic AddressSettings
deriving instance Generic AgentCryptoError
deriving instance Generic AgentErrorType
deriving instance Generic AutoAccept
deriving instance Generic BlockingInfo
deriving instance Generic BlockingReason
deriving instance Generic BrokerErrorType
deriving instance Generic BusinessChatInfo
deriving instance Generic BusinessChatType
deriving instance Generic ChatDeleteMode
deriving instance Generic ChatError
deriving instance Generic ChatErrorType
deriving instance Generic ChatFeature
deriving instance Generic ChatItemDeletion
deriving instance Generic ChatRef
deriving instance Generic ChatSettings
deriving instance Generic ChatStats
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
deriving instance Generic Color
deriving instance Generic CommandError
deriving instance Generic CommandErrorType
deriving instance Generic ComposedMessage
deriving instance Generic Connection
deriving instance Generic ConnectionEntity
deriving instance Generic ConnectionErrorType
deriving instance Generic ConnectionMode
deriving instance Generic ConnectionPlan
deriving instance Generic ConnStatus
deriving instance Generic ConnType
deriving instance Generic Contact
deriving instance Generic ContactAddressPlan
deriving instance Generic ContactShortLinkData
deriving instance Generic ContactStatus
deriving instance Generic ContactUserPreferences
deriving instance Generic CryptoFile
deriving instance Generic CryptoFileArgs
deriving instance Generic E2EInfo
deriving instance Generic ErrorType
deriving instance Generic FeatureAllowed
deriving instance Generic FileDescr
deriving instance Generic FileError
deriving instance Generic FileErrorType
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
deriving instance Generic HandshakeError
deriving instance Generic InlineFileMode
deriving instance Generic InvitationLinkPlan
deriving instance Generic InvitedBy
deriving instance Generic JSONChatInfo
deriving instance Generic JSONCIContent
deriving instance Generic JSONCIDeleted
deriving instance Generic JSONCIDirection
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
deriving instance Generic ProxyClientError
deriving instance Generic ProxyError
deriving instance Generic RatchetSyncState
deriving instance Generic RCErrorType
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
deriving instance Generic SMPAgentError
deriving instance Generic SndCIStatusProgress
deriving instance Generic SndConnEvent
deriving instance Generic SndError
deriving instance Generic SndFileTransfer
deriving instance Generic SndGroupEvent
deriving instance Generic SrvError
deriving instance Generic StoreError
deriving instance Generic SwitchPhase
deriving instance Generic TimedMessagesGroupPreference
deriving instance Generic TimedMessagesPreference
deriving instance Generic TransportError
deriving instance Generic UIColorMode
deriving instance Generic UIColors
deriving instance Generic UIThemeEntityOverride
deriving instance Generic UIThemeEntityOverrides
deriving instance Generic UpdatedMessage
deriving instance Generic User
deriving instance Generic UserContact
deriving instance Generic UserContactLink
deriving instance Generic UserContactRequest
deriving instance Generic UserInfo
deriving instance Generic UserProfileUpdateSummary
deriving instance Generic UserPwdHash
deriving instance Generic XFTPErrorType
deriving instance Generic XFTPRcvFile
deriving instance Generic XFTPSndFile

-- deriving instance Generic DatabaseError
-- deriving instance Generic ChatItemInfo
-- deriving instance Generic ChatItemVersion
-- deriving instance Generic ChatListQuery
-- deriving instance Generic ChatName
-- deriving instance Generic ChatPagination
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
