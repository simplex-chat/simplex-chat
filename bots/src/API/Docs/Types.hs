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
import qualified Data.Text as T
import GHC.Generics
import Simplex.Chat.Controller
import Simplex.Chat.Markdown
import Simplex.Chat.Messages
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Protocol
import Simplex.Chat.Store.Groups
import Simplex.Chat.Store.Profiles
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences
import Simplex.Chat.Types.Shared
import Simplex.Chat.Types.UITheme
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Crypto.File
import Simplex.Messaging.Parsers (dropPrefix)

data CTDoc = CTDoc {typeInfo :: SumTypeInfo, jsonEncoding :: Maybe SumTypeJsonEncoding, consPrefix :: String, typeDescr :: Text}

docTypeName :: CTDoc -> String
docTypeName CTDoc {typeInfo = SumTypeInfo name _} = name

chatTypesDocs :: [CTDoc]
chatTypesDocs = map toCTDoc chatTypesDocsData
  where
    toCTDoc (typeInfo, jsonEncoding, consPrefix, typeDescr) = CTDoc {typeInfo, jsonEncoding, consPrefix, typeDescr}

primitiveTypes :: [ConsName]
primitiveTypes = ["Bool", "Int", "Int64", "Double", "String", "UTCTime", "JSONObject"]

data SumTypeJsonEncoding = STRecord | STUnion | STEnum | STEnum' (RecordTypeInfo -> Maybe String)

enumEnc :: String -> String -> Int -> RecordTypeInfo -> Maybe String
enumEnc unknown prefix trimEnd RecordTypeInfo {consName}
  | consName == unknown = Nothing
  | otherwise = Just $ T.unpack $ T.dropEnd trimEnd $ T.pack $ dropPrefix prefix consName

chatTypesDocsData :: [(SumTypeInfo, Maybe SumTypeJsonEncoding, String, Text)]
chatTypesDocsData =
  [ (SumTypeInfo "ACIReaction" [RecordTypeInfo "ACIReaction" [FieldInfo "chatInfo" (ti "ChatInfo"), FieldInfo "chatReaction" (ti "CIReaction")]], Just STRecord, "", ""),
    (SumTypeInfo "AChat" [], Just STRecord, "", ""),
    (SumTypeInfo "AChatInfo" [], Just STRecord, "", ""),
    (SumTypeInfo "AChatItem" [], Just STRecord, "", ""),
    (SumTypeInfo "ACreatedConnLink" [], Just STRecord, "", ""),
    (SumTypeInfo "AddressSettings" [], Just STRecord, "", ""),
    (SumTypeInfo "CChatItem" [], Just STRecord, "", ""),
    (SumTypeInfo "CIDeleteMode" [], Just STRecord, "", ""),
    (SumTypeInfo "CIDirection" [], Just STRecord, "", ""),
    (SumTypeInfo "ChatError" [], Just STRecord, "", ""),
    (sumTypeInfo @(CIReaction 'CTDirect 'MDRcv), Just STRecord, "", ""),
    (sumTypeInfo @ChatDeleteMode, Just STUnion, "CDM", ""),
    (SumTypeInfo "ChatInfo" [], Just STRecord, "", ""),
    (sumTypeInfo @ChatItemDeletion, Just STRecord, "", "Message deletion result."),
    (sumTypeInfo @ChatItemInfo, Just STRecord, "", ""),
    (sumTypeInfo @ChatItemVersion, Just STRecord, "", ""),
    (sumTypeInfo @ChatListQuery, Just STUnion, "CLQ", ""),
    (sumTypeInfo @ChatName, Just STRecord, "", ""),
    (sumTypeInfo @ChatPagination, Nothing, "CP", ""),
    (sumTypeInfo @ChatRef, Just STRecord, "", ""),
    (sumTypeInfo @ChatType, Just STEnum, "CT", ""),
    (sumTypeInfo @ChatSettings, Just STRecord, "", ""),
    (sumTypeInfo @ChatWallpaper, Just STRecord, "", ""),
    (sumTypeInfo @ChatWallpaperScale, Just STEnum, "CWS", ""),
    (sumTypeInfo @ComposedMessage, Just STRecord, "", ""),
    (SumTypeInfo "ConnStatus" [], Just STRecord, "", ""),
    (sumTypeInfo @ConnectionPlan, Just STUnion, "CP", ""),
    (sumTypeInfo @ConnectionStats, Just STRecord, "", ""),
    (sumTypeInfo @Contact, Just STRecord, "", ""),
    (sumTypeInfo @ContactAddressPlan, Just STUnion, "CAP", ""),
    (sumTypeInfo @ContactShortLinkData, Just STRecord, "", ""),
    (sumTypeInfo @(CreatedConnLink 'CMContact), Just STRecord, "", ""),
    (sumTypeInfo @CryptoFileArgs, Just STRecord, "", ""),
    (sumTypeInfo @FileDescr, Just STRecord, "", ""),
    (sumTypeInfo @FileInvitation, Just STRecord, "", ""),
    (sumTypeInfo @FileStatus, Just STEnum, "FS", ""),
    (sumTypeInfo @FileTransferMeta, Just STRecord, "", ""),
    (sumTypeInfo @FeatureAllowed, Just STEnum, "FA", ""),
    (SumTypeInfo "FormatColor" [], Just STRecord, "", ""),
    (sumTypeInfo @Format, Just STUnion, "", ""),
    (sumTypeInfo @FormattedText, Just STRecord, "", ""),
    (sumTypeInfo @FullPreferences, Just STRecord, "", ""),
    (sumTypeInfo @Group, Just STRecord, "", ""),
    (sumTypeInfo @GroupChatScope, Just STUnion, "GCS", ""),
    (sumTypeInfo @GroupInfo, Just STRecord, "", ""),
    (sumTypeInfo @GroupInfoSummary, Just STRecord, "", ""),
    (sumTypeInfo @GroupLink, Just STRecord, "", ""),
    (sumTypeInfo @GroupLinkPlan, Just STUnion, "GLP", ""),
    (sumTypeInfo @GroupMember, Just STRecord, "", ""),
    (sumTypeInfo @GroupMemberRole, Just STEnum, "GR", ""),
    (sumTypeInfo @GroupPreference, Just STRecord, "", ""),
    (sumTypeInfo @GroupProfile, Just STRecord, "", ""),
    (sumTypeInfo @GroupShortLinkData, Just STRecord, "", ""),
    (sumTypeInfo @GroupSndStatus, Just STUnion, "GSS", ""),
    (sumTypeInfo @InlineFileMode, Just STEnum, "IFM", ""),
    (sumTypeInfo @InvitationLinkPlan, Just STUnion, "ILP", ""),
    (sumTypeInfo @LinkContent, Just STUnion, "LC", ""),
    (sumTypeInfo @LinkPreview, Just STRecord, "", ""),
    (sumTypeInfo @LocalProfile, Just STRecord, "", ""),
    (sumTypeInfo @MemberDeliveryStatus, Just STRecord, "", ""),
    (sumTypeInfo @MemberReaction, Just STRecord, "", ""),
    (sumTypeInfo @MsgChatLink, Just STUnion, "MCL", "Connection link sent in a message - only short links are allowed."),
    (sumTypeInfo @MsgContent, Just STUnion, "MC", ""),
    (sumTypeInfo @MsgContentTag, Just (STEnum' $ enumEnc "MCUnknown_" "MC" 1), "", ""),
    (sumTypeInfo @MsgFilter, Just STEnum, "MF", ""),
    (sumTypeInfo @MsgReaction, Just STUnion, "MR", ""),
    (sumTypeInfo @MsgReceiptStatus, Just STEnum, "MR", ""),
    (sumTypeInfo @NavigationInfo, Just STRecord, "", ""),
    (sumTypeInfo @NewUser, Just STRecord, "", ""),
    (sumTypeInfo @PaginationByTime, Nothing, "", ""),
    (sumTypeInfo @PendingContactConnection, Just STRecord, "", ""),
    (sumTypeInfo @Preferences, Just STRecord, "", ""),
    (sumTypeInfo @Profile, Just STRecord, "", ""),
    (sumTypeInfo @RcvFileDescr, Just STRecord, "", ""),
    (sumTypeInfo @RcvFileInfo, Just STRecord, "", ""),
    (sumTypeInfo @RcvFileStatus, Just STUnion, "RFS", ""),
    (sumTypeInfo @RcvFileTransfer, Just STRecord, "", ""),
    (sumTypeInfo @ReportReason, Just (STEnum' $ enumEnc "RRUnknown" "RR" 0), "RR", ""),
    (sumTypeInfo @RoleGroupPreference, Just STRecord, "", ""),
    (sumTypeInfo @SimplePreference, Just STRecord, "", ""),
    (sumTypeInfo @SendRef, Nothing, "", ""),
    (sumTypeInfo @SimplexLinkType, Just STEnum, "XL", ""),
    (sumTypeInfo @SndError, Just STUnion, "SndErr", ""),
    (sumTypeInfo @SndFileTransfer, Just STRecord, "", ""),
    (sumTypeInfo @SrvError, Just STUnion, "SrvErr", ""),
    (sumTypeInfo @TimedMessagesPreference, Just STRecord, "", ""),
    (sumTypeInfo @TimedMessagesGroupPreference, Just STRecord, "", ""),
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
    (SumTypeInfo "VersionRange" [RecordTypeInfo "ACIReaction" [FieldInfo "minVersion" (ti "Int"), FieldInfo "maxVersion" (ti "Int")]], Just STRecord, "", ""),
    (sumTypeInfo @XFTPRcvFile, Just STRecord, "", ""),

    (sumTypeInfo @BusinessChatInfo, Just STRecord, "", ""),
    (sumTypeInfo @BusinessChatType, Just STEnum, "BC", ""),
    (sumTypeInfo @Connection, Just STRecord, "", ""),
    (sumTypeInfo @ConnectionMode, Just STEnum, "CM", ""), -- incorrect, should be inv/con
    (sumTypeInfo @ConnType, Just STEnum, "", ""), -- incorrect
    (sumTypeInfo @ContactStatus, Just STEnum, "CS", ""),
    (sumTypeInfo @(ContactUserPref SimplePreference), Just STUnion, "CUP", ""),
    (sumTypeInfo @(ContactUserPreference SimplePreference), Just STRecord, "", ""),
    (sumTypeInfo @ContactUserPreferences, Just STRecord, "", ""),
    (sumTypeInfo @CryptoFile, Just STRecord, "", ""),
    (sumTypeInfo @FullGroupPreferences, Just STRecord, "", ""),
    (sumTypeInfo @GroupFeatureEnabled, Just STEnum, "FE", ""),
    (sumTypeInfo @GroupMemberAdmission, Just STRecord, "", ""),
    (sumTypeInfo @GroupMemberCategory, Just (STEnum' $ enumEnc "" "GC" 6), "", ""),
    (sumTypeInfo @GroupMemberSettings, Just STRecord, "", ""),
    (sumTypeInfo @GroupMemberStatus, Just STEnum, "", ""), -- incorrect
    (sumTypeInfo @GroupPreferences, Just STRecord, "", ""),
    (sumTypeInfo @GroupSummary, Just STRecord, "", ""),
    (sumTypeInfo @GroupSupportChat, Just STRecord, "", ""),
    (sumTypeInfo @InvitedBy, Just STUnion, "IB", ""),
    (sumTypeInfo @MemberCriteria, Just STEnum, "MC", ""),
    (sumTypeInfo @PrefEnabled, Just STRecord, "", ""),
    (sumTypeInfo @PreparedContact, Just STRecord, "", ""),
    (sumTypeInfo @PreparedGroup, Just STRecord, "", ""),
    (sumTypeInfo @RatchetSyncState, Just STEnum, "RS", ""),
    (sumTypeInfo @RcvQueueInfo, Just STRecord, "", ""),
    (sumTypeInfo @RcvSwitchStatus, Just STEnum, "", ""), -- incorrect
    (sumTypeInfo @SecurityCode, Just STRecord, "", ""),
    (sumTypeInfo @SndQueueInfo, Just STRecord, "", ""),
    (sumTypeInfo @SndSwitchStatus, Just STEnum, "", "") -- incorrect
    (sumTypeInfo @XFTPSndFile, Just STRecord, "", ""),

  ]

data SimplePreference = SimplePreference {allow :: FeatureAllowed} deriving (Generic)

data RoleGroupPreference = RoleGroupPreference {enable :: GroupFeatureEnabled, role :: Maybe GroupMemberRole} deriving (Generic)

deriving instance Generic (CIReaction c d)

deriving instance Generic (ContactUserPref p)

deriving instance Generic (ContactUserPreference p)

deriving instance Generic (CreatedConnLink m)

deriving instance Generic BusinessChatInfo

deriving instance Generic BusinessChatType

deriving instance Generic ChatDeleteMode

deriving instance Generic ChatItemDeletion

deriving instance Generic ChatItemInfo

deriving instance Generic ChatItemVersion

deriving instance Generic ChatListQuery

deriving instance Generic ChatName

deriving instance Generic ChatPagination

deriving instance Generic ChatRef

deriving instance Generic ChatSettings

deriving instance Generic ChatType

deriving instance Generic ChatWallpaper

deriving instance Generic ChatWallpaperScale

deriving instance Generic ComposedMessage

deriving instance Generic Connection

deriving instance Generic ConnectionMode

deriving instance Generic ConnectionPlan

deriving instance Generic ConnectionStats

deriving instance Generic ConnType

deriving instance Generic Contact

deriving instance Generic ContactAddressPlan

deriving instance Generic ContactShortLinkData

deriving instance Generic ContactStatus

deriving instance Generic ContactUserPreferences

deriving instance Generic CryptoFile

deriving instance Generic CryptoFileArgs

deriving instance Generic FeatureAllowed

deriving instance Generic FileDescr

deriving instance Generic FileInvitation

deriving instance Generic FileStatus

deriving instance Generic FileTransferMeta

deriving instance Generic Format

deriving instance Generic FormattedText

deriving instance Generic FullGroupPreferences

deriving instance Generic FullPreferences

deriving instance Generic Group

deriving instance Generic GroupChatScope

deriving instance Generic GroupFeatureEnabled

deriving instance Generic GroupInfo

deriving instance Generic GroupInfoSummary

deriving instance Generic GroupLink

deriving instance Generic GroupLinkPlan

deriving instance Generic GroupMember

deriving instance Generic GroupMemberAdmission

deriving instance Generic GroupMemberCategory

deriving instance Generic GroupMemberRole

deriving instance Generic GroupMemberSettings

deriving instance Generic GroupMemberStatus

deriving instance Generic GroupPreference

deriving instance Generic GroupPreferences

deriving instance Generic GroupProfile

deriving instance Generic GroupShortLinkData

deriving instance Generic GroupSndStatus

deriving instance Generic GroupSummary

deriving instance Generic GroupSupportChat

deriving instance Generic InlineFileMode

deriving instance Generic InvitationLinkPlan

deriving instance Generic InvitedBy

deriving instance Generic LinkContent

deriving instance Generic LinkPreview

deriving instance Generic LocalProfile

deriving instance Generic MemberCriteria

deriving instance Generic MemberDeliveryStatus

deriving instance Generic MemberReaction

deriving instance Generic MsgChatLink

deriving instance Generic MsgContent

deriving instance Generic MsgContentTag

deriving instance Generic MsgFilter

deriving instance Generic MsgReaction

deriving instance Generic MsgReceiptStatus

deriving instance Generic NavigationInfo

deriving instance Generic NewUser

deriving instance Generic PaginationByTime

deriving instance Generic PendingContactConnection

deriving instance Generic PrefEnabled

deriving instance Generic Preferences

deriving instance Generic PreparedContact

deriving instance Generic PreparedGroup

deriving instance Generic Profile

deriving instance Generic RatchetSyncState

deriving instance Generic RcvFileDescr

deriving instance Generic RcvFileInfo

deriving instance Generic RcvFileStatus

deriving instance Generic RcvFileTransfer

deriving instance Generic RcvQueueInfo

deriving instance Generic RcvSwitchStatus

deriving instance Generic ReportReason

deriving instance Generic SecurityCode

deriving instance Generic SendRef

deriving instance Generic SimplexLinkType

deriving instance Generic SndError

deriving instance Generic SndFileTransfer

deriving instance Generic SndQueueInfo

deriving instance Generic SndSwitchStatus

deriving instance Generic SrvError

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
