# API Types

This file is generated automatically.

- [ACIReaction](#acireaction)
- [AChat](#achat)
- [AChatInfo](#achatinfo)
- [AChatItem](#achatitem)
- [ACreatedConnLink](#acreatedconnlink)
- [AddressSettings](#addresssettings)
- [BusinessChatInfo](#businesschatinfo)
- [BusinessChatType](#businesschattype)
- [CChatItem](#cchatitem)
- [CIDeleteMode](#cideletemode)
- [CIDirection](#cidirection)
- [CIReaction](#cireaction)
- [ChatDeleteMode](#chatdeletemode)
- [ChatError](#chaterror)
- [ChatInfo](#chatinfo)
- [ChatItemDeletion](#chatitemdeletion)
- [ChatItemInfo](#chatiteminfo)
- [ChatItemVersion](#chatitemversion)
- [ChatListQuery](#chatlistquery)
- [ChatName](#chatname)
- [ChatPagination](#chatpagination)
- [ChatRef](#chatref)
- [ChatSettings](#chatsettings)
- [ChatType](#chattype)
- [ChatWallpaper](#chatwallpaper)
- [ChatWallpaperScale](#chatwallpaperscale)
- [ComposedMessage](#composedmessage)
- [ConnStatus](#connstatus)
- [ConnType](#conntype)
- [Connection](#connection)
- [ConnectionMode](#connectionmode)
- [ConnectionPlan](#connectionplan)
- [ConnectionStats](#connectionstats)
- [Contact](#contact)
- [ContactAddressPlan](#contactaddressplan)
- [ContactShortLinkData](#contactshortlinkdata)
- [ContactStatus](#contactstatus)
- [ContactUserPref](#contactuserpref)
- [ContactUserPreference](#contactuserpreference)
- [ContactUserPreferences](#contactuserpreferences)
- [CreatedConnLink](#createdconnlink)
- [CryptoFile](#cryptofile)
- [CryptoFileArgs](#cryptofileargs)
- [FeatureAllowed](#featureallowed)
- [FileDescr](#filedescr)
- [FileInvitation](#fileinvitation)
- [FileStatus](#filestatus)
- [FileTransferMeta](#filetransfermeta)
- [Format](#format)
- [FormatColor](#formatcolor)
- [FormattedText](#formattedtext)
- [FullGroupPreferences](#fullgrouppreferences)
- [FullPreferences](#fullpreferences)
- [Group](#group)
- [GroupChatScope](#groupchatscope)
- [GroupFeatureEnabled](#groupfeatureenabled)
- [GroupInfo](#groupinfo)
- [GroupInfoSummary](#groupinfosummary)
- [GroupLink](#grouplink)
- [GroupLinkPlan](#grouplinkplan)
- [GroupMember](#groupmember)
- [GroupMemberAdmission](#groupmemberadmission)
- [GroupMemberCategory](#groupmembercategory)
- [GroupMemberRole](#groupmemberrole)
- [GroupMemberSettings](#groupmembersettings)
- [GroupMemberStatus](#groupmemberstatus)
- [GroupPreference](#grouppreference)
- [GroupPreferences](#grouppreferences)
- [GroupProfile](#groupprofile)
- [GroupShortLinkData](#groupshortlinkdata)
- [GroupSndStatus](#groupsndstatus)
- [GroupSummary](#groupsummary)
- [GroupSupportChat](#groupsupportchat)
- [InlineFileMode](#inlinefilemode)
- [InvitationLinkPlan](#invitationlinkplan)
- [InvitedBy](#invitedby)
- [LinkContent](#linkcontent)
- [LinkPreview](#linkpreview)
- [LocalProfile](#localprofile)
- [MemberCriteria](#membercriteria)
- [MemberDeliveryStatus](#memberdeliverystatus)
- [MemberReaction](#memberreaction)
- [MsgChatLink](#msgchatlink)
- [MsgContent](#msgcontent)
- [MsgContentTag](#msgcontenttag)
- [MsgFilter](#msgfilter)
- [MsgReaction](#msgreaction)
- [MsgReceiptStatus](#msgreceiptstatus)
- [NavigationInfo](#navigationinfo)
- [NewUser](#newuser)
- [PaginationByTime](#paginationbytime)
- [PendingContactConnection](#pendingcontactconnection)
- [PrefEnabled](#prefenabled)
- [Preferences](#preferences)
- [PreparedContact](#preparedcontact)
- [PreparedGroup](#preparedgroup)
- [Profile](#profile)
- [RatchetSyncState](#ratchetsyncstate)
- [RcvFileDescr](#rcvfiledescr)
- [RcvFileInfo](#rcvfileinfo)
- [RcvFileStatus](#rcvfilestatus)
- [RcvFileTransfer](#rcvfiletransfer)
- [RcvQueueInfo](#rcvqueueinfo)
- [RcvSwitchStatus](#rcvswitchstatus)
- [ReportReason](#reportreason)
- [RoleGroupPreference](#rolegrouppreference)
- [SecurityCode](#securitycode)
- [SendRef](#sendref)
- [SimplePreference](#simplepreference)
- [SimplexLinkType](#simplexlinktype)
- [SndError](#snderror)
- [SndFileTransfer](#sndfiletransfer)
- [SndQueueInfo](#sndqueueinfo)
- [SndSwitchStatus](#sndswitchstatus)
- [SrvError](#srverror)
- [TimedMessagesGroupPreference](#timedmessagesgrouppreference)
- [TimedMessagesPreference](#timedmessagespreference)
- [UIColorMode](#uicolormode)
- [UIColors](#uicolors)
- [UIThemeEntityOverride](#uithemeentityoverride)
- [UIThemeEntityOverrides](#uithemeentityoverrides)
- [UpdatedMessage](#updatedmessage)
- [User](#user)
- [UserContactLink](#usercontactlink)
- [UserContactRequest](#usercontactrequest)
- [UserInfo](#userinfo)
- [UserProfileUpdateSummary](#userprofileupdatesummary)
- [UserPwdHash](#userpwdhash)
- [VersionRange](#versionrange)
- [XFTPRcvFile](#xftprcvfile)
- [XFTPSndFile](#xftpsndfile)


## ACIReaction

**Record type**:
- chatInfo: [ChatInfo](#chatinfo)
- chatReaction: [CIReaction](#cireaction)


## AChat


## AChatInfo


## AChatItem


## ACreatedConnLink


## AddressSettings


## BusinessChatInfo

**Record type**:
- chatType: [BusinessChatType](#businesschattype)
- businessId: String
- customerId: String


## BusinessChatType

**Enum type**:
- "business"
- "customer"


## CChatItem


## CIDeleteMode


## CIDirection


## CIReaction

**Record type**:
- chatDir: [CIDirection](#cidirection)
- chatItem: [CChatItem](#cchatitem)
- sentAt: UTCTime
- reaction: [MsgReaction](#msgreaction)


## ChatDeleteMode

**Discriminated union type**:

CDMFull record:
- type: "full"
- notify: Bool

CDMEntity record:
- type: "entity"
- notify: Bool

CDMMessages record:
- type: "messages"


## ChatError


## ChatInfo


## ChatItemDeletion

Message deletion result.

**Record type**:
- deletedChatItem: [AChatItem](#achatitem)
- toChatItem: [AChatItem](#achatitem)?


## ChatItemInfo

**Record type**:
- itemVersions: [[ChatItemVersion](#chatitemversion)]
- memberDeliveryStatuses: [[MemberDeliveryStatus](#memberdeliverystatus)]?
- forwardedFromChatItem: [AChatItem](#achatitem)?


## ChatItemVersion

**Record type**:
- chatItemVersionId: Int64
- msgContent: [MsgContent](#msgcontent)
- formattedText: [[FormattedText](#formattedtext)]?
- itemVersionTs: UTCTime
- createdAt: UTCTime


## ChatListQuery

**Discriminated union type**:

CLQFilters record:
- type: "filters"
- favorite: Bool
- unread: Bool

CLQSearch record:
- type: "search"
- search: String


## ChatName

**Record type**:
- chatType: [ChatType](#chattype)
- chatName: String


## ChatPagination


## ChatRef

**Record type**:
- : [ChatType](#chattype)
- : Int64
- : [GroupChatScope](#groupchatscope)?


## ChatSettings

**Record type**:
- enableNtfs: [MsgFilter](#msgfilter)
- sendRcpts: Bool?
- favorite: Bool


## ChatType

**Enum type**:
- "direct"
- "group"
- "local"
- "contactRequest"
- "contactConnection"


## ChatWallpaper

**Record type**:
- preset: String?
- imageFile: String?
- background: String?
- tint: String?
- scaleType: [ChatWallpaperScale](#chatwallpaperscale)?
- scale: Double?


## ChatWallpaperScale

**Enum type**:
- "fill"
- "fit"
- "repeat"


## ComposedMessage

**Record type**:
- fileSource: [CryptoFile](#cryptofile)?
- quotedItemId: Int64?
- msgContent: [MsgContent](#msgcontent)
- mentions: {String : Int64}


## ConnStatus


## ConnType

**Enum type**:
- "connContact"
- "connMember"
- "connSndFile"
- "connRcvFile"
- "connUserContact"


## Connection

**Record type**:
- connId: Int64
- agentConnId: String
- connChatVersion: Int
- peerChatVRange: [VersionRange](#versionrange)
- connLevel: Int
- viaContact: Int64?
- viaUserContactLink: Int64?
- viaGroupLink: Bool
- groupLinkId: String?
- xContactId: String?
- customUserProfileId: Int64?
- connType: [ConnType](#conntype)
- connStatus: [ConnStatus](#connstatus)
- contactConnInitiated: Bool
- localAlias: String
- entityId: Int64?
- connectionCode: [SecurityCode](#securitycode)?
- pqSupport: Bool
- pqEncryption: Bool
- pqSndEnabled: Bool?
- pqRcvEnabled: Bool?
- authErrCounter: Int
- quotaErrCounter: Int
- createdAt: UTCTime


## ConnectionMode

**Enum type**:
- "invitation"
- "contact"


## ConnectionPlan

**Discriminated union type**:

CPInvitationLink record:
- type: "invitationLink"
- invitationLinkPlan: [InvitationLinkPlan](#invitationlinkplan)

CPContactAddress record:
- type: "contactAddress"
- contactAddressPlan: [ContactAddressPlan](#contactaddressplan)

CPGroupLink record:
- type: "groupLink"
- groupLinkPlan: [GroupLinkPlan](#grouplinkplan)

CPError record:
- type: "error"
- chatError: [ChatError](#chaterror)


## ConnectionStats

**Record type**:
- connAgentVersion: Int
- rcvQueuesInfo: [[RcvQueueInfo](#rcvqueueinfo)]
- sndQueuesInfo: [[SndQueueInfo](#sndqueueinfo)]
- ratchetSyncState: [RatchetSyncState](#ratchetsyncstate)
- ratchetSyncSupported: Bool


## Contact

**Record type**:
- contactId: Int64
- localDisplayName: String
- profile: [LocalProfile](#localprofile)
- activeConn: [Connection](#connection)?
- viaGroup: Int64?
- contactUsed: Bool
- contactStatus: [ContactStatus](#contactstatus)
- chatSettings: [ChatSettings](#chatsettings)
- userPreferences: [Preferences](#preferences)
- mergedPreferences: [ContactUserPreferences](#contactuserpreferences)
- createdAt: UTCTime
- updatedAt: UTCTime
- chatTs: UTCTime?
- preparedContact: [PreparedContact](#preparedcontact)?
- contactRequestId: Int64?
- contactGroupMemberId: Int64?
- contactGrpInvSent: Bool
- chatTags: [Int64]
- chatItemTTL: Int64?
- uiThemes: [UIThemeEntityOverrides](#uithemeentityoverrides)?
- chatDeleted: Bool
- customData: JSONObject?


## ContactAddressPlan

**Discriminated union type**:

CAPOk record:
- type: "ok"
- contactSLinkData_: [ContactShortLinkData](#contactshortlinkdata)?

CAPOwnLink record:
- type: "ownLink"

CAPConnectingConfirmReconnect record:
- type: "connectingConfirmReconnect"

CAPConnectingProhibit record:
- type: "connectingProhibit"
- contact: [Contact](#contact)

CAPKnown record:
- type: "known"
- contact: [Contact](#contact)

CAPContactViaAddress record:
- type: "contactViaAddress"
- contact: [Contact](#contact)


## ContactShortLinkData

**Record type**:
- profile: [Profile](#profile)
- message: [MsgContent](#msgcontent)?
- business: Bool


## ContactStatus

**Enum type**:
- "active"
- "deleted"
- "deletedByUser"


## ContactUserPref

**Discriminated union type**:

CUPContact record:
- type: "contact"
- preference: [SimplePreference](#simplepreference)

CUPUser record:
- type: "user"
- preference: [SimplePreference](#simplepreference)


## ContactUserPreference

**Record type**:
- enabled: [PrefEnabled](#prefenabled)
- userPreference: [ContactUserPref](#contactuserpref)
- contactPreference: [SimplePreference](#simplepreference)


## ContactUserPreferences

**Record type**:
- timedMessages: [ContactUserPreference](#contactuserpreference)
- fullDelete: [ContactUserPreference](#contactuserpreference)
- reactions: [ContactUserPreference](#contactuserpreference)
- voice: [ContactUserPreference](#contactuserpreference)
- calls: [ContactUserPreference](#contactuserpreference)


## CreatedConnLink

**Record type**:
- connFullLink: String
- connShortLink: String?


## CryptoFile

**Record type**:
- filePath: String
- cryptoArgs: [CryptoFileArgs](#cryptofileargs)?


## CryptoFileArgs

**Record type**:
- fileKey: String
- fileNonce: String


## FeatureAllowed

**Enum type**:
- "always"
- "yes"
- "no"


## FileDescr

**Record type**:
- fileDescrText: String
- fileDescrPartNo: Int
- fileDescrComplete: Bool


## FileInvitation

**Record type**:
- fileName: String
- fileSize: Int64
- fileDigest: String?
- fileConnReq: String?
- fileInline: [InlineFileMode](#inlinefilemode)?
- fileDescr: [FileDescr](#filedescr)?


## FileStatus

**Enum type**:
- "new"
- "accepted"
- "connected"
- "complete"
- "cancelled"


## FileTransferMeta

**Record type**:
- fileId: Int64
- xftpSndFile: [XFTPSndFile](#xftpsndfile)?
- xftpRedirectFor: Int64?
- fileName: String
- filePath: String
- fileSize: Int64
- fileInline: [InlineFileMode](#inlinefilemode)?
- chunkSize: Int64
- cancelled: Bool


## Format

**Discriminated union type**:

Bold record:
- type: "bold"

Italic record:
- type: "italic"

StrikeThrough record:
- type: "strikeThrough"

Snippet record:
- type: "snippet"

Secret record:
- type: "secret"

Colored record:
- type: "colored"
- color: [FormatColor](#formatcolor)

Uri record:
- type: "uri"

SimplexLink record:
- type: "simplexLink"
- linkType: [SimplexLinkType](#simplexlinktype)
- simplexUri: String
- smpHosts: [String]

Mention record:
- type: "mention"
- memberName: String

Email record:
- type: "email"

Phone record:
- type: "phone"


## FormatColor


## FormattedText

**Record type**:
- format: [Format](#format)?
- text: String


## FullGroupPreferences

**Record type**:
- timedMessages: [TimedMessagesGroupPreference](#timedmessagesgrouppreference)
- directMessages: [RoleGroupPreference](#rolegrouppreference)
- fullDelete: [GroupPreference](#grouppreference)
- reactions: [GroupPreference](#grouppreference)
- voice: [RoleGroupPreference](#rolegrouppreference)
- files: [RoleGroupPreference](#rolegrouppreference)
- simplexLinks: [RoleGroupPreference](#rolegrouppreference)
- reports: [GroupPreference](#grouppreference)
- history: [GroupPreference](#grouppreference)


## FullPreferences

**Record type**:
- timedMessages: [TimedMessagesPreference](#timedmessagespreference)
- fullDelete: [SimplePreference](#simplepreference)
- reactions: [SimplePreference](#simplepreference)
- voice: [SimplePreference](#simplepreference)
- calls: [SimplePreference](#simplepreference)


## Group

**Record type**:
- groupInfo: [GroupInfo](#groupinfo)
- members: [[GroupMember](#groupmember)]


## GroupChatScope

**Discriminated union type**:

GCSMemberSupport record:
- type: "memberSupport"
- groupMemberId_: Int64?


## GroupFeatureEnabled

**Enum type**:
- "on"
- "off"


## GroupInfo

**Record type**:
- groupId: Int64
- localDisplayName: String
- groupProfile: [GroupProfile](#groupprofile)
- localAlias: String
- businessChat: [BusinessChatInfo](#businesschatinfo)?
- fullGroupPreferences: [FullGroupPreferences](#fullgrouppreferences)
- membership: [GroupMember](#groupmember)
- chatSettings: [ChatSettings](#chatsettings)
- createdAt: UTCTime
- updatedAt: UTCTime
- chatTs: UTCTime?
- userMemberProfileSentAt: UTCTime?
- preparedGroup: [PreparedGroup](#preparedgroup)?
- chatTags: [Int64]
- chatItemTTL: Int64?
- uiThemes: [UIThemeEntityOverrides](#uithemeentityoverrides)?
- customData: JSONObject?
- membersRequireAttention: Int


## GroupInfoSummary

**Record type**:
- groupInfo: [GroupInfo](#groupinfo)
- groupSummary: [GroupSummary](#groupsummary)


## GroupLink

**Record type**:
- userContactLinkId: Int64
- connLinkContact: [CreatedConnLink](#createdconnlink)
- shortLinkDataSet: Bool
- groupLinkId: String
- acceptMemberRole: [GroupMemberRole](#groupmemberrole)


## GroupLinkPlan

**Discriminated union type**:

GLPOk record:
- type: "ok"
- groupSLinkData_: [GroupShortLinkData](#groupshortlinkdata)?

GLPOwnLink record:
- type: "ownLink"
- groupInfo: [GroupInfo](#groupinfo)

GLPConnectingConfirmReconnect record:
- type: "connectingConfirmReconnect"

GLPConnectingProhibit record:
- type: "connectingProhibit"
- groupInfo_: [GroupInfo](#groupinfo)?

GLPKnown record:
- type: "known"
- groupInfo: [GroupInfo](#groupinfo)


## GroupMember

**Record type**:
- groupMemberId: Int64
- groupId: Int64
- memberId: String
- memberRole: [GroupMemberRole](#groupmemberrole)
- memberCategory: [GroupMemberCategory](#groupmembercategory)
- memberStatus: [GroupMemberStatus](#groupmemberstatus)
- memberSettings: [GroupMemberSettings](#groupmembersettings)
- blockedByAdmin: Bool
- invitedBy: [InvitedBy](#invitedby)
- invitedByGroupMemberId: Int64?
- localDisplayName: String
- memberProfile: [LocalProfile](#localprofile)
- memberContactId: Int64?
- memberContactProfileId: Int64
- activeConn: [Connection](#connection)?
- memberChatVRange: [VersionRange](#versionrange)
- createdAt: UTCTime
- updatedAt: UTCTime
- supportChat: [GroupSupportChat](#groupsupportchat)?


## GroupMemberAdmission

**Record type**:
- review: [MemberCriteria](#membercriteria)?


## GroupMemberCategory

**Enum type**:
- "user"
- "invitee"
- "host"
- "pre"
- "post"


## GroupMemberRole

**Enum type**:
- "observer"
- "author"
- "member"
- "moderator"
- "admin"
- "owner"


## GroupMemberSettings

**Record type**:
- showMessages: Bool


## GroupMemberStatus

**Enum type**:
- "gSMemRejected"
- "gSMemRemoved"
- "gSMemLeft"
- "gSMemGroupDeleted"
- "gSMemUnknown"
- "gSMemInvited"
- "gSMemPendingApproval"
- "gSMemPendingReview"
- "gSMemIntroduced"
- "gSMemIntroInvited"
- "gSMemAccepted"
- "gSMemAnnounced"
- "gSMemConnected"
- "gSMemComplete"
- "gSMemCreator"


## GroupPreference

**Record type**:
- enable: [GroupFeatureEnabled](#groupfeatureenabled)


## GroupPreferences

**Record type**:
- timedMessages: [TimedMessagesGroupPreference](#timedmessagesgrouppreference)?
- directMessages: [RoleGroupPreference](#rolegrouppreference)?
- fullDelete: [GroupPreference](#grouppreference)?
- reactions: [GroupPreference](#grouppreference)?
- voice: [RoleGroupPreference](#rolegrouppreference)?
- files: [RoleGroupPreference](#rolegrouppreference)?
- simplexLinks: [RoleGroupPreference](#rolegrouppreference)?
- reports: [GroupPreference](#grouppreference)?
- history: [GroupPreference](#grouppreference)?


## GroupProfile

**Record type**:
- displayName: String
- fullName: String
- shortDescr: String?
- description: String?
- image: String?
- groupPreferences: [GroupPreferences](#grouppreferences)?
- memberAdmission: [GroupMemberAdmission](#groupmemberadmission)?


## GroupShortLinkData

**Record type**:
- groupProfile: [GroupProfile](#groupprofile)


## GroupSndStatus

**Discriminated union type**:

GSSNew record:
- type: "new"

GSSForwarded record:
- type: "forwarded"

GSSInactive record:
- type: "inactive"

GSSSent record:
- type: "sent"

GSSRcvd record:
- type: "rcvd"
- msgRcptStatus: [MsgReceiptStatus](#msgreceiptstatus)

GSSError record:
- type: "error"
- agentError: [SndError](#snderror)

GSSWarning record:
- type: "warning"
- agentError: [SndError](#snderror)

GSSInvalid record:
- type: "invalid"
- text: String


## GroupSummary

**Record type**:
- currentMembers: Int


## GroupSupportChat

**Record type**:
- chatTs: UTCTime
- unread: Int64
- memberAttention: Int64
- mentions: Int64
- lastMsgFromMemberTs: UTCTime?


## InlineFileMode

**Enum type**:
- "offer"
- "sent"


## InvitationLinkPlan

**Discriminated union type**:

ILPOk record:
- type: "ok"
- contactSLinkData_: [ContactShortLinkData](#contactshortlinkdata)?

ILPOwnLink record:
- type: "ownLink"

ILPConnecting record:
- type: "connecting"
- contact_: [Contact](#contact)?

ILPKnown record:
- type: "known"
- contact: [Contact](#contact)


## InvitedBy

**Discriminated union type**:

IBContact record:
- type: "contact"
- byContactId: Int64

IBUser record:
- type: "user"

IBUnknown record:
- type: "unknown"


## LinkContent

**Discriminated union type**:

LCPage record:
- type: "page"

LCImage record:
- type: "image"

LCVideo record:
- type: "video"
- duration: Int?

LCUnknown record:
- type: "unknown"
- tag: String
- json: JSONObject


## LinkPreview

**Record type**:
- uri: String
- title: String
- description: String
- image: String
- content: [LinkContent](#linkcontent)?


## LocalProfile

**Record type**:
- profileId: Int64
- displayName: String
- fullName: String
- shortDescr: String?
- image: String?
- contactLink: String?
- preferences: [Preferences](#preferences)?
- localAlias: String


## MemberCriteria

**Enum type**:
- "all"


## MemberDeliveryStatus

**Record type**:
- groupMemberId: Int64
- memberDeliveryStatus: [GroupSndStatus](#groupsndstatus)
- sentViaProxy: Bool?


## MemberReaction

**Record type**:
- groupMember: [GroupMember](#groupmember)
- reactionTs: UTCTime


## MsgChatLink

Connection link sent in a message - only short links are allowed.

**Discriminated union type**:

MCLContact record:
- type: "contact"
- connLink: String
- profile: [Profile](#profile)
- business: Bool

MCLInvitation record:
- type: "invitation"
- invLink: String
- profile: [Profile](#profile)

MCLGroup record:
- type: "group"
- connLink: String
- groupProfile: [GroupProfile](#groupprofile)


## MsgContent

**Discriminated union type**:

MCText record:
- type: "text"
- text: String

MCLink record:
- type: "link"
- text: String
- preview: [LinkPreview](#linkpreview)

MCImage record:
- type: "image"
- text: String
- image: String

MCVideo record:
- type: "video"
- text: String
- image: String
- duration: Int

MCVoice record:
- type: "voice"
- text: String
- duration: Int

MCFile record:
- type: "file"
- text: String

MCReport record:
- type: "report"
- text: String
- reason: [ReportReason](#reportreason)

MCChat record:
- type: "chat"
- text: String
- chatLink: [MsgChatLink](#msgchatlink)

MCUnknown record:
- type: "unknown"
- tag: String
- text: String
- json: JSONObject


## MsgContentTag

**Enum type**:
- "text"
- "link"
- "image"
- "video"
- "voice"
- "file"
- "report"
- "chat"


## MsgFilter

**Enum type**:
- "none"
- "all"
- "mentions"


## MsgReaction

**Discriminated union type**:

MREmoji record:
- type: "emoji"
- emoji: String

MRUnknown record:
- type: "unknown"
- tag: String
- json: JSONObject


## MsgReceiptStatus

**Enum type**:
- "ok"
- "badMsgHash"


## NavigationInfo

**Record type**:
- afterUnread: Int
- afterTotal: Int


## NewUser

**Record type**:
- profile: [Profile](#profile)?
- pastTimestamp: Bool


## PaginationByTime


## PendingContactConnection

**Record type**:
- pccConnId: Int64
- pccAgentConnId: String
- pccConnStatus: [ConnStatus](#connstatus)
- viaContactUri: Bool
- viaUserContactLink: Int64?
- groupLinkId: String?
- customUserProfileId: Int64?
- connLinkInv: [CreatedConnLink](#createdconnlink)?
- localAlias: String
- createdAt: UTCTime
- updatedAt: UTCTime


## PrefEnabled

**Record type**:
- forUser: Bool
- forContact: Bool


## Preferences

**Record type**:
- timedMessages: [TimedMessagesPreference](#timedmessagespreference)?
- fullDelete: [SimplePreference](#simplepreference)?
- reactions: [SimplePreference](#simplepreference)?
- voice: [SimplePreference](#simplepreference)?
- calls: [SimplePreference](#simplepreference)?


## PreparedContact

**Record type**:
- connLinkToConnect: [ACreatedConnLink](#acreatedconnlink)
- uiConnLinkType: [ConnectionMode](#connectionmode)
- welcomeSharedMsgId: String?
- requestSharedMsgId: String?


## PreparedGroup

**Record type**:
- connLinkToConnect: [CreatedConnLink](#createdconnlink)
- connLinkPreparedConnection: Bool
- connLinkStartedConnection: Bool
- welcomeSharedMsgId: String?
- requestSharedMsgId: String?


## Profile

**Record type**:
- displayName: String
- fullName: String
- shortDescr: String?
- image: String?
- contactLink: String?
- preferences: [Preferences](#preferences)?


## RatchetSyncState

**Enum type**:
- "ok"
- "allowed"
- "required"
- "started"
- "agreed"


## RcvFileDescr

**Record type**:
- fileDescrId: Int64
- fileDescrText: String
- fileDescrPartNo: Int
- fileDescrComplete: Bool


## RcvFileInfo

**Record type**:
- filePath: String
- connId: Int64?
- agentConnId: String?


## RcvFileStatus

**Discriminated union type**:

RFSNew record:
- type: "new"

RFSAccepted record:
- type: "accepted"
- : [RcvFileInfo](#rcvfileinfo)

RFSConnected record:
- type: "connected"
- : [RcvFileInfo](#rcvfileinfo)

RFSComplete record:
- type: "complete"
- : [RcvFileInfo](#rcvfileinfo)

RFSCancelled record:
- type: "cancelled"
- : [RcvFileInfo](#rcvfileinfo)?


## RcvFileTransfer

**Record type**:
- fileId: Int64
- xftpRcvFile: [XFTPRcvFile](#xftprcvfile)?
- fileInvitation: [FileInvitation](#fileinvitation)
- fileStatus: [RcvFileStatus](#rcvfilestatus)
- rcvFileInline: [InlineFileMode](#inlinefilemode)?
- senderDisplayName: String
- chunkSize: Int64
- cancelled: Bool
- grpMemberId: Int64?
- cryptoArgs: [CryptoFileArgs](#cryptofileargs)?


## RcvQueueInfo

**Record type**:
- rcvServer: String
- rcvSwitchStatus: [RcvSwitchStatus](#rcvswitchstatus)?
- canAbortSwitch: Bool


## RcvSwitchStatus

**Enum type**:
- "rSSwitchStarted"
- "rSSendingQADD"
- "rSSendingQUSE"
- "rSReceivedMessage"


## ReportReason

**Enum type**:
- "spam"
- "content"
- "community"
- "profile"
- "other"


## RoleGroupPreference

**Record type**:
- enable: [GroupFeatureEnabled](#groupfeatureenabled)
- role: [GroupMemberRole](#groupmemberrole)?


## SecurityCode

**Record type**:
- securityCode: String
- verifiedAt: UTCTime


## SendRef


## SimplePreference

**Record type**:
- allow: [FeatureAllowed](#featureallowed)


## SimplexLinkType

**Enum type**:
- "contact"
- "invitation"
- "group"
- "channel"


## SndError

**Discriminated union type**:

SndErrAuth record:
- type: "auth"

SndErrQuota record:
- type: "quota"

SndErrExpired record:
- type: "expired"

SndErrRelay record:
- type: "relay"
- srvError: [SrvError](#srverror)

SndErrProxy record:
- type: "proxy"
- proxyServer: String
- srvError: [SrvError](#srverror)

SndErrProxyRelay record:
- type: "proxyRelay"
- proxyServer: String
- srvError: [SrvError](#srverror)

SndErrOther record:
- type: "other"
- sndError: String


## SndFileTransfer

**Record type**:
- fileId: Int64
- fileName: String
- filePath: String
- fileSize: Int64
- chunkSize: Int64
- recipientDisplayName: String
- connId: Int64
- agentConnId: String
- groupMemberId: Int64?
- fileStatus: [FileStatus](#filestatus)
- fileDescrId: Int64?
- fileInline: [InlineFileMode](#inlinefilemode)?


## SndQueueInfo

**Record type**:
- sndServer: String
- sndSwitchStatus: [SndSwitchStatus](#sndswitchstatus)?


## SndSwitchStatus

**Enum type**:
- "sSSendingQKEY"
- "sSSendingQTEST"


## SrvError

**Discriminated union type**:

SrvErrHost record:
- type: "host"

SrvErrVersion record:
- type: "version"

SrvErrOther record:
- type: "other"
- srvError: String


## TimedMessagesGroupPreference

**Record type**:
- enable: [GroupFeatureEnabled](#groupfeatureenabled)
- ttl: Int?


## TimedMessagesPreference

**Record type**:
- allow: [FeatureAllowed](#featureallowed)
- ttl: Int?


## UIColorMode

**Enum type**:
- "light"
- "dark"


## UIColors

**Record type**:
- accent: String?
- accentVariant: String?
- secondary: String?
- secondaryVariant: String?
- background: String?
- menus: String?
- title: String?
- accentVariant2: String?
- sentMessage: String?
- sentReply: String?
- receivedMessage: String?
- receivedReply: String?


## UIThemeEntityOverride

**Record type**:
- mode: [UIColorMode](#uicolormode)
- wallpaper: [ChatWallpaper](#chatwallpaper)?
- colors: [UIColors](#uicolors)


## UIThemeEntityOverrides

**Record type**:
- light: [UIThemeEntityOverride](#uithemeentityoverride)?
- dark: [UIThemeEntityOverride](#uithemeentityoverride)?


## UpdatedMessage

**Record type**:
- msgContent: [MsgContent](#msgcontent)
- mentions: {String : Int64}


## User

**Record type**:
- userId: Int64
- agentUserId: Int64
- userContactId: Int64
- localDisplayName: String
- profile: [LocalProfile](#localprofile)
- fullPreferences: [FullPreferences](#fullpreferences)
- activeUser: Bool
- activeOrder: Int64
- viewPwdHash: [UserPwdHash](#userpwdhash)?
- showNtfs: Bool
- sendRcptsContacts: Bool
- sendRcptsSmallGroups: Bool
- userMemberProfileUpdatedAt: UTCTime?
- uiThemes: [UIThemeEntityOverrides](#uithemeentityoverrides)?


## UserContactLink

**Record type**:
- userContactLinkId: Int64
- connLinkContact: [CreatedConnLink](#createdconnlink)
- shortLinkDataSet: Bool
- addressSettings: [AddressSettings](#addresssettings)


## UserContactRequest

**Record type**:
- contactRequestId: Int64
- agentInvitationId: String
- contactId_: Int64?
- businessGroupId_: Int64?
- userContactLinkId_: Int64?
- cReqChatVRange: [VersionRange](#versionrange)
- localDisplayName: String
- profileId: Int64
- profile: [Profile](#profile)
- createdAt: UTCTime
- updatedAt: UTCTime
- xContactId: String?
- pqSupport: Bool
- welcomeSharedMsgId: String?
- requestSharedMsgId: String?


## UserInfo

**Record type**:
- user: [User](#user)
- unreadCount: Int


## UserProfileUpdateSummary

**Record type**:
- updateSuccesses: Int
- updateFailures: Int
- changedContacts: [[Contact](#contact)]


## UserPwdHash

**Record type**:
- hash: String
- salt: String


## VersionRange

**Record type**:
- minVersion: Int
- maxVersion: Int


## XFTPRcvFile

**Record type**:
- rcvFileDescription: [RcvFileDescr](#rcvfiledescr)
- agentRcvFileId: String?
- agentRcvFileDeleted: Bool
- userApprovedRelays: Bool


## XFTPSndFile

**Record type**:
- agentSndFileId: String
- privateSndFileDescr: String?
- agentSndFileDeleted: Bool
- cryptoArgs: [CryptoFileArgs](#cryptofileargs)?
