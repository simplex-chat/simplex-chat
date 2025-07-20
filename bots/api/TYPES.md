# API Types

This file is generated automatically.

- [ACIReaction](#acireaction)
- [AChatItem](#achatitem)
- [AddressSettings](#addresssettings)
- [AutoAccept](#autoaccept)
- [BlockingInfo](#blockinginfo)
- [BlockingReason](#blockingreason)
- [BusinessChatInfo](#businesschatinfo)
- [BusinessChatType](#businesschattype)
- [CICallStatus](#cicallstatus)
- [CIContent](#cicontent)
- [CIDeleteMode](#cideletemode)
- [CIDeleted](#cideleted)
- [CIDirection](#cidirection)
- [CIFile](#cifile)
- [CIFileStatus](#cifilestatus)
- [CIForwardedFrom](#ciforwardedfrom)
- [CIGroupInvitation](#cigroupinvitation)
- [CIGroupInvitationStatus](#cigroupinvitationstatus)
- [CIMention](#cimention)
- [CIMentionMember](#cimentionmember)
- [CIMeta](#cimeta)
- [CIQDirection](#ciqdirection)
- [CIQuote](#ciquote)
- [CIReaction](#cireaction)
- [CIReactionCount](#cireactioncount)
- [CIStatus](#cistatus)
- [CITimed](#citimed)
- [ChatDeleteMode](#chatdeletemode)
- [ChatError](#chaterror)
- [ChatFeature](#chatfeature)
- [ChatInfo](#chatinfo)
- [ChatItem](#chatitem)
- [ChatItemDeletion](#chatitemdeletion)
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
- [E2EInfo](#e2einfo)
- [FeatureAllowed](#featureallowed)
- [FileDescr](#filedescr)
- [FileError](#fileerror)
- [FileInvitation](#fileinvitation)
- [FileProtocol](#fileprotocol)
- [FileStatus](#filestatus)
- [FileTransferMeta](#filetransfermeta)
- [Format](#format)
- [FormatColor](#formatcolor)
- [FormattedText](#formattedtext)
- [FullGroupPreferences](#fullgrouppreferences)
- [FullPreferences](#fullpreferences)
- [GroupChatScope](#groupchatscope)
- [GroupChatScopeInfo](#groupchatscopeinfo)
- [GroupFeature](#groupfeature)
- [GroupFeatureEnabled](#groupfeatureenabled)
- [GroupInfo](#groupinfo)
- [GroupInfoSummary](#groupinfosummary)
- [GroupLink](#grouplink)
- [GroupLinkPlan](#grouplinkplan)
- [GroupMember](#groupmember)
- [GroupMemberAdmission](#groupmemberadmission)
- [GroupMemberCategory](#groupmembercategory)
- [GroupMemberRef](#groupmemberref)
- [GroupMemberRole](#groupmemberrole)
- [GroupMemberSettings](#groupmembersettings)
- [GroupMemberStatus](#groupmemberstatus)
- [GroupPreference](#grouppreference)
- [GroupPreferences](#grouppreferences)
- [GroupProfile](#groupprofile)
- [GroupShortLinkData](#groupshortlinkdata)
- [GroupSummary](#groupsummary)
- [GroupSupportChat](#groupsupportchat)
- [InlineFileMode](#inlinefilemode)
- [InvitationLinkPlan](#invitationlinkplan)
- [InvitedBy](#invitedby)
- [JSONObject](#jsonobject)
- [LinkContent](#linkcontent)
- [LinkPreview](#linkpreview)
- [LocalProfile](#localprofile)
- [MemberCriteria](#membercriteria)
- [MsgChatLink](#msgchatlink)
- [MsgContent](#msgcontent)
- [MsgDecryptError](#msgdecrypterror)
- [MsgDirection](#msgdirection)
- [MsgErrorType](#msgerrortype)
- [MsgFilter](#msgfilter)
- [MsgReaction](#msgreaction)
- [MsgReceiptStatus](#msgreceiptstatus)
- [NewUser](#newuser)
- [NoteFolder](#notefolder)
- [PendingContactConnection](#pendingcontactconnection)
- [PrefEnabled](#prefenabled)
- [Preferences](#preferences)
- [PreparedContact](#preparedcontact)
- [PreparedGroup](#preparedgroup)
- [Profile](#profile)
- [RatchetSyncState](#ratchetsyncstate)
- [RcvConnEvent](#rcvconnevent)
- [RcvDirectEvent](#rcvdirectevent)
- [RcvFileDescr](#rcvfiledescr)
- [RcvFileInfo](#rcvfileinfo)
- [RcvFileStatus](#rcvfilestatus)
- [RcvFileTransfer](#rcvfiletransfer)
- [RcvGroupEvent](#rcvgroupevent)
- [ReportReason](#reportreason)
- [RoleGroupPreference](#rolegrouppreference)
- [SecurityCode](#securitycode)
- [SendRef](#sendref)
- [SimplePreference](#simplepreference)
- [SimplexLinkType](#simplexlinktype)
- [SndCIStatusProgress](#sndcistatusprogress)
- [SndConnEvent](#sndconnevent)
- [SndError](#snderror)
- [SndFileTransfer](#sndfiletransfer)
- [SndGroupEvent](#sndgroupevent)
- [SrvError](#srverror)
- [SwitchPhase](#switchphase)
- [TimedMessagesGroupPreference](#timedmessagesgrouppreference)
- [TimedMessagesPreference](#timedmessagespreference)
- [UIColorMode](#uicolormode)
- [UIColors](#uicolors)
- [UIThemeEntityOverride](#uithemeentityoverride)
- [UIThemeEntityOverrides](#uithemeentityoverrides)
- [UTCTime](#utctime)
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


---

## ACIReaction

**Record type**:
- chatInfo: [ChatInfo](#chatinfo)
- chatReaction: [CIReaction](#cireaction)


---

## AChatItem

**Record type**:
- chatInfo: [ChatInfo](#chatinfo)
- chatItem: [ChatItem](#chatitem)


---

## AddressSettings

**Record type**:
- businessAddress: bool
- autoAccept: [AutoAccept](#autoaccept)?
- autoReply: [MsgContent](#msgcontent)?


---

## AutoAccept

**Record type**:
- acceptIncognito: bool


---

## BlockingInfo

**Record type**:
- reason: [BlockingReason](#blockingreason)


---

## BlockingReason

**Enum type**:
- "spam"
- "content"


---

## BusinessChatInfo

**Record type**:
- chatType: [BusinessChatType](#businesschattype)
- businessId: string
- customerId: string


---

## BusinessChatType

**Enum type**:
- "business"
- "customer"


---

## CICallStatus

**Enum type**:
- "pending"
- "missed"
- "rejected"
- "accepted"
- "negotiated"
- "progress"
- "ended"
- "error"


---

## CIContent

**Discriminated union type**:

SndMsgContent:
- type: "sndMsgContent"
- msgContent: [MsgContent](#msgcontent)

RcvMsgContent:
- type: "rcvMsgContent"
- msgContent: [MsgContent](#msgcontent)

SndDeleted:
- type: "sndDeleted"
- deleteMode: [CIDeleteMode](#cideletemode)

RcvDeleted:
- type: "rcvDeleted"
- deleteMode: [CIDeleteMode](#cideletemode)

SndCall:
- type: "sndCall"
- status: [CICallStatus](#cicallstatus)
- duration: int

RcvCall:
- type: "rcvCall"
- status: [CICallStatus](#cicallstatus)
- duration: int

RcvIntegrityError:
- type: "rcvIntegrityError"
- msgError: [MsgErrorType](#msgerrortype)

RcvDecryptionError:
- type: "rcvDecryptionError"
- msgDecryptError: [MsgDecryptError](#msgdecrypterror)
- msgCount: word32

RcvGroupInvitation:
- type: "rcvGroupInvitation"
- groupInvitation: [CIGroupInvitation](#cigroupinvitation)
- memberRole: [GroupMemberRole](#groupmemberrole)

SndGroupInvitation:
- type: "sndGroupInvitation"
- groupInvitation: [CIGroupInvitation](#cigroupinvitation)
- memberRole: [GroupMemberRole](#groupmemberrole)

RcvDirectEvent:
- type: "rcvDirectEvent"
- rcvDirectEvent: [RcvDirectEvent](#rcvdirectevent)

RcvGroupEvent:
- type: "rcvGroupEvent"
- rcvGroupEvent: [RcvGroupEvent](#rcvgroupevent)

SndGroupEvent:
- type: "sndGroupEvent"
- sndGroupEvent: [SndGroupEvent](#sndgroupevent)

RcvConnEvent:
- type: "rcvConnEvent"
- rcvConnEvent: [RcvConnEvent](#rcvconnevent)

SndConnEvent:
- type: "sndConnEvent"
- sndConnEvent: [SndConnEvent](#sndconnevent)

RcvChatFeature:
- type: "rcvChatFeature"
- feature: [ChatFeature](#chatfeature)
- enabled: [PrefEnabled](#prefenabled)
- param: int?

SndChatFeature:
- type: "sndChatFeature"
- feature: [ChatFeature](#chatfeature)
- enabled: [PrefEnabled](#prefenabled)
- param: int?

RcvChatPreference:
- type: "rcvChatPreference"
- feature: [ChatFeature](#chatfeature)
- allowed: [FeatureAllowed](#featureallowed)
- param: int?

SndChatPreference:
- type: "sndChatPreference"
- feature: [ChatFeature](#chatfeature)
- allowed: [FeatureAllowed](#featureallowed)
- param: int?

RcvGroupFeature:
- type: "rcvGroupFeature"
- groupFeature: [GroupFeature](#groupfeature)
- preference: [GroupPreference](#grouppreference)
- param: int?
- memberRole_: [GroupMemberRole](#groupmemberrole)?

SndGroupFeature:
- type: "sndGroupFeature"
- groupFeature: [GroupFeature](#groupfeature)
- preference: [GroupPreference](#grouppreference)
- param: int?
- memberRole_: [GroupMemberRole](#groupmemberrole)?

RcvChatFeatureRejected:
- type: "rcvChatFeatureRejected"
- feature: [ChatFeature](#chatfeature)

RcvGroupFeatureRejected:
- type: "rcvGroupFeatureRejected"
- groupFeature: [GroupFeature](#groupfeature)

SndModerated:
- type: "sndModerated"

RcvModerated:
- type: "rcvModerated"

RcvBlocked:
- type: "rcvBlocked"

SndDirectE2EEInfo:
- type: "sndDirectE2EEInfo"
- e2eeInfo: [E2EInfo](#e2einfo)

RcvDirectE2EEInfo:
- type: "rcvDirectE2EEInfo"
- e2eeInfo: [E2EInfo](#e2einfo)

SndGroupE2EEInfo:
- type: "sndGroupE2EEInfo"
- e2eeInfo: [E2EInfo](#e2einfo)

RcvGroupE2EEInfo:
- type: "rcvGroupE2EEInfo"
- e2eeInfo: [E2EInfo](#e2einfo)

InvalidJSON:
- type: "invalidJSON"
- direction: [MsgDirection](#msgdirection)
- json: string


---

## CIDeleteMode

**Enum type**:
- "broadcast"
- "internal"
- "internalMark"


---

## CIDeleted

**Discriminated union type**:

Deleted:
- type: "deleted"
- deletedTs: [UTCTime](#utctime)?
- chatType: [ChatType](#chattype)

Blocked:
- type: "blocked"
- deletedTs: [UTCTime](#utctime)?

BlockedByAdmin:
- type: "blockedByAdmin"
- deletedTs: [UTCTime](#utctime)?

Moderated:
- type: "moderated"
- deletedTs: [UTCTime](#utctime)?
- byGroupMember: [GroupMember](#groupmember)


---

## CIDirection

**Discriminated union type**:

DirectSnd:
- type: "directSnd"

DirectRcv:
- type: "directRcv"

GroupSnd:
- type: "groupSnd"

GroupRcv:
- type: "groupRcv"
- groupMember: [GroupMember](#groupmember)

LocalSnd:
- type: "localSnd"

LocalRcv:
- type: "localRcv"


---

## CIFile

**Record type**:
- fileId: int64
- fileName: string
- fileSize: int64
- fileSource: [CryptoFile](#cryptofile)?
- fileStatus: [CIFileStatus](#cifilestatus)
- fileProtocol: [FileProtocol](#fileprotocol)


---

## CIFileStatus

**Discriminated union type**:

SndStored:
- type: "sndStored"

SndTransfer:
- type: "sndTransfer"
- sndProgress: int64
- sndTotal: int64

SndCancelled:
- type: "sndCancelled"

SndComplete:
- type: "sndComplete"

SndError:
- type: "sndError"
- sndFileError: [FileError](#fileerror)

SndWarning:
- type: "sndWarning"
- sndFileError: [FileError](#fileerror)

RcvInvitation:
- type: "rcvInvitation"

RcvAccepted:
- type: "rcvAccepted"

RcvTransfer:
- type: "rcvTransfer"
- rcvProgress: int64
- rcvTotal: int64

RcvAborted:
- type: "rcvAborted"

RcvComplete:
- type: "rcvComplete"

RcvCancelled:
- type: "rcvCancelled"

RcvError:
- type: "rcvError"
- rcvFileError: [FileError](#fileerror)

RcvWarning:
- type: "rcvWarning"
- rcvFileError: [FileError](#fileerror)

Invalid:
- type: "invalid"
- text: string


---

## CIForwardedFrom

**Discriminated union type**:

Unknown:
- type: "unknown"

Contact:
- type: "contact"
- chatName: string
- msgDir: [MsgDirection](#msgdirection)
- contactId: int64?
- chatItemId: int64?

Group:
- type: "group"
- chatName: string
- msgDir: [MsgDirection](#msgdirection)
- groupId: int64?
- chatItemId: int64?


---

## CIGroupInvitation

**Record type**:
- groupId: int64
- groupMemberId: int64
- localDisplayName: string
- groupProfile: [GroupProfile](#groupprofile)
- status: [CIGroupInvitationStatus](#cigroupinvitationstatus)


---

## CIGroupInvitationStatus

**Enum type**:
- "pending"
- "accepted"
- "rejected"
- "expired"


---

## CIMention

**Record type**:
- memberId: string
- memberRef: [CIMentionMember](#cimentionmember)?


---

## CIMentionMember

**Record type**:
- groupMemberId: int64
- displayName: string
- localAlias: string?
- memberRole: [GroupMemberRole](#groupmemberrole)


---

## CIMeta

**Record type**:
- itemId: int64
- itemTs: [UTCTime](#utctime)
- itemText: string
- itemStatus: [CIStatus](#cistatus)
- sentViaProxy: bool?
- itemSharedMsgId: string?
- itemForwarded: [CIForwardedFrom](#ciforwardedfrom)?
- itemDeleted: [CIDeleted](#cideleted)?
- itemEdited: bool
- itemTimed: [CITimed](#citimed)?
- itemLive: bool?
- userMention: bool
- deletable: bool
- editable: bool
- forwardedByMember: int64?
- showGroupAsSender: bool
- createdAt: [UTCTime](#utctime)
- updatedAt: [UTCTime](#utctime)


---

## CIQDirection

**Discriminated union type**:

DirectSnd:
- type: "directSnd"

DirectRcv:
- type: "directRcv"

GroupSnd:
- type: "groupSnd"

GroupRcv:
- type: "groupRcv"
- groupMember: [GroupMember](#groupmember)

LocalSnd:
- type: "localSnd"

LocalRcv:
- type: "localRcv"


---

## CIQuote

**Record type**:
- chatDir: [CIQDirection](#ciqdirection)
- itemId: int64?
- sharedMsgId: string?
- sentAt: [UTCTime](#utctime)
- content: [MsgContent](#msgcontent)
- formattedText: [[FormattedText](#formattedtext)]?


---

## CIReaction

**Record type**:
- chatDir: [CIDirection](#cidirection)
- chatItem: [ChatItem](#chatitem)
- sentAt: [UTCTime](#utctime)
- reaction: [MsgReaction](#msgreaction)


---

## CIReactionCount

**Record type**:
- reaction: [MsgReaction](#msgreaction)
- userReacted: bool
- totalReacted: int


---

## CIStatus

**Discriminated union type**:

SndNew:
- type: "sndNew"

SndSent:
- type: "sndSent"
- sndProgress: [SndCIStatusProgress](#sndcistatusprogress)

SndRcvd:
- type: "sndRcvd"
- msgRcptStatus: [MsgReceiptStatus](#msgreceiptstatus)
- sndProgress: [SndCIStatusProgress](#sndcistatusprogress)

SndErrorAuth:
- type: "sndErrorAuth"

SndError:
- type: "sndError"
- agentError: [SndError](#snderror)

SndWarning:
- type: "sndWarning"
- agentError: [SndError](#snderror)

RcvNew:
- type: "rcvNew"

RcvRead:
- type: "rcvRead"

Invalid:
- type: "invalid"
- text: string


---

## CITimed

**Record type**:
- ttl: int
- deleteAt: [UTCTime](#utctime)?


---

## ChatDeleteMode

**Discriminated union type**:

Full:
- type: "full"
- notify: bool

Entity:
- type: "entity"
- notify: bool

Messages:
- type: "messages"


---

## ChatError


---

## ChatFeature

**Enum type**:
- "timedMessages"
- "fullDelete"
- "reactions"
- "voice"
- "calls"


---

## ChatInfo

**Discriminated union type**:

Direct:
- type: "direct"
- contact: [Contact](#contact)

Group:
- type: "group"
- groupInfo: [GroupInfo](#groupinfo)
- groupChatScope: [GroupChatScopeInfo](#groupchatscopeinfo)?

Local:
- type: "local"
- noteFolder: [NoteFolder](#notefolder)

ContactRequest:
- type: "contactRequest"
- contactRequest: [UserContactRequest](#usercontactrequest)

ContactConnection:
- type: "contactConnection"
- contactConnection: [PendingContactConnection](#pendingcontactconnection)


---

## ChatItem

**Record type**:
- chatDir: [CIDirection](#cidirection)
- meta: [CIMeta](#cimeta)
- content: [CIContent](#cicontent)
- mentions: {string : [CIMention](#cimention)}
- formattedText: [[FormattedText](#formattedtext)]?
- quotedItem: [CIQuote](#ciquote)?
- reactions: [[CIReactionCount](#cireactioncount)]
- file: [CIFile](#cifile)?


---

## ChatItemDeletion

Message deletion result.

**Record type**:
- deletedChatItem: [AChatItem](#achatitem)
- toChatItem: [AChatItem](#achatitem)?


---

## ChatRef


---

## ChatSettings

**Record type**:
- enableNtfs: [MsgFilter](#msgfilter)
- sendRcpts: bool?
- favorite: bool


---

## ChatType

**Enum type**:
- "direct"
- "group"
- "local"
- "contactRequest"
- "contactConnection"


---

## ChatWallpaper

**Record type**:
- preset: string?
- imageFile: string?
- background: string?
- tint: string?
- scaleType: [ChatWallpaperScale](#chatwallpaperscale)?
- scale: double?


---

## ChatWallpaperScale

**Enum type**:
- "fill"
- "fit"
- "repeat"


---

## ComposedMessage

**Record type**:
- fileSource: [CryptoFile](#cryptofile)?
- quotedItemId: int64?
- msgContent: [MsgContent](#msgcontent)
- mentions: {string : int64}


---

## ConnStatus


---

## ConnType

**Enum type**:
- "connContact"
- "connMember"
- "connSndFile"
- "connRcvFile"
- "connUserContact"


---

## Connection

**Record type**:
- connId: int64
- agentConnId: string
- connChatVersion: int
- peerChatVRange: [VersionRange](#versionrange)
- connLevel: int
- viaContact: int64?
- viaUserContactLink: int64?
- viaGroupLink: bool
- groupLinkId: string?
- xContactId: string?
- customUserProfileId: int64?
- connType: [ConnType](#conntype)
- connStatus: [ConnStatus](#connstatus)
- contactConnInitiated: bool
- localAlias: string
- entityId: int64?
- connectionCode: [SecurityCode](#securitycode)?
- pqSupport: bool
- pqEncryption: bool
- pqSndEnabled: bool?
- pqRcvEnabled: bool?
- authErrCounter: int
- quotaErrCounter: int
- createdAt: [UTCTime](#utctime)


---

## ConnectionMode

**Enum type**:
- "invitation"
- "contact"


---

## ConnectionPlan

**Discriminated union type**:

InvitationLink:
- type: "invitationLink"
- invitationLinkPlan: [InvitationLinkPlan](#invitationlinkplan)

ContactAddress:
- type: "contactAddress"
- contactAddressPlan: [ContactAddressPlan](#contactaddressplan)

GroupLink:
- type: "groupLink"
- groupLinkPlan: [GroupLinkPlan](#grouplinkplan)

Error:
- type: "error"
- chatError: [ChatError](#chaterror)


---

## Contact

**Record type**:
- contactId: int64
- localDisplayName: string
- profile: [LocalProfile](#localprofile)
- activeConn: [Connection](#connection)?
- viaGroup: int64?
- contactUsed: bool
- contactStatus: [ContactStatus](#contactstatus)
- chatSettings: [ChatSettings](#chatsettings)
- userPreferences: [Preferences](#preferences)
- mergedPreferences: [ContactUserPreferences](#contactuserpreferences)
- createdAt: [UTCTime](#utctime)
- updatedAt: [UTCTime](#utctime)
- chatTs: [UTCTime](#utctime)?
- preparedContact: [PreparedContact](#preparedcontact)?
- contactRequestId: int64?
- contactGroupMemberId: int64?
- contactGrpInvSent: bool
- chatTags: [int64]
- chatItemTTL: int64?
- uiThemes: [UIThemeEntityOverrides](#uithemeentityoverrides)?
- chatDeleted: bool
- customData: [JSONObject](#jsonobject)?


---

## ContactAddressPlan

**Discriminated union type**:

Ok:
- type: "ok"
- contactSLinkData_: [ContactShortLinkData](#contactshortlinkdata)?

OwnLink:
- type: "ownLink"

ConnectingConfirmReconnect:
- type: "connectingConfirmReconnect"

ConnectingProhibit:
- type: "connectingProhibit"
- contact: [Contact](#contact)

Known:
- type: "known"
- contact: [Contact](#contact)

ContactViaAddress:
- type: "contactViaAddress"
- contact: [Contact](#contact)


---

## ContactShortLinkData

**Record type**:
- profile: [Profile](#profile)
- message: [MsgContent](#msgcontent)?
- business: bool


---

## ContactStatus

**Enum type**:
- "active"
- "deleted"
- "deletedByUser"


---

## ContactUserPref

**Discriminated union type**:

Contact:
- type: "contact"
- preference: [SimplePreference](#simplepreference)

User:
- type: "user"
- preference: [SimplePreference](#simplepreference)


---

## ContactUserPreference

**Record type**:
- enabled: [PrefEnabled](#prefenabled)
- userPreference: [ContactUserPref](#contactuserpref)
- contactPreference: [SimplePreference](#simplepreference)


---

## ContactUserPreferences

**Record type**:
- timedMessages: [ContactUserPreference](#contactuserpreference)
- fullDelete: [ContactUserPreference](#contactuserpreference)
- reactions: [ContactUserPreference](#contactuserpreference)
- voice: [ContactUserPreference](#contactuserpreference)
- calls: [ContactUserPreference](#contactuserpreference)


---

## CreatedConnLink

**Record type**:
- connFullLink: string
- connShortLink: string?


---

## CryptoFile

**Record type**:
- filePath: string
- cryptoArgs: [CryptoFileArgs](#cryptofileargs)?


---

## CryptoFileArgs

**Record type**:
- fileKey: string
- fileNonce: string


---

## E2EInfo

**Record type**:
- pqEnabled: bool?


---

## FeatureAllowed

**Enum type**:
- "always"
- "yes"
- "no"


---

## FileDescr

**Record type**:
- fileDescrText: string
- fileDescrPartNo: int
- fileDescrComplete: bool


---

## FileError

**Discriminated union type**:

Auth:
- type: "auth"

Blocked:
- type: "blocked"
- server: string
- blockInfo: [BlockingInfo](#blockinginfo)

NoFile:
- type: "noFile"

Relay:
- type: "relay"
- srvError: [SrvError](#srverror)

Other:
- type: "other"
- fileError: string


---

## FileInvitation

**Record type**:
- fileName: string
- fileSize: int64
- fileDigest: string?
- fileConnReq: string?
- fileInline: [InlineFileMode](#inlinefilemode)?
- fileDescr: [FileDescr](#filedescr)?


---

## FileProtocol

**Enum type**:
- "sMP"
- "xFTP"
- "local"


---

## FileStatus

**Enum type**:
- "new"
- "accepted"
- "connected"
- "complete"
- "cancelled"


---

## FileTransferMeta

**Record type**:
- fileId: int64
- xftpSndFile: [XFTPSndFile](#xftpsndfile)?
- xftpRedirectFor: int64?
- fileName: string
- filePath: string
- fileSize: int64
- fileInline: [InlineFileMode](#inlinefilemode)?
- chunkSize: int64
- cancelled: bool


---

## Format

**Discriminated union type**:

Bold:
- type: "bold"

Italic:
- type: "italic"

StrikeThrough:
- type: "strikeThrough"

Snippet:
- type: "snippet"

Secret:
- type: "secret"

Colored:
- type: "colored"
- color: [FormatColor](#formatcolor)

Uri:
- type: "uri"

SimplexLink:
- type: "simplexLink"
- linkType: [SimplexLinkType](#simplexlinktype)
- simplexUri: string
- smpHosts: [string]

Mention:
- type: "mention"
- memberName: string

Email:
- type: "email"

Phone:
- type: "phone"


---

## FormatColor


---

## FormattedText

**Record type**:
- format: [Format](#format)?
- text: string


---

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


---

## FullPreferences

**Record type**:
- timedMessages: [TimedMessagesPreference](#timedmessagespreference)
- fullDelete: [SimplePreference](#simplepreference)
- reactions: [SimplePreference](#simplepreference)
- voice: [SimplePreference](#simplepreference)
- calls: [SimplePreference](#simplepreference)


---

## GroupChatScope

**Discriminated union type**:

MemberSupport:
- type: "memberSupport"
- groupMemberId_: int64?


---

## GroupChatScopeInfo

**Discriminated union type**:

MemberSupport:
- type: "memberSupport"
- groupMember_: [GroupMember](#groupmember)?


---

## GroupFeature

**Enum type**:
- "timedMessages"
- "directMessages"
- "fullDelete"
- "reactions"
- "voice"
- "files"
- "simplexLinks"
- "reports"
- "history"


---

## GroupFeatureEnabled

**Enum type**:
- "on"
- "off"


---

## GroupInfo

**Record type**:
- groupId: int64
- localDisplayName: string
- groupProfile: [GroupProfile](#groupprofile)
- localAlias: string
- businessChat: [BusinessChatInfo](#businesschatinfo)?
- fullGroupPreferences: [FullGroupPreferences](#fullgrouppreferences)
- membership: [GroupMember](#groupmember)
- chatSettings: [ChatSettings](#chatsettings)
- createdAt: [UTCTime](#utctime)
- updatedAt: [UTCTime](#utctime)
- chatTs: [UTCTime](#utctime)?
- userMemberProfileSentAt: [UTCTime](#utctime)?
- preparedGroup: [PreparedGroup](#preparedgroup)?
- chatTags: [int64]
- chatItemTTL: int64?
- uiThemes: [UIThemeEntityOverrides](#uithemeentityoverrides)?
- customData: [JSONObject](#jsonobject)?
- membersRequireAttention: int


---

## GroupInfoSummary

**Record type**:
- groupInfo: [GroupInfo](#groupinfo)
- groupSummary: [GroupSummary](#groupsummary)


---

## GroupLink

**Record type**:
- userContactLinkId: int64
- connLinkContact: [CreatedConnLink](#createdconnlink)
- shortLinkDataSet: bool
- groupLinkId: string
- acceptMemberRole: [GroupMemberRole](#groupmemberrole)


---

## GroupLinkPlan

**Discriminated union type**:

Ok:
- type: "ok"
- groupSLinkData_: [GroupShortLinkData](#groupshortlinkdata)?

OwnLink:
- type: "ownLink"
- groupInfo: [GroupInfo](#groupinfo)

ConnectingConfirmReconnect:
- type: "connectingConfirmReconnect"

ConnectingProhibit:
- type: "connectingProhibit"
- groupInfo_: [GroupInfo](#groupinfo)?

Known:
- type: "known"
- groupInfo: [GroupInfo](#groupinfo)


---

## GroupMember

**Record type**:
- groupMemberId: int64
- groupId: int64
- memberId: string
- memberRole: [GroupMemberRole](#groupmemberrole)
- memberCategory: [GroupMemberCategory](#groupmembercategory)
- memberStatus: [GroupMemberStatus](#groupmemberstatus)
- memberSettings: [GroupMemberSettings](#groupmembersettings)
- blockedByAdmin: bool
- invitedBy: [InvitedBy](#invitedby)
- invitedByGroupMemberId: int64?
- localDisplayName: string
- memberProfile: [LocalProfile](#localprofile)
- memberContactId: int64?
- memberContactProfileId: int64
- activeConn: [Connection](#connection)?
- memberChatVRange: [VersionRange](#versionrange)
- createdAt: [UTCTime](#utctime)
- updatedAt: [UTCTime](#utctime)
- supportChat: [GroupSupportChat](#groupsupportchat)?


---

## GroupMemberAdmission

**Record type**:
- review: [MemberCriteria](#membercriteria)?


---

## GroupMemberCategory

**Enum type**:
- "user"
- "invitee"
- "host"
- "pre"
- "post"


---

## GroupMemberRef

**Record type**:
- groupMemberId: int64
- profile: [Profile](#profile)


---

## GroupMemberRole

**Enum type**:
- "observer"
- "author"
- "member"
- "moderator"
- "admin"
- "owner"


---

## GroupMemberSettings

**Record type**:
- showMessages: bool


---

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


---

## GroupPreference

**Record type**:
- enable: [GroupFeatureEnabled](#groupfeatureenabled)


---

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


---

## GroupProfile

**Record type**:
- displayName: string
- fullName: string
- shortDescr: string?
- description: string?
- image: string?
- groupPreferences: [GroupPreferences](#grouppreferences)?
- memberAdmission: [GroupMemberAdmission](#groupmemberadmission)?


---

## GroupShortLinkData

**Record type**:
- groupProfile: [GroupProfile](#groupprofile)


---

## GroupSummary

**Record type**:
- currentMembers: int


---

## GroupSupportChat

**Record type**:
- chatTs: [UTCTime](#utctime)
- unread: int64
- memberAttention: int64
- mentions: int64
- lastMsgFromMemberTs: [UTCTime](#utctime)?


---

## InlineFileMode

**Enum type**:
- "offer"
- "sent"


---

## InvitationLinkPlan

**Discriminated union type**:

Ok:
- type: "ok"
- contactSLinkData_: [ContactShortLinkData](#contactshortlinkdata)?

OwnLink:
- type: "ownLink"

Connecting:
- type: "connecting"
- contact_: [Contact](#contact)?

Known:
- type: "known"
- contact: [Contact](#contact)


---

## InvitedBy

**Discriminated union type**:

Contact:
- type: "contact"
- byContactId: int64

User:
- type: "user"

Unknown:
- type: "unknown"


---

## JSONObject

Arbitrary JSON object.


---

## LinkContent

**Discriminated union type**:

Page:
- type: "page"

Image:
- type: "image"

Video:
- type: "video"
- duration: int?

Unknown:
- type: "unknown"
- tag: string
- json: [JSONObject](#jsonobject)


---

## LinkPreview

**Record type**:
- uri: string
- title: string
- description: string
- image: string
- content: [LinkContent](#linkcontent)?


---

## LocalProfile

**Record type**:
- profileId: int64
- displayName: string
- fullName: string
- shortDescr: string?
- image: string?
- contactLink: string?
- preferences: [Preferences](#preferences)?
- localAlias: string


---

## MemberCriteria

**Enum type**:
- "all"


---

## MsgChatLink

Connection link sent in a message - only short links are allowed.

**Discriminated union type**:

Contact:
- type: "contact"
- connLink: string
- profile: [Profile](#profile)
- business: bool

Invitation:
- type: "invitation"
- invLink: string
- profile: [Profile](#profile)

Group:
- type: "group"
- connLink: string
- groupProfile: [GroupProfile](#groupprofile)


---

## MsgContent

**Discriminated union type**:

Text:
- type: "text"
- text: string

Link:
- type: "link"
- text: string
- preview: [LinkPreview](#linkpreview)

Image:
- type: "image"
- text: string
- image: string

Video:
- type: "video"
- text: string
- image: string
- duration: int

Voice:
- type: "voice"
- text: string
- duration: int

File:
- type: "file"
- text: string

Report:
- type: "report"
- text: string
- reason: [ReportReason](#reportreason)

Chat:
- type: "chat"
- text: string
- chatLink: [MsgChatLink](#msgchatlink)

Unknown:
- type: "unknown"
- tag: string
- text: string
- json: [JSONObject](#jsonobject)


---

## MsgDecryptError

**Enum type**:
- "ratchetHeader"
- "tooManySkipped"
- "ratchetEarlier"
- "other"
- "ratchetSync"


---

## MsgDirection

**Enum type**:
- "rcv"
- "snd"


---

## MsgErrorType

**Discriminated union type**:

MsgSkipped:
- type: "msgSkipped"
- fromMsgId: int64
- toMsgId: int64

MsgBadId:
- type: "msgBadId"
- msgId: int64

MsgBadHash:
- type: "msgBadHash"

MsgDuplicate:
- type: "msgDuplicate"


---

## MsgFilter

**Enum type**:
- "none"
- "all"
- "mentions"


---

## MsgReaction

**Discriminated union type**:

Emoji:
- type: "emoji"
- emoji: string

Unknown:
- type: "unknown"
- tag: string
- json: [JSONObject](#jsonobject)


---

## MsgReceiptStatus

**Enum type**:
- "ok"
- "badMsgHash"


---

## NewUser

**Record type**:
- profile: [Profile](#profile)?
- pastTimestamp: bool


---

## NoteFolder

**Record type**:
- noteFolderId: int64
- userId: int64
- createdAt: [UTCTime](#utctime)
- updatedAt: [UTCTime](#utctime)
- chatTs: [UTCTime](#utctime)
- favorite: bool
- unread: bool


---

## PendingContactConnection

**Record type**:
- pccConnId: int64
- pccAgentConnId: string
- pccConnStatus: [ConnStatus](#connstatus)
- viaContactUri: bool
- viaUserContactLink: int64?
- groupLinkId: string?
- customUserProfileId: int64?
- connLinkInv: [CreatedConnLink](#createdconnlink)?
- localAlias: string
- createdAt: [UTCTime](#utctime)
- updatedAt: [UTCTime](#utctime)


---

## PrefEnabled

**Record type**:
- forUser: bool
- forContact: bool


---

## Preferences

**Record type**:
- timedMessages: [TimedMessagesPreference](#timedmessagespreference)?
- fullDelete: [SimplePreference](#simplepreference)?
- reactions: [SimplePreference](#simplepreference)?
- voice: [SimplePreference](#simplepreference)?
- calls: [SimplePreference](#simplepreference)?


---

## PreparedContact

**Record type**:
- connLinkToConnect: [CreatedConnLink](#createdconnlink)
- uiConnLinkType: [ConnectionMode](#connectionmode)
- welcomeSharedMsgId: string?
- requestSharedMsgId: string?


---

## PreparedGroup

**Record type**:
- connLinkToConnect: [CreatedConnLink](#createdconnlink)
- connLinkPreparedConnection: bool
- connLinkStartedConnection: bool
- welcomeSharedMsgId: string?
- requestSharedMsgId: string?


---

## Profile

**Record type**:
- displayName: string
- fullName: string
- shortDescr: string?
- image: string?
- contactLink: string?
- preferences: [Preferences](#preferences)?


---

## RatchetSyncState

**Enum type**:
- "ok"
- "allowed"
- "required"
- "started"
- "agreed"


---

## RcvConnEvent

**Discriminated union type**:

SwitchQueue:
- type: "switchQueue"
- phase: [SwitchPhase](#switchphase)

RatchetSync:
- type: "ratchetSync"
- syncStatus: [RatchetSyncState](#ratchetsyncstate)

VerificationCodeReset:
- type: "verificationCodeReset"

PqEnabled:
- type: "pqEnabled"
- enabled: bool


---

## RcvDirectEvent

**Discriminated union type**:

ContactDeleted:
- type: "contactDeleted"

ProfileUpdated:
- type: "profileUpdated"
- fromProfile: [Profile](#profile)
- toProfile: [Profile](#profile)


---

## RcvFileDescr

**Record type**:
- fileDescrId: int64
- fileDescrText: string
- fileDescrPartNo: int
- fileDescrComplete: bool


---

## RcvFileInfo

**Record type**:
- filePath: string
- connId: int64?
- agentConnId: string?


---

## RcvFileStatus

**Discriminated union type**:

New:
- type: "new"

Accepted:
- type: "accepted"
- fileInfo: [RcvFileInfo](#rcvfileinfo)

Connected:
- type: "connected"
- fileInfo: [RcvFileInfo](#rcvfileinfo)

Complete:
- type: "complete"
- fileInfo: [RcvFileInfo](#rcvfileinfo)

Cancelled:
- type: "cancelled"
- fileInfo_: [RcvFileInfo](#rcvfileinfo)?


---

## RcvFileTransfer

**Record type**:
- fileId: int64
- xftpRcvFile: [XFTPRcvFile](#xftprcvfile)?
- fileInvitation: [FileInvitation](#fileinvitation)
- fileStatus: [RcvFileStatus](#rcvfilestatus)
- rcvFileInline: [InlineFileMode](#inlinefilemode)?
- senderDisplayName: string
- chunkSize: int64
- cancelled: bool
- grpMemberId: int64?
- cryptoArgs: [CryptoFileArgs](#cryptofileargs)?


---

## RcvGroupEvent

**Discriminated union type**:

MemberAdded:
- type: "memberAdded"
- groupMemberId: int64
- profile: [Profile](#profile)

MemberConnected:
- type: "memberConnected"

MemberAccepted:
- type: "memberAccepted"
- groupMemberId: int64
- profile: [Profile](#profile)

UserAccepted:
- type: "userAccepted"

MemberLeft:
- type: "memberLeft"

MemberRole:
- type: "memberRole"
- groupMemberId: int64
- profile: [Profile](#profile)
- role: [GroupMemberRole](#groupmemberrole)

MemberBlocked:
- type: "memberBlocked"
- groupMemberId: int64
- profile: [Profile](#profile)
- blocked: bool

UserRole:
- type: "userRole"
- role: [GroupMemberRole](#groupmemberrole)

MemberDeleted:
- type: "memberDeleted"
- groupMemberId: int64
- profile: [Profile](#profile)

UserDeleted:
- type: "userDeleted"

GroupDeleted:
- type: "groupDeleted"

GroupUpdated:
- type: "groupUpdated"
- groupProfile: [GroupProfile](#groupprofile)

InvitedViaGroupLink:
- type: "invitedViaGroupLink"

MemberCreatedContact:
- type: "memberCreatedContact"

MemberProfileUpdated:
- type: "memberProfileUpdated"
- fromProfile: [Profile](#profile)
- toProfile: [Profile](#profile)

NewMemberPendingReview:
- type: "newMemberPendingReview"


---

## ReportReason

**Enum type**:
- "spam"
- "content"
- "community"
- "profile"
- "other"


---

## RoleGroupPreference

**Record type**:
- enable: [GroupFeatureEnabled](#groupfeatureenabled)
- role: [GroupMemberRole](#groupmemberrole)?


---

## SecurityCode

**Record type**:
- securityCode: string
- verifiedAt: [UTCTime](#utctime)


---

## SendRef


---

## SimplePreference

**Record type**:
- allow: [FeatureAllowed](#featureallowed)


---

## SimplexLinkType

**Enum type**:
- "contact"
- "invitation"
- "group"
- "channel"


---

## SndCIStatusProgress

**Enum type**:
- "partial"
- "complete"


---

## SndConnEvent

**Discriminated union type**:

SwitchQueue:
- type: "switchQueue"
- phase: [SwitchPhase](#switchphase)
- member: [GroupMemberRef](#groupmemberref)?

RatchetSync:
- type: "ratchetSync"
- syncStatus: [RatchetSyncState](#ratchetsyncstate)
- member: [GroupMemberRef](#groupmemberref)?

PqEnabled:
- type: "pqEnabled"
- enabled: bool


---

## SndError

**Discriminated union type**:

Auth:
- type: "auth"

Quota:
- type: "quota"

Expired:
- type: "expired"

Relay:
- type: "relay"
- srvError: [SrvError](#srverror)

Proxy:
- type: "proxy"
- proxyServer: string
- srvError: [SrvError](#srverror)

ProxyRelay:
- type: "proxyRelay"
- proxyServer: string
- srvError: [SrvError](#srverror)

Other:
- type: "other"
- sndError: string


---

## SndFileTransfer

**Record type**:
- fileId: int64
- fileName: string
- filePath: string
- fileSize: int64
- chunkSize: int64
- recipientDisplayName: string
- connId: int64
- agentConnId: string
- groupMemberId: int64?
- fileStatus: [FileStatus](#filestatus)
- fileDescrId: int64?
- fileInline: [InlineFileMode](#inlinefilemode)?


---

## SndGroupEvent

**Discriminated union type**:

MemberRole:
- type: "memberRole"
- groupMemberId: int64
- profile: [Profile](#profile)
- role: [GroupMemberRole](#groupmemberrole)

MemberBlocked:
- type: "memberBlocked"
- groupMemberId: int64
- profile: [Profile](#profile)
- blocked: bool

UserRole:
- type: "userRole"
- role: [GroupMemberRole](#groupmemberrole)

MemberDeleted:
- type: "memberDeleted"
- groupMemberId: int64
- profile: [Profile](#profile)

UserLeft:
- type: "userLeft"

GroupUpdated:
- type: "groupUpdated"
- groupProfile: [GroupProfile](#groupprofile)

MemberAccepted:
- type: "memberAccepted"
- groupMemberId: int64
- profile: [Profile](#profile)

UserPendingReview:
- type: "userPendingReview"


---

## SrvError

**Discriminated union type**:

Host:
- type: "host"

Version:
- type: "version"

Other:
- type: "other"
- srvError: string


---

## SwitchPhase

**Enum type**:
- "started"
- "confirmed"
- "secured"
- "completed"


---

## TimedMessagesGroupPreference

**Record type**:
- enable: [GroupFeatureEnabled](#groupfeatureenabled)
- ttl: int?


---

## TimedMessagesPreference

**Record type**:
- allow: [FeatureAllowed](#featureallowed)
- ttl: int?


---

## UIColorMode

**Enum type**:
- "light"
- "dark"


---

## UIColors

**Record type**:
- accent: string?
- accentVariant: string?
- secondary: string?
- secondaryVariant: string?
- background: string?
- menus: string?
- title: string?
- accentVariant2: string?
- sentMessage: string?
- sentReply: string?
- receivedMessage: string?
- receivedReply: string?


---

## UIThemeEntityOverride

**Record type**:
- mode: [UIColorMode](#uicolormode)
- wallpaper: [ChatWallpaper](#chatwallpaper)?
- colors: [UIColors](#uicolors)


---

## UIThemeEntityOverrides

**Record type**:
- light: [UIThemeEntityOverride](#uithemeentityoverride)?
- dark: [UIThemeEntityOverride](#uithemeentityoverride)?


---

## UTCTime

Timestampe in ISO8601 format as string.


---

## UpdatedMessage

**Record type**:
- msgContent: [MsgContent](#msgcontent)
- mentions: {string : int64}


---

## User

**Record type**:
- userId: int64
- agentUserId: int64
- userContactId: int64
- localDisplayName: string
- profile: [LocalProfile](#localprofile)
- fullPreferences: [FullPreferences](#fullpreferences)
- activeUser: bool
- activeOrder: int64
- viewPwdHash: [UserPwdHash](#userpwdhash)?
- showNtfs: bool
- sendRcptsContacts: bool
- sendRcptsSmallGroups: bool
- userMemberProfileUpdatedAt: [UTCTime](#utctime)?
- uiThemes: [UIThemeEntityOverrides](#uithemeentityoverrides)?


---

## UserContactLink

**Record type**:
- userContactLinkId: int64
- connLinkContact: [CreatedConnLink](#createdconnlink)
- shortLinkDataSet: bool
- addressSettings: [AddressSettings](#addresssettings)


---

## UserContactRequest

**Record type**:
- contactRequestId: int64
- agentInvitationId: string
- contactId_: int64?
- businessGroupId_: int64?
- userContactLinkId_: int64?
- cReqChatVRange: [VersionRange](#versionrange)
- localDisplayName: string
- profileId: int64
- profile: [Profile](#profile)
- createdAt: [UTCTime](#utctime)
- updatedAt: [UTCTime](#utctime)
- xContactId: string?
- pqSupport: bool
- welcomeSharedMsgId: string?
- requestSharedMsgId: string?


---

## UserInfo

**Record type**:
- user: [User](#user)
- unreadCount: int


---

## UserProfileUpdateSummary

**Record type**:
- updateSuccesses: int
- updateFailures: int
- changedContacts: [[Contact](#contact)]


---

## UserPwdHash

**Record type**:
- hash: string
- salt: string


---

## VersionRange

**Record type**:
- minVersion: int
- maxVersion: int


---

## XFTPRcvFile

**Record type**:
- rcvFileDescription: [RcvFileDescr](#rcvfiledescr)
- agentRcvFileId: string?
- agentRcvFileDeleted: bool
- userApprovedRelays: bool


---

## XFTPSndFile

**Record type**:
- agentSndFileId: string
- privateSndFileDescr: string?
- agentSndFileDeleted: bool
- cryptoArgs: [CryptoFileArgs](#cryptofileargs)?
