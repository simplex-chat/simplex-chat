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
- businessAddress: Bool
- autoAccept: [AutoAccept](#autoaccept)?
- autoReply: [MsgContent](#msgcontent)?


---

## AutoAccept

**Record type**:
- acceptIncognito: Bool


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
- businessId: String
- customerId: String


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
- duration: Int

RcvCall:
- type: "rcvCall"
- status: [CICallStatus](#cicallstatus)
- duration: Int

RcvIntegrityError:
- type: "rcvIntegrityError"
- msgError: [MsgErrorType](#msgerrortype)

RcvDecryptionError:
- type: "rcvDecryptionError"
- msgDecryptError: [MsgDecryptError](#msgdecrypterror)
- msgCount: Word32

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
- param: Int?

SndChatFeature:
- type: "sndChatFeature"
- feature: [ChatFeature](#chatfeature)
- enabled: [PrefEnabled](#prefenabled)
- param: Int?

RcvChatPreference:
- type: "rcvChatPreference"
- feature: [ChatFeature](#chatfeature)
- allowed: [FeatureAllowed](#featureallowed)
- param: Int?

SndChatPreference:
- type: "sndChatPreference"
- feature: [ChatFeature](#chatfeature)
- allowed: [FeatureAllowed](#featureallowed)
- param: Int?

RcvGroupFeature:
- type: "rcvGroupFeature"
- groupFeature: [GroupFeature](#groupfeature)
- preference: [GroupPreference](#grouppreference)
- param: Int?
- memberRole_: [GroupMemberRole](#groupmemberrole)?

SndGroupFeature:
- type: "sndGroupFeature"
- groupFeature: [GroupFeature](#groupfeature)
- preference: [GroupPreference](#grouppreference)
- param: Int?
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
- json: String


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
- deletedTs: UTCTime?
- chatType: [ChatType](#chattype)

Blocked:
- type: "blocked"
- deletedTs: UTCTime?

BlockedByAdmin:
- type: "blockedByAdmin"
- deletedTs: UTCTime?

Moderated:
- type: "moderated"
- deletedTs: UTCTime?
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
- fileId: Int64
- fileName: String
- fileSize: Int64
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
- sndProgress: Int64
- sndTotal: Int64

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
- rcvProgress: Int64
- rcvTotal: Int64

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
- text: String


---

## CIForwardedFrom

**Discriminated union type**:

Unknown:
- type: "unknown"

Contact:
- type: "contact"
- chatName: String
- msgDir: [MsgDirection](#msgdirection)
- contactId: Int64?
- chatItemId: Int64?

Group:
- type: "group"
- chatName: String
- msgDir: [MsgDirection](#msgdirection)
- groupId: Int64?
- chatItemId: Int64?


---

## CIGroupInvitation

**Record type**:
- groupId: Int64
- groupMemberId: Int64
- localDisplayName: String
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
- memberId: String
- memberRef: [CIMentionMember](#cimentionmember)?


---

## CIMentionMember

**Record type**:
- groupMemberId: Int64
- displayName: String
- localAlias: String?
- memberRole: [GroupMemberRole](#groupmemberrole)


---

## CIMeta

**Record type**:
- itemId: Int64
- itemTs: UTCTime
- itemText: String
- itemStatus: [CIStatus](#cistatus)
- sentViaProxy: Bool?
- itemSharedMsgId: String?
- itemForwarded: [CIForwardedFrom](#ciforwardedfrom)?
- itemDeleted: [CIDeleted](#cideleted)?
- itemEdited: Bool
- itemTimed: [CITimed](#citimed)?
- itemLive: Bool?
- userMention: Bool
- deletable: Bool
- editable: Bool
- forwardedByMember: Int64?
- showGroupAsSender: Bool
- createdAt: UTCTime
- updatedAt: UTCTime


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
- itemId: Int64?
- sharedMsgId: String?
- sentAt: UTCTime
- content: [MsgContent](#msgcontent)
- formattedText: [[FormattedText](#formattedtext)]?


---

## CIReaction

**Record type**:
- chatDir: [CIDirection](#cidirection)
- chatItem: [ChatItem](#chatitem)
- sentAt: UTCTime
- reaction: [MsgReaction](#msgreaction)


---

## CIReactionCount

**Record type**:
- reaction: [MsgReaction](#msgreaction)
- userReacted: Bool
- totalReacted: Int


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
- text: String


---

## CITimed

**Record type**:
- ttl: Int
- deleteAt: UTCTime?


---

## ChatDeleteMode

**Discriminated union type**:

Full:
- type: "full"
- notify: Bool

Entity:
- type: "entity"
- notify: Bool

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
- mentions: {String : [CIMention](#cimention)}
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
- sendRcpts: Bool?
- favorite: Bool


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
- preset: String?
- imageFile: String?
- background: String?
- tint: String?
- scaleType: [ChatWallpaperScale](#chatwallpaperscale)?
- scale: Double?


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
- quotedItemId: Int64?
- msgContent: [MsgContent](#msgcontent)
- mentions: {String : Int64}


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
- business: Bool


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
- connFullLink: String
- connShortLink: String?


---

## CryptoFile

**Record type**:
- filePath: String
- cryptoArgs: [CryptoFileArgs](#cryptofileargs)?


---

## CryptoFileArgs

**Record type**:
- fileKey: String
- fileNonce: String


---

## E2EInfo

**Record type**:
- pqEnabled: Bool?


---

## FeatureAllowed

**Enum type**:
- "always"
- "yes"
- "no"


---

## FileDescr

**Record type**:
- fileDescrText: String
- fileDescrPartNo: Int
- fileDescrComplete: Bool


---

## FileError

**Discriminated union type**:

Auth:
- type: "auth"

Blocked:
- type: "blocked"
- server: String
- blockInfo: [BlockingInfo](#blockinginfo)

NoFile:
- type: "noFile"

Relay:
- type: "relay"
- srvError: [SrvError](#srverror)

Other:
- type: "other"
- fileError: String


---

## FileInvitation

**Record type**:
- fileName: String
- fileSize: Int64
- fileDigest: String?
- fileConnReq: String?
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
- fileId: Int64
- xftpSndFile: [XFTPSndFile](#xftpsndfile)?
- xftpRedirectFor: Int64?
- fileName: String
- filePath: String
- fileSize: Int64
- fileInline: [InlineFileMode](#inlinefilemode)?
- chunkSize: Int64
- cancelled: Bool


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
- simplexUri: String
- smpHosts: [String]

Mention:
- type: "mention"
- memberName: String

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
- text: String


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
- groupMemberId_: Int64?


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


---

## GroupInfoSummary

**Record type**:
- groupInfo: [GroupInfo](#groupinfo)
- groupSummary: [GroupSummary](#groupsummary)


---

## GroupLink

**Record type**:
- userContactLinkId: Int64
- connLinkContact: [CreatedConnLink](#createdconnlink)
- shortLinkDataSet: Bool
- groupLinkId: String
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
- groupMemberId: Int64
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
- showMessages: Bool


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
- displayName: String
- fullName: String
- shortDescr: String?
- description: String?
- image: String?
- groupPreferences: [GroupPreferences](#grouppreferences)?
- memberAdmission: [GroupMemberAdmission](#groupmemberadmission)?


---

## GroupShortLinkData

**Record type**:
- groupProfile: [GroupProfile](#groupprofile)


---

## GroupSummary

**Record type**:
- currentMembers: Int


---

## GroupSupportChat

**Record type**:
- chatTs: UTCTime
- unread: Int64
- memberAttention: Int64
- mentions: Int64
- lastMsgFromMemberTs: UTCTime?


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
- byContactId: Int64

User:
- type: "user"

Unknown:
- type: "unknown"


---

## LinkContent

**Discriminated union type**:

Page:
- type: "page"

Image:
- type: "image"

Video:
- type: "video"
- duration: Int?

Unknown:
- type: "unknown"
- tag: String
- json: JSONObject


---

## LinkPreview

**Record type**:
- uri: String
- title: String
- description: String
- image: String
- content: [LinkContent](#linkcontent)?


---

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
- connLink: String
- profile: [Profile](#profile)
- business: Bool

Invitation:
- type: "invitation"
- invLink: String
- profile: [Profile](#profile)

Group:
- type: "group"
- connLink: String
- groupProfile: [GroupProfile](#groupprofile)


---

## MsgContent

**Discriminated union type**:

Text:
- type: "text"
- text: String

Link:
- type: "link"
- text: String
- preview: [LinkPreview](#linkpreview)

Image:
- type: "image"
- text: String
- image: String

Video:
- type: "video"
- text: String
- image: String
- duration: Int

Voice:
- type: "voice"
- text: String
- duration: Int

File:
- type: "file"
- text: String

Report:
- type: "report"
- text: String
- reason: [ReportReason](#reportreason)

Chat:
- type: "chat"
- text: String
- chatLink: [MsgChatLink](#msgchatlink)

Unknown:
- type: "unknown"
- tag: String
- text: String
- json: JSONObject


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
- fromMsgId: Int64
- toMsgId: Int64

MsgBadId:
- type: "msgBadId"
- msgId: Int64

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
- emoji: String

Unknown:
- type: "unknown"
- tag: String
- json: JSONObject


---

## MsgReceiptStatus

**Enum type**:
- "ok"
- "badMsgHash"


---

## NewUser

**Record type**:
- profile: [Profile](#profile)?
- pastTimestamp: Bool


---

## NoteFolder

**Record type**:
- noteFolderId: Int64
- userId: Int64
- createdAt: UTCTime
- updatedAt: UTCTime
- chatTs: UTCTime
- favorite: Bool
- unread: Bool


---

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


---

## PrefEnabled

**Record type**:
- forUser: Bool
- forContact: Bool


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
- welcomeSharedMsgId: String?
- requestSharedMsgId: String?


---

## PreparedGroup

**Record type**:
- connLinkToConnect: [CreatedConnLink](#createdconnlink)
- connLinkPreparedConnection: Bool
- connLinkStartedConnection: Bool
- welcomeSharedMsgId: String?
- requestSharedMsgId: String?


---

## Profile

**Record type**:
- displayName: String
- fullName: String
- shortDescr: String?
- image: String?
- contactLink: String?
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
- enabled: Bool


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
- fileDescrId: Int64
- fileDescrText: String
- fileDescrPartNo: Int
- fileDescrComplete: Bool


---

## RcvFileInfo

**Record type**:
- filePath: String
- connId: Int64?
- agentConnId: String?


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


---

## RcvGroupEvent

**Discriminated union type**:

MemberAdded:
- type: "memberAdded"
- groupMemberId: Int64
- profile: [Profile](#profile)

MemberConnected:
- type: "memberConnected"

MemberAccepted:
- type: "memberAccepted"
- groupMemberId: Int64
- profile: [Profile](#profile)

UserAccepted:
- type: "userAccepted"

MemberLeft:
- type: "memberLeft"

MemberRole:
- type: "memberRole"
- groupMemberId: Int64
- profile: [Profile](#profile)
- role: [GroupMemberRole](#groupmemberrole)

MemberBlocked:
- type: "memberBlocked"
- groupMemberId: Int64
- profile: [Profile](#profile)
- blocked: Bool

UserRole:
- type: "userRole"
- role: [GroupMemberRole](#groupmemberrole)

MemberDeleted:
- type: "memberDeleted"
- groupMemberId: Int64
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
- securityCode: String
- verifiedAt: UTCTime


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
- enabled: Bool


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
- proxyServer: String
- srvError: [SrvError](#srverror)

ProxyRelay:
- type: "proxyRelay"
- proxyServer: String
- srvError: [SrvError](#srverror)

Other:
- type: "other"
- sndError: String


---

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


---

## SndGroupEvent

**Discriminated union type**:

MemberRole:
- type: "memberRole"
- groupMemberId: Int64
- profile: [Profile](#profile)
- role: [GroupMemberRole](#groupmemberrole)

MemberBlocked:
- type: "memberBlocked"
- groupMemberId: Int64
- profile: [Profile](#profile)
- blocked: Bool

UserRole:
- type: "userRole"
- role: [GroupMemberRole](#groupmemberrole)

MemberDeleted:
- type: "memberDeleted"
- groupMemberId: Int64
- profile: [Profile](#profile)

UserLeft:
- type: "userLeft"

GroupUpdated:
- type: "groupUpdated"
- groupProfile: [GroupProfile](#groupprofile)

MemberAccepted:
- type: "memberAccepted"
- groupMemberId: Int64
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
- srvError: String


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
- ttl: Int?


---

## TimedMessagesPreference

**Record type**:
- allow: [FeatureAllowed](#featureallowed)
- ttl: Int?


---

## UIColorMode

**Enum type**:
- "light"
- "dark"


---

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

## UpdatedMessage

**Record type**:
- msgContent: [MsgContent](#msgcontent)
- mentions: {String : Int64}


---

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


---

## UserContactLink

**Record type**:
- userContactLinkId: Int64
- connLinkContact: [CreatedConnLink](#createdconnlink)
- shortLinkDataSet: Bool
- addressSettings: [AddressSettings](#addresssettings)


---

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


---

## UserInfo

**Record type**:
- user: [User](#user)
- unreadCount: Int


---

## UserProfileUpdateSummary

**Record type**:
- updateSuccesses: Int
- updateFailures: Int
- changedContacts: [[Contact](#contact)]


---

## UserPwdHash

**Record type**:
- hash: String
- salt: String


---

## VersionRange

**Record type**:
- minVersion: Int
- maxVersion: Int


---

## XFTPRcvFile

**Record type**:
- rcvFileDescription: [RcvFileDescr](#rcvfiledescr)
- agentRcvFileId: String?
- agentRcvFileDeleted: Bool
- userApprovedRelays: Bool


---

## XFTPSndFile

**Record type**:
- agentSndFileId: String
- privateSndFileDescr: String?
- agentSndFileDeleted: Bool
- cryptoArgs: [CryptoFileArgs](#cryptofileargs)?
