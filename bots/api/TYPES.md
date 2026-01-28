# API Types

This file is generated automatically.

- [ACIReaction](#acireaction)
- [AChat](#achat)
- [AChatItem](#achatitem)
- [AddressSettings](#addresssettings)
- [AgentCryptoError](#agentcryptoerror)
- [AgentErrorType](#agenterrortype)
- [AutoAccept](#autoaccept)
- [BlockingInfo](#blockinginfo)
- [BlockingReason](#blockingreason)
- [BrokerErrorType](#brokererrortype)
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
- [CIQuote](#ciquote)
- [CIReaction](#cireaction)
- [CIReactionCount](#cireactioncount)
- [CIStatus](#cistatus)
- [CITimed](#citimed)
- [ChatBotCommand](#chatbotcommand)
- [ChatDeleteMode](#chatdeletemode)
- [ChatError](#chaterror)
- [ChatErrorType](#chaterrortype)
- [ChatFeature](#chatfeature)
- [ChatInfo](#chatinfo)
- [ChatItem](#chatitem)
- [ChatItemDeletion](#chatitemdeletion)
- [ChatPeerType](#chatpeertype)
- [ChatRef](#chatref)
- [ChatSettings](#chatsettings)
- [ChatStats](#chatstats)
- [ChatType](#chattype)
- [ChatWallpaper](#chatwallpaper)
- [ChatWallpaperScale](#chatwallpaperscale)
- [ClientNotice](#clientnotice)
- [Color](#color)
- [CommandError](#commanderror)
- [CommandErrorType](#commanderrortype)
- [ComposedMessage](#composedmessage)
- [ConnStatus](#connstatus)
- [ConnType](#conntype)
- [Connection](#connection)
- [ConnectionEntity](#connectionentity)
- [ConnectionErrorType](#connectionerrortype)
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
- [ErrorType](#errortype)
- [FeatureAllowed](#featureallowed)
- [FileDescr](#filedescr)
- [FileError](#fileerror)
- [FileErrorType](#fileerrortype)
- [FileInvitation](#fileinvitation)
- [FileProtocol](#fileprotocol)
- [FileStatus](#filestatus)
- [FileTransferMeta](#filetransfermeta)
- [Format](#format)
- [FormattedText](#formattedtext)
- [FullGroupPreferences](#fullgrouppreferences)
- [FullPreferences](#fullpreferences)
- [Group](#group)
- [GroupChatScope](#groupchatscope)
- [GroupChatScopeInfo](#groupchatscopeinfo)
- [GroupDirectInvitation](#groupdirectinvitation)
- [GroupFeature](#groupfeature)
- [GroupFeatureEnabled](#groupfeatureenabled)
- [GroupInfo](#groupinfo)
- [GroupKeys](#groupkeys)
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
- [GroupRelay](#grouprelay)
- [GroupRootKey](#grouprootkey)
- [GroupShortLinkData](#groupshortlinkdata)
- [GroupShortLinkInfo](#groupshortlinkinfo)
- [GroupSummary](#groupsummary)
- [GroupSupportChat](#groupsupportchat)
- [HandshakeError](#handshakeerror)
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
- [NetworkError](#networkerror)
- [NewUser](#newuser)
- [NoteFolder](#notefolder)
- [PendingContactConnection](#pendingcontactconnection)
- [PrefEnabled](#prefenabled)
- [Preferences](#preferences)
- [PreparedContact](#preparedcontact)
- [PreparedGroup](#preparedgroup)
- [Profile](#profile)
- [ProxyClientError](#proxyclienterror)
- [ProxyError](#proxyerror)
- [RCErrorType](#rcerrortype)
- [RatchetSyncState](#ratchetsyncstate)
- [RcvConnEvent](#rcvconnevent)
- [RcvDirectEvent](#rcvdirectevent)
- [RcvFileDescr](#rcvfiledescr)
- [RcvFileStatus](#rcvfilestatus)
- [RcvFileTransfer](#rcvfiletransfer)
- [RcvGroupEvent](#rcvgroupevent)
- [RelayStatus](#relaystatus)
- [ReportReason](#reportreason)
- [RoleGroupPreference](#rolegrouppreference)
- [SMPAgentError](#smpagenterror)
- [SecurityCode](#securitycode)
- [SimplePreference](#simplepreference)
- [SimplexLinkType](#simplexlinktype)
- [SndCIStatusProgress](#sndcistatusprogress)
- [SndConnEvent](#sndconnevent)
- [SndError](#snderror)
- [SndFileTransfer](#sndfiletransfer)
- [SndGroupEvent](#sndgroupevent)
- [SrvError](#srverror)
- [StoreError](#storeerror)
- [SubscriptionStatus](#subscriptionstatus)
- [SwitchPhase](#switchphase)
- [TimedMessagesGroupPreference](#timedmessagesgrouppreference)
- [TimedMessagesPreference](#timedmessagespreference)
- [TransportError](#transporterror)
- [UIColorMode](#uicolormode)
- [UIColors](#uicolors)
- [UIThemeEntityOverride](#uithemeentityoverride)
- [UIThemeEntityOverrides](#uithemeentityoverrides)
- [UpdatedMessage](#updatedmessage)
- [User](#user)
- [UserContact](#usercontact)
- [UserContactLink](#usercontactlink)
- [UserContactRequest](#usercontactrequest)
- [UserInfo](#userinfo)
- [UserProfileUpdateSummary](#userprofileupdatesummary)
- [UserPwdHash](#userpwdhash)
- [VersionRange](#versionrange)
- [XFTPErrorType](#xftperrortype)
- [XFTPRcvFile](#xftprcvfile)
- [XFTPSndFile](#xftpsndfile)


---

## ACIReaction

**Record type**:
- chatInfo: [ChatInfo](#chatinfo)
- chatReaction: [CIReaction](#cireaction)


---

## AChat

**Record type**:
- chatInfo: [ChatInfo](#chatinfo)
- chatItems: [[ChatItem](#chatitem)]
- chatStats: [ChatStats](#chatstats)


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

## AgentCryptoError

**Discriminated union type**:

DECRYPT_AES:
- type: "DECRYPT_AES"

DECRYPT_CB:
- type: "DECRYPT_CB"

RATCHET_HEADER:
- type: "RATCHET_HEADER"

RATCHET_SYNC:
- type: "RATCHET_SYNC"


---

## AgentErrorType

**Discriminated union type**:

CMD:
- type: "CMD"
- cmdErr: [CommandErrorType](#commanderrortype)
- errContext: string

CONN:
- type: "CONN"
- connErr: [ConnectionErrorType](#connectionerrortype)
- errContext: string

NO_USER:
- type: "NO_USER"

SMP:
- type: "SMP"
- serverAddress: string
- smpErr: [ErrorType](#errortype)

NTF:
- type: "NTF"
- serverAddress: string
- ntfErr: [ErrorType](#errortype)

XFTP:
- type: "XFTP"
- serverAddress: string
- xftpErr: [XFTPErrorType](#xftperrortype)

FILE:
- type: "FILE"
- fileErr: [FileErrorType](#fileerrortype)

PROXY:
- type: "PROXY"
- proxyServer: string
- relayServer: string
- proxyErr: [ProxyClientError](#proxyclienterror)

RCP:
- type: "RCP"
- rcpErr: [RCErrorType](#rcerrortype)

BROKER:
- type: "BROKER"
- brokerAddress: string
- brokerErr: [BrokerErrorType](#brokererrortype)

AGENT:
- type: "AGENT"
- agentErr: [SMPAgentError](#smpagenterror)

NOTICE:
- type: "NOTICE"
- server: string
- preset: bool
- expiresAt: UTCTime?

INTERNAL:
- type: "INTERNAL"
- internalErr: string

CRITICAL:
- type: "CRITICAL"
- offerRestart: bool
- criticalErr: string

INACTIVE:
- type: "INACTIVE"


---

## AutoAccept

**Record type**:
- acceptIncognito: bool


---

## BlockingInfo

**Record type**:
- reason: [BlockingReason](#blockingreason)
- notice: [ClientNotice](#clientnotice)?


---

## BlockingReason

**Enum type**:
- "spam"
- "content"


---

## BrokerErrorType

**Discriminated union type**:

RESPONSE:
- type: "RESPONSE"
- respErr: string

UNEXPECTED:
- type: "UNEXPECTED"
- respErr: string

NETWORK:
- type: "NETWORK"
- networkError: [NetworkError](#networkerror)

HOST:
- type: "HOST"

NO_SERVICE:
- type: "NO_SERVICE"

TRANSPORT:
- type: "TRANSPORT"
- transportErr: [TransportError](#transporterror)

TIMEOUT:
- type: "TIMEOUT"


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

ChatBanner:
- type: "chatBanner"


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
- groupMember: [GroupMember](#groupmember)?

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
- itemTs: UTCTime
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
- hasLink: bool
- deletable: bool
- editable: bool
- forwardedByMember: int64?
- showGroupAsSender: bool
- createdAt: UTCTime
- updatedAt: UTCTime


---

## CIQuote

**Record type**:
- chatDir: [CIDirection](#cidirection)?
- itemId: int64?
- sharedMsgId: string?
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
- deleteAt: UTCTime?


---

## ChatBotCommand

**Discriminated union type**:

Command:
- type: "command"
- keyword: string
- label: string
- params: string?

Menu:
- type: "menu"
- label: string
- commands: [[ChatBotCommand](#chatbotcommand)]


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

**Syntax**:

```
full|entity|messages[ notify=off]
```

```javascript
type + (type == 'messages' ? '' : (!notify ? ' notify=off' : '')) // JavaScript
```

```python
str(type) + ('' if str(type) == 'messages' else (' notify=off' if not notify else '')) # Python
```


---

## ChatError

**Discriminated union type**:

Error:
- type: "error"
- errorType: [ChatErrorType](#chaterrortype)

ErrorAgent:
- type: "errorAgent"
- agentError: [AgentErrorType](#agenterrortype)
- agentConnId: string
- connectionEntity_: [ConnectionEntity](#connectionentity)?

ErrorStore:
- type: "errorStore"
- storeError: [StoreError](#storeerror)


---

## ChatErrorType

**Discriminated union type**:

NoActiveUser:
- type: "noActiveUser"

NoConnectionUser:
- type: "noConnectionUser"
- agentConnId: string

NoSndFileUser:
- type: "noSndFileUser"
- agentSndFileId: string

NoRcvFileUser:
- type: "noRcvFileUser"
- agentRcvFileId: string

UserUnknown:
- type: "userUnknown"

ActiveUserExists:
- type: "activeUserExists"

UserExists:
- type: "userExists"
- contactName: string

ChatRelayExists:
- type: "chatRelayExists"

DifferentActiveUser:
- type: "differentActiveUser"
- commandUserId: int64
- activeUserId: int64

CantDeleteActiveUser:
- type: "cantDeleteActiveUser"
- userId: int64

CantDeleteLastUser:
- type: "cantDeleteLastUser"
- userId: int64

CantHideLastUser:
- type: "cantHideLastUser"
- userId: int64

HiddenUserAlwaysMuted:
- type: "hiddenUserAlwaysMuted"
- userId: int64

EmptyUserPassword:
- type: "emptyUserPassword"
- userId: int64

UserAlreadyHidden:
- type: "userAlreadyHidden"
- userId: int64

UserNotHidden:
- type: "userNotHidden"
- userId: int64

InvalidDisplayName:
- type: "invalidDisplayName"
- displayName: string
- validName: string

ChatNotStarted:
- type: "chatNotStarted"

ChatNotStopped:
- type: "chatNotStopped"

ChatStoreChanged:
- type: "chatStoreChanged"

InvalidConnReq:
- type: "invalidConnReq"

UnsupportedConnReq:
- type: "unsupportedConnReq"

ConnReqMessageProhibited:
- type: "connReqMessageProhibited"

ContactNotReady:
- type: "contactNotReady"
- contact: [Contact](#contact)

ContactNotActive:
- type: "contactNotActive"
- contact: [Contact](#contact)

ContactDisabled:
- type: "contactDisabled"
- contact: [Contact](#contact)

ConnectionDisabled:
- type: "connectionDisabled"
- connection: [Connection](#connection)

GroupUserRole:
- type: "groupUserRole"
- groupInfo: [GroupInfo](#groupinfo)
- requiredRole: [GroupMemberRole](#groupmemberrole)

GroupMemberInitialRole:
- type: "groupMemberInitialRole"
- groupInfo: [GroupInfo](#groupinfo)
- initialRole: [GroupMemberRole](#groupmemberrole)

ContactIncognitoCantInvite:
- type: "contactIncognitoCantInvite"

GroupIncognitoCantInvite:
- type: "groupIncognitoCantInvite"

GroupContactRole:
- type: "groupContactRole"
- contactName: string

GroupDuplicateMember:
- type: "groupDuplicateMember"
- contactName: string

GroupDuplicateMemberId:
- type: "groupDuplicateMemberId"

GroupNotJoined:
- type: "groupNotJoined"
- groupInfo: [GroupInfo](#groupinfo)

GroupMemberNotActive:
- type: "groupMemberNotActive"

CantBlockMemberForSelf:
- type: "cantBlockMemberForSelf"
- groupInfo: [GroupInfo](#groupinfo)
- member: [GroupMember](#groupmember)
- setShowMessages: bool

GroupMemberUserRemoved:
- type: "groupMemberUserRemoved"

GroupMemberNotFound:
- type: "groupMemberNotFound"

GroupCantResendInvitation:
- type: "groupCantResendInvitation"
- groupInfo: [GroupInfo](#groupinfo)
- contactName: string

GroupInternal:
- type: "groupInternal"
- message: string

FileNotFound:
- type: "fileNotFound"
- message: string

FileSize:
- type: "fileSize"
- filePath: string

FileAlreadyReceiving:
- type: "fileAlreadyReceiving"
- message: string

FileCancelled:
- type: "fileCancelled"
- message: string

FileCancel:
- type: "fileCancel"
- fileId: int64
- message: string

FileAlreadyExists:
- type: "fileAlreadyExists"
- filePath: string

FileWrite:
- type: "fileWrite"
- filePath: string
- message: string

FileSend:
- type: "fileSend"
- fileId: int64
- agentError: [AgentErrorType](#agenterrortype)

FileRcvChunk:
- type: "fileRcvChunk"
- message: string

FileInternal:
- type: "fileInternal"
- message: string

FileImageType:
- type: "fileImageType"
- filePath: string

FileImageSize:
- type: "fileImageSize"
- filePath: string

FileNotReceived:
- type: "fileNotReceived"
- fileId: int64

FileNotApproved:
- type: "fileNotApproved"
- fileId: int64
- unknownServers: [string]

FallbackToSMPProhibited:
- type: "fallbackToSMPProhibited"
- fileId: int64

InlineFileProhibited:
- type: "inlineFileProhibited"
- fileId: int64

InvalidForward:
- type: "invalidForward"

InvalidChatItemUpdate:
- type: "invalidChatItemUpdate"

InvalidChatItemDelete:
- type: "invalidChatItemDelete"

HasCurrentCall:
- type: "hasCurrentCall"

NoCurrentCall:
- type: "noCurrentCall"

CallContact:
- type: "callContact"
- contactId: int64

DirectMessagesProhibited:
- type: "directMessagesProhibited"
- direction: [MsgDirection](#msgdirection)
- contact: [Contact](#contact)

AgentVersion:
- type: "agentVersion"

AgentNoSubResult:
- type: "agentNoSubResult"
- agentConnId: string

CommandError:
- type: "commandError"
- message: string

AgentCommandError:
- type: "agentCommandError"
- message: string

InvalidFileDescription:
- type: "invalidFileDescription"
- message: string

ConnectionIncognitoChangeProhibited:
- type: "connectionIncognitoChangeProhibited"

ConnectionUserChangeProhibited:
- type: "connectionUserChangeProhibited"

PeerChatVRangeIncompatible:
- type: "peerChatVRangeIncompatible"

InternalError:
- type: "internalError"
- message: string

Exception:
- type: "exception"
- message: string


---

## ChatFeature

**Enum type**:
- "timedMessages"
- "fullDelete"
- "reactions"
- "voice"
- "files"
- "calls"
- "sessions"


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

## ChatPeerType

**Enum type**:
- "human"
- "bot"


---

## ChatRef

Used in API commands. Chat scope can only be passed with groups.

**Record type**:
- chatType: [ChatType](#chattype)
- chatId: int64
- chatScope: [GroupChatScope](#groupchatscope)?

**Syntax**:

```
<str(chatType)><chatId>[<str(chatScope)>]
```

```javascript
ChatType.cmdString(chatType) + chatId + (chatScope ? GroupChatScope.cmdString(chatScope) : '') // JavaScript
```

```python
str(chatType) + str(chatId) + ((str(chatScope)) if chatScope is not None else '') # Python
```


---

## ChatSettings

**Record type**:
- enableNtfs: [MsgFilter](#msgfilter)
- sendRcpts: bool?
- favorite: bool


---

## ChatStats

**Record type**:
- unreadCount: int
- unreadMentions: int
- reportsCount: int
- minUnreadItemId: int64
- unreadChat: bool


---

## ChatType

**Enum type**:
- "direct"
- "group"
- "local"

**Syntax**:

```
@|#|*|
```

```javascript
self == 'direct' ? '@' : self == 'group' ? '#' : self == 'local' ? '*' : '' // JavaScript
```

```python
'@' if str(self) == 'direct' else '#' if str(self) == 'group' else '*' if str(self) == 'local' else '' # Python
```


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

## ClientNotice

**Record type**:
- ttl: int64?


---

## Color

**Enum type**:
- "black"
- "red"
- "green"
- "yellow"
- "blue"
- "magenta"
- "cyan"
- "white"


---

## CommandError

**Discriminated union type**:

UNKNOWN:
- type: "UNKNOWN"

SYNTAX:
- type: "SYNTAX"

PROHIBITED:
- type: "PROHIBITED"

NO_AUTH:
- type: "NO_AUTH"

HAS_AUTH:
- type: "HAS_AUTH"

NO_ENTITY:
- type: "NO_ENTITY"


---

## CommandErrorType

**Discriminated union type**:

PROHIBITED:
- type: "PROHIBITED"

SYNTAX:
- type: "SYNTAX"

NO_CONN:
- type: "NO_CONN"

SIZE:
- type: "SIZE"

LARGE:
- type: "LARGE"


---

## ComposedMessage

**Record type**:
- fileSource: [CryptoFile](#cryptofile)?
- quotedItemId: int64?
- msgContent: [MsgContent](#msgcontent)
- mentions: {string : int64}


---

## ConnStatus

**Enum type**:
- "new"
- "prepared"
- "joined"
- "requested"
- "accepted"
- "snd-ready"
- "ready"
- "deleted"


---

## ConnType

**Enum type**:
- "contact"
- "member"
- "user_contact"


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
- createdAt: UTCTime


---

## ConnectionEntity

**Discriminated union type**:

RcvDirectMsgConnection:
- type: "rcvDirectMsgConnection"
- entityConnection: [Connection](#connection)
- contact: [Contact](#contact)?

RcvGroupMsgConnection:
- type: "rcvGroupMsgConnection"
- entityConnection: [Connection](#connection)
- groupInfo: [GroupInfo](#groupinfo)
- groupMember: [GroupMember](#groupmember)

UserContactConnection:
- type: "userContactConnection"
- entityConnection: [Connection](#connection)
- userContact: [UserContact](#usercontact)


---

## ConnectionErrorType

**Discriminated union type**:

NOT_FOUND:
- type: "NOT_FOUND"

DUPLICATE:
- type: "DUPLICATE"

SIMPLEX:
- type: "SIMPLEX"

NOT_ACCEPTED:
- type: "NOT_ACCEPTED"

NOT_AVAILABLE:
- type: "NOT_AVAILABLE"


---

## ConnectionMode

**Enum type**:
- "inv"
- "con"


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
- contactUsed: bool
- contactStatus: [ContactStatus](#contactstatus)
- chatSettings: [ChatSettings](#chatsettings)
- userPreferences: [Preferences](#preferences)
- mergedPreferences: [ContactUserPreferences](#contactuserpreferences)
- createdAt: UTCTime
- updatedAt: UTCTime
- chatTs: UTCTime?
- preparedContact: [PreparedContact](#preparedcontact)?
- contactRequestId: int64?
- contactGroupMemberId: int64?
- contactGrpInvSent: bool
- groupDirectInv: [GroupDirectInvitation](#groupdirectinvitation)?
- chatTags: [int64]
- chatItemTTL: int64?
- uiThemes: [UIThemeEntityOverrides](#uithemeentityoverrides)?
- chatDeleted: bool
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
- files: [ContactUserPreference](#contactuserpreference)
- calls: [ContactUserPreference](#contactuserpreference)
- sessions: [ContactUserPreference](#contactuserpreference)
- commands: [[ChatBotCommand](#chatbotcommand)]?


---

## CreatedConnLink

**Record type**:
- connFullLink: string
- connShortLink: string?

**Syntax**:

```
<connFullLink>[ <connShortLink>]
```

```javascript
connFullLink + (connShortLink ? ' ' + connShortLink : '') // JavaScript
```

```python
connFullLink + ((' ' + connShortLink) if connShortLink is not None else '') # Python
```


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

## ErrorType

**Discriminated union type**:

BLOCK:
- type: "BLOCK"

SESSION:
- type: "SESSION"

CMD:
- type: "CMD"
- cmdErr: [CommandError](#commanderror)

PROXY:
- type: "PROXY"
- proxyErr: [ProxyError](#proxyerror)

AUTH:
- type: "AUTH"

BLOCKED:
- type: "BLOCKED"
- blockInfo: [BlockingInfo](#blockinginfo)

SERVICE:
- type: "SERVICE"

CRYPTO:
- type: "CRYPTO"

QUOTA:
- type: "QUOTA"

STORE:
- type: "STORE"
- storeErr: string

NO_MSG:
- type: "NO_MSG"

LARGE_MSG:
- type: "LARGE_MSG"

EXPIRED:
- type: "EXPIRED"

INTERNAL:
- type: "INTERNAL"

DUPLICATE_:
- type: "DUPLICATE_"


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

## FileErrorType

**Discriminated union type**:

NOT_APPROVED:
- type: "NOT_APPROVED"

SIZE:
- type: "SIZE"

REDIRECT:
- type: "REDIRECT"
- redirectError: string

FILE_IO:
- type: "FILE_IO"
- fileIOError: string

NO_FILE:
- type: "NO_FILE"


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
- "smp"
- "xftp"
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
- color: [Color](#color)

Uri:
- type: "uri"

HyperLink:
- type: "hyperLink"
- showText: string?
- linkUri: string

SimplexLink:
- type: "simplexLink"
- showText: string?
- linkType: [SimplexLinkType](#simplexlinktype)
- simplexUri: string
- smpHosts: [string]

Command:
- type: "command"
- commandStr: string

Mention:
- type: "mention"
- memberName: string

Email:
- type: "email"

Phone:
- type: "phone"


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
- sessions: [RoleGroupPreference](#rolegrouppreference)
- commands: [[ChatBotCommand](#chatbotcommand)]


---

## FullPreferences

**Record type**:
- timedMessages: [TimedMessagesPreference](#timedmessagespreference)
- fullDelete: [SimplePreference](#simplepreference)
- reactions: [SimplePreference](#simplepreference)
- voice: [SimplePreference](#simplepreference)
- files: [SimplePreference](#simplepreference)
- calls: [SimplePreference](#simplepreference)
- sessions: [SimplePreference](#simplepreference)
- commands: [[ChatBotCommand](#chatbotcommand)]


---

## Group

**Record type**:
- groupInfo: [GroupInfo](#groupinfo)
- members: [[GroupMember](#groupmember)]


---

## GroupChatScope

**Discriminated union type**:

MemberSupport:
- type: "memberSupport"
- groupMemberId_: int64?

**Syntax**:

```
(_support[:<groupMemberId_>])
```

```javascript
'(_support' + (groupMemberId_ ? ':' + groupMemberId_ : '') + ')' // JavaScript
```

```python
'(_support' + ((':' + str(groupMemberId_)) if groupMemberId_ is not None else '') + ')' # Python
```


---

## GroupChatScopeInfo

**Discriminated union type**:

MemberSupport:
- type: "memberSupport"
- groupMember_: [GroupMember](#groupmember)?


---

## GroupDirectInvitation

**Record type**:
- groupDirectInvLink: string
- fromGroupId_: int64?
- fromGroupMemberId_: int64?
- fromGroupMemberConnId_: int64?
- groupDirectInvStartedConnection: bool


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
- "sessions"


---

## GroupFeatureEnabled

**Enum type**:
- "on"
- "off"


---

## GroupInfo

**Record type**:
- groupId: int64
- useRelays: bool
- relayOwnStatus: [RelayStatus](#relaystatus)?
- localDisplayName: string
- groupProfile: [GroupProfile](#groupprofile)
- localAlias: string
- businessChat: [BusinessChatInfo](#businesschatinfo)?
- fullGroupPreferences: [FullGroupPreferences](#fullgrouppreferences)
- membership: [GroupMember](#groupmember)
- chatSettings: [ChatSettings](#chatsettings)
- createdAt: UTCTime
- updatedAt: UTCTime
- chatTs: UTCTime?
- userMemberProfileSentAt: UTCTime?
- preparedGroup: [PreparedGroup](#preparedgroup)?
- chatTags: [int64]
- chatItemTTL: int64?
- uiThemes: [UIThemeEntityOverrides](#uithemeentityoverrides)?
- customData: JSONObject?
- groupSummary: [GroupSummary](#groupsummary)
- membersRequireAttention: int
- viaGroupLinkUri: string?
- groupKeys: [GroupKeys](#groupkeys)?


---

## GroupKeys

**Record type**:
- sharedGroupId: string
- groupRootKey: [GroupRootKey](#grouprootkey)
- memberPrivKey: string


---

## GroupLink

**Record type**:
- userContactLinkId: int64
- connLinkContact: [CreatedConnLink](#createdconnlink)
- shortLinkDataSet: bool
- shortLinkLargeDataSet: bool
- groupLinkId: string
- acceptMemberRole: [GroupMemberRole](#groupmemberrole)


---

## GroupLinkPlan

**Discriminated union type**:

Ok:
- type: "ok"
- groupSLinkInfo_: [GroupShortLinkInfo](#groupshortlinkinfo)?
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
- indexInGroup: int64
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
- createdAt: UTCTime
- updatedAt: UTCTime
- supportChat: [GroupSupportChat](#groupsupportchat)?
- memberPubKey: string?


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
- "relay"
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
- "rejected"
- "removed"
- "left"
- "deleted"
- "unknown"
- "invited"
- "pending_approval"
- "pending_review"
- "introduced"
- "intro-inv"
- "accepted"
- "announced"
- "connected"
- "complete"
- "creator"


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
- sessions: [RoleGroupPreference](#rolegrouppreference)?
- commands: [[ChatBotCommand](#chatbotcommand)]?


---

## GroupProfile

**Record type**:
- displayName: string
- fullName: string
- shortDescr: string?
- description: string?
- image: string?
- groupLink: string?
- groupPreferences: [GroupPreferences](#grouppreferences)?
- memberAdmission: [GroupMemberAdmission](#groupmemberadmission)?


---

## GroupRelay

**Record type**:
- groupRelayId: int64
- groupMemberId: int64
- userChatRelayId: int64
- relayStatus: [RelayStatus](#relaystatus)
- relayLink: string?


---

## GroupRootKey

**Discriminated union type**:

Private:
- type: "private"
- rootPrivKey: string

Public:
- type: "public"
- rootPubKey: string


---

## GroupShortLinkData

**Record type**:
- groupProfile: [GroupProfile](#groupprofile)


---

## GroupShortLinkInfo

**Record type**:
- direct: bool
- groupRelays: [string]
- sharedGroupId: string?


---

## GroupSummary

**Record type**:
- currentMembers: int64


---

## GroupSupportChat

**Record type**:
- chatTs: UTCTime
- unread: int64
- memberAttention: int64
- mentions: int64
- lastMsgFromMemberTs: UTCTime?


---

## HandshakeError

**Enum type**:
- "PARSE"
- "IDENTITY"
- "BAD_AUTH"
- "BAD_SERVICE"


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
- json: JSONObject


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
- peerType: [ChatPeerType](#chatpeertype)?
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
- json: JSONObject


---

## MsgReceiptStatus

**Enum type**:
- "ok"
- "badMsgHash"


---

## NetworkError

**Discriminated union type**:

ConnectError:
- type: "connectError"
- connectError: string

TLSError:
- type: "tLSError"
- tlsError: string

UnknownCAError:
- type: "unknownCAError"

FailedError:
- type: "failedError"

TimeoutError:
- type: "timeoutError"

SubscribeError:
- type: "subscribeError"
- subscribeError: string


---

## NewUser

**Record type**:
- profile: [Profile](#profile)?
- pastTimestamp: bool
- userChatRelay: bool


---

## NoteFolder

**Record type**:
- noteFolderId: int64
- userId: int64
- createdAt: UTCTime
- updatedAt: UTCTime
- chatTs: UTCTime
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
- createdAt: UTCTime
- updatedAt: UTCTime


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
- files: [SimplePreference](#simplepreference)?
- calls: [SimplePreference](#simplepreference)?
- sessions: [SimplePreference](#simplepreference)?
- commands: [[ChatBotCommand](#chatbotcommand)]?


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
- peerType: [ChatPeerType](#chatpeertype)?


---

## ProxyClientError

**Discriminated union type**:

ProtocolError:
- type: "protocolError"
- protocolErr: [ErrorType](#errortype)

UnexpectedResponse:
- type: "unexpectedResponse"
- responseStr: string

ResponseError:
- type: "responseError"
- responseErr: [ErrorType](#errortype)


---

## ProxyError

**Discriminated union type**:

PROTOCOL:
- type: "PROTOCOL"
- protocolErr: [ErrorType](#errortype)

BROKER:
- type: "BROKER"
- brokerErr: [BrokerErrorType](#brokererrortype)

BASIC_AUTH:
- type: "BASIC_AUTH"

NO_SESSION:
- type: "NO_SESSION"


---

## RCErrorType

**Discriminated union type**:

Internal:
- type: "internal"
- internalErr: string

Identity:
- type: "identity"

NoLocalAddress:
- type: "noLocalAddress"

NewController:
- type: "newController"

NotDiscovered:
- type: "notDiscovered"

TLSStartFailed:
- type: "tLSStartFailed"

Exception:
- type: "exception"
- exception: string

CtrlAuth:
- type: "ctrlAuth"

CtrlNotFound:
- type: "ctrlNotFound"

CtrlError:
- type: "ctrlError"
- ctrlErr: string

Invitation:
- type: "invitation"

Version:
- type: "version"

Encrypt:
- type: "encrypt"

Decrypt:
- type: "decrypt"

BlockSize:
- type: "blockSize"

Syntax:
- type: "syntax"
- syntaxErr: string


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

GroupInvLinkReceived:
- type: "groupInvLinkReceived"
- groupProfile: [GroupProfile](#groupprofile)


---

## RcvFileDescr

**Record type**:
- fileDescrId: int64
- fileDescrText: string
- fileDescrPartNo: int
- fileDescrComplete: bool


---

## RcvFileStatus

**Discriminated union type**:

New:
- type: "new"

Accepted:
- type: "accepted"
- filePath: string

Connected:
- type: "connected"
- filePath: string

Complete:
- type: "complete"
- filePath: string

Cancelled:
- type: "cancelled"
- filePath_: string?


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

## RelayStatus

**Enum type**:
- "new"
- "invited"
- "accepted"
- "active"


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

## SMPAgentError

**Discriminated union type**:

A_MESSAGE:
- type: "A_MESSAGE"

A_PROHIBITED:
- type: "A_PROHIBITED"
- prohibitedErr: string

A_VERSION:
- type: "A_VERSION"

A_LINK:
- type: "A_LINK"
- linkErr: string

A_CRYPTO:
- type: "A_CRYPTO"
- cryptoErr: [AgentCryptoError](#agentcryptoerror)

A_DUPLICATE:
- type: "A_DUPLICATE"

A_QUEUE:
- type: "A_QUEUE"
- queueErr: string


---

## SecurityCode

**Record type**:
- securityCode: string
- verifiedAt: UTCTime


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
- "relay"


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

## StoreError

**Discriminated union type**:

DuplicateName:
- type: "duplicateName"

UserNotFound:
- type: "userNotFound"
- userId: int64

RelayUserNotFound:
- type: "relayUserNotFound"

UserNotFoundByName:
- type: "userNotFoundByName"
- contactName: string

UserNotFoundByContactId:
- type: "userNotFoundByContactId"
- contactId: int64

UserNotFoundByGroupId:
- type: "userNotFoundByGroupId"
- groupId: int64

UserNotFoundByFileId:
- type: "userNotFoundByFileId"
- fileId: int64

UserNotFoundByContactRequestId:
- type: "userNotFoundByContactRequestId"
- contactRequestId: int64

ContactNotFound:
- type: "contactNotFound"
- contactId: int64

ContactNotFoundByName:
- type: "contactNotFoundByName"
- contactName: string

ContactNotFoundByMemberId:
- type: "contactNotFoundByMemberId"
- groupMemberId: int64

ContactNotReady:
- type: "contactNotReady"
- contactName: string

DuplicateContactLink:
- type: "duplicateContactLink"

UserContactLinkNotFound:
- type: "userContactLinkNotFound"

ContactRequestNotFound:
- type: "contactRequestNotFound"
- contactRequestId: int64

ContactRequestNotFoundByName:
- type: "contactRequestNotFoundByName"
- contactName: string

InvalidContactRequestEntity:
- type: "invalidContactRequestEntity"
- contactRequestId: int64

InvalidBusinessChatContactRequest:
- type: "invalidBusinessChatContactRequest"

GroupNotFound:
- type: "groupNotFound"
- groupId: int64

GroupNotFoundByName:
- type: "groupNotFoundByName"
- groupName: string

GroupMemberNameNotFound:
- type: "groupMemberNameNotFound"
- groupId: int64
- groupMemberName: string

GroupMemberNotFound:
- type: "groupMemberNotFound"
- groupMemberId: int64

GroupMemberNotFoundByIndex:
- type: "groupMemberNotFoundByIndex"
- groupMemberIndex: int64

MemberRelationsVectorNotFound:
- type: "memberRelationsVectorNotFound"
- groupMemberId: int64

GroupHostMemberNotFound:
- type: "groupHostMemberNotFound"
- groupId: int64

GroupMemberNotFoundByMemberId:
- type: "groupMemberNotFoundByMemberId"
- memberId: string

MemberContactGroupMemberNotFound:
- type: "memberContactGroupMemberNotFound"
- contactId: int64

InvalidMemberRelationUpdate:
- type: "invalidMemberRelationUpdate"

GroupWithoutUser:
- type: "groupWithoutUser"

DuplicateGroupMember:
- type: "duplicateGroupMember"

DuplicateMemberId:
- type: "duplicateMemberId"

GroupAlreadyJoined:
- type: "groupAlreadyJoined"

GroupInvitationNotFound:
- type: "groupInvitationNotFound"

NoteFolderAlreadyExists:
- type: "noteFolderAlreadyExists"
- noteFolderId: int64

NoteFolderNotFound:
- type: "noteFolderNotFound"
- noteFolderId: int64

UserNoteFolderNotFound:
- type: "userNoteFolderNotFound"

SndFileNotFound:
- type: "sndFileNotFound"
- fileId: int64

SndFileInvalid:
- type: "sndFileInvalid"
- fileId: int64

RcvFileNotFound:
- type: "rcvFileNotFound"
- fileId: int64

RcvFileDescrNotFound:
- type: "rcvFileDescrNotFound"
- fileId: int64

FileNotFound:
- type: "fileNotFound"
- fileId: int64

RcvFileInvalid:
- type: "rcvFileInvalid"
- fileId: int64

RcvFileInvalidDescrPart:
- type: "rcvFileInvalidDescrPart"

LocalFileNoTransfer:
- type: "localFileNoTransfer"
- fileId: int64

SharedMsgIdNotFoundByFileId:
- type: "sharedMsgIdNotFoundByFileId"
- fileId: int64

FileIdNotFoundBySharedMsgId:
- type: "fileIdNotFoundBySharedMsgId"
- sharedMsgId: string

SndFileNotFoundXFTP:
- type: "sndFileNotFoundXFTP"
- agentSndFileId: string

RcvFileNotFoundXFTP:
- type: "rcvFileNotFoundXFTP"
- agentRcvFileId: string

ConnectionNotFound:
- type: "connectionNotFound"
- agentConnId: string

ConnectionNotFoundById:
- type: "connectionNotFoundById"
- connId: int64

ConnectionNotFoundByMemberId:
- type: "connectionNotFoundByMemberId"
- groupMemberId: int64

PendingConnectionNotFound:
- type: "pendingConnectionNotFound"
- connId: int64

UniqueID:
- type: "uniqueID"

LargeMsg:
- type: "largeMsg"

InternalError:
- type: "internalError"
- message: string

DBException:
- type: "dBException"
- message: string

DBBusyError:
- type: "dBBusyError"
- message: string

BadChatItem:
- type: "badChatItem"
- itemId: int64
- itemTs: UTCTime?

ChatItemNotFound:
- type: "chatItemNotFound"
- itemId: int64

ChatItemNotFoundByText:
- type: "chatItemNotFoundByText"
- text: string

ChatItemSharedMsgIdNotFound:
- type: "chatItemSharedMsgIdNotFound"
- sharedMsgId: string

ChatItemNotFoundByFileId:
- type: "chatItemNotFoundByFileId"
- fileId: int64

ChatItemNotFoundByContactId:
- type: "chatItemNotFoundByContactId"
- contactId: int64

ChatItemNotFoundByGroupId:
- type: "chatItemNotFoundByGroupId"
- groupId: int64

ProfileNotFound:
- type: "profileNotFound"
- profileId: int64

DuplicateGroupLink:
- type: "duplicateGroupLink"
- groupInfo: [GroupInfo](#groupinfo)

GroupLinkNotFound:
- type: "groupLinkNotFound"
- groupInfo: [GroupInfo](#groupinfo)

HostMemberIdNotFound:
- type: "hostMemberIdNotFound"
- groupId: int64

ContactNotFoundByFileId:
- type: "contactNotFoundByFileId"
- fileId: int64

NoGroupSndStatus:
- type: "noGroupSndStatus"
- itemId: int64
- groupMemberId: int64

DuplicateGroupMessage:
- type: "duplicateGroupMessage"
- groupId: int64
- sharedMsgId: string
- authorGroupMemberId: int64?
- forwardedByGroupMemberId: int64?

RemoteHostNotFound:
- type: "remoteHostNotFound"
- remoteHostId: int64

RemoteHostUnknown:
- type: "remoteHostUnknown"

RemoteHostDuplicateCA:
- type: "remoteHostDuplicateCA"

RemoteCtrlNotFound:
- type: "remoteCtrlNotFound"
- remoteCtrlId: int64

RemoteCtrlDuplicateCA:
- type: "remoteCtrlDuplicateCA"

ProhibitedDeleteUser:
- type: "prohibitedDeleteUser"
- userId: int64
- contactId: int64

OperatorNotFound:
- type: "operatorNotFound"
- serverOperatorId: int64

UsageConditionsNotFound:
- type: "usageConditionsNotFound"

UserChatRelayNotFound:
- type: "userChatRelayNotFound"
- chatRelayId: int64

GroupRelayNotFound:
- type: "groupRelayNotFound"
- groupRelayId: int64

GroupRelayNotFoundByMemberId:
- type: "groupRelayNotFoundByMemberId"
- groupMemberId: int64

InvalidQuote:
- type: "invalidQuote"

InvalidMention:
- type: "invalidMention"

InvalidDeliveryTask:
- type: "invalidDeliveryTask"
- taskId: int64

DeliveryTaskNotFound:
- type: "deliveryTaskNotFound"
- taskId: int64

InvalidDeliveryJob:
- type: "invalidDeliveryJob"
- jobId: int64

DeliveryJobNotFound:
- type: "deliveryJobNotFound"
- jobId: int64

WorkItemError:
- type: "workItemError"
- errContext: string


---

## SubscriptionStatus

**Discriminated union type**:

Active:
- type: "active"

Pending:
- type: "pending"

Removed:
- type: "removed"
- subError: string

NoSub:
- type: "noSub"


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

## TransportError

**Discriminated union type**:

BadBlock:
- type: "badBlock"

Version:
- type: "version"

LargeMsg:
- type: "largeMsg"

BadSession:
- type: "badSession"

NoServerAuth:
- type: "noServerAuth"

Handshake:
- type: "handshake"
- handshakeErr: [HandshakeError](#handshakeerror)


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
- autoAcceptMemberContacts: bool
- userMemberProfileUpdatedAt: UTCTime?
- uiThemes: [UIThemeEntityOverrides](#uithemeentityoverrides)?
- userChatRelay: bool


---

## UserContact

**Record type**:
- userContactLinkId: int64
- connReqContact: string
- groupId: int64?


---

## UserContactLink

**Record type**:
- userContactLinkId: int64
- connLinkContact: [CreatedConnLink](#createdconnlink)
- shortLinkDataSet: bool
- shortLinkLargeDataSet: bool
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
- createdAt: UTCTime
- updatedAt: UTCTime
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

## XFTPErrorType

**Discriminated union type**:

BLOCK:
- type: "BLOCK"

SESSION:
- type: "SESSION"

HANDSHAKE:
- type: "HANDSHAKE"

CMD:
- type: "CMD"
- cmdErr: [CommandError](#commanderror)

AUTH:
- type: "AUTH"

BLOCKED:
- type: "BLOCKED"
- blockInfo: [BlockingInfo](#blockinginfo)

SIZE:
- type: "SIZE"

QUOTA:
- type: "QUOTA"

DIGEST:
- type: "DIGEST"

CRYPTO:
- type: "CRYPTO"

NO_FILE:
- type: "NO_FILE"

HAS_FILE:
- type: "HAS_FILE"

FILE_IO:
- type: "FILE_IO"

TIMEOUT:
- type: "TIMEOUT"

INTERNAL:
- type: "INTERNAL"

DUPLICATE_:
- type: "DUPLICATE_"


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
