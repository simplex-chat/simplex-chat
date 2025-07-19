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


## ACIReaction

**Record type**:
- chatInfo: [ChatInfo](#chatinfo)
- chatReaction: [CIReaction](#cireaction)


## AChatItem

**Record type**:
- chatInfo: [ChatInfo](#chatinfo)
- chatItem: [ChatItem](#chatitem)


## AddressSettings

**Record type**:
- businessAddress: Bool
- autoAccept: [AutoAccept](#autoaccept)?
- autoReply: [MsgContent](#msgcontent)?


## AutoAccept

**Record type**:
- acceptIncognito: Bool


## BlockingInfo

**Record type**:
- reason: [BlockingReason](#blockingreason)


## BlockingReason

**Enum type**:
- "spam"
- "content"


## BusinessChatInfo

**Record type**:
- chatType: [BusinessChatType](#businesschattype)
- businessId: String
- customerId: String


## BusinessChatType

**Enum type**:
- "business"
- "customer"


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


## CIContent

**Discriminated union type**:

Record SndMsgContent:
- type: "sndMsgContent"
- msgContent: [MsgContent](#msgcontent)

Record RcvMsgContent:
- type: "rcvMsgContent"
- msgContent: [MsgContent](#msgcontent)

Record SndDeleted:
- type: "sndDeleted"
- deleteMode: [CIDeleteMode](#cideletemode)

Record RcvDeleted:
- type: "rcvDeleted"
- deleteMode: [CIDeleteMode](#cideletemode)

Record SndCall:
- type: "sndCall"
- status: [CICallStatus](#cicallstatus)
- duration: Int

Record RcvCall:
- type: "rcvCall"
- status: [CICallStatus](#cicallstatus)
- duration: Int

Record RcvIntegrityError:
- type: "rcvIntegrityError"
- msgError: [MsgErrorType](#msgerrortype)

Record RcvDecryptionError:
- type: "rcvDecryptionError"
- msgDecryptError: [MsgDecryptError](#msgdecrypterror)
- msgCount: Word32

Record RcvGroupInvitation:
- type: "rcvGroupInvitation"
- groupInvitation: [CIGroupInvitation](#cigroupinvitation)
- memberRole: [GroupMemberRole](#groupmemberrole)

Record SndGroupInvitation:
- type: "sndGroupInvitation"
- groupInvitation: [CIGroupInvitation](#cigroupinvitation)
- memberRole: [GroupMemberRole](#groupmemberrole)

Record RcvDirectEvent:
- type: "rcvDirectEvent"
- rcvDirectEvent: [RcvDirectEvent](#rcvdirectevent)

Record RcvGroupEvent:
- type: "rcvGroupEvent"
- rcvGroupEvent: [RcvGroupEvent](#rcvgroupevent)

Record SndGroupEvent:
- type: "sndGroupEvent"
- sndGroupEvent: [SndGroupEvent](#sndgroupevent)

Record RcvConnEvent:
- type: "rcvConnEvent"
- rcvConnEvent: [RcvConnEvent](#rcvconnevent)

Record SndConnEvent:
- type: "sndConnEvent"
- sndConnEvent: [SndConnEvent](#sndconnevent)

Record RcvChatFeature:
- type: "rcvChatFeature"
- feature: [ChatFeature](#chatfeature)
- enabled: [PrefEnabled](#prefenabled)
- param: Int?

Record SndChatFeature:
- type: "sndChatFeature"
- feature: [ChatFeature](#chatfeature)
- enabled: [PrefEnabled](#prefenabled)
- param: Int?

Record RcvChatPreference:
- type: "rcvChatPreference"
- feature: [ChatFeature](#chatfeature)
- allowed: [FeatureAllowed](#featureallowed)
- param: Int?

Record SndChatPreference:
- type: "sndChatPreference"
- feature: [ChatFeature](#chatfeature)
- allowed: [FeatureAllowed](#featureallowed)
- param: Int?

Record RcvGroupFeature:
- type: "rcvGroupFeature"
- groupFeature: [GroupFeature](#groupfeature)
- preference: [GroupPreference](#grouppreference)
- param: Int?
- memberRole_: [GroupMemberRole](#groupmemberrole)?

Record SndGroupFeature:
- type: "sndGroupFeature"
- groupFeature: [GroupFeature](#groupfeature)
- preference: [GroupPreference](#grouppreference)
- param: Int?
- memberRole_: [GroupMemberRole](#groupmemberrole)?

Record RcvChatFeatureRejected:
- type: "rcvChatFeatureRejected"
- feature: [ChatFeature](#chatfeature)

Record RcvGroupFeatureRejected:
- type: "rcvGroupFeatureRejected"
- groupFeature: [GroupFeature](#groupfeature)

Record SndModerated:
- type: "sndModerated"

Record RcvModerated:
- type: "rcvModerated"

Record RcvBlocked:
- type: "rcvBlocked"

Record SndDirectE2EEInfo:
- type: "sndDirectE2EEInfo"
- e2eeInfo: [E2EInfo](#e2einfo)

Record RcvDirectE2EEInfo:
- type: "rcvDirectE2EEInfo"
- e2eeInfo: [E2EInfo](#e2einfo)

Record SndGroupE2EEInfo:
- type: "sndGroupE2EEInfo"
- e2eeInfo: [E2EInfo](#e2einfo)

Record RcvGroupE2EEInfo:
- type: "rcvGroupE2EEInfo"
- e2eeInfo: [E2EInfo](#e2einfo)

Record InvalidJSON:
- type: "invalidJSON"
- direction: [MsgDirection](#msgdirection)
- json: String


## CIDeleteMode

**Enum type**:
- "broadcast"
- "internal"
- "internalMark"


## CIDeleted

**Discriminated union type**:

Record Deleted:
- type: "deleted"
- deletedTs: UTCTime?
- chatType: [ChatType](#chattype)

Record Blocked:
- type: "blocked"
- deletedTs: UTCTime?

Record BlockedByAdmin:
- type: "blockedByAdmin"
- deletedTs: UTCTime?

Record Moderated:
- type: "moderated"
- deletedTs: UTCTime?
- byGroupMember: [GroupMember](#groupmember)


## CIDirection

**Discriminated union type**:

Record DirectSnd:
- type: "directSnd"

Record DirectRcv:
- type: "directRcv"

Record GroupSnd:
- type: "groupSnd"

Record GroupRcv:
- type: "groupRcv"
- groupMember: [GroupMember](#groupmember)

Record LocalSnd:
- type: "localSnd"

Record LocalRcv:
- type: "localRcv"


## CIFile

**Record type**:
- fileId: Int64
- fileName: String
- fileSize: Int64
- fileSource: [CryptoFile](#cryptofile)?
- fileStatus: [CIFileStatus](#cifilestatus)
- fileProtocol: [FileProtocol](#fileprotocol)


## CIFileStatus

**Discriminated union type**:

Record SndStored:
- type: "sndStored"

Record SndTransfer:
- type: "sndTransfer"
- sndProgress: Int64
- sndTotal: Int64

Record SndCancelled:
- type: "sndCancelled"

Record SndComplete:
- type: "sndComplete"

Record SndError:
- type: "sndError"
- sndFileError: [FileError](#fileerror)

Record SndWarning:
- type: "sndWarning"
- sndFileError: [FileError](#fileerror)

Record RcvInvitation:
- type: "rcvInvitation"

Record RcvAccepted:
- type: "rcvAccepted"

Record RcvTransfer:
- type: "rcvTransfer"
- rcvProgress: Int64
- rcvTotal: Int64

Record RcvAborted:
- type: "rcvAborted"

Record RcvComplete:
- type: "rcvComplete"

Record RcvCancelled:
- type: "rcvCancelled"

Record RcvError:
- type: "rcvError"
- rcvFileError: [FileError](#fileerror)

Record RcvWarning:
- type: "rcvWarning"
- rcvFileError: [FileError](#fileerror)

Record Invalid:
- type: "invalid"
- text: String


## CIForwardedFrom

**Discriminated union type**:

Record Unknown:
- type: "unknown"

Record Contact:
- type: "contact"
- chatName: String
- msgDir: [MsgDirection](#msgdirection)
- contactId: Int64?
- chatItemId: Int64?

Record Group:
- type: "group"
- chatName: String
- msgDir: [MsgDirection](#msgdirection)
- groupId: Int64?
- chatItemId: Int64?


## CIGroupInvitation

**Record type**:
- groupId: Int64
- groupMemberId: Int64
- localDisplayName: String
- groupProfile: [GroupProfile](#groupprofile)
- status: [CIGroupInvitationStatus](#cigroupinvitationstatus)


## CIGroupInvitationStatus

**Enum type**:
- "pending"
- "accepted"
- "rejected"
- "expired"


## CIMention

**Record type**:
- memberId: String
- memberRef: [CIMentionMember](#cimentionmember)?


## CIMentionMember

**Record type**:
- groupMemberId: Int64
- displayName: String
- localAlias: String?
- memberRole: [GroupMemberRole](#groupmemberrole)


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


## CIQDirection

**Discriminated union type**:

Record DirectSnd:
- type: "directSnd"

Record DirectRcv:
- type: "directRcv"

Record GroupSnd:
- type: "groupSnd"

Record GroupRcv:
- type: "groupRcv"
- groupMember: [GroupMember](#groupmember)

Record LocalSnd:
- type: "localSnd"

Record LocalRcv:
- type: "localRcv"


## CIQuote

**Record type**:
- chatDir: [CIQDirection](#ciqdirection)
- itemId: Int64?
- sharedMsgId: String?
- sentAt: UTCTime
- content: [MsgContent](#msgcontent)
- formattedText: [[FormattedText](#formattedtext)]?


## CIReaction

**Record type**:
- chatDir: [CIDirection](#cidirection)
- chatItem: [ChatItem](#chatitem)
- sentAt: UTCTime
- reaction: [MsgReaction](#msgreaction)


## CIReactionCount

**Record type**:
- reaction: [MsgReaction](#msgreaction)
- userReacted: Bool
- totalReacted: Int


## CIStatus

**Discriminated union type**:

Record SndNew:
- type: "sndNew"

Record SndSent:
- type: "sndSent"
- sndProgress: [SndCIStatusProgress](#sndcistatusprogress)

Record SndRcvd:
- type: "sndRcvd"
- msgRcptStatus: [MsgReceiptStatus](#msgreceiptstatus)
- sndProgress: [SndCIStatusProgress](#sndcistatusprogress)

Record SndErrorAuth:
- type: "sndErrorAuth"

Record SndError:
- type: "sndError"
- agentError: [SndError](#snderror)

Record SndWarning:
- type: "sndWarning"
- agentError: [SndError](#snderror)

Record RcvNew:
- type: "rcvNew"

Record RcvRead:
- type: "rcvRead"

Record Invalid:
- type: "invalid"
- text: String


## CITimed

**Record type**:
- ttl: Int
- deleteAt: UTCTime?


## ChatDeleteMode

**Discriminated union type**:

Record Full:
- type: "full"
- notify: Bool

Record Entity:
- type: "entity"
- notify: Bool

Record Messages:
- type: "messages"


## ChatError


## ChatFeature

**Enum type**:
- "timedMessages"
- "fullDelete"
- "reactions"
- "voice"
- "calls"


## ChatInfo

**Discriminated union type**:

Record Direct:
- type: "direct"
- contact: [Contact](#contact)

Record Group:
- type: "group"
- groupInfo: [GroupInfo](#groupinfo)
- groupChatScope: [GroupChatScopeInfo](#groupchatscopeinfo)?

Record Local:
- type: "local"
- noteFolder: [NoteFolder](#notefolder)

Record ContactRequest:
- type: "contactRequest"
- contactRequest: [UserContactRequest](#usercontactrequest)

Record ContactConnection:
- type: "contactConnection"
- contactConnection: [PendingContactConnection](#pendingcontactconnection)


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


## ChatItemDeletion

Message deletion result.

**Record type**:
- deletedChatItem: [AChatItem](#achatitem)
- toChatItem: [AChatItem](#achatitem)?


## ChatRef


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

Record InvitationLink:
- type: "invitationLink"
- invitationLinkPlan: [InvitationLinkPlan](#invitationlinkplan)

Record ContactAddress:
- type: "contactAddress"
- contactAddressPlan: [ContactAddressPlan](#contactaddressplan)

Record GroupLink:
- type: "groupLink"
- groupLinkPlan: [GroupLinkPlan](#grouplinkplan)

Record Error:
- type: "error"
- chatError: [ChatError](#chaterror)


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

Record Ok:
- type: "ok"
- contactSLinkData_: [ContactShortLinkData](#contactshortlinkdata)?

Record OwnLink:
- type: "ownLink"

Record ConnectingConfirmReconnect:
- type: "connectingConfirmReconnect"

Record ConnectingProhibit:
- type: "connectingProhibit"
- contact: [Contact](#contact)

Record Known:
- type: "known"
- contact: [Contact](#contact)

Record ContactViaAddress:
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

Record Contact:
- type: "contact"
- preference: [SimplePreference](#simplepreference)

Record User:
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


## E2EInfo

**Record type**:
- pqEnabled: Bool?


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


## FileError

**Discriminated union type**:

Record Auth:
- type: "auth"

Record Blocked:
- type: "blocked"
- server: String
- blockInfo: [BlockingInfo](#blockinginfo)

Record NoFile:
- type: "noFile"

Record Relay:
- type: "relay"
- srvError: [SrvError](#srverror)

Record Other:
- type: "other"
- fileError: String


## FileInvitation

**Record type**:
- fileName: String
- fileSize: Int64
- fileDigest: String?
- fileConnReq: String?
- fileInline: [InlineFileMode](#inlinefilemode)?
- fileDescr: [FileDescr](#filedescr)?


## FileProtocol

**Enum type**:
- "sMP"
- "xFTP"
- "local"


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

Record Bold:
- type: "bold"

Record Italic:
- type: "italic"

Record StrikeThrough:
- type: "strikeThrough"

Record Snippet:
- type: "snippet"

Record Secret:
- type: "secret"

Record Colored:
- type: "colored"
- color: [FormatColor](#formatcolor)

Record Uri:
- type: "uri"

Record SimplexLink:
- type: "simplexLink"
- linkType: [SimplexLinkType](#simplexlinktype)
- simplexUri: String
- smpHosts: [String]

Record Mention:
- type: "mention"
- memberName: String

Record Email:
- type: "email"

Record Phone:
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


## GroupChatScope

**Discriminated union type**:

Record MemberSupport:
- type: "memberSupport"
- groupMemberId_: Int64?


## GroupChatScopeInfo

**Discriminated union type**:

Record MemberSupport:
- type: "memberSupport"
- groupMember_: [GroupMember](#groupmember)?


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

Record Ok:
- type: "ok"
- groupSLinkData_: [GroupShortLinkData](#groupshortlinkdata)?

Record OwnLink:
- type: "ownLink"
- groupInfo: [GroupInfo](#groupinfo)

Record ConnectingConfirmReconnect:
- type: "connectingConfirmReconnect"

Record ConnectingProhibit:
- type: "connectingProhibit"
- groupInfo_: [GroupInfo](#groupinfo)?

Record Known:
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


## GroupMemberRef

**Record type**:
- groupMemberId: Int64
- profile: [Profile](#profile)


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

Record Ok:
- type: "ok"
- contactSLinkData_: [ContactShortLinkData](#contactshortlinkdata)?

Record OwnLink:
- type: "ownLink"

Record Connecting:
- type: "connecting"
- contact_: [Contact](#contact)?

Record Known:
- type: "known"
- contact: [Contact](#contact)


## InvitedBy

**Discriminated union type**:

Record Contact:
- type: "contact"
- byContactId: Int64

Record User:
- type: "user"

Record Unknown:
- type: "unknown"


## LinkContent

**Discriminated union type**:

Record Page:
- type: "page"

Record Image:
- type: "image"

Record Video:
- type: "video"
- duration: Int?

Record Unknown:
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


## MsgChatLink

Connection link sent in a message - only short links are allowed.

**Discriminated union type**:

Record Contact:
- type: "contact"
- connLink: String
- profile: [Profile](#profile)
- business: Bool

Record Invitation:
- type: "invitation"
- invLink: String
- profile: [Profile](#profile)

Record Group:
- type: "group"
- connLink: String
- groupProfile: [GroupProfile](#groupprofile)


## MsgContent

**Discriminated union type**:

Record Text:
- type: "text"
- text: String

Record Link:
- type: "link"
- text: String
- preview: [LinkPreview](#linkpreview)

Record Image:
- type: "image"
- text: String
- image: String

Record Video:
- type: "video"
- text: String
- image: String
- duration: Int

Record Voice:
- type: "voice"
- text: String
- duration: Int

Record File:
- type: "file"
- text: String

Record Report:
- type: "report"
- text: String
- reason: [ReportReason](#reportreason)

Record Chat:
- type: "chat"
- text: String
- chatLink: [MsgChatLink](#msgchatlink)

Record Unknown:
- type: "unknown"
- tag: String
- text: String
- json: JSONObject


## MsgDecryptError

**Enum type**:
- "ratchetHeader"
- "tooManySkipped"
- "ratchetEarlier"
- "other"
- "ratchetSync"


## MsgDirection

**Enum type**:
- "rcv"
- "snd"


## MsgErrorType

**Discriminated union type**:

Record MsgSkipped:
- type: "msgSkipped"
- fromMsgId: Int64
- toMsgId: Int64

Record MsgBadId:
- type: "msgBadId"
- msgId: Int64

Record MsgBadHash:
- type: "msgBadHash"

Record MsgDuplicate:
- type: "msgDuplicate"


## MsgFilter

**Enum type**:
- "none"
- "all"
- "mentions"


## MsgReaction

**Discriminated union type**:

Record Emoji:
- type: "emoji"
- emoji: String

Record Unknown:
- type: "unknown"
- tag: String
- json: JSONObject


## MsgReceiptStatus

**Enum type**:
- "ok"
- "badMsgHash"


## NewUser

**Record type**:
- profile: [Profile](#profile)?
- pastTimestamp: Bool


## NoteFolder

**Record type**:
- noteFolderId: Int64
- userId: Int64
- createdAt: UTCTime
- updatedAt: UTCTime
- chatTs: UTCTime
- favorite: Bool
- unread: Bool


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
- connLinkToConnect: [CreatedConnLink](#createdconnlink)
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


## RcvConnEvent

**Discriminated union type**:

Record SwitchQueue:
- type: "switchQueue"
- phase: [SwitchPhase](#switchphase)

Record RatchetSync:
- type: "ratchetSync"
- syncStatus: [RatchetSyncState](#ratchetsyncstate)

Record VerificationCodeReset:
- type: "verificationCodeReset"

Record PqEnabled:
- type: "pqEnabled"
- enabled: Bool


## RcvDirectEvent

**Discriminated union type**:

Record ContactDeleted:
- type: "contactDeleted"

Record ProfileUpdated:
- type: "profileUpdated"
- fromProfile: [Profile](#profile)
- toProfile: [Profile](#profile)


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

Record New:
- type: "new"

Record Accepted:
- type: "accepted"
- fileInfo: [RcvFileInfo](#rcvfileinfo)

Record Connected:
- type: "connected"
- fileInfo: [RcvFileInfo](#rcvfileinfo)

Record Complete:
- type: "complete"
- fileInfo: [RcvFileInfo](#rcvfileinfo)

Record Cancelled:
- type: "cancelled"
- fileInfo_: [RcvFileInfo](#rcvfileinfo)?


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


## RcvGroupEvent

**Discriminated union type**:

Record MemberAdded:
- type: "memberAdded"
- groupMemberId: Int64
- profile: [Profile](#profile)

Record MemberConnected:
- type: "memberConnected"

Record MemberAccepted:
- type: "memberAccepted"
- groupMemberId: Int64
- profile: [Profile](#profile)

Record UserAccepted:
- type: "userAccepted"

Record MemberLeft:
- type: "memberLeft"

Record MemberRole:
- type: "memberRole"
- groupMemberId: Int64
- profile: [Profile](#profile)
- role: [GroupMemberRole](#groupmemberrole)

Record MemberBlocked:
- type: "memberBlocked"
- groupMemberId: Int64
- profile: [Profile](#profile)
- blocked: Bool

Record UserRole:
- type: "userRole"
- role: [GroupMemberRole](#groupmemberrole)

Record MemberDeleted:
- type: "memberDeleted"
- groupMemberId: Int64
- profile: [Profile](#profile)

Record UserDeleted:
- type: "userDeleted"

Record GroupDeleted:
- type: "groupDeleted"

Record GroupUpdated:
- type: "groupUpdated"
- groupProfile: [GroupProfile](#groupprofile)

Record InvitedViaGroupLink:
- type: "invitedViaGroupLink"

Record MemberCreatedContact:
- type: "memberCreatedContact"

Record MemberProfileUpdated:
- type: "memberProfileUpdated"
- fromProfile: [Profile](#profile)
- toProfile: [Profile](#profile)

Record NewMemberPendingReview:
- type: "newMemberPendingReview"


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


## SndCIStatusProgress

**Enum type**:
- "partial"
- "complete"


## SndConnEvent

**Discriminated union type**:

Record SwitchQueue:
- type: "switchQueue"
- phase: [SwitchPhase](#switchphase)
- member: [GroupMemberRef](#groupmemberref)?

Record RatchetSync:
- type: "ratchetSync"
- syncStatus: [RatchetSyncState](#ratchetsyncstate)
- member: [GroupMemberRef](#groupmemberref)?

Record PqEnabled:
- type: "pqEnabled"
- enabled: Bool


## SndError

**Discriminated union type**:

Record Auth:
- type: "auth"

Record Quota:
- type: "quota"

Record Expired:
- type: "expired"

Record Relay:
- type: "relay"
- srvError: [SrvError](#srverror)

Record Proxy:
- type: "proxy"
- proxyServer: String
- srvError: [SrvError](#srverror)

Record ProxyRelay:
- type: "proxyRelay"
- proxyServer: String
- srvError: [SrvError](#srverror)

Record Other:
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


## SndGroupEvent

**Discriminated union type**:

Record MemberRole:
- type: "memberRole"
- groupMemberId: Int64
- profile: [Profile](#profile)
- role: [GroupMemberRole](#groupmemberrole)

Record MemberBlocked:
- type: "memberBlocked"
- groupMemberId: Int64
- profile: [Profile](#profile)
- blocked: Bool

Record UserRole:
- type: "userRole"
- role: [GroupMemberRole](#groupmemberrole)

Record MemberDeleted:
- type: "memberDeleted"
- groupMemberId: Int64
- profile: [Profile](#profile)

Record UserLeft:
- type: "userLeft"

Record GroupUpdated:
- type: "groupUpdated"
- groupProfile: [GroupProfile](#groupprofile)

Record MemberAccepted:
- type: "memberAccepted"
- groupMemberId: Int64
- profile: [Profile](#profile)

Record UserPendingReview:
- type: "userPendingReview"


## SrvError

**Discriminated union type**:

Record Host:
- type: "host"

Record Version:
- type: "version"

Record Other:
- type: "other"
- srvError: String


## SwitchPhase

**Enum type**:
- "started"
- "confirmed"
- "secured"
- "completed"


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
