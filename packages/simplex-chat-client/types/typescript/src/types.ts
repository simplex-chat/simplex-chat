// API Types
// This file is generated automatically.

export interface ACIReaction {
  chatInfo: ChatInfo
  chatReaction: CIReaction
}

export interface AChat {
  chatInfo: ChatInfo
  chatItems: ChatItem[]
  chatStats: ChatStats
}

export interface AChatItem {
  chatInfo: ChatInfo
  chatItem: ChatItem
}

export interface AddressSettings {
  businessAddress: boolean
  autoAccept?: AutoAccept
  autoReply?: MsgContent
}

export type AgentCryptoError = 
  | AgentCryptoError.DECRYPT_AES
  | AgentCryptoError.DECRYPT_CB
  | AgentCryptoError.RATCHET_HEADER
  | AgentCryptoError.RATCHET_SYNC

export namespace AgentCryptoError {
  export type Tag = "DECRYPT_AES" | "DECRYPT_CB" | "RATCHET_HEADER" | "RATCHET_SYNC"

  interface Interface {
    type: Tag
  }

  export interface DECRYPT_AES extends Interface {
    type: "DECRYPT_AES"
  }

  export interface DECRYPT_CB extends Interface {
    type: "DECRYPT_CB"
  }

  export interface RATCHET_HEADER extends Interface {
    type: "RATCHET_HEADER"
  }

  export interface RATCHET_SYNC extends Interface {
    type: "RATCHET_SYNC"
  }
}

export type AgentErrorType = 
  | AgentErrorType.CMD
  | AgentErrorType.CONN
  | AgentErrorType.NO_USER
  | AgentErrorType.SMP
  | AgentErrorType.NTF
  | AgentErrorType.XFTP
  | AgentErrorType.FILE
  | AgentErrorType.PROXY
  | AgentErrorType.RCP
  | AgentErrorType.BROKER
  | AgentErrorType.AGENT
  | AgentErrorType.NOTICE
  | AgentErrorType.INTERNAL
  | AgentErrorType.CRITICAL
  | AgentErrorType.INACTIVE

export namespace AgentErrorType {
  export type Tag = 
    | "CMD"
    | "CONN"
    | "NO_USER"
    | "SMP"
    | "NTF"
    | "XFTP"
    | "FILE"
    | "PROXY"
    | "RCP"
    | "BROKER"
    | "AGENT"
    | "NOTICE"
    | "INTERNAL"
    | "CRITICAL"
    | "INACTIVE"

  interface Interface {
    type: Tag
  }

  export interface CMD extends Interface {
    type: "CMD"
    cmdErr: CommandErrorType
    errContext: string
  }

  export interface CONN extends Interface {
    type: "CONN"
    connErr: ConnectionErrorType
    errContext: string
  }

  export interface NO_USER extends Interface {
    type: "NO_USER"
  }

  export interface SMP extends Interface {
    type: "SMP"
    serverAddress: string
    smpErr: ErrorType
  }

  export interface NTF extends Interface {
    type: "NTF"
    serverAddress: string
    ntfErr: ErrorType
  }

  export interface XFTP extends Interface {
    type: "XFTP"
    serverAddress: string
    xftpErr: XFTPErrorType
  }

  export interface FILE extends Interface {
    type: "FILE"
    fileErr: FileErrorType
  }

  export interface PROXY extends Interface {
    type: "PROXY"
    proxyServer: string
    relayServer: string
    proxyErr: ProxyClientError
  }

  export interface RCP extends Interface {
    type: "RCP"
    rcpErr: RCErrorType
  }

  export interface BROKER extends Interface {
    type: "BROKER"
    brokerAddress: string
    brokerErr: BrokerErrorType
  }

  export interface AGENT extends Interface {
    type: "AGENT"
    agentErr: SMPAgentError
  }

  export interface NOTICE extends Interface {
    type: "NOTICE"
    server: string
    preset: boolean
    expiresAt?: string // ISO-8601 timestamp
  }

  export interface INTERNAL extends Interface {
    type: "INTERNAL"
    internalErr: string
  }

  export interface CRITICAL extends Interface {
    type: "CRITICAL"
    offerRestart: boolean
    criticalErr: string
  }

  export interface INACTIVE extends Interface {
    type: "INACTIVE"
  }
}

export interface AutoAccept {
  acceptIncognito: boolean
}

export interface BlockingInfo {
  reason: BlockingReason
  notice?: ClientNotice
}

export enum BlockingReason {
  Spam = "spam",
  Content = "content",
}

export type BrokerErrorType = 
  | BrokerErrorType.RESPONSE
  | BrokerErrorType.UNEXPECTED
  | BrokerErrorType.NETWORK
  | BrokerErrorType.HOST
  | BrokerErrorType.NO_SERVICE
  | BrokerErrorType.TRANSPORT
  | BrokerErrorType.TIMEOUT

export namespace BrokerErrorType {
  export type Tag = 
    | "RESPONSE"
    | "UNEXPECTED"
    | "NETWORK"
    | "HOST"
    | "NO_SERVICE"
    | "TRANSPORT"
    | "TIMEOUT"

  interface Interface {
    type: Tag
  }

  export interface RESPONSE extends Interface {
    type: "RESPONSE"
    respErr: string
  }

  export interface UNEXPECTED extends Interface {
    type: "UNEXPECTED"
    respErr: string
  }

  export interface NETWORK extends Interface {
    type: "NETWORK"
    networkError: NetworkError
  }

  export interface HOST extends Interface {
    type: "HOST"
  }

  export interface NO_SERVICE extends Interface {
    type: "NO_SERVICE"
  }

  export interface TRANSPORT extends Interface {
    type: "TRANSPORT"
    transportErr: TransportError
  }

  export interface TIMEOUT extends Interface {
    type: "TIMEOUT"
  }
}

export interface BusinessChatInfo {
  chatType: BusinessChatType
  businessId: string
  customerId: string
}

export enum BusinessChatType {
  Business = "business",
  Customer = "customer",
}

export enum CICallStatus {
  Pending = "pending",
  Missed = "missed",
  Rejected = "rejected",
  Accepted = "accepted",
  Negotiated = "negotiated",
  Progress = "progress",
  Ended = "ended",
  Error = "error",
}

export type CIContent = 
  | CIContent.SndMsgContent
  | CIContent.RcvMsgContent
  | CIContent.SndDeleted
  | CIContent.RcvDeleted
  | CIContent.SndCall
  | CIContent.RcvCall
  | CIContent.RcvIntegrityError
  | CIContent.RcvDecryptionError
  | CIContent.RcvGroupInvitation
  | CIContent.SndGroupInvitation
  | CIContent.RcvDirectEvent
  | CIContent.RcvGroupEvent
  | CIContent.SndGroupEvent
  | CIContent.RcvConnEvent
  | CIContent.SndConnEvent
  | CIContent.RcvChatFeature
  | CIContent.SndChatFeature
  | CIContent.RcvChatPreference
  | CIContent.SndChatPreference
  | CIContent.RcvGroupFeature
  | CIContent.SndGroupFeature
  | CIContent.RcvChatFeatureRejected
  | CIContent.RcvGroupFeatureRejected
  | CIContent.SndModerated
  | CIContent.RcvModerated
  | CIContent.RcvBlocked
  | CIContent.SndDirectE2EEInfo
  | CIContent.RcvDirectE2EEInfo
  | CIContent.SndGroupE2EEInfo
  | CIContent.RcvGroupE2EEInfo
  | CIContent.ChatBanner

export namespace CIContent {
  export type Tag = 
    | "sndMsgContent"
    | "rcvMsgContent"
    | "sndDeleted"
    | "rcvDeleted"
    | "sndCall"
    | "rcvCall"
    | "rcvIntegrityError"
    | "rcvDecryptionError"
    | "rcvGroupInvitation"
    | "sndGroupInvitation"
    | "rcvDirectEvent"
    | "rcvGroupEvent"
    | "sndGroupEvent"
    | "rcvConnEvent"
    | "sndConnEvent"
    | "rcvChatFeature"
    | "sndChatFeature"
    | "rcvChatPreference"
    | "sndChatPreference"
    | "rcvGroupFeature"
    | "sndGroupFeature"
    | "rcvChatFeatureRejected"
    | "rcvGroupFeatureRejected"
    | "sndModerated"
    | "rcvModerated"
    | "rcvBlocked"
    | "sndDirectE2EEInfo"
    | "rcvDirectE2EEInfo"
    | "sndGroupE2EEInfo"
    | "rcvGroupE2EEInfo"
    | "chatBanner"

  interface Interface {
    type: Tag
  }

  export interface SndMsgContent extends Interface {
    type: "sndMsgContent"
    msgContent: MsgContent
  }

  export interface RcvMsgContent extends Interface {
    type: "rcvMsgContent"
    msgContent: MsgContent
  }

  export interface SndDeleted extends Interface {
    type: "sndDeleted"
    deleteMode: CIDeleteMode
  }

  export interface RcvDeleted extends Interface {
    type: "rcvDeleted"
    deleteMode: CIDeleteMode
  }

  export interface SndCall extends Interface {
    type: "sndCall"
    status: CICallStatus
    duration: number // int
  }

  export interface RcvCall extends Interface {
    type: "rcvCall"
    status: CICallStatus
    duration: number // int
  }

  export interface RcvIntegrityError extends Interface {
    type: "rcvIntegrityError"
    msgError: MsgErrorType
  }

  export interface RcvDecryptionError extends Interface {
    type: "rcvDecryptionError"
    msgDecryptError: MsgDecryptError
    msgCount: number // word32
  }

  export interface RcvGroupInvitation extends Interface {
    type: "rcvGroupInvitation"
    groupInvitation: CIGroupInvitation
    memberRole: GroupMemberRole
  }

  export interface SndGroupInvitation extends Interface {
    type: "sndGroupInvitation"
    groupInvitation: CIGroupInvitation
    memberRole: GroupMemberRole
  }

  export interface RcvDirectEvent extends Interface {
    type: "rcvDirectEvent"
    rcvDirectEvent: RcvDirectEvent
  }

  export interface RcvGroupEvent extends Interface {
    type: "rcvGroupEvent"
    rcvGroupEvent: RcvGroupEvent
  }

  export interface SndGroupEvent extends Interface {
    type: "sndGroupEvent"
    sndGroupEvent: SndGroupEvent
  }

  export interface RcvConnEvent extends Interface {
    type: "rcvConnEvent"
    rcvConnEvent: RcvConnEvent
  }

  export interface SndConnEvent extends Interface {
    type: "sndConnEvent"
    sndConnEvent: SndConnEvent
  }

  export interface RcvChatFeature extends Interface {
    type: "rcvChatFeature"
    feature: ChatFeature
    enabled: PrefEnabled
    param?: number // int
  }

  export interface SndChatFeature extends Interface {
    type: "sndChatFeature"
    feature: ChatFeature
    enabled: PrefEnabled
    param?: number // int
  }

  export interface RcvChatPreference extends Interface {
    type: "rcvChatPreference"
    feature: ChatFeature
    allowed: FeatureAllowed
    param?: number // int
  }

  export interface SndChatPreference extends Interface {
    type: "sndChatPreference"
    feature: ChatFeature
    allowed: FeatureAllowed
    param?: number // int
  }

  export interface RcvGroupFeature extends Interface {
    type: "rcvGroupFeature"
    groupFeature: GroupFeature
    preference: GroupPreference
    param?: number // int
    memberRole_?: GroupMemberRole
  }

  export interface SndGroupFeature extends Interface {
    type: "sndGroupFeature"
    groupFeature: GroupFeature
    preference: GroupPreference
    param?: number // int
    memberRole_?: GroupMemberRole
  }

  export interface RcvChatFeatureRejected extends Interface {
    type: "rcvChatFeatureRejected"
    feature: ChatFeature
  }

  export interface RcvGroupFeatureRejected extends Interface {
    type: "rcvGroupFeatureRejected"
    groupFeature: GroupFeature
  }

  export interface SndModerated extends Interface {
    type: "sndModerated"
  }

  export interface RcvModerated extends Interface {
    type: "rcvModerated"
  }

  export interface RcvBlocked extends Interface {
    type: "rcvBlocked"
  }

  export interface SndDirectE2EEInfo extends Interface {
    type: "sndDirectE2EEInfo"
    e2eeInfo: E2EInfo
  }

  export interface RcvDirectE2EEInfo extends Interface {
    type: "rcvDirectE2EEInfo"
    e2eeInfo: E2EInfo
  }

  export interface SndGroupE2EEInfo extends Interface {
    type: "sndGroupE2EEInfo"
    e2eeInfo: E2EInfo
  }

  export interface RcvGroupE2EEInfo extends Interface {
    type: "rcvGroupE2EEInfo"
    e2eeInfo: E2EInfo
  }

  export interface ChatBanner extends Interface {
    type: "chatBanner"
  }
}

export enum CIDeleteMode {
  Broadcast = "broadcast",
  Internal = "internal",
  InternalMark = "internalMark",
}

export type CIDeleted = CIDeleted.Deleted | CIDeleted.Blocked | CIDeleted.BlockedByAdmin | CIDeleted.Moderated

export namespace CIDeleted {
  export type Tag = "deleted" | "blocked" | "blockedByAdmin" | "moderated"

  interface Interface {
    type: Tag
  }

  export interface Deleted extends Interface {
    type: "deleted"
    deletedTs?: string // ISO-8601 timestamp
    chatType: ChatType
  }

  export interface Blocked extends Interface {
    type: "blocked"
    deletedTs?: string // ISO-8601 timestamp
  }

  export interface BlockedByAdmin extends Interface {
    type: "blockedByAdmin"
    deletedTs?: string // ISO-8601 timestamp
  }

  export interface Moderated extends Interface {
    type: "moderated"
    deletedTs?: string // ISO-8601 timestamp
    byGroupMember: GroupMember
  }
}

export type CIDirection = 
  | CIDirection.DirectSnd
  | CIDirection.DirectRcv
  | CIDirection.GroupSnd
  | CIDirection.GroupRcv
  | CIDirection.LocalSnd
  | CIDirection.LocalRcv

export namespace CIDirection {
  export type Tag = "directSnd" | "directRcv" | "groupSnd" | "groupRcv" | "localSnd" | "localRcv"

  interface Interface {
    type: Tag
  }

  export interface DirectSnd extends Interface {
    type: "directSnd"
  }

  export interface DirectRcv extends Interface {
    type: "directRcv"
  }

  export interface GroupSnd extends Interface {
    type: "groupSnd"
  }

  export interface GroupRcv extends Interface {
    type: "groupRcv"
    groupMember: GroupMember
  }

  export interface LocalSnd extends Interface {
    type: "localSnd"
  }

  export interface LocalRcv extends Interface {
    type: "localRcv"
  }
}

export interface CIFile {
  fileId: number // int64
  fileName: string
  fileSize: number // int64
  fileSource?: CryptoFile
  fileStatus: CIFileStatus
  fileProtocol: FileProtocol
}

export type CIFileStatus = 
  | CIFileStatus.SndStored
  | CIFileStatus.SndTransfer
  | CIFileStatus.SndCancelled
  | CIFileStatus.SndComplete
  | CIFileStatus.SndError
  | CIFileStatus.SndWarning
  | CIFileStatus.RcvInvitation
  | CIFileStatus.RcvAccepted
  | CIFileStatus.RcvTransfer
  | CIFileStatus.RcvAborted
  | CIFileStatus.RcvComplete
  | CIFileStatus.RcvCancelled
  | CIFileStatus.RcvError
  | CIFileStatus.RcvWarning
  | CIFileStatus.Invalid

export namespace CIFileStatus {
  export type Tag = 
    | "sndStored"
    | "sndTransfer"
    | "sndCancelled"
    | "sndComplete"
    | "sndError"
    | "sndWarning"
    | "rcvInvitation"
    | "rcvAccepted"
    | "rcvTransfer"
    | "rcvAborted"
    | "rcvComplete"
    | "rcvCancelled"
    | "rcvError"
    | "rcvWarning"
    | "invalid"

  interface Interface {
    type: Tag
  }

  export interface SndStored extends Interface {
    type: "sndStored"
  }

  export interface SndTransfer extends Interface {
    type: "sndTransfer"
    sndProgress: number // int64
    sndTotal: number // int64
  }

  export interface SndCancelled extends Interface {
    type: "sndCancelled"
  }

  export interface SndComplete extends Interface {
    type: "sndComplete"
  }

  export interface SndError extends Interface {
    type: "sndError"
    sndFileError: FileError
  }

  export interface SndWarning extends Interface {
    type: "sndWarning"
    sndFileError: FileError
  }

  export interface RcvInvitation extends Interface {
    type: "rcvInvitation"
  }

  export interface RcvAccepted extends Interface {
    type: "rcvAccepted"
  }

  export interface RcvTransfer extends Interface {
    type: "rcvTransfer"
    rcvProgress: number // int64
    rcvTotal: number // int64
  }

  export interface RcvAborted extends Interface {
    type: "rcvAborted"
  }

  export interface RcvComplete extends Interface {
    type: "rcvComplete"
  }

  export interface RcvCancelled extends Interface {
    type: "rcvCancelled"
  }

  export interface RcvError extends Interface {
    type: "rcvError"
    rcvFileError: FileError
  }

  export interface RcvWarning extends Interface {
    type: "rcvWarning"
    rcvFileError: FileError
  }

  export interface Invalid extends Interface {
    type: "invalid"
    text: string
  }
}

export type CIForwardedFrom = CIForwardedFrom.Unknown | CIForwardedFrom.Contact | CIForwardedFrom.Group

export namespace CIForwardedFrom {
  export type Tag = "unknown" | "contact" | "group"

  interface Interface {
    type: Tag
  }

  export interface Unknown extends Interface {
    type: "unknown"
  }

  export interface Contact extends Interface {
    type: "contact"
    chatName: string
    msgDir: MsgDirection
    contactId?: number // int64
    chatItemId?: number // int64
  }

  export interface Group extends Interface {
    type: "group"
    chatName: string
    msgDir: MsgDirection
    groupId?: number // int64
    chatItemId?: number // int64
  }
}

export interface CIGroupInvitation {
  groupId: number // int64
  groupMemberId: number // int64
  localDisplayName: string
  groupProfile: GroupProfile
  status: CIGroupInvitationStatus
}

export enum CIGroupInvitationStatus {
  Pending = "pending",
  Accepted = "accepted",
  Rejected = "rejected",
  Expired = "expired",
}

export interface CIMention {
  memberId: string
  memberRef?: CIMentionMember
}

export interface CIMentionMember {
  groupMemberId: number // int64
  displayName: string
  localAlias?: string
  memberRole: GroupMemberRole
}

export interface CIMeta {
  itemId: number // int64
  itemTs: string // ISO-8601 timestamp
  itemText: string
  itemStatus: CIStatus
  sentViaProxy?: boolean
  itemSharedMsgId?: string
  itemForwarded?: CIForwardedFrom
  itemDeleted?: CIDeleted
  itemEdited: boolean
  itemTimed?: CITimed
  itemLive?: boolean
  userMention: boolean
  hasLink: boolean
  deletable: boolean
  editable: boolean
  forwardedByMember?: number // int64
  showGroupAsSender: boolean
  createdAt: string // ISO-8601 timestamp
  updatedAt: string // ISO-8601 timestamp
}

export interface CIQuote {
  chatDir?: CIDirection
  itemId?: number // int64
  sharedMsgId?: string
  sentAt: string // ISO-8601 timestamp
  content: MsgContent
  formattedText?: FormattedText[]
}

export interface CIReaction {
  chatDir: CIDirection
  chatItem: ChatItem
  sentAt: string // ISO-8601 timestamp
  reaction: MsgReaction
}

export interface CIReactionCount {
  reaction: MsgReaction
  userReacted: boolean
  totalReacted: number // int
}

export type CIStatus = 
  | CIStatus.SndNew
  | CIStatus.SndSent
  | CIStatus.SndRcvd
  | CIStatus.SndErrorAuth
  | CIStatus.SndError
  | CIStatus.SndWarning
  | CIStatus.RcvNew
  | CIStatus.RcvRead
  | CIStatus.Invalid

export namespace CIStatus {
  export type Tag = 
    | "sndNew"
    | "sndSent"
    | "sndRcvd"
    | "sndErrorAuth"
    | "sndError"
    | "sndWarning"
    | "rcvNew"
    | "rcvRead"
    | "invalid"

  interface Interface {
    type: Tag
  }

  export interface SndNew extends Interface {
    type: "sndNew"
  }

  export interface SndSent extends Interface {
    type: "sndSent"
    sndProgress: SndCIStatusProgress
  }

  export interface SndRcvd extends Interface {
    type: "sndRcvd"
    msgRcptStatus: MsgReceiptStatus
    sndProgress: SndCIStatusProgress
  }

  export interface SndErrorAuth extends Interface {
    type: "sndErrorAuth"
  }

  export interface SndError extends Interface {
    type: "sndError"
    agentError: SndError
  }

  export interface SndWarning extends Interface {
    type: "sndWarning"
    agentError: SndError
  }

  export interface RcvNew extends Interface {
    type: "rcvNew"
  }

  export interface RcvRead extends Interface {
    type: "rcvRead"
  }

  export interface Invalid extends Interface {
    type: "invalid"
    text: string
  }
}

export interface CITimed {
  ttl: number // int
  deleteAt?: string // ISO-8601 timestamp
}

export type ChatBotCommand = ChatBotCommand.Command | ChatBotCommand.Menu

export namespace ChatBotCommand {
  export type Tag = "command" | "menu"

  interface Interface {
    type: Tag
  }

  export interface Command extends Interface {
    type: "command"
    keyword: string
    label: string
    params?: string
  }

  export interface Menu extends Interface {
    type: "menu"
    label: string
    commands: ChatBotCommand[]
  }
}

export type ChatDeleteMode = ChatDeleteMode.Full | ChatDeleteMode.Entity | ChatDeleteMode.Messages

export namespace ChatDeleteMode {
  export type Tag = "full" | "entity" | "messages"

  interface Interface {
    type: Tag
  }

  export interface Full extends Interface {
    type: "full"
    notify: boolean
  }

  export interface Entity extends Interface {
    type: "entity"
    notify: boolean
  }

  export interface Messages extends Interface {
    type: "messages"
  }

  export function cmdString(self: ChatDeleteMode): string {
    return self.type + (self.type == 'messages' ? '' : (!self.notify ? ' notify=off' : ''))
  }
}

export type ChatError = ChatError.Error | ChatError.ErrorAgent | ChatError.ErrorStore

export namespace ChatError {
  export type Tag = "error" | "errorAgent" | "errorStore"

  interface Interface {
    type: Tag
  }

  export interface Error extends Interface {
    type: "error"
    errorType: ChatErrorType
  }

  export interface ErrorAgent extends Interface {
    type: "errorAgent"
    agentError: AgentErrorType
    agentConnId: string
    connectionEntity_?: ConnectionEntity
  }

  export interface ErrorStore extends Interface {
    type: "errorStore"
    storeError: StoreError
  }
}

export type ChatErrorType = 
  | ChatErrorType.NoActiveUser
  | ChatErrorType.NoConnectionUser
  | ChatErrorType.NoSndFileUser
  | ChatErrorType.NoRcvFileUser
  | ChatErrorType.UserUnknown
  | ChatErrorType.ActiveUserExists
  | ChatErrorType.UserExists
  | ChatErrorType.ChatRelayExists
  | ChatErrorType.DifferentActiveUser
  | ChatErrorType.CantDeleteActiveUser
  | ChatErrorType.CantDeleteLastUser
  | ChatErrorType.CantHideLastUser
  | ChatErrorType.HiddenUserAlwaysMuted
  | ChatErrorType.EmptyUserPassword
  | ChatErrorType.UserAlreadyHidden
  | ChatErrorType.UserNotHidden
  | ChatErrorType.InvalidDisplayName
  | ChatErrorType.ChatNotStarted
  | ChatErrorType.ChatNotStopped
  | ChatErrorType.ChatStoreChanged
  | ChatErrorType.InvalidConnReq
  | ChatErrorType.UnsupportedConnReq
  | ChatErrorType.ConnReqMessageProhibited
  | ChatErrorType.ContactNotReady
  | ChatErrorType.ContactNotActive
  | ChatErrorType.ContactDisabled
  | ChatErrorType.ConnectionDisabled
  | ChatErrorType.GroupUserRole
  | ChatErrorType.GroupMemberInitialRole
  | ChatErrorType.ContactIncognitoCantInvite
  | ChatErrorType.GroupIncognitoCantInvite
  | ChatErrorType.GroupContactRole
  | ChatErrorType.GroupDuplicateMember
  | ChatErrorType.GroupDuplicateMemberId
  | ChatErrorType.GroupNotJoined
  | ChatErrorType.GroupMemberNotActive
  | ChatErrorType.CantBlockMemberForSelf
  | ChatErrorType.GroupMemberUserRemoved
  | ChatErrorType.GroupMemberNotFound
  | ChatErrorType.GroupCantResendInvitation
  | ChatErrorType.GroupInternal
  | ChatErrorType.FileNotFound
  | ChatErrorType.FileSize
  | ChatErrorType.FileAlreadyReceiving
  | ChatErrorType.FileCancelled
  | ChatErrorType.FileCancel
  | ChatErrorType.FileAlreadyExists
  | ChatErrorType.FileWrite
  | ChatErrorType.FileSend
  | ChatErrorType.FileRcvChunk
  | ChatErrorType.FileInternal
  | ChatErrorType.FileImageType
  | ChatErrorType.FileImageSize
  | ChatErrorType.FileNotReceived
  | ChatErrorType.FileNotApproved
  | ChatErrorType.FallbackToSMPProhibited
  | ChatErrorType.InlineFileProhibited
  | ChatErrorType.InvalidForward
  | ChatErrorType.InvalidChatItemUpdate
  | ChatErrorType.InvalidChatItemDelete
  | ChatErrorType.HasCurrentCall
  | ChatErrorType.NoCurrentCall
  | ChatErrorType.CallContact
  | ChatErrorType.DirectMessagesProhibited
  | ChatErrorType.AgentVersion
  | ChatErrorType.AgentNoSubResult
  | ChatErrorType.CommandError
  | ChatErrorType.AgentCommandError
  | ChatErrorType.InvalidFileDescription
  | ChatErrorType.ConnectionIncognitoChangeProhibited
  | ChatErrorType.ConnectionUserChangeProhibited
  | ChatErrorType.PeerChatVRangeIncompatible
  | ChatErrorType.InternalError
  | ChatErrorType.Exception

export namespace ChatErrorType {
  export type Tag = 
    | "noActiveUser"
    | "noConnectionUser"
    | "noSndFileUser"
    | "noRcvFileUser"
    | "userUnknown"
    | "activeUserExists"
    | "userExists"
    | "chatRelayExists"
    | "differentActiveUser"
    | "cantDeleteActiveUser"
    | "cantDeleteLastUser"
    | "cantHideLastUser"
    | "hiddenUserAlwaysMuted"
    | "emptyUserPassword"
    | "userAlreadyHidden"
    | "userNotHidden"
    | "invalidDisplayName"
    | "chatNotStarted"
    | "chatNotStopped"
    | "chatStoreChanged"
    | "invalidConnReq"
    | "unsupportedConnReq"
    | "connReqMessageProhibited"
    | "contactNotReady"
    | "contactNotActive"
    | "contactDisabled"
    | "connectionDisabled"
    | "groupUserRole"
    | "groupMemberInitialRole"
    | "contactIncognitoCantInvite"
    | "groupIncognitoCantInvite"
    | "groupContactRole"
    | "groupDuplicateMember"
    | "groupDuplicateMemberId"
    | "groupNotJoined"
    | "groupMemberNotActive"
    | "cantBlockMemberForSelf"
    | "groupMemberUserRemoved"
    | "groupMemberNotFound"
    | "groupCantResendInvitation"
    | "groupInternal"
    | "fileNotFound"
    | "fileSize"
    | "fileAlreadyReceiving"
    | "fileCancelled"
    | "fileCancel"
    | "fileAlreadyExists"
    | "fileWrite"
    | "fileSend"
    | "fileRcvChunk"
    | "fileInternal"
    | "fileImageType"
    | "fileImageSize"
    | "fileNotReceived"
    | "fileNotApproved"
    | "fallbackToSMPProhibited"
    | "inlineFileProhibited"
    | "invalidForward"
    | "invalidChatItemUpdate"
    | "invalidChatItemDelete"
    | "hasCurrentCall"
    | "noCurrentCall"
    | "callContact"
    | "directMessagesProhibited"
    | "agentVersion"
    | "agentNoSubResult"
    | "commandError"
    | "agentCommandError"
    | "invalidFileDescription"
    | "connectionIncognitoChangeProhibited"
    | "connectionUserChangeProhibited"
    | "peerChatVRangeIncompatible"
    | "internalError"
    | "exception"

  interface Interface {
    type: Tag
  }

  export interface NoActiveUser extends Interface {
    type: "noActiveUser"
  }

  export interface NoConnectionUser extends Interface {
    type: "noConnectionUser"
    agentConnId: string
  }

  export interface NoSndFileUser extends Interface {
    type: "noSndFileUser"
    agentSndFileId: string
  }

  export interface NoRcvFileUser extends Interface {
    type: "noRcvFileUser"
    agentRcvFileId: string
  }

  export interface UserUnknown extends Interface {
    type: "userUnknown"
  }

  export interface ActiveUserExists extends Interface {
    type: "activeUserExists"
  }

  export interface UserExists extends Interface {
    type: "userExists"
    contactName: string
  }

  export interface ChatRelayExists extends Interface {
    type: "chatRelayExists"
  }

  export interface DifferentActiveUser extends Interface {
    type: "differentActiveUser"
    commandUserId: number // int64
    activeUserId: number // int64
  }

  export interface CantDeleteActiveUser extends Interface {
    type: "cantDeleteActiveUser"
    userId: number // int64
  }

  export interface CantDeleteLastUser extends Interface {
    type: "cantDeleteLastUser"
    userId: number // int64
  }

  export interface CantHideLastUser extends Interface {
    type: "cantHideLastUser"
    userId: number // int64
  }

  export interface HiddenUserAlwaysMuted extends Interface {
    type: "hiddenUserAlwaysMuted"
    userId: number // int64
  }

  export interface EmptyUserPassword extends Interface {
    type: "emptyUserPassword"
    userId: number // int64
  }

  export interface UserAlreadyHidden extends Interface {
    type: "userAlreadyHidden"
    userId: number // int64
  }

  export interface UserNotHidden extends Interface {
    type: "userNotHidden"
    userId: number // int64
  }

  export interface InvalidDisplayName extends Interface {
    type: "invalidDisplayName"
    displayName: string
    validName: string
  }

  export interface ChatNotStarted extends Interface {
    type: "chatNotStarted"
  }

  export interface ChatNotStopped extends Interface {
    type: "chatNotStopped"
  }

  export interface ChatStoreChanged extends Interface {
    type: "chatStoreChanged"
  }

  export interface InvalidConnReq extends Interface {
    type: "invalidConnReq"
  }

  export interface UnsupportedConnReq extends Interface {
    type: "unsupportedConnReq"
  }

  export interface ConnReqMessageProhibited extends Interface {
    type: "connReqMessageProhibited"
  }

  export interface ContactNotReady extends Interface {
    type: "contactNotReady"
    contact: Contact
  }

  export interface ContactNotActive extends Interface {
    type: "contactNotActive"
    contact: Contact
  }

  export interface ContactDisabled extends Interface {
    type: "contactDisabled"
    contact: Contact
  }

  export interface ConnectionDisabled extends Interface {
    type: "connectionDisabled"
    connection: Connection
  }

  export interface GroupUserRole extends Interface {
    type: "groupUserRole"
    groupInfo: GroupInfo
    requiredRole: GroupMemberRole
  }

  export interface GroupMemberInitialRole extends Interface {
    type: "groupMemberInitialRole"
    groupInfo: GroupInfo
    initialRole: GroupMemberRole
  }

  export interface ContactIncognitoCantInvite extends Interface {
    type: "contactIncognitoCantInvite"
  }

  export interface GroupIncognitoCantInvite extends Interface {
    type: "groupIncognitoCantInvite"
  }

  export interface GroupContactRole extends Interface {
    type: "groupContactRole"
    contactName: string
  }

  export interface GroupDuplicateMember extends Interface {
    type: "groupDuplicateMember"
    contactName: string
  }

  export interface GroupDuplicateMemberId extends Interface {
    type: "groupDuplicateMemberId"
  }

  export interface GroupNotJoined extends Interface {
    type: "groupNotJoined"
    groupInfo: GroupInfo
  }

  export interface GroupMemberNotActive extends Interface {
    type: "groupMemberNotActive"
  }

  export interface CantBlockMemberForSelf extends Interface {
    type: "cantBlockMemberForSelf"
    groupInfo: GroupInfo
    member: GroupMember
    setShowMessages: boolean
  }

  export interface GroupMemberUserRemoved extends Interface {
    type: "groupMemberUserRemoved"
  }

  export interface GroupMemberNotFound extends Interface {
    type: "groupMemberNotFound"
  }

  export interface GroupCantResendInvitation extends Interface {
    type: "groupCantResendInvitation"
    groupInfo: GroupInfo
    contactName: string
  }

  export interface GroupInternal extends Interface {
    type: "groupInternal"
    message: string
  }

  export interface FileNotFound extends Interface {
    type: "fileNotFound"
    message: string
  }

  export interface FileSize extends Interface {
    type: "fileSize"
    filePath: string
  }

  export interface FileAlreadyReceiving extends Interface {
    type: "fileAlreadyReceiving"
    message: string
  }

  export interface FileCancelled extends Interface {
    type: "fileCancelled"
    message: string
  }

  export interface FileCancel extends Interface {
    type: "fileCancel"
    fileId: number // int64
    message: string
  }

  export interface FileAlreadyExists extends Interface {
    type: "fileAlreadyExists"
    filePath: string
  }

  export interface FileWrite extends Interface {
    type: "fileWrite"
    filePath: string
    message: string
  }

  export interface FileSend extends Interface {
    type: "fileSend"
    fileId: number // int64
    agentError: AgentErrorType
  }

  export interface FileRcvChunk extends Interface {
    type: "fileRcvChunk"
    message: string
  }

  export interface FileInternal extends Interface {
    type: "fileInternal"
    message: string
  }

  export interface FileImageType extends Interface {
    type: "fileImageType"
    filePath: string
  }

  export interface FileImageSize extends Interface {
    type: "fileImageSize"
    filePath: string
  }

  export interface FileNotReceived extends Interface {
    type: "fileNotReceived"
    fileId: number // int64
  }

  export interface FileNotApproved extends Interface {
    type: "fileNotApproved"
    fileId: number // int64
    unknownServers: string[]
  }

  export interface FallbackToSMPProhibited extends Interface {
    type: "fallbackToSMPProhibited"
    fileId: number // int64
  }

  export interface InlineFileProhibited extends Interface {
    type: "inlineFileProhibited"
    fileId: number // int64
  }

  export interface InvalidForward extends Interface {
    type: "invalidForward"
  }

  export interface InvalidChatItemUpdate extends Interface {
    type: "invalidChatItemUpdate"
  }

  export interface InvalidChatItemDelete extends Interface {
    type: "invalidChatItemDelete"
  }

  export interface HasCurrentCall extends Interface {
    type: "hasCurrentCall"
  }

  export interface NoCurrentCall extends Interface {
    type: "noCurrentCall"
  }

  export interface CallContact extends Interface {
    type: "callContact"
    contactId: number // int64
  }

  export interface DirectMessagesProhibited extends Interface {
    type: "directMessagesProhibited"
    direction: MsgDirection
    contact: Contact
  }

  export interface AgentVersion extends Interface {
    type: "agentVersion"
  }

  export interface AgentNoSubResult extends Interface {
    type: "agentNoSubResult"
    agentConnId: string
  }

  export interface CommandError extends Interface {
    type: "commandError"
    message: string
  }

  export interface AgentCommandError extends Interface {
    type: "agentCommandError"
    message: string
  }

  export interface InvalidFileDescription extends Interface {
    type: "invalidFileDescription"
    message: string
  }

  export interface ConnectionIncognitoChangeProhibited extends Interface {
    type: "connectionIncognitoChangeProhibited"
  }

  export interface ConnectionUserChangeProhibited extends Interface {
    type: "connectionUserChangeProhibited"
  }

  export interface PeerChatVRangeIncompatible extends Interface {
    type: "peerChatVRangeIncompatible"
  }

  export interface InternalError extends Interface {
    type: "internalError"
    message: string
  }

  export interface Exception extends Interface {
    type: "exception"
    message: string
  }
}

export enum ChatFeature {
  TimedMessages = "timedMessages",
  FullDelete = "fullDelete",
  Reactions = "reactions",
  Voice = "voice",
  Files = "files",
  Calls = "calls",
  Sessions = "sessions",
}

export type ChatInfo = 
  | ChatInfo.Direct
  | ChatInfo.Group
  | ChatInfo.Local
  | ChatInfo.ContactRequest
  | ChatInfo.ContactConnection

export namespace ChatInfo {
  export type Tag = "direct" | "group" | "local" | "contactRequest" | "contactConnection"

  interface Interface {
    type: Tag
  }

  export interface Direct extends Interface {
    type: "direct"
    contact: Contact
  }

  export interface Group extends Interface {
    type: "group"
    groupInfo: GroupInfo
    groupChatScope?: GroupChatScopeInfo
  }

  export interface Local extends Interface {
    type: "local"
    noteFolder: NoteFolder
  }

  export interface ContactRequest extends Interface {
    type: "contactRequest"
    contactRequest: UserContactRequest
  }

  export interface ContactConnection extends Interface {
    type: "contactConnection"
    contactConnection: PendingContactConnection
  }
}

export interface ChatItem {
  chatDir: CIDirection
  meta: CIMeta
  content: CIContent
  mentions: {[key: string]: CIMention}
  formattedText?: FormattedText[]
  quotedItem?: CIQuote
  reactions: CIReactionCount[]
  file?: CIFile
}
// Message deletion result.

export interface ChatItemDeletion {
  deletedChatItem: AChatItem
  toChatItem?: AChatItem
}

export enum ChatPeerType {
  Human = "human",
  Bot = "bot",
}
// Used in API commands. Chat scope can only be passed with groups.

export interface ChatRef {
  chatType: ChatType
  chatId: number // int64
  chatScope?: GroupChatScope
}

export namespace ChatRef {
  export function cmdString(self: ChatRef): string {
    return ChatType.cmdString(self.chatType) + self.chatId + (self.chatScope ? GroupChatScope.cmdString(self.chatScope) : '')
  }
}

export interface ChatSettings {
  enableNtfs: MsgFilter
  sendRcpts?: boolean
  favorite: boolean
}

export interface ChatStats {
  unreadCount: number // int
  unreadMentions: number // int
  reportsCount: number // int
  minUnreadItemId: number // int64
  unreadChat: boolean
}

export enum ChatType {
  Direct = "direct",
  Group = "group",
  Local = "local",
}

export namespace ChatType {
  export function cmdString(self: ChatType): string {
    return self == 'direct' ? '@' : self == 'group' ? '#' : self == 'local' ? '*' : ''
  }
}

export interface ChatWallpaper {
  preset?: string
  imageFile?: string
  background?: string
  tint?: string
  scaleType?: ChatWallpaperScale
  scale?: number // double
}

export enum ChatWallpaperScale {
  Fill = "fill",
  Fit = "fit",
  Repeat = "repeat",
}

export interface ClientNotice {
  ttl?: number // int64
}

export enum Color {
  Black = "black",
  Red = "red",
  Green = "green",
  Yellow = "yellow",
  Blue = "blue",
  Magenta = "magenta",
  Cyan = "cyan",
  White = "white",
}

export type CommandError = 
  | CommandError.UNKNOWN
  | CommandError.SYNTAX
  | CommandError.PROHIBITED
  | CommandError.NO_AUTH
  | CommandError.HAS_AUTH
  | CommandError.NO_ENTITY

export namespace CommandError {
  export type Tag = "UNKNOWN" | "SYNTAX" | "PROHIBITED" | "NO_AUTH" | "HAS_AUTH" | "NO_ENTITY"

  interface Interface {
    type: Tag
  }

  export interface UNKNOWN extends Interface {
    type: "UNKNOWN"
  }

  export interface SYNTAX extends Interface {
    type: "SYNTAX"
  }

  export interface PROHIBITED extends Interface {
    type: "PROHIBITED"
  }

  export interface NO_AUTH extends Interface {
    type: "NO_AUTH"
  }

  export interface HAS_AUTH extends Interface {
    type: "HAS_AUTH"
  }

  export interface NO_ENTITY extends Interface {
    type: "NO_ENTITY"
  }
}

export type CommandErrorType = 
  | CommandErrorType.PROHIBITED
  | CommandErrorType.SYNTAX
  | CommandErrorType.NO_CONN
  | CommandErrorType.SIZE
  | CommandErrorType.LARGE

export namespace CommandErrorType {
  export type Tag = "PROHIBITED" | "SYNTAX" | "NO_CONN" | "SIZE" | "LARGE"

  interface Interface {
    type: Tag
  }

  export interface PROHIBITED extends Interface {
    type: "PROHIBITED"
  }

  export interface SYNTAX extends Interface {
    type: "SYNTAX"
  }

  export interface NO_CONN extends Interface {
    type: "NO_CONN"
  }

  export interface SIZE extends Interface {
    type: "SIZE"
  }

  export interface LARGE extends Interface {
    type: "LARGE"
  }
}

export interface ComposedMessage {
  fileSource?: CryptoFile
  quotedItemId?: number // int64
  msgContent: MsgContent
  mentions: {[key: string]: number} // string : int64
}

export enum ConnStatus {
  New = "new",
  Prepared = "prepared",
  Joined = "joined",
  Requested = "requested",
  Accepted = "accepted",
  Snd_ready = "snd-ready",
  Ready = "ready",
  Deleted = "deleted",
}

export enum ConnType {
  Contact = "contact",
  Member = "member",
  User_contact = "user_contact",
}

export interface Connection {
  connId: number // int64
  agentConnId: string
  connChatVersion: number // int
  peerChatVRange: VersionRange
  connLevel: number // int
  viaContact?: number // int64
  viaUserContactLink?: number // int64
  viaGroupLink: boolean
  groupLinkId?: string
  xContactId?: string
  customUserProfileId?: number // int64
  connType: ConnType
  connStatus: ConnStatus
  contactConnInitiated: boolean
  localAlias: string
  entityId?: number // int64
  connectionCode?: SecurityCode
  pqSupport: boolean
  pqEncryption: boolean
  pqSndEnabled?: boolean
  pqRcvEnabled?: boolean
  authErrCounter: number // int
  quotaErrCounter: number // int
  createdAt: string // ISO-8601 timestamp
}

export type ConnectionEntity = 
  | ConnectionEntity.RcvDirectMsgConnection
  | ConnectionEntity.RcvGroupMsgConnection
  | ConnectionEntity.UserContactConnection

export namespace ConnectionEntity {
  export type Tag = "rcvDirectMsgConnection" | "rcvGroupMsgConnection" | "userContactConnection"

  interface Interface {
    type: Tag
  }

  export interface RcvDirectMsgConnection extends Interface {
    type: "rcvDirectMsgConnection"
    entityConnection: Connection
    contact?: Contact
  }

  export interface RcvGroupMsgConnection extends Interface {
    type: "rcvGroupMsgConnection"
    entityConnection: Connection
    groupInfo: GroupInfo
    groupMember: GroupMember
  }

  export interface UserContactConnection extends Interface {
    type: "userContactConnection"
    entityConnection: Connection
    userContact: UserContact
  }
}

export type ConnectionErrorType = 
  | ConnectionErrorType.NOT_FOUND
  | ConnectionErrorType.DUPLICATE
  | ConnectionErrorType.SIMPLEX
  | ConnectionErrorType.NOT_ACCEPTED
  | ConnectionErrorType.NOT_AVAILABLE

export namespace ConnectionErrorType {
  export type Tag = "NOT_FOUND" | "DUPLICATE" | "SIMPLEX" | "NOT_ACCEPTED" | "NOT_AVAILABLE"

  interface Interface {
    type: Tag
  }

  export interface NOT_FOUND extends Interface {
    type: "NOT_FOUND"
  }

  export interface DUPLICATE extends Interface {
    type: "DUPLICATE"
  }

  export interface SIMPLEX extends Interface {
    type: "SIMPLEX"
  }

  export interface NOT_ACCEPTED extends Interface {
    type: "NOT_ACCEPTED"
  }

  export interface NOT_AVAILABLE extends Interface {
    type: "NOT_AVAILABLE"
  }
}

export enum ConnectionMode {
  INV = "inv",
  CON = "con",
}

export type ConnectionPlan = 
  | ConnectionPlan.InvitationLink
  | ConnectionPlan.ContactAddress
  | ConnectionPlan.GroupLink
  | ConnectionPlan.Error

export namespace ConnectionPlan {
  export type Tag = "invitationLink" | "contactAddress" | "groupLink" | "error"

  interface Interface {
    type: Tag
  }

  export interface InvitationLink extends Interface {
    type: "invitationLink"
    invitationLinkPlan: InvitationLinkPlan
  }

  export interface ContactAddress extends Interface {
    type: "contactAddress"
    contactAddressPlan: ContactAddressPlan
  }

  export interface GroupLink extends Interface {
    type: "groupLink"
    groupLinkPlan: GroupLinkPlan
  }

  export interface Error extends Interface {
    type: "error"
    chatError: ChatError
  }
}

export interface Contact {
  contactId: number // int64
  localDisplayName: string
  profile: LocalProfile
  activeConn?: Connection
  contactUsed: boolean
  contactStatus: ContactStatus
  chatSettings: ChatSettings
  userPreferences: Preferences
  mergedPreferences: ContactUserPreferences
  createdAt: string // ISO-8601 timestamp
  updatedAt: string // ISO-8601 timestamp
  chatTs?: string // ISO-8601 timestamp
  preparedContact?: PreparedContact
  contactRequestId?: number // int64
  contactGroupMemberId?: number // int64
  contactGrpInvSent: boolean
  groupDirectInv?: GroupDirectInvitation
  chatTags: number[] // int64
  chatItemTTL?: number // int64
  uiThemes?: UIThemeEntityOverrides
  chatDeleted: boolean
  customData?: object
}

export type ContactAddressPlan = 
  | ContactAddressPlan.Ok
  | ContactAddressPlan.OwnLink
  | ContactAddressPlan.ConnectingConfirmReconnect
  | ContactAddressPlan.ConnectingProhibit
  | ContactAddressPlan.Known
  | ContactAddressPlan.ContactViaAddress

export namespace ContactAddressPlan {
  export type Tag = 
    | "ok"
    | "ownLink"
    | "connectingConfirmReconnect"
    | "connectingProhibit"
    | "known"
    | "contactViaAddress"

  interface Interface {
    type: Tag
  }

  export interface Ok extends Interface {
    type: "ok"
    contactSLinkData_?: ContactShortLinkData
  }

  export interface OwnLink extends Interface {
    type: "ownLink"
  }

  export interface ConnectingConfirmReconnect extends Interface {
    type: "connectingConfirmReconnect"
  }

  export interface ConnectingProhibit extends Interface {
    type: "connectingProhibit"
    contact: Contact
  }

  export interface Known extends Interface {
    type: "known"
    contact: Contact
  }

  export interface ContactViaAddress extends Interface {
    type: "contactViaAddress"
    contact: Contact
  }
}

export interface ContactShortLinkData {
  profile: Profile
  message?: MsgContent
  business: boolean
}

export enum ContactStatus {
  Active = "active",
  Deleted = "deleted",
  DeletedByUser = "deletedByUser",
}

export type ContactUserPref = ContactUserPref.Contact | ContactUserPref.User

export namespace ContactUserPref {
  export type Tag = "contact" | "user"

  interface Interface {
    type: Tag
  }

  export interface Contact extends Interface {
    type: "contact"
    preference: SimplePreference
  }

  export interface User extends Interface {
    type: "user"
    preference: SimplePreference
  }
}

export interface ContactUserPreference {
  enabled: PrefEnabled
  userPreference: ContactUserPref
  contactPreference: SimplePreference
}

export interface ContactUserPreferences {
  timedMessages: ContactUserPreference
  fullDelete: ContactUserPreference
  reactions: ContactUserPreference
  voice: ContactUserPreference
  files: ContactUserPreference
  calls: ContactUserPreference
  sessions: ContactUserPreference
  commands?: ChatBotCommand[]
}

export interface CreatedConnLink {
  connFullLink: string
  connShortLink?: string
}

export namespace CreatedConnLink {
  export function cmdString(self: CreatedConnLink): string {
    return self.connFullLink + (self.connShortLink ? ' ' + self.connShortLink : '')
  }
}

export interface CryptoFile {
  filePath: string
  cryptoArgs?: CryptoFileArgs
}

export interface CryptoFileArgs {
  fileKey: string
  fileNonce: string
}

export interface E2EInfo {
  pqEnabled?: boolean
}

export type ErrorType = 
  | ErrorType.BLOCK
  | ErrorType.SESSION
  | ErrorType.CMD
  | ErrorType.PROXY
  | ErrorType.AUTH
  | ErrorType.BLOCKED
  | ErrorType.SERVICE
  | ErrorType.CRYPTO
  | ErrorType.QUOTA
  | ErrorType.STORE
  | ErrorType.NO_MSG
  | ErrorType.LARGE_MSG
  | ErrorType.EXPIRED
  | ErrorType.INTERNAL
  | ErrorType.DUPLICATE_

export namespace ErrorType {
  export type Tag = 
    | "BLOCK"
    | "SESSION"
    | "CMD"
    | "PROXY"
    | "AUTH"
    | "BLOCKED"
    | "SERVICE"
    | "CRYPTO"
    | "QUOTA"
    | "STORE"
    | "NO_MSG"
    | "LARGE_MSG"
    | "EXPIRED"
    | "INTERNAL"
    | "DUPLICATE_"

  interface Interface {
    type: Tag
  }

  export interface BLOCK extends Interface {
    type: "BLOCK"
  }

  export interface SESSION extends Interface {
    type: "SESSION"
  }

  export interface CMD extends Interface {
    type: "CMD"
    cmdErr: CommandError
  }

  export interface PROXY extends Interface {
    type: "PROXY"
    proxyErr: ProxyError
  }

  export interface AUTH extends Interface {
    type: "AUTH"
  }

  export interface BLOCKED extends Interface {
    type: "BLOCKED"
    blockInfo: BlockingInfo
  }

  export interface SERVICE extends Interface {
    type: "SERVICE"
  }

  export interface CRYPTO extends Interface {
    type: "CRYPTO"
  }

  export interface QUOTA extends Interface {
    type: "QUOTA"
  }

  export interface STORE extends Interface {
    type: "STORE"
    storeErr: string
  }

  export interface NO_MSG extends Interface {
    type: "NO_MSG"
  }

  export interface LARGE_MSG extends Interface {
    type: "LARGE_MSG"
  }

  export interface EXPIRED extends Interface {
    type: "EXPIRED"
  }

  export interface INTERNAL extends Interface {
    type: "INTERNAL"
  }

  export interface DUPLICATE_ extends Interface {
    type: "DUPLICATE_"
  }
}

export enum FeatureAllowed {
  Always = "always",
  Yes = "yes",
  No = "no",
}

export interface FileDescr {
  fileDescrText: string
  fileDescrPartNo: number // int
  fileDescrComplete: boolean
}

export type FileError = 
  | FileError.Auth
  | FileError.Blocked
  | FileError.NoFile
  | FileError.Relay
  | FileError.Other

export namespace FileError {
  export type Tag = "auth" | "blocked" | "noFile" | "relay" | "other"

  interface Interface {
    type: Tag
  }

  export interface Auth extends Interface {
    type: "auth"
  }

  export interface Blocked extends Interface {
    type: "blocked"
    server: string
    blockInfo: BlockingInfo
  }

  export interface NoFile extends Interface {
    type: "noFile"
  }

  export interface Relay extends Interface {
    type: "relay"
    srvError: SrvError
  }

  export interface Other extends Interface {
    type: "other"
    fileError: string
  }
}

export type FileErrorType = 
  | FileErrorType.NOT_APPROVED
  | FileErrorType.SIZE
  | FileErrorType.REDIRECT
  | FileErrorType.FILE_IO
  | FileErrorType.NO_FILE

export namespace FileErrorType {
  export type Tag = "NOT_APPROVED" | "SIZE" | "REDIRECT" | "FILE_IO" | "NO_FILE"

  interface Interface {
    type: Tag
  }

  export interface NOT_APPROVED extends Interface {
    type: "NOT_APPROVED"
  }

  export interface SIZE extends Interface {
    type: "SIZE"
  }

  export interface REDIRECT extends Interface {
    type: "REDIRECT"
    redirectError: string
  }

  export interface FILE_IO extends Interface {
    type: "FILE_IO"
    fileIOError: string
  }

  export interface NO_FILE extends Interface {
    type: "NO_FILE"
  }
}

export interface FileInvitation {
  fileName: string
  fileSize: number // int64
  fileDigest?: string
  fileConnReq?: string
  fileInline?: InlineFileMode
  fileDescr?: FileDescr
}

export enum FileProtocol {
  SMP = "smp",
  XFTP = "xftp",
  LOCAL = "local",
}

export enum FileStatus {
  New = "new",
  Accepted = "accepted",
  Connected = "connected",
  Complete = "complete",
  Cancelled = "cancelled",
}

export interface FileTransferMeta {
  fileId: number // int64
  xftpSndFile?: XFTPSndFile
  xftpRedirectFor?: number // int64
  fileName: string
  filePath: string
  fileSize: number // int64
  fileInline?: InlineFileMode
  chunkSize: number // int64
  cancelled: boolean
}

export type Format = 
  | Format.Bold
  | Format.Italic
  | Format.StrikeThrough
  | Format.Snippet
  | Format.Secret
  | Format.Colored
  | Format.Uri
  | Format.HyperLink
  | Format.SimplexLink
  | Format.Command
  | Format.Mention
  | Format.Email
  | Format.Phone

export namespace Format {
  export type Tag = 
    | "bold"
    | "italic"
    | "strikeThrough"
    | "snippet"
    | "secret"
    | "colored"
    | "uri"
    | "hyperLink"
    | "simplexLink"
    | "command"
    | "mention"
    | "email"
    | "phone"

  interface Interface {
    type: Tag
  }

  export interface Bold extends Interface {
    type: "bold"
  }

  export interface Italic extends Interface {
    type: "italic"
  }

  export interface StrikeThrough extends Interface {
    type: "strikeThrough"
  }

  export interface Snippet extends Interface {
    type: "snippet"
  }

  export interface Secret extends Interface {
    type: "secret"
  }

  export interface Colored extends Interface {
    type: "colored"
    color: Color
  }

  export interface Uri extends Interface {
    type: "uri"
  }

  export interface HyperLink extends Interface {
    type: "hyperLink"
    showText?: string
    linkUri: string
  }

  export interface SimplexLink extends Interface {
    type: "simplexLink"
    showText?: string
    linkType: SimplexLinkType
    simplexUri: string
    smpHosts: string[] // non-empty
  }

  export interface Command extends Interface {
    type: "command"
    commandStr: string
  }

  export interface Mention extends Interface {
    type: "mention"
    memberName: string
  }

  export interface Email extends Interface {
    type: "email"
  }

  export interface Phone extends Interface {
    type: "phone"
  }
}

export interface FormattedText {
  format?: Format
  text: string
}

export interface FullGroupPreferences {
  timedMessages: TimedMessagesGroupPreference
  directMessages: RoleGroupPreference
  fullDelete: GroupPreference
  reactions: GroupPreference
  voice: RoleGroupPreference
  files: RoleGroupPreference
  simplexLinks: RoleGroupPreference
  reports: GroupPreference
  history: GroupPreference
  sessions: RoleGroupPreference
  commands: ChatBotCommand[]
}

export interface FullPreferences {
  timedMessages: TimedMessagesPreference
  fullDelete: SimplePreference
  reactions: SimplePreference
  voice: SimplePreference
  files: SimplePreference
  calls: SimplePreference
  sessions: SimplePreference
  commands: ChatBotCommand[]
}

export interface Group {
  groupInfo: GroupInfo
  members: GroupMember[]
}

export type GroupChatScope = GroupChatScope.MemberSupport

export namespace GroupChatScope {
  export type Tag = "memberSupport"

  interface Interface {
    type: Tag
  }

  export interface MemberSupport extends Interface {
    type: "memberSupport"
    groupMemberId_?: number // int64
  }

  export function cmdString(self: GroupChatScope): string {
    return '(_support' + (self.groupMemberId_ ? ':' + self.groupMemberId_ : '') + ')'
  }
}

export type GroupChatScopeInfo = GroupChatScopeInfo.MemberSupport

export namespace GroupChatScopeInfo {
  export type Tag = "memberSupport"

  interface Interface {
    type: Tag
  }

  export interface MemberSupport extends Interface {
    type: "memberSupport"
    groupMember_?: GroupMember
  }
}

export interface GroupDirectInvitation {
  groupDirectInvLink: string
  fromGroupId_?: number // int64
  fromGroupMemberId_?: number // int64
  fromGroupMemberConnId_?: number // int64
  groupDirectInvStartedConnection: boolean
}

export enum GroupFeature {
  TimedMessages = "timedMessages",
  DirectMessages = "directMessages",
  FullDelete = "fullDelete",
  Reactions = "reactions",
  Voice = "voice",
  Files = "files",
  SimplexLinks = "simplexLinks",
  Reports = "reports",
  History = "history",
  Sessions = "sessions",
}

export enum GroupFeatureEnabled {
  On = "on",
  Off = "off",
}

export interface GroupInfo {
  groupId: number // int64
  useRelays: boolean
  relayOwnStatus?: RelayStatus
  localDisplayName: string
  groupProfile: GroupProfile
  localAlias: string
  businessChat?: BusinessChatInfo
  fullGroupPreferences: FullGroupPreferences
  membership: GroupMember
  chatSettings: ChatSettings
  createdAt: string // ISO-8601 timestamp
  updatedAt: string // ISO-8601 timestamp
  chatTs?: string // ISO-8601 timestamp
  userMemberProfileSentAt?: string // ISO-8601 timestamp
  preparedGroup?: PreparedGroup
  chatTags: number[] // int64
  chatItemTTL?: number // int64
  uiThemes?: UIThemeEntityOverrides
  customData?: object
  groupSummary: GroupSummary
  membersRequireAttention: number // int
  viaGroupLinkUri?: string
  groupKeys?: GroupKeys
}

export interface GroupKeys {
  sharedGroupId: string
  groupRootKey: GroupRootKey
  memberPrivKey: string
}

export interface GroupLink {
  userContactLinkId: number // int64
  connLinkContact: CreatedConnLink
  shortLinkDataSet: boolean
  shortLinkLargeDataSet: boolean
  groupLinkId: string
  acceptMemberRole: GroupMemberRole
}

export type GroupLinkPlan = 
  | GroupLinkPlan.Ok
  | GroupLinkPlan.OwnLink
  | GroupLinkPlan.ConnectingConfirmReconnect
  | GroupLinkPlan.ConnectingProhibit
  | GroupLinkPlan.Known

export namespace GroupLinkPlan {
  export type Tag = "ok" | "ownLink" | "connectingConfirmReconnect" | "connectingProhibit" | "known"

  interface Interface {
    type: Tag
  }

  export interface Ok extends Interface {
    type: "ok"
    groupSLinkInfo_?: GroupShortLinkInfo
    groupSLinkData_?: GroupShortLinkData
  }

  export interface OwnLink extends Interface {
    type: "ownLink"
    groupInfo: GroupInfo
  }

  export interface ConnectingConfirmReconnect extends Interface {
    type: "connectingConfirmReconnect"
  }

  export interface ConnectingProhibit extends Interface {
    type: "connectingProhibit"
    groupInfo_?: GroupInfo
  }

  export interface Known extends Interface {
    type: "known"
    groupInfo: GroupInfo
  }
}

export interface GroupMember {
  groupMemberId: number // int64
  groupId: number // int64
  indexInGroup: number // int64
  memberId: string
  memberRole: GroupMemberRole
  memberCategory: GroupMemberCategory
  memberStatus: GroupMemberStatus
  memberSettings: GroupMemberSettings
  blockedByAdmin: boolean
  invitedBy: InvitedBy
  invitedByGroupMemberId?: number // int64
  localDisplayName: string
  memberProfile: LocalProfile
  memberContactId?: number // int64
  memberContactProfileId: number // int64
  activeConn?: Connection
  memberChatVRange: VersionRange
  createdAt: string // ISO-8601 timestamp
  updatedAt: string // ISO-8601 timestamp
  supportChat?: GroupSupportChat
  memberPubKey?: string
}

export interface GroupMemberAdmission {
  review?: MemberCriteria
}

export enum GroupMemberCategory {
  User = "user",
  Invitee = "invitee",
  Host = "host",
  Pre = "pre",
  Post = "post",
}

export interface GroupMemberRef {
  groupMemberId: number // int64
  profile: Profile
}

export enum GroupMemberRole {
  Relay = "relay",
  Observer = "observer",
  Author = "author",
  Member = "member",
  Moderator = "moderator",
  Admin = "admin",
  Owner = "owner",
}

export interface GroupMemberSettings {
  showMessages: boolean
}

export enum GroupMemberStatus {
  Rejected = "rejected",
  Removed = "removed",
  Left = "left",
  Deleted = "deleted",
  Unknown = "unknown",
  Invited = "invited",
  Pending_approval = "pending_approval",
  Pending_review = "pending_review",
  Introduced = "introduced",
  Intro_inv = "intro-inv",
  Accepted = "accepted",
  Announced = "announced",
  Connected = "connected",
  Complete = "complete",
  Creator = "creator",
}

export interface GroupPreference {
  enable: GroupFeatureEnabled
}

export interface GroupPreferences {
  timedMessages?: TimedMessagesGroupPreference
  directMessages?: RoleGroupPreference
  fullDelete?: GroupPreference
  reactions?: GroupPreference
  voice?: RoleGroupPreference
  files?: RoleGroupPreference
  simplexLinks?: RoleGroupPreference
  reports?: GroupPreference
  history?: GroupPreference
  sessions?: RoleGroupPreference
  commands?: ChatBotCommand[]
}

export interface GroupProfile {
  displayName: string
  fullName: string
  shortDescr?: string
  description?: string
  image?: string
  groupLink?: string
  groupPreferences?: GroupPreferences
  memberAdmission?: GroupMemberAdmission
}

export interface GroupRelay {
  groupRelayId: number // int64
  groupMemberId: number // int64
  userChatRelayId: number // int64
  relayStatus: RelayStatus
  relayLink?: string
}

export type GroupRootKey = GroupRootKey.Private | GroupRootKey.Public

export namespace GroupRootKey {
  export type Tag = "private" | "public"

  interface Interface {
    type: Tag
  }

  export interface Private extends Interface {
    type: "private"
    rootPrivKey: string
  }

  export interface Public extends Interface {
    type: "public"
    rootPubKey: string
  }
}

export interface GroupShortLinkData {
  groupProfile: GroupProfile
}

export interface GroupShortLinkInfo {
  direct: boolean
  groupRelays: string[]
  sharedGroupId?: string
}

export interface GroupSummary {
  currentMembers: number // int64
}

export interface GroupSupportChat {
  chatTs: string // ISO-8601 timestamp
  unread: number // int64
  memberAttention: number // int64
  mentions: number // int64
  lastMsgFromMemberTs?: string // ISO-8601 timestamp
}

export enum HandshakeError {
  PARSE = "PARSE",
  IDENTITY = "IDENTITY",
  BAD_AUTH = "BAD_AUTH",
  BAD_SERVICE = "BAD_SERVICE",
}

export enum InlineFileMode {
  Offer = "offer",
  Sent = "sent",
}

export type InvitationLinkPlan = 
  | InvitationLinkPlan.Ok
  | InvitationLinkPlan.OwnLink
  | InvitationLinkPlan.Connecting
  | InvitationLinkPlan.Known

export namespace InvitationLinkPlan {
  export type Tag = "ok" | "ownLink" | "connecting" | "known"

  interface Interface {
    type: Tag
  }

  export interface Ok extends Interface {
    type: "ok"
    contactSLinkData_?: ContactShortLinkData
  }

  export interface OwnLink extends Interface {
    type: "ownLink"
  }

  export interface Connecting extends Interface {
    type: "connecting"
    contact_?: Contact
  }

  export interface Known extends Interface {
    type: "known"
    contact: Contact
  }
}

export type InvitedBy = InvitedBy.Contact | InvitedBy.User | InvitedBy.Unknown

export namespace InvitedBy {
  export type Tag = "contact" | "user" | "unknown"

  interface Interface {
    type: Tag
  }

  export interface Contact extends Interface {
    type: "contact"
    byContactId: number // int64
  }

  export interface User extends Interface {
    type: "user"
  }

  export interface Unknown extends Interface {
    type: "unknown"
  }
}

export type LinkContent = LinkContent.Page | LinkContent.Image | LinkContent.Video | LinkContent.Unknown

export namespace LinkContent {
  export type Tag = "page" | "image" | "video" | "unknown"

  interface Interface {
    type: Tag
  }

  export interface Page extends Interface {
    type: "page"
  }

  export interface Image extends Interface {
    type: "image"
  }

  export interface Video extends Interface {
    type: "video"
    duration?: number // int
  }

  export interface Unknown extends Interface {
    type: "unknown"
    tag: string
    json: object
  }
}

export interface LinkPreview {
  uri: string
  title: string
  description: string
  image: string
  content?: LinkContent
}

export interface LocalProfile {
  profileId: number // int64
  displayName: string
  fullName: string
  shortDescr?: string
  image?: string
  contactLink?: string
  preferences?: Preferences
  peerType?: ChatPeerType
  localAlias: string
}

export enum MemberCriteria {
  All = "all",
}
// Connection link sent in a message - only short links are allowed.

export type MsgChatLink = MsgChatLink.Contact | MsgChatLink.Invitation | MsgChatLink.Group

export namespace MsgChatLink {
  export type Tag = "contact" | "invitation" | "group"

  interface Interface {
    type: Tag
  }

  export interface Contact extends Interface {
    type: "contact"
    connLink: string
    profile: Profile
    business: boolean
  }

  export interface Invitation extends Interface {
    type: "invitation"
    invLink: string
    profile: Profile
  }

  export interface Group extends Interface {
    type: "group"
    connLink: string
    groupProfile: GroupProfile
  }
}

export type MsgContent = 
  | MsgContent.Text
  | MsgContent.Link
  | MsgContent.Image
  | MsgContent.Video
  | MsgContent.Voice
  | MsgContent.File
  | MsgContent.Report
  | MsgContent.Chat
  | MsgContent.Unknown

export namespace MsgContent {
  export type Tag = "text" | "link" | "image" | "video" | "voice" | "file" | "report" | "chat" | "unknown"

  interface Interface {
    type: Tag
  }

  export interface Text extends Interface {
    type: "text"
    text: string
  }

  export interface Link extends Interface {
    type: "link"
    text: string
    preview: LinkPreview
  }

  export interface Image extends Interface {
    type: "image"
    text: string
    image: string
  }

  export interface Video extends Interface {
    type: "video"
    text: string
    image: string
    duration: number // int
  }

  export interface Voice extends Interface {
    type: "voice"
    text: string
    duration: number // int
  }

  export interface File extends Interface {
    type: "file"
    text: string
  }

  export interface Report extends Interface {
    type: "report"
    text: string
    reason: ReportReason
  }

  export interface Chat extends Interface {
    type: "chat"
    text: string
    chatLink: MsgChatLink
  }

  export interface Unknown extends Interface {
    type: "unknown"
    tag: string
    text: string
    json: object
  }
}

export enum MsgDecryptError {
  RatchetHeader = "ratchetHeader",
  TooManySkipped = "tooManySkipped",
  RatchetEarlier = "ratchetEarlier",
  Other = "other",
  RatchetSync = "ratchetSync",
}

export enum MsgDirection {
  Rcv = "rcv",
  Snd = "snd",
}

export type MsgErrorType = 
  | MsgErrorType.MsgSkipped
  | MsgErrorType.MsgBadId
  | MsgErrorType.MsgBadHash
  | MsgErrorType.MsgDuplicate

export namespace MsgErrorType {
  export type Tag = "msgSkipped" | "msgBadId" | "msgBadHash" | "msgDuplicate"

  interface Interface {
    type: Tag
  }

  export interface MsgSkipped extends Interface {
    type: "msgSkipped"
    fromMsgId: number // int64
    toMsgId: number // int64
  }

  export interface MsgBadId extends Interface {
    type: "msgBadId"
    msgId: number // int64
  }

  export interface MsgBadHash extends Interface {
    type: "msgBadHash"
  }

  export interface MsgDuplicate extends Interface {
    type: "msgDuplicate"
  }
}

export enum MsgFilter {
  None = "none",
  All = "all",
  Mentions = "mentions",
}

export type MsgReaction = MsgReaction.Emoji | MsgReaction.Unknown

export namespace MsgReaction {
  export type Tag = "emoji" | "unknown"

  interface Interface {
    type: Tag
  }

  export interface Emoji extends Interface {
    type: "emoji"
    emoji: string
  }

  export interface Unknown extends Interface {
    type: "unknown"
    tag: string
    json: object
  }
}

export enum MsgReceiptStatus {
  Ok = "ok",
  BadMsgHash = "badMsgHash",
}

export type NetworkError = 
  | NetworkError.ConnectError
  | NetworkError.TLSError
  | NetworkError.UnknownCAError
  | NetworkError.FailedError
  | NetworkError.TimeoutError
  | NetworkError.SubscribeError

export namespace NetworkError {
  export type Tag = 
    | "connectError"
    | "tLSError"
    | "unknownCAError"
    | "failedError"
    | "timeoutError"
    | "subscribeError"

  interface Interface {
    type: Tag
  }

  export interface ConnectError extends Interface {
    type: "connectError"
    connectError: string
  }

  export interface TLSError extends Interface {
    type: "tLSError"
    tlsError: string
  }

  export interface UnknownCAError extends Interface {
    type: "unknownCAError"
  }

  export interface FailedError extends Interface {
    type: "failedError"
  }

  export interface TimeoutError extends Interface {
    type: "timeoutError"
  }

  export interface SubscribeError extends Interface {
    type: "subscribeError"
    subscribeError: string
  }
}

export interface NewUser {
  profile?: Profile
  pastTimestamp: boolean
  userChatRelay: boolean
}

export interface NoteFolder {
  noteFolderId: number // int64
  userId: number // int64
  createdAt: string // ISO-8601 timestamp
  updatedAt: string // ISO-8601 timestamp
  chatTs: string // ISO-8601 timestamp
  favorite: boolean
  unread: boolean
}

export interface PendingContactConnection {
  pccConnId: number // int64
  pccAgentConnId: string
  pccConnStatus: ConnStatus
  viaContactUri: boolean
  viaUserContactLink?: number // int64
  groupLinkId?: string
  customUserProfileId?: number // int64
  connLinkInv?: CreatedConnLink
  localAlias: string
  createdAt: string // ISO-8601 timestamp
  updatedAt: string // ISO-8601 timestamp
}

export interface PrefEnabled {
  forUser: boolean
  forContact: boolean
}

export interface Preferences {
  timedMessages?: TimedMessagesPreference
  fullDelete?: SimplePreference
  reactions?: SimplePreference
  voice?: SimplePreference
  files?: SimplePreference
  calls?: SimplePreference
  sessions?: SimplePreference
  commands?: ChatBotCommand[]
}

export interface PreparedContact {
  connLinkToConnect: CreatedConnLink
  uiConnLinkType: ConnectionMode
  welcomeSharedMsgId?: string
  requestSharedMsgId?: string
}

export interface PreparedGroup {
  connLinkToConnect: CreatedConnLink
  connLinkPreparedConnection: boolean
  connLinkStartedConnection: boolean
  welcomeSharedMsgId?: string
  requestSharedMsgId?: string
}

export interface Profile {
  displayName: string
  fullName: string
  shortDescr?: string
  image?: string
  contactLink?: string
  preferences?: Preferences
  peerType?: ChatPeerType
}

export type ProxyClientError = 
  | ProxyClientError.ProtocolError
  | ProxyClientError.UnexpectedResponse
  | ProxyClientError.ResponseError

export namespace ProxyClientError {
  export type Tag = "protocolError" | "unexpectedResponse" | "responseError"

  interface Interface {
    type: Tag
  }

  export interface ProtocolError extends Interface {
    type: "protocolError"
    protocolErr: ErrorType
  }

  export interface UnexpectedResponse extends Interface {
    type: "unexpectedResponse"
    responseStr: string
  }

  export interface ResponseError extends Interface {
    type: "responseError"
    responseErr: ErrorType
  }
}

export type ProxyError = ProxyError.PROTOCOL | ProxyError.BROKER | ProxyError.BASIC_AUTH | ProxyError.NO_SESSION

export namespace ProxyError {
  export type Tag = "PROTOCOL" | "BROKER" | "BASIC_AUTH" | "NO_SESSION"

  interface Interface {
    type: Tag
  }

  export interface PROTOCOL extends Interface {
    type: "PROTOCOL"
    protocolErr: ErrorType
  }

  export interface BROKER extends Interface {
    type: "BROKER"
    brokerErr: BrokerErrorType
  }

  export interface BASIC_AUTH extends Interface {
    type: "BASIC_AUTH"
  }

  export interface NO_SESSION extends Interface {
    type: "NO_SESSION"
  }
}

export type RCErrorType = 
  | RCErrorType.Internal
  | RCErrorType.Identity
  | RCErrorType.NoLocalAddress
  | RCErrorType.NewController
  | RCErrorType.NotDiscovered
  | RCErrorType.TLSStartFailed
  | RCErrorType.Exception
  | RCErrorType.CtrlAuth
  | RCErrorType.CtrlNotFound
  | RCErrorType.CtrlError
  | RCErrorType.Invitation
  | RCErrorType.Version
  | RCErrorType.Encrypt
  | RCErrorType.Decrypt
  | RCErrorType.BlockSize
  | RCErrorType.Syntax

export namespace RCErrorType {
  export type Tag = 
    | "internal"
    | "identity"
    | "noLocalAddress"
    | "newController"
    | "notDiscovered"
    | "tLSStartFailed"
    | "exception"
    | "ctrlAuth"
    | "ctrlNotFound"
    | "ctrlError"
    | "invitation"
    | "version"
    | "encrypt"
    | "decrypt"
    | "blockSize"
    | "syntax"

  interface Interface {
    type: Tag
  }

  export interface Internal extends Interface {
    type: "internal"
    internalErr: string
  }

  export interface Identity extends Interface {
    type: "identity"
  }

  export interface NoLocalAddress extends Interface {
    type: "noLocalAddress"
  }

  export interface NewController extends Interface {
    type: "newController"
  }

  export interface NotDiscovered extends Interface {
    type: "notDiscovered"
  }

  export interface TLSStartFailed extends Interface {
    type: "tLSStartFailed"
  }

  export interface Exception extends Interface {
    type: "exception"
    exception: string
  }

  export interface CtrlAuth extends Interface {
    type: "ctrlAuth"
  }

  export interface CtrlNotFound extends Interface {
    type: "ctrlNotFound"
  }

  export interface CtrlError extends Interface {
    type: "ctrlError"
    ctrlErr: string
  }

  export interface Invitation extends Interface {
    type: "invitation"
  }

  export interface Version extends Interface {
    type: "version"
  }

  export interface Encrypt extends Interface {
    type: "encrypt"
  }

  export interface Decrypt extends Interface {
    type: "decrypt"
  }

  export interface BlockSize extends Interface {
    type: "blockSize"
  }

  export interface Syntax extends Interface {
    type: "syntax"
    syntaxErr: string
  }
}

export enum RatchetSyncState {
  Ok = "ok",
  Allowed = "allowed",
  Required = "required",
  Started = "started",
  Agreed = "agreed",
}

export type RcvConnEvent = 
  | RcvConnEvent.SwitchQueue
  | RcvConnEvent.RatchetSync
  | RcvConnEvent.VerificationCodeReset
  | RcvConnEvent.PqEnabled

export namespace RcvConnEvent {
  export type Tag = "switchQueue" | "ratchetSync" | "verificationCodeReset" | "pqEnabled"

  interface Interface {
    type: Tag
  }

  export interface SwitchQueue extends Interface {
    type: "switchQueue"
    phase: SwitchPhase
  }

  export interface RatchetSync extends Interface {
    type: "ratchetSync"
    syncStatus: RatchetSyncState
  }

  export interface VerificationCodeReset extends Interface {
    type: "verificationCodeReset"
  }

  export interface PqEnabled extends Interface {
    type: "pqEnabled"
    enabled: boolean
  }
}

export type RcvDirectEvent = 
  | RcvDirectEvent.ContactDeleted
  | RcvDirectEvent.ProfileUpdated
  | RcvDirectEvent.GroupInvLinkReceived

export namespace RcvDirectEvent {
  export type Tag = "contactDeleted" | "profileUpdated" | "groupInvLinkReceived"

  interface Interface {
    type: Tag
  }

  export interface ContactDeleted extends Interface {
    type: "contactDeleted"
  }

  export interface ProfileUpdated extends Interface {
    type: "profileUpdated"
    fromProfile: Profile
    toProfile: Profile
  }

  export interface GroupInvLinkReceived extends Interface {
    type: "groupInvLinkReceived"
    groupProfile: GroupProfile
  }
}

export interface RcvFileDescr {
  fileDescrId: number // int64
  fileDescrText: string
  fileDescrPartNo: number // int
  fileDescrComplete: boolean
}

export type RcvFileStatus = 
  | RcvFileStatus.New
  | RcvFileStatus.Accepted
  | RcvFileStatus.Connected
  | RcvFileStatus.Complete
  | RcvFileStatus.Cancelled

export namespace RcvFileStatus {
  export type Tag = "new" | "accepted" | "connected" | "complete" | "cancelled"

  interface Interface {
    type: Tag
  }

  export interface New extends Interface {
    type: "new"
  }

  export interface Accepted extends Interface {
    type: "accepted"
    filePath: string
  }

  export interface Connected extends Interface {
    type: "connected"
    filePath: string
  }

  export interface Complete extends Interface {
    type: "complete"
    filePath: string
  }

  export interface Cancelled extends Interface {
    type: "cancelled"
    filePath_?: string
  }
}

export interface RcvFileTransfer {
  fileId: number // int64
  xftpRcvFile?: XFTPRcvFile
  fileInvitation: FileInvitation
  fileStatus: RcvFileStatus
  rcvFileInline?: InlineFileMode
  senderDisplayName: string
  chunkSize: number // int64
  cancelled: boolean
  grpMemberId?: number // int64
  cryptoArgs?: CryptoFileArgs
}

export type RcvGroupEvent = 
  | RcvGroupEvent.MemberAdded
  | RcvGroupEvent.MemberConnected
  | RcvGroupEvent.MemberAccepted
  | RcvGroupEvent.UserAccepted
  | RcvGroupEvent.MemberLeft
  | RcvGroupEvent.MemberRole
  | RcvGroupEvent.MemberBlocked
  | RcvGroupEvent.UserRole
  | RcvGroupEvent.MemberDeleted
  | RcvGroupEvent.UserDeleted
  | RcvGroupEvent.GroupDeleted
  | RcvGroupEvent.GroupUpdated
  | RcvGroupEvent.InvitedViaGroupLink
  | RcvGroupEvent.MemberCreatedContact
  | RcvGroupEvent.MemberProfileUpdated
  | RcvGroupEvent.NewMemberPendingReview

export namespace RcvGroupEvent {
  export type Tag = 
    | "memberAdded"
    | "memberConnected"
    | "memberAccepted"
    | "userAccepted"
    | "memberLeft"
    | "memberRole"
    | "memberBlocked"
    | "userRole"
    | "memberDeleted"
    | "userDeleted"
    | "groupDeleted"
    | "groupUpdated"
    | "invitedViaGroupLink"
    | "memberCreatedContact"
    | "memberProfileUpdated"
    | "newMemberPendingReview"

  interface Interface {
    type: Tag
  }

  export interface MemberAdded extends Interface {
    type: "memberAdded"
    groupMemberId: number // int64
    profile: Profile
  }

  export interface MemberConnected extends Interface {
    type: "memberConnected"
  }

  export interface MemberAccepted extends Interface {
    type: "memberAccepted"
    groupMemberId: number // int64
    profile: Profile
  }

  export interface UserAccepted extends Interface {
    type: "userAccepted"
  }

  export interface MemberLeft extends Interface {
    type: "memberLeft"
  }

  export interface MemberRole extends Interface {
    type: "memberRole"
    groupMemberId: number // int64
    profile: Profile
    role: GroupMemberRole
  }

  export interface MemberBlocked extends Interface {
    type: "memberBlocked"
    groupMemberId: number // int64
    profile: Profile
    blocked: boolean
  }

  export interface UserRole extends Interface {
    type: "userRole"
    role: GroupMemberRole
  }

  export interface MemberDeleted extends Interface {
    type: "memberDeleted"
    groupMemberId: number // int64
    profile: Profile
  }

  export interface UserDeleted extends Interface {
    type: "userDeleted"
  }

  export interface GroupDeleted extends Interface {
    type: "groupDeleted"
  }

  export interface GroupUpdated extends Interface {
    type: "groupUpdated"
    groupProfile: GroupProfile
  }

  export interface InvitedViaGroupLink extends Interface {
    type: "invitedViaGroupLink"
  }

  export interface MemberCreatedContact extends Interface {
    type: "memberCreatedContact"
  }

  export interface MemberProfileUpdated extends Interface {
    type: "memberProfileUpdated"
    fromProfile: Profile
    toProfile: Profile
  }

  export interface NewMemberPendingReview extends Interface {
    type: "newMemberPendingReview"
  }
}

export enum RelayStatus {
  New = "new",
  Invited = "invited",
  Accepted = "accepted",
  Active = "active",
}

export enum ReportReason {
  Spam = "spam",
  Content = "content",
  Community = "community",
  Profile = "profile",
  Other = "other",
}

export interface RoleGroupPreference {
  enable: GroupFeatureEnabled
  role?: GroupMemberRole
}

export type SMPAgentError = 
  | SMPAgentError.A_MESSAGE
  | SMPAgentError.A_PROHIBITED
  | SMPAgentError.A_VERSION
  | SMPAgentError.A_LINK
  | SMPAgentError.A_CRYPTO
  | SMPAgentError.A_DUPLICATE
  | SMPAgentError.A_QUEUE

export namespace SMPAgentError {
  export type Tag = 
    | "A_MESSAGE"
    | "A_PROHIBITED"
    | "A_VERSION"
    | "A_LINK"
    | "A_CRYPTO"
    | "A_DUPLICATE"
    | "A_QUEUE"

  interface Interface {
    type: Tag
  }

  export interface A_MESSAGE extends Interface {
    type: "A_MESSAGE"
  }

  export interface A_PROHIBITED extends Interface {
    type: "A_PROHIBITED"
    prohibitedErr: string
  }

  export interface A_VERSION extends Interface {
    type: "A_VERSION"
  }

  export interface A_LINK extends Interface {
    type: "A_LINK"
    linkErr: string
  }

  export interface A_CRYPTO extends Interface {
    type: "A_CRYPTO"
    cryptoErr: AgentCryptoError
  }

  export interface A_DUPLICATE extends Interface {
    type: "A_DUPLICATE"
  }

  export interface A_QUEUE extends Interface {
    type: "A_QUEUE"
    queueErr: string
  }
}

export interface SecurityCode {
  securityCode: string
  verifiedAt: string // ISO-8601 timestamp
}

export interface SimplePreference {
  allow: FeatureAllowed
}

export enum SimplexLinkType {
  Contact = "contact",
  Invitation = "invitation",
  Group = "group",
  Channel = "channel",
  Relay = "relay",
}

export enum SndCIStatusProgress {
  Partial = "partial",
  Complete = "complete",
}

export type SndConnEvent = SndConnEvent.SwitchQueue | SndConnEvent.RatchetSync | SndConnEvent.PqEnabled

export namespace SndConnEvent {
  export type Tag = "switchQueue" | "ratchetSync" | "pqEnabled"

  interface Interface {
    type: Tag
  }

  export interface SwitchQueue extends Interface {
    type: "switchQueue"
    phase: SwitchPhase
    member?: GroupMemberRef
  }

  export interface RatchetSync extends Interface {
    type: "ratchetSync"
    syncStatus: RatchetSyncState
    member?: GroupMemberRef
  }

  export interface PqEnabled extends Interface {
    type: "pqEnabled"
    enabled: boolean
  }
}

export type SndError = 
  | SndError.Auth
  | SndError.Quota
  | SndError.Expired
  | SndError.Relay
  | SndError.Proxy
  | SndError.ProxyRelay
  | SndError.Other

export namespace SndError {
  export type Tag = "auth" | "quota" | "expired" | "relay" | "proxy" | "proxyRelay" | "other"

  interface Interface {
    type: Tag
  }

  export interface Auth extends Interface {
    type: "auth"
  }

  export interface Quota extends Interface {
    type: "quota"
  }

  export interface Expired extends Interface {
    type: "expired"
  }

  export interface Relay extends Interface {
    type: "relay"
    srvError: SrvError
  }

  export interface Proxy extends Interface {
    type: "proxy"
    proxyServer: string
    srvError: SrvError
  }

  export interface ProxyRelay extends Interface {
    type: "proxyRelay"
    proxyServer: string
    srvError: SrvError
  }

  export interface Other extends Interface {
    type: "other"
    sndError: string
  }
}

export interface SndFileTransfer {
  fileId: number // int64
  fileName: string
  filePath: string
  fileSize: number // int64
  chunkSize: number // int64
  recipientDisplayName: string
  connId: number // int64
  agentConnId: string
  groupMemberId?: number // int64
  fileStatus: FileStatus
  fileDescrId?: number // int64
  fileInline?: InlineFileMode
}

export type SndGroupEvent = 
  | SndGroupEvent.MemberRole
  | SndGroupEvent.MemberBlocked
  | SndGroupEvent.UserRole
  | SndGroupEvent.MemberDeleted
  | SndGroupEvent.UserLeft
  | SndGroupEvent.GroupUpdated
  | SndGroupEvent.MemberAccepted
  | SndGroupEvent.UserPendingReview

export namespace SndGroupEvent {
  export type Tag = 
    | "memberRole"
    | "memberBlocked"
    | "userRole"
    | "memberDeleted"
    | "userLeft"
    | "groupUpdated"
    | "memberAccepted"
    | "userPendingReview"

  interface Interface {
    type: Tag
  }

  export interface MemberRole extends Interface {
    type: "memberRole"
    groupMemberId: number // int64
    profile: Profile
    role: GroupMemberRole
  }

  export interface MemberBlocked extends Interface {
    type: "memberBlocked"
    groupMemberId: number // int64
    profile: Profile
    blocked: boolean
  }

  export interface UserRole extends Interface {
    type: "userRole"
    role: GroupMemberRole
  }

  export interface MemberDeleted extends Interface {
    type: "memberDeleted"
    groupMemberId: number // int64
    profile: Profile
  }

  export interface UserLeft extends Interface {
    type: "userLeft"
  }

  export interface GroupUpdated extends Interface {
    type: "groupUpdated"
    groupProfile: GroupProfile
  }

  export interface MemberAccepted extends Interface {
    type: "memberAccepted"
    groupMemberId: number // int64
    profile: Profile
  }

  export interface UserPendingReview extends Interface {
    type: "userPendingReview"
  }
}

export type SrvError = SrvError.Host | SrvError.Version | SrvError.Other

export namespace SrvError {
  export type Tag = "host" | "version" | "other"

  interface Interface {
    type: Tag
  }

  export interface Host extends Interface {
    type: "host"
  }

  export interface Version extends Interface {
    type: "version"
  }

  export interface Other extends Interface {
    type: "other"
    srvError: string
  }
}

export type StoreError = 
  | StoreError.DuplicateName
  | StoreError.UserNotFound
  | StoreError.RelayUserNotFound
  | StoreError.UserNotFoundByName
  | StoreError.UserNotFoundByContactId
  | StoreError.UserNotFoundByGroupId
  | StoreError.UserNotFoundByFileId
  | StoreError.UserNotFoundByContactRequestId
  | StoreError.ContactNotFound
  | StoreError.ContactNotFoundByName
  | StoreError.ContactNotFoundByMemberId
  | StoreError.ContactNotReady
  | StoreError.DuplicateContactLink
  | StoreError.UserContactLinkNotFound
  | StoreError.ContactRequestNotFound
  | StoreError.ContactRequestNotFoundByName
  | StoreError.InvalidContactRequestEntity
  | StoreError.InvalidBusinessChatContactRequest
  | StoreError.GroupNotFound
  | StoreError.GroupNotFoundByName
  | StoreError.GroupMemberNameNotFound
  | StoreError.GroupMemberNotFound
  | StoreError.GroupMemberNotFoundByIndex
  | StoreError.MemberRelationsVectorNotFound
  | StoreError.GroupHostMemberNotFound
  | StoreError.GroupMemberNotFoundByMemberId
  | StoreError.MemberContactGroupMemberNotFound
  | StoreError.InvalidMemberRelationUpdate
  | StoreError.GroupWithoutUser
  | StoreError.DuplicateGroupMember
  | StoreError.DuplicateMemberId
  | StoreError.GroupAlreadyJoined
  | StoreError.GroupInvitationNotFound
  | StoreError.NoteFolderAlreadyExists
  | StoreError.NoteFolderNotFound
  | StoreError.UserNoteFolderNotFound
  | StoreError.SndFileNotFound
  | StoreError.SndFileInvalid
  | StoreError.RcvFileNotFound
  | StoreError.RcvFileDescrNotFound
  | StoreError.FileNotFound
  | StoreError.RcvFileInvalid
  | StoreError.RcvFileInvalidDescrPart
  | StoreError.LocalFileNoTransfer
  | StoreError.SharedMsgIdNotFoundByFileId
  | StoreError.FileIdNotFoundBySharedMsgId
  | StoreError.SndFileNotFoundXFTP
  | StoreError.RcvFileNotFoundXFTP
  | StoreError.ConnectionNotFound
  | StoreError.ConnectionNotFoundById
  | StoreError.ConnectionNotFoundByMemberId
  | StoreError.PendingConnectionNotFound
  | StoreError.UniqueID
  | StoreError.LargeMsg
  | StoreError.InternalError
  | StoreError.DBException
  | StoreError.DBBusyError
  | StoreError.BadChatItem
  | StoreError.ChatItemNotFound
  | StoreError.ChatItemNotFoundByText
  | StoreError.ChatItemSharedMsgIdNotFound
  | StoreError.ChatItemNotFoundByFileId
  | StoreError.ChatItemNotFoundByContactId
  | StoreError.ChatItemNotFoundByGroupId
  | StoreError.ProfileNotFound
  | StoreError.DuplicateGroupLink
  | StoreError.GroupLinkNotFound
  | StoreError.HostMemberIdNotFound
  | StoreError.ContactNotFoundByFileId
  | StoreError.NoGroupSndStatus
  | StoreError.DuplicateGroupMessage
  | StoreError.RemoteHostNotFound
  | StoreError.RemoteHostUnknown
  | StoreError.RemoteHostDuplicateCA
  | StoreError.RemoteCtrlNotFound
  | StoreError.RemoteCtrlDuplicateCA
  | StoreError.ProhibitedDeleteUser
  | StoreError.OperatorNotFound
  | StoreError.UsageConditionsNotFound
  | StoreError.UserChatRelayNotFound
  | StoreError.GroupRelayNotFound
  | StoreError.GroupRelayNotFoundByMemberId
  | StoreError.InvalidQuote
  | StoreError.InvalidMention
  | StoreError.InvalidDeliveryTask
  | StoreError.DeliveryTaskNotFound
  | StoreError.InvalidDeliveryJob
  | StoreError.DeliveryJobNotFound
  | StoreError.WorkItemError

export namespace StoreError {
  export type Tag = 
    | "duplicateName"
    | "userNotFound"
    | "relayUserNotFound"
    | "userNotFoundByName"
    | "userNotFoundByContactId"
    | "userNotFoundByGroupId"
    | "userNotFoundByFileId"
    | "userNotFoundByContactRequestId"
    | "contactNotFound"
    | "contactNotFoundByName"
    | "contactNotFoundByMemberId"
    | "contactNotReady"
    | "duplicateContactLink"
    | "userContactLinkNotFound"
    | "contactRequestNotFound"
    | "contactRequestNotFoundByName"
    | "invalidContactRequestEntity"
    | "invalidBusinessChatContactRequest"
    | "groupNotFound"
    | "groupNotFoundByName"
    | "groupMemberNameNotFound"
    | "groupMemberNotFound"
    | "groupMemberNotFoundByIndex"
    | "memberRelationsVectorNotFound"
    | "groupHostMemberNotFound"
    | "groupMemberNotFoundByMemberId"
    | "memberContactGroupMemberNotFound"
    | "invalidMemberRelationUpdate"
    | "groupWithoutUser"
    | "duplicateGroupMember"
    | "duplicateMemberId"
    | "groupAlreadyJoined"
    | "groupInvitationNotFound"
    | "noteFolderAlreadyExists"
    | "noteFolderNotFound"
    | "userNoteFolderNotFound"
    | "sndFileNotFound"
    | "sndFileInvalid"
    | "rcvFileNotFound"
    | "rcvFileDescrNotFound"
    | "fileNotFound"
    | "rcvFileInvalid"
    | "rcvFileInvalidDescrPart"
    | "localFileNoTransfer"
    | "sharedMsgIdNotFoundByFileId"
    | "fileIdNotFoundBySharedMsgId"
    | "sndFileNotFoundXFTP"
    | "rcvFileNotFoundXFTP"
    | "connectionNotFound"
    | "connectionNotFoundById"
    | "connectionNotFoundByMemberId"
    | "pendingConnectionNotFound"
    | "uniqueID"
    | "largeMsg"
    | "internalError"
    | "dBException"
    | "dBBusyError"
    | "badChatItem"
    | "chatItemNotFound"
    | "chatItemNotFoundByText"
    | "chatItemSharedMsgIdNotFound"
    | "chatItemNotFoundByFileId"
    | "chatItemNotFoundByContactId"
    | "chatItemNotFoundByGroupId"
    | "profileNotFound"
    | "duplicateGroupLink"
    | "groupLinkNotFound"
    | "hostMemberIdNotFound"
    | "contactNotFoundByFileId"
    | "noGroupSndStatus"
    | "duplicateGroupMessage"
    | "remoteHostNotFound"
    | "remoteHostUnknown"
    | "remoteHostDuplicateCA"
    | "remoteCtrlNotFound"
    | "remoteCtrlDuplicateCA"
    | "prohibitedDeleteUser"
    | "operatorNotFound"
    | "usageConditionsNotFound"
    | "userChatRelayNotFound"
    | "groupRelayNotFound"
    | "groupRelayNotFoundByMemberId"
    | "invalidQuote"
    | "invalidMention"
    | "invalidDeliveryTask"
    | "deliveryTaskNotFound"
    | "invalidDeliveryJob"
    | "deliveryJobNotFound"
    | "workItemError"

  interface Interface {
    type: Tag
  }

  export interface DuplicateName extends Interface {
    type: "duplicateName"
  }

  export interface UserNotFound extends Interface {
    type: "userNotFound"
    userId: number // int64
  }

  export interface RelayUserNotFound extends Interface {
    type: "relayUserNotFound"
  }

  export interface UserNotFoundByName extends Interface {
    type: "userNotFoundByName"
    contactName: string
  }

  export interface UserNotFoundByContactId extends Interface {
    type: "userNotFoundByContactId"
    contactId: number // int64
  }

  export interface UserNotFoundByGroupId extends Interface {
    type: "userNotFoundByGroupId"
    groupId: number // int64
  }

  export interface UserNotFoundByFileId extends Interface {
    type: "userNotFoundByFileId"
    fileId: number // int64
  }

  export interface UserNotFoundByContactRequestId extends Interface {
    type: "userNotFoundByContactRequestId"
    contactRequestId: number // int64
  }

  export interface ContactNotFound extends Interface {
    type: "contactNotFound"
    contactId: number // int64
  }

  export interface ContactNotFoundByName extends Interface {
    type: "contactNotFoundByName"
    contactName: string
  }

  export interface ContactNotFoundByMemberId extends Interface {
    type: "contactNotFoundByMemberId"
    groupMemberId: number // int64
  }

  export interface ContactNotReady extends Interface {
    type: "contactNotReady"
    contactName: string
  }

  export interface DuplicateContactLink extends Interface {
    type: "duplicateContactLink"
  }

  export interface UserContactLinkNotFound extends Interface {
    type: "userContactLinkNotFound"
  }

  export interface ContactRequestNotFound extends Interface {
    type: "contactRequestNotFound"
    contactRequestId: number // int64
  }

  export interface ContactRequestNotFoundByName extends Interface {
    type: "contactRequestNotFoundByName"
    contactName: string
  }

  export interface InvalidContactRequestEntity extends Interface {
    type: "invalidContactRequestEntity"
    contactRequestId: number // int64
  }

  export interface InvalidBusinessChatContactRequest extends Interface {
    type: "invalidBusinessChatContactRequest"
  }

  export interface GroupNotFound extends Interface {
    type: "groupNotFound"
    groupId: number // int64
  }

  export interface GroupNotFoundByName extends Interface {
    type: "groupNotFoundByName"
    groupName: string
  }

  export interface GroupMemberNameNotFound extends Interface {
    type: "groupMemberNameNotFound"
    groupId: number // int64
    groupMemberName: string
  }

  export interface GroupMemberNotFound extends Interface {
    type: "groupMemberNotFound"
    groupMemberId: number // int64
  }

  export interface GroupMemberNotFoundByIndex extends Interface {
    type: "groupMemberNotFoundByIndex"
    groupMemberIndex: number // int64
  }

  export interface MemberRelationsVectorNotFound extends Interface {
    type: "memberRelationsVectorNotFound"
    groupMemberId: number // int64
  }

  export interface GroupHostMemberNotFound extends Interface {
    type: "groupHostMemberNotFound"
    groupId: number // int64
  }

  export interface GroupMemberNotFoundByMemberId extends Interface {
    type: "groupMemberNotFoundByMemberId"
    memberId: string
  }

  export interface MemberContactGroupMemberNotFound extends Interface {
    type: "memberContactGroupMemberNotFound"
    contactId: number // int64
  }

  export interface InvalidMemberRelationUpdate extends Interface {
    type: "invalidMemberRelationUpdate"
  }

  export interface GroupWithoutUser extends Interface {
    type: "groupWithoutUser"
  }

  export interface DuplicateGroupMember extends Interface {
    type: "duplicateGroupMember"
  }

  export interface DuplicateMemberId extends Interface {
    type: "duplicateMemberId"
  }

  export interface GroupAlreadyJoined extends Interface {
    type: "groupAlreadyJoined"
  }

  export interface GroupInvitationNotFound extends Interface {
    type: "groupInvitationNotFound"
  }

  export interface NoteFolderAlreadyExists extends Interface {
    type: "noteFolderAlreadyExists"
    noteFolderId: number // int64
  }

  export interface NoteFolderNotFound extends Interface {
    type: "noteFolderNotFound"
    noteFolderId: number // int64
  }

  export interface UserNoteFolderNotFound extends Interface {
    type: "userNoteFolderNotFound"
  }

  export interface SndFileNotFound extends Interface {
    type: "sndFileNotFound"
    fileId: number // int64
  }

  export interface SndFileInvalid extends Interface {
    type: "sndFileInvalid"
    fileId: number // int64
  }

  export interface RcvFileNotFound extends Interface {
    type: "rcvFileNotFound"
    fileId: number // int64
  }

  export interface RcvFileDescrNotFound extends Interface {
    type: "rcvFileDescrNotFound"
    fileId: number // int64
  }

  export interface FileNotFound extends Interface {
    type: "fileNotFound"
    fileId: number // int64
  }

  export interface RcvFileInvalid extends Interface {
    type: "rcvFileInvalid"
    fileId: number // int64
  }

  export interface RcvFileInvalidDescrPart extends Interface {
    type: "rcvFileInvalidDescrPart"
  }

  export interface LocalFileNoTransfer extends Interface {
    type: "localFileNoTransfer"
    fileId: number // int64
  }

  export interface SharedMsgIdNotFoundByFileId extends Interface {
    type: "sharedMsgIdNotFoundByFileId"
    fileId: number // int64
  }

  export interface FileIdNotFoundBySharedMsgId extends Interface {
    type: "fileIdNotFoundBySharedMsgId"
    sharedMsgId: string
  }

  export interface SndFileNotFoundXFTP extends Interface {
    type: "sndFileNotFoundXFTP"
    agentSndFileId: string
  }

  export interface RcvFileNotFoundXFTP extends Interface {
    type: "rcvFileNotFoundXFTP"
    agentRcvFileId: string
  }

  export interface ConnectionNotFound extends Interface {
    type: "connectionNotFound"
    agentConnId: string
  }

  export interface ConnectionNotFoundById extends Interface {
    type: "connectionNotFoundById"
    connId: number // int64
  }

  export interface ConnectionNotFoundByMemberId extends Interface {
    type: "connectionNotFoundByMemberId"
    groupMemberId: number // int64
  }

  export interface PendingConnectionNotFound extends Interface {
    type: "pendingConnectionNotFound"
    connId: number // int64
  }

  export interface UniqueID extends Interface {
    type: "uniqueID"
  }

  export interface LargeMsg extends Interface {
    type: "largeMsg"
  }

  export interface InternalError extends Interface {
    type: "internalError"
    message: string
  }

  export interface DBException extends Interface {
    type: "dBException"
    message: string
  }

  export interface DBBusyError extends Interface {
    type: "dBBusyError"
    message: string
  }

  export interface BadChatItem extends Interface {
    type: "badChatItem"
    itemId: number // int64
    itemTs?: string // ISO-8601 timestamp
  }

  export interface ChatItemNotFound extends Interface {
    type: "chatItemNotFound"
    itemId: number // int64
  }

  export interface ChatItemNotFoundByText extends Interface {
    type: "chatItemNotFoundByText"
    text: string
  }

  export interface ChatItemSharedMsgIdNotFound extends Interface {
    type: "chatItemSharedMsgIdNotFound"
    sharedMsgId: string
  }

  export interface ChatItemNotFoundByFileId extends Interface {
    type: "chatItemNotFoundByFileId"
    fileId: number // int64
  }

  export interface ChatItemNotFoundByContactId extends Interface {
    type: "chatItemNotFoundByContactId"
    contactId: number // int64
  }

  export interface ChatItemNotFoundByGroupId extends Interface {
    type: "chatItemNotFoundByGroupId"
    groupId: number // int64
  }

  export interface ProfileNotFound extends Interface {
    type: "profileNotFound"
    profileId: number // int64
  }

  export interface DuplicateGroupLink extends Interface {
    type: "duplicateGroupLink"
    groupInfo: GroupInfo
  }

  export interface GroupLinkNotFound extends Interface {
    type: "groupLinkNotFound"
    groupInfo: GroupInfo
  }

  export interface HostMemberIdNotFound extends Interface {
    type: "hostMemberIdNotFound"
    groupId: number // int64
  }

  export interface ContactNotFoundByFileId extends Interface {
    type: "contactNotFoundByFileId"
    fileId: number // int64
  }

  export interface NoGroupSndStatus extends Interface {
    type: "noGroupSndStatus"
    itemId: number // int64
    groupMemberId: number // int64
  }

  export interface DuplicateGroupMessage extends Interface {
    type: "duplicateGroupMessage"
    groupId: number // int64
    sharedMsgId: string
    authorGroupMemberId?: number // int64
    forwardedByGroupMemberId?: number // int64
  }

  export interface RemoteHostNotFound extends Interface {
    type: "remoteHostNotFound"
    remoteHostId: number // int64
  }

  export interface RemoteHostUnknown extends Interface {
    type: "remoteHostUnknown"
  }

  export interface RemoteHostDuplicateCA extends Interface {
    type: "remoteHostDuplicateCA"
  }

  export interface RemoteCtrlNotFound extends Interface {
    type: "remoteCtrlNotFound"
    remoteCtrlId: number // int64
  }

  export interface RemoteCtrlDuplicateCA extends Interface {
    type: "remoteCtrlDuplicateCA"
  }

  export interface ProhibitedDeleteUser extends Interface {
    type: "prohibitedDeleteUser"
    userId: number // int64
    contactId: number // int64
  }

  export interface OperatorNotFound extends Interface {
    type: "operatorNotFound"
    serverOperatorId: number // int64
  }

  export interface UsageConditionsNotFound extends Interface {
    type: "usageConditionsNotFound"
  }

  export interface UserChatRelayNotFound extends Interface {
    type: "userChatRelayNotFound"
    chatRelayId: number // int64
  }

  export interface GroupRelayNotFound extends Interface {
    type: "groupRelayNotFound"
    groupRelayId: number // int64
  }

  export interface GroupRelayNotFoundByMemberId extends Interface {
    type: "groupRelayNotFoundByMemberId"
    groupMemberId: number // int64
  }

  export interface InvalidQuote extends Interface {
    type: "invalidQuote"
  }

  export interface InvalidMention extends Interface {
    type: "invalidMention"
  }

  export interface InvalidDeliveryTask extends Interface {
    type: "invalidDeliveryTask"
    taskId: number // int64
  }

  export interface DeliveryTaskNotFound extends Interface {
    type: "deliveryTaskNotFound"
    taskId: number // int64
  }

  export interface InvalidDeliveryJob extends Interface {
    type: "invalidDeliveryJob"
    jobId: number // int64
  }

  export interface DeliveryJobNotFound extends Interface {
    type: "deliveryJobNotFound"
    jobId: number // int64
  }

  export interface WorkItemError extends Interface {
    type: "workItemError"
    errContext: string
  }
}

export type SubscriptionStatus = 
  | SubscriptionStatus.Active
  | SubscriptionStatus.Pending
  | SubscriptionStatus.Removed
  | SubscriptionStatus.NoSub

export namespace SubscriptionStatus {
  export type Tag = "active" | "pending" | "removed" | "noSub"

  interface Interface {
    type: Tag
  }

  export interface Active extends Interface {
    type: "active"
  }

  export interface Pending extends Interface {
    type: "pending"
  }

  export interface Removed extends Interface {
    type: "removed"
    subError: string
  }

  export interface NoSub extends Interface {
    type: "noSub"
  }
}

export enum SwitchPhase {
  Started = "started",
  Confirmed = "confirmed",
  Secured = "secured",
  Completed = "completed",
}

export interface TimedMessagesGroupPreference {
  enable: GroupFeatureEnabled
  ttl?: number // int
}

export interface TimedMessagesPreference {
  allow: FeatureAllowed
  ttl?: number // int
}

export type TransportError = 
  | TransportError.BadBlock
  | TransportError.Version
  | TransportError.LargeMsg
  | TransportError.BadSession
  | TransportError.NoServerAuth
  | TransportError.Handshake

export namespace TransportError {
  export type Tag = "badBlock" | "version" | "largeMsg" | "badSession" | "noServerAuth" | "handshake"

  interface Interface {
    type: Tag
  }

  export interface BadBlock extends Interface {
    type: "badBlock"
  }

  export interface Version extends Interface {
    type: "version"
  }

  export interface LargeMsg extends Interface {
    type: "largeMsg"
  }

  export interface BadSession extends Interface {
    type: "badSession"
  }

  export interface NoServerAuth extends Interface {
    type: "noServerAuth"
  }

  export interface Handshake extends Interface {
    type: "handshake"
    handshakeErr: HandshakeError
  }
}

export enum UIColorMode {
  Light = "light",
  Dark = "dark",
}

export interface UIColors {
  accent?: string
  accentVariant?: string
  secondary?: string
  secondaryVariant?: string
  background?: string
  menus?: string
  title?: string
  accentVariant2?: string
  sentMessage?: string
  sentReply?: string
  receivedMessage?: string
  receivedReply?: string
}

export interface UIThemeEntityOverride {
  mode: UIColorMode
  wallpaper?: ChatWallpaper
  colors: UIColors
}

export interface UIThemeEntityOverrides {
  light?: UIThemeEntityOverride
  dark?: UIThemeEntityOverride
}

export interface UpdatedMessage {
  msgContent: MsgContent
  mentions: {[key: string]: number} // string : int64
}

export interface User {
  userId: number // int64
  agentUserId: number // int64
  userContactId: number // int64
  localDisplayName: string
  profile: LocalProfile
  fullPreferences: FullPreferences
  activeUser: boolean
  activeOrder: number // int64
  viewPwdHash?: UserPwdHash
  showNtfs: boolean
  sendRcptsContacts: boolean
  sendRcptsSmallGroups: boolean
  autoAcceptMemberContacts: boolean
  userMemberProfileUpdatedAt?: string // ISO-8601 timestamp
  uiThemes?: UIThemeEntityOverrides
  userChatRelay: boolean
}

export interface UserContact {
  userContactLinkId: number // int64
  connReqContact: string
  groupId?: number // int64
}

export interface UserContactLink {
  userContactLinkId: number // int64
  connLinkContact: CreatedConnLink
  shortLinkDataSet: boolean
  shortLinkLargeDataSet: boolean
  addressSettings: AddressSettings
}

export interface UserContactRequest {
  contactRequestId: number // int64
  agentInvitationId: string
  contactId_?: number // int64
  businessGroupId_?: number // int64
  userContactLinkId_?: number // int64
  cReqChatVRange: VersionRange
  localDisplayName: string
  profileId: number // int64
  profile: Profile
  createdAt: string // ISO-8601 timestamp
  updatedAt: string // ISO-8601 timestamp
  xContactId?: string
  pqSupport: boolean
  welcomeSharedMsgId?: string
  requestSharedMsgId?: string
}

export interface UserInfo {
  user: User
  unreadCount: number // int
}

export interface UserProfileUpdateSummary {
  updateSuccesses: number // int
  updateFailures: number // int
  changedContacts: Contact[]
}

export interface UserPwdHash {
  hash: string
  salt: string
}

export interface VersionRange {
  minVersion: number // int
  maxVersion: number // int
}

export type XFTPErrorType = 
  | XFTPErrorType.BLOCK
  | XFTPErrorType.SESSION
  | XFTPErrorType.HANDSHAKE
  | XFTPErrorType.CMD
  | XFTPErrorType.AUTH
  | XFTPErrorType.BLOCKED
  | XFTPErrorType.SIZE
  | XFTPErrorType.QUOTA
  | XFTPErrorType.DIGEST
  | XFTPErrorType.CRYPTO
  | XFTPErrorType.NO_FILE
  | XFTPErrorType.HAS_FILE
  | XFTPErrorType.FILE_IO
  | XFTPErrorType.TIMEOUT
  | XFTPErrorType.INTERNAL
  | XFTPErrorType.DUPLICATE_

export namespace XFTPErrorType {
  export type Tag = 
    | "BLOCK"
    | "SESSION"
    | "HANDSHAKE"
    | "CMD"
    | "AUTH"
    | "BLOCKED"
    | "SIZE"
    | "QUOTA"
    | "DIGEST"
    | "CRYPTO"
    | "NO_FILE"
    | "HAS_FILE"
    | "FILE_IO"
    | "TIMEOUT"
    | "INTERNAL"
    | "DUPLICATE_"

  interface Interface {
    type: Tag
  }

  export interface BLOCK extends Interface {
    type: "BLOCK"
  }

  export interface SESSION extends Interface {
    type: "SESSION"
  }

  export interface HANDSHAKE extends Interface {
    type: "HANDSHAKE"
  }

  export interface CMD extends Interface {
    type: "CMD"
    cmdErr: CommandError
  }

  export interface AUTH extends Interface {
    type: "AUTH"
  }

  export interface BLOCKED extends Interface {
    type: "BLOCKED"
    blockInfo: BlockingInfo
  }

  export interface SIZE extends Interface {
    type: "SIZE"
  }

  export interface QUOTA extends Interface {
    type: "QUOTA"
  }

  export interface DIGEST extends Interface {
    type: "DIGEST"
  }

  export interface CRYPTO extends Interface {
    type: "CRYPTO"
  }

  export interface NO_FILE extends Interface {
    type: "NO_FILE"
  }

  export interface HAS_FILE extends Interface {
    type: "HAS_FILE"
  }

  export interface FILE_IO extends Interface {
    type: "FILE_IO"
  }

  export interface TIMEOUT extends Interface {
    type: "TIMEOUT"
  }

  export interface INTERNAL extends Interface {
    type: "INTERNAL"
  }

  export interface DUPLICATE_ extends Interface {
    type: "DUPLICATE_"
  }
}

export interface XFTPRcvFile {
  rcvFileDescription: RcvFileDescr
  agentRcvFileId?: string
  agentRcvFileDeleted: boolean
  userApprovedRelays: boolean
}

export interface XFTPSndFile {
  agentSndFileId: string
  privateSndFileDescr?: string
  agentSndFileDeleted: boolean
  cryptoArgs?: CryptoFileArgs
}
