// API Types: This file is generated automatically.

export interface ACIReaction {
  chatInfo: ChatInfo
  chatReaction: CIReaction
}

export interface AChat {
  chatInfo: ChatInfo
  chatItems: [ChatItem]
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

export type AgentCryptoErrorTag = "DECRYPT_AES" | "DECRYPT_CB" | "RATCHET_HEADER" | "RATCHET_EARLIER" | "RATCHET_SKIPPED" | "RATCHET_SYNC"

export interface AgentCryptoError {
}

export type AgentErrorTypeTag = 
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
  | "INTERNAL"
  | "CRITICAL"
  | "INACTIVE"

export interface AgentErrorType {
}

export interface AutoAccept {
  acceptIncognito: boolean
}

export interface BlockingInfo {
  reason: BlockingReason
}

export enum BlockingReason {
  Spam = "spam",
  Content = "content",
}

export type BrokerErrorTypeTag = "RESPONSE" | "UNEXPECTED" | "NETWORK" | "HOST" | "NO_SERVICE" | "TRANSPORT" | "TIMEOUT"

export interface BrokerErrorType {
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

export type CIContentTag = 
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

export interface CIContent {
}

export enum CIDeleteMode {
  Broadcast = "broadcast",
  Internal = "internal",
  InternalMark = "internalMark",
}

export type CIDeletedTag = "deleted" | "blocked" | "blockedByAdmin" | "moderated"

export interface CIDeleted {
}

export type CIDirectionTag = "directSnd" | "directRcv" | "groupSnd" | "groupRcv" | "localSnd" | "localRcv"

export interface CIDirection {
}

export interface CIFile {
  fileId: number // int64
  fileName: string
  fileSize: number // int64
  fileSource?: CryptoFile
  fileStatus: CIFileStatus
  fileProtocol: FileProtocol
}

export type CIFileStatusTag = 
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

export interface CIFileStatus {
}

export type CIForwardedFromTag = "unknown" | "contact" | "group"

export interface CIForwardedFrom {
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
  formattedText?: [FormattedText]
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

export type CIStatusTag = 
  | "sndNew"
  | "sndSent"
  | "sndRcvd"
  | "sndErrorAuth"
  | "sndError"
  | "sndWarning"
  | "rcvNew"
  | "rcvRead"
  | "invalid"

export interface CIStatus {
}

export interface CITimed {
  ttl: number // int
  deleteAt?: string // ISO-8601 timestamp
}

export type ChatBotCommandTag = "command" | "menu"

export interface ChatBotCommand {
}

export type ChatDeleteModeTag = "full" | "entity" | "messages"

export interface ChatDeleteMode {
}

export type ChatErrorTag = "error" | "errorAgent" | "errorStore"

export interface ChatError {
}

export type ChatErrorTypeTag = 
  | "noActiveUser"
  | "noConnectionUser"
  | "noSndFileUser"
  | "noRcvFileUser"
  | "userUnknown"
  | "activeUserExists"
  | "userExists"
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
  | "fileRead"
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

export interface ChatErrorType {
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

export type ChatInfoTag = "direct" | "group" | "local" | "contactRequest" | "contactConnection"

export interface ChatInfo {
}

export interface ChatItem {
  chatDir: CIDirection
  meta: CIMeta
  content: CIContent
  mentions: {[key: string]: CIMention}
  formattedText?: [FormattedText]
  quotedItem?: CIQuote
  reactions: [CIReactionCount]
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

export type CommandErrorTag = "UNKNOWN" | "SYNTAX" | "PROHIBITED" | "NO_AUTH" | "HAS_AUTH" | "NO_ENTITY"

export interface CommandError {
}

export type CommandErrorTypeTag = "PROHIBITED" | "SYNTAX" | "NO_CONN" | "SIZE" | "LARGE"

export interface CommandErrorType {
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

export type ConnectionEntityTag = "rcvDirectMsgConnection" | "rcvGroupMsgConnection" | "sndFileConnection" | "rcvFileConnection" | "userContactConnection"

export interface ConnectionEntity {
}

export type ConnectionErrorTypeTag = "NOT_FOUND" | "DUPLICATE" | "SIMPLEX" | "NOT_ACCEPTED" | "NOT_AVAILABLE"

export interface ConnectionErrorType {
}

export enum ConnectionMode {
  INV = "inv",
  CON = "con",
}

export type ConnectionPlanTag = "invitationLink" | "contactAddress" | "groupLink" | "error"

export interface ConnectionPlan {
}

export interface Contact {
  contactId: number // int64
  localDisplayName: string
  profile: LocalProfile
  activeConn?: Connection
  viaGroup?: number // int64
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
  chatTags: [number] // int64
  chatItemTTL?: number // int64
  uiThemes?: UIThemeEntityOverrides
  chatDeleted: boolean
  customData?: object
}

export type ContactAddressPlanTag = "ok" | "ownLink" | "connectingConfirmReconnect" | "connectingProhibit" | "known" | "contactViaAddress"

export interface ContactAddressPlan {
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

export type ContactUserPrefTag = "contact" | "user"

export interface ContactUserPref {
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
  commands?: [ChatBotCommand]
}

export interface CreatedConnLink {
  connFullLink: string
  connShortLink?: string
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

export type ErrorTypeTag = 
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

export interface ErrorType {
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

export type FileErrorTag = "auth" | "blocked" | "noFile" | "relay" | "other"

export interface FileError {
}

export type FileErrorTypeTag = "NOT_APPROVED" | "SIZE" | "REDIRECT" | "FILE_IO" | "NO_FILE"

export interface FileErrorType {
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

export type FormatTag = 
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

export interface Format {
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
  commands: [ChatBotCommand]
}

export interface FullPreferences {
  timedMessages: TimedMessagesPreference
  fullDelete: SimplePreference
  reactions: SimplePreference
  voice: SimplePreference
  files: SimplePreference
  calls: SimplePreference
  sessions: SimplePreference
  commands: [ChatBotCommand]
}

export interface Group {
  groupInfo: GroupInfo
  members: [GroupMember]
}

export type GroupChatScopeTag = "memberSupport"

export interface GroupChatScope {
}

export type GroupChatScopeInfoTag = "memberSupport"

export interface GroupChatScopeInfo {
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
  chatTags: [number] // int64
  chatItemTTL?: number // int64
  uiThemes?: UIThemeEntityOverrides
  customData?: object
  membersRequireAttention: number // int
  viaGroupLinkUri?: string
}

export interface GroupInfoSummary {
  groupInfo: GroupInfo
  groupSummary: GroupSummary
}

export interface GroupLink {
  userContactLinkId: number // int64
  connLinkContact: CreatedConnLink
  shortLinkDataSet: boolean
  shortLinkLargeDataSet: boolean
  groupLinkId: string
  acceptMemberRole: GroupMemberRole
}

export type GroupLinkPlanTag = "ok" | "ownLink" | "connectingConfirmReconnect" | "connectingProhibit" | "known"

export interface GroupLinkPlan {
}

export interface GroupMember {
  groupMemberId: number // int64
  groupId: number // int64
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
  commands?: [ChatBotCommand]
}

export interface GroupProfile {
  displayName: string
  fullName: string
  shortDescr?: string
  description?: string
  image?: string
  groupPreferences?: GroupPreferences
  memberAdmission?: GroupMemberAdmission
}

export interface GroupShortLinkData {
  groupProfile: GroupProfile
}

export interface GroupSummary {
  currentMembers: number // int
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

export type InvitationLinkPlanTag = "ok" | "ownLink" | "connecting" | "known"

export interface InvitationLinkPlan {
}

export type InvitedByTag = "contact" | "user" | "unknown"

export interface InvitedBy {
}

export type LinkContentTag = "page" | "image" | "video" | "unknown"

export interface LinkContent {
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

export type MsgChatLinkTag = "contact" | "invitation" | "group"

export interface MsgChatLink {
}

export type MsgContentTag = 
  | "text"
  | "link"
  | "image"
  | "video"
  | "voice"
  | "file"
  | "report"
  | "chat"
  | "unknown"

export interface MsgContent {
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

export type MsgErrorTypeTag = "msgSkipped" | "msgBadId" | "msgBadHash" | "msgDuplicate"

export interface MsgErrorType {
}

export enum MsgFilter {
  None = "none",
  All = "all",
  Mentions = "mentions",
}

export type MsgReactionTag = "emoji" | "unknown"

export interface MsgReaction {
}

export enum MsgReceiptStatus {
  Ok = "ok",
  BadMsgHash = "badMsgHash",
}

export interface NewUser {
  profile?: Profile
  pastTimestamp: boolean
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
  commands?: [ChatBotCommand]
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

export type ProxyClientErrorTag = "protocolError" | "unexpectedResponse" | "responseError"

export interface ProxyClientError {
}

export type ProxyErrorTag = "PROTOCOL" | "BROKER" | "BASIC_AUTH" | "NO_SESSION"

export interface ProxyError {
}

export type RCErrorTypeTag = 
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

export interface RCErrorType {
}

export enum RatchetSyncState {
  Ok = "ok",
  Allowed = "allowed",
  Required = "required",
  Started = "started",
  Agreed = "agreed",
}

export type RcvConnEventTag = "switchQueue" | "ratchetSync" | "verificationCodeReset" | "pqEnabled"

export interface RcvConnEvent {
}

export type RcvDirectEventTag = "contactDeleted" | "profileUpdated" | "groupInvLinkReceived"

export interface RcvDirectEvent {
}

export interface RcvFileDescr {
  fileDescrId: number // int64
  fileDescrText: string
  fileDescrPartNo: number // int
  fileDescrComplete: boolean
}

export interface RcvFileInfo {
  filePath: string
  connId?: number // int64
  agentConnId?: string
}

export type RcvFileStatusTag = "new" | "accepted" | "connected" | "complete" | "cancelled"

export interface RcvFileStatus {
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

export type RcvGroupEventTag = 
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

export interface RcvGroupEvent {
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

export type SMPAgentErrorTag = "A_MESSAGE" | "A_PROHIBITED" | "A_VERSION" | "A_LINK" | "A_CRYPTO" | "A_DUPLICATE" | "A_QUEUE"

export interface SMPAgentError {
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

export type SndConnEventTag = "switchQueue" | "ratchetSync" | "pqEnabled"

export interface SndConnEvent {
}

export type SndErrorTag = "auth" | "quota" | "expired" | "relay" | "proxy" | "proxyRelay" | "other"

export interface SndError {
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

export type SndGroupEventTag = 
  | "memberRole"
  | "memberBlocked"
  | "userRole"
  | "memberDeleted"
  | "userLeft"
  | "groupUpdated"
  | "memberAccepted"
  | "userPendingReview"

export interface SndGroupEvent {
}

export type SrvErrorTag = "host" | "version" | "other"

export interface SrvError {
}

export type StoreErrorTag = 
  | "duplicateName"
  | "userNotFound"
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
  | "groupHostMemberNotFound"
  | "groupMemberNotFoundByMemberId"
  | "memberContactGroupMemberNotFound"
  | "groupWithoutUser"
  | "duplicateGroupMember"
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
  | "introNotFound"
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
  | "invalidQuote"
  | "invalidMention"

export interface StoreError {
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

export type TransportErrorTag = "badBlock" | "version" | "largeMsg" | "badSession" | "noServerAuth" | "handshake"

export interface TransportError {
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
  changedContacts: [Contact]
}

export interface UserPwdHash {
  hash: string
  salt: string
}

export interface VersionRange {
  minVersion: number // int
  maxVersion: number // int
}

export type XFTPErrorTypeTag = 
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

export interface XFTPErrorType {
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
