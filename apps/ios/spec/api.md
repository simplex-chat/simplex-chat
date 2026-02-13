# SimpleX Chat iOS -- Chat API Reference

> Complete specification of the ChatCommand, ChatResponse, ChatEvent, and ChatError types that form the API between the Swift UI layer and the Haskell core.
>
> Related specs: [Architecture](architecture.md) | [State Management](state.md) | [README](README.md)
> Related product: [Concept Index](../product/concepts.md)

---

## Table of Contents

1. [Overview](#1-overview)
2. [Command Categories (ChatCommand)](#2-command-categories)
3. [Response Types (ChatResponse)](#3-response-types)
4. [Event Types (ChatEvent)](#4-event-types)
5. [Error Types (ChatError)](#5-error-types)
6. [FFI Bridge Functions](#6-ffi-bridge-functions)
7. [Result Type (APIResult)](#7-result-type)

---

## 1. Overview

The iOS app communicates with the Haskell core exclusively through a command/response protocol:

1. Swift constructs a `ChatCommand` enum value
2. The command's `cmdString` property serializes it to a text command
3. The FFI bridge sends the string to Haskell via `chat_send_cmd_retry`
4. Haskell returns a JSON response, decoded as `APIResult<ChatResponse>`
5. Async events arrive separately via `chat_recv_msg_wait`, decoded as `ChatEvent`

**Source files**:
- `Shared/Model/AppAPITypes.swift` -- `ChatCommand`, `ChatResponse0/1/2`, `ChatEvent` enums
- `SimpleXChat/APITypes.swift` -- `APIResult<R>`, `ChatError`, `ChatCmdProtocol` protocol
- `Shared/Model/SimpleXAPI.swift` -- FFI bridge functions
- `SimpleXChat/ChatTypes.swift` -- Data types used in commands/responses (User, Contact, GroupInfo, ChatItem, etc.)

---

## 2. Command Categories

The `ChatCommand` enum (`Shared/Model/AppAPITypes.swift`) contains all commands the iOS app can send to the Haskell core. Commands are organized below by functional area.

### 2.1 User Management

| Command | Parameters | Description |
|---------|-----------|-------------|
| `showActiveUser` | -- | Get current active user |
| `createActiveUser` | `profile: Profile?, pastTimestamp: Bool` | Create new user profile |
| `listUsers` | -- | List all user profiles |
| `apiSetActiveUser` | `userId: Int64, viewPwd: String?` | Switch active user |
| `apiHideUser` | `userId: Int64, viewPwd: String` | Hide user behind password |
| `apiUnhideUser` | `userId: Int64, viewPwd: String` | Unhide hidden user |
| `apiMuteUser` | `userId: Int64` | Mute notifications for user |
| `apiUnmuteUser` | `userId: Int64` | Unmute notifications for user |
| `apiDeleteUser` | `userId: Int64, delSMPQueues: Bool, viewPwd: String?` | Delete user profile |
| `apiUpdateProfile` | `userId: Int64, profile: Profile` | Update user display name/image |
| `setAllContactReceipts` | `enable: Bool` | Set delivery receipts for all contacts |
| `apiSetUserContactReceipts` | `userId: Int64, userMsgReceiptSettings` | Per-user contact receipt settings |
| `apiSetUserGroupReceipts` | `userId: Int64, userMsgReceiptSettings` | Per-user group receipt settings |
| `apiSetUserAutoAcceptMemberContacts` | `userId: Int64, enable: Bool` | Auto-accept group member contacts |

### 2.2 Chat Lifecycle Control

| Command | Parameters | Description |
|---------|-----------|-------------|
| `startChat` | `mainApp: Bool, enableSndFiles: Bool` | Start chat engine |
| `checkChatRunning` | -- | Check if chat is running |
| `apiStopChat` | -- | Stop chat engine |
| `apiActivateChat` | `restoreChat: Bool` | Resume from background |
| `apiSuspendChat` | `timeoutMicroseconds: Int` | Suspend for background |
| `apiSetAppFilePaths` | `filesFolder, tempFolder, assetsFolder` | Set file storage paths |
| `apiSetEncryptLocalFiles` | `enable: Bool` | Toggle local file encryption |

### 2.3 Chat & Message Operations

| Command | Parameters | Description |
|---------|-----------|-------------|
| `apiGetChats` | `userId: Int64` | Get all chat previews for user |
| `apiGetChat` | `chatId, scope, contentTag, pagination, search` | Get messages for a chat |
| `apiGetChatContentTypes` | `chatId, scope` | Get content type counts for a chat |
| `apiGetChatItemInfo` | `type, id, scope, itemId` | Get detailed info for a message |
| `apiSendMessages` | `type, id, scope, live, ttl, composedMessages` | Send one or more messages |
| `apiCreateChatItems` | `noteFolderId, composedMessages` | Create items in notes folder |
| `apiUpdateChatItem` | `type, id, scope, itemId, updatedMessage, live` | Edit a sent message |
| `apiDeleteChatItem` | `type, id, scope, itemIds, mode` | Delete messages |
| `apiDeleteMemberChatItem` | `groupId, itemIds` | Moderate group messages |
| `apiChatItemReaction` | `type, id, scope, itemId, add, reaction` | Add/remove emoji reaction |
| `apiGetReactionMembers` | `userId, groupId, itemId, reaction` | Get who reacted |
| `apiPlanForwardChatItems` | `fromChatType, fromChatId, fromScope, itemIds` | Plan message forwarding |
| `apiForwardChatItems` | `toChatType, toChatId, toScope, from..., itemIds, ttl` | Forward messages |
| `apiReportMessage` | `groupId, chatItemId, reportReason, reportText` | Report group message |
| `apiChatRead` | `type, id, scope` | Mark entire chat as read |
| `apiChatItemsRead` | `type, id, scope, itemIds` | Mark specific items as read |
| `apiChatUnread` | `type, id, unreadChat` | Toggle unread badge |

### 2.4 Contact Management

| Command | Parameters | Description |
|---------|-----------|-------------|
| `apiAddContact` | `userId, incognito` | Create invitation link |
| `apiConnect` | `userId, incognito, connLink` | Connect via link |
| `apiConnectPlan` | `userId, connLink` | Plan connection (preview) |
| `apiPrepareContact` | `userId, connLink, contactShortLinkData` | Prepare contact from link |
| `apiConnectPreparedContact` | `contactId, incognito, msg` | Connect prepared contact |
| `apiConnectContactViaAddress` | `userId, incognito, contactId` | Connect via address |
| `apiAcceptContact` | `incognito, contactReqId` | Accept contact request |
| `apiRejectContact` | `contactReqId` | Reject contact request |
| `apiDeleteChat` | `type, id, chatDeleteMode` | Delete conversation |
| `apiClearChat` | `type, id` | Clear conversation history |
| `apiListContacts` | `userId` | List all contacts |
| `apiSetContactPrefs` | `contactId, preferences` | Set contact preferences |
| `apiSetContactAlias` | `contactId, localAlias` | Set local alias |
| `apiSetConnectionAlias` | `connId, localAlias` | Set pending connection alias |
| `apiContactInfo` | `contactId` | Get contact info + connection stats |
| `apiSetConnectionIncognito` | `connId, incognito` | Toggle incognito on pending connection |

### 2.5 Group Management

| Command | Parameters | Description |
|---------|-----------|-------------|
| `apiNewGroup` | `userId, incognito, groupProfile` | Create new group |
| `apiAddMember` | `groupId, contactId, memberRole` | Invite contact to group |
| `apiJoinGroup` | `groupId` | Accept group invitation |
| `apiAcceptMember` | `groupId, groupMemberId, memberRole` | Accept member (knocking) |
| `apiRemoveMembers` | `groupId, memberIds, withMessages` | Remove members |
| `apiLeaveGroup` | `groupId` | Leave group |
| `apiListMembers` | `groupId` | List group members |
| `apiUpdateGroupProfile` | `groupId, groupProfile` | Update group name/image/description |
| `apiMembersRole` | `groupId, memberIds, memberRole` | Change member roles |
| `apiBlockMembersForAll` | `groupId, memberIds, blocked` | Block members for all |
| `apiCreateGroupLink` | `groupId, memberRole` | Create shareable group link |
| `apiGroupLinkMemberRole` | `groupId, memberRole` | Change group link default role |
| `apiDeleteGroupLink` | `groupId` | Delete group link |
| `apiGetGroupLink` | `groupId` | Get existing group link |
| `apiAddGroupShortLink` | `groupId` | Add short link to group |
| `apiCreateMemberContact` | `groupId, groupMemberId` | Create direct contact from group member |
| `apiSendMemberContactInvitation` | `contactId, msg` | Send contact invitation to member |
| `apiGroupMemberInfo` | `groupId, groupMemberId` | Get member info + connection stats |
| `apiDeleteMemberSupportChat` | `groupId, groupMemberId` | Delete member support chat |
| `apiSetMemberSettings` | `groupId, groupMemberId, memberSettings` | Set per-member settings |
| `apiSetGroupAlias` | `groupId, localAlias` | Set local group alias |

### 2.6 Chat Tags

| Command | Parameters | Description |
|---------|-----------|-------------|
| `apiGetChatTags` | `userId` | Get all user tags |
| `apiCreateChatTag` | `tag: ChatTagData` | Create a new tag |
| `apiSetChatTags` | `type, id, tagIds` | Assign tags to a chat |
| `apiDeleteChatTag` | `tagId` | Delete a tag |
| `apiUpdateChatTag` | `tagId, tagData` | Update tag name/emoji |
| `apiReorderChatTags` | `tagIds` | Reorder tags |

### 2.7 File Operations

| Command | Parameters | Description |
|---------|-----------|-------------|
| `receiveFile` | `fileId, userApprovedRelays, encrypted, inline` | Accept and download file |
| `setFileToReceive` | `fileId, userApprovedRelays, encrypted` | Mark file for auto-receive |
| `cancelFile` | `fileId` | Cancel file transfer |
| `apiUploadStandaloneFile` | `userId, file: CryptoFile` | Upload file to XFTP (no chat) |
| `apiDownloadStandaloneFile` | `userId, url, file: CryptoFile` | Download from XFTP URL |
| `apiStandaloneFileInfo` | `url` | Get file metadata from XFTP URL |

### 2.8 WebRTC Call Operations

| Command | Parameters | Description |
|---------|-----------|-------------|
| `apiSendCallInvitation` | `contact, callType` | Initiate call |
| `apiRejectCall` | `contact` | Reject incoming call |
| `apiSendCallOffer` | `contact, callOffer: WebRTCCallOffer` | Send SDP offer |
| `apiSendCallAnswer` | `contact, answer: WebRTCSession` | Send SDP answer |
| `apiSendCallExtraInfo` | `contact, extraInfo: WebRTCExtraInfo` | Send ICE candidates |
| `apiEndCall` | `contact` | End active call |
| `apiGetCallInvitations` | -- | Get pending call invitations |
| `apiCallStatus` | `contact, callStatus` | Report call status change |

### 2.9 Push Notifications

| Command | Parameters | Description |
|---------|-----------|-------------|
| `apiGetNtfToken` | -- | Get current notification token |
| `apiRegisterToken` | `token, notificationMode` | Register device token with server |
| `apiVerifyToken` | `token, nonce, code` | Verify token registration |
| `apiCheckToken` | `token` | Check token status |
| `apiDeleteToken` | `token` | Unregister token |
| `apiGetNtfConns` | `nonce, encNtfInfo` | Get notification connections (NSE) |
| `apiGetConnNtfMessages` | `connMsgReqs` | Get notification messages (NSE) |

### 2.10 Settings & Configuration

| Command | Parameters | Description |
|---------|-----------|-------------|
| `apiSaveSettings` | `settings: AppSettings` | Save app settings to core |
| `apiGetSettings` | `settings: AppSettings` | Get settings from core |
| `apiSetChatSettings` | `type, id, chatSettings` | Per-chat notification settings |
| `apiSetChatItemTTL` | `userId, seconds` | Set global message TTL |
| `apiGetChatItemTTL` | `userId` | Get global message TTL |
| `apiSetChatTTL` | `userId, type, id, seconds` | Per-chat message TTL |
| `apiSetNetworkConfig` | `networkConfig: NetCfg` | Set network configuration |
| `apiGetNetworkConfig` | -- | Get network configuration |
| `apiSetNetworkInfo` | `networkInfo: UserNetworkInfo` | Set network type/status |
| `reconnectAllServers` | -- | Force reconnect all servers |
| `reconnectServer` | `userId, smpServer` | Reconnect specific server |

### 2.11 Database & Storage

| Command | Parameters | Description |
|---------|-----------|-------------|
| `apiStorageEncryption` | `config: DBEncryptionConfig` | Set/change database encryption |
| `testStorageEncryption` | `key: String` | Test encryption key |
| `apiExportArchive` | `config: ArchiveConfig` | Export database archive |
| `apiImportArchive` | `config: ArchiveConfig` | Import database archive |
| `apiDeleteStorage` | -- | Delete all storage |

### 2.12 Server Operations

| Command | Parameters | Description |
|---------|-----------|-------------|
| `apiGetServerOperators` | -- | Get server operators |
| `apiSetServerOperators` | `operators` | Set server operators |
| `apiGetUserServers` | `userId` | Get user's configured servers |
| `apiSetUserServers` | `userId, userServers` | Set user's servers |
| `apiValidateServers` | `userId, userServers` | Validate server configuration |
| `apiGetUsageConditions` | -- | Get usage conditions |
| `apiAcceptConditions` | `conditionsId, operatorIds` | Accept usage conditions |
| `apiTestProtoServer` | `userId, server` | Test server connectivity |

### 2.13 Theme & UI

| Command | Parameters | Description |
|---------|-----------|-------------|
| `apiSetUserUIThemes` | `userId, themes: ThemeModeOverrides?` | Set per-user theme |
| `apiSetChatUIThemes` | `chatId, themes: ThemeModeOverrides?` | Set per-chat theme |

### 2.14 Remote Desktop

| Command | Parameters | Description |
|---------|-----------|-------------|
| `setLocalDeviceName` | `displayName` | Set device name for pairing |
| `connectRemoteCtrl` | `xrcpInvitation` | Connect to desktop via QR code |
| `findKnownRemoteCtrl` | -- | Find previously paired desktops |
| `confirmRemoteCtrl` | `remoteCtrlId` | Confirm known remote controller |
| `verifyRemoteCtrlSession` | `sessionCode` | Verify session code |
| `listRemoteCtrls` | -- | List known remote controllers |
| `stopRemoteCtrl` | -- | Stop remote session |
| `deleteRemoteCtrl` | `remoteCtrlId` | Delete known controller |

### 2.15 Diagnostics

| Command | Parameters | Description |
|---------|-----------|-------------|
| `showVersion` | -- | Get core version info |
| `getAgentSubsTotal` | `userId` | Get total SMP subscriptions |
| `getAgentServersSummary` | `userId` | Get server summary stats |
| `resetAgentServersStats` | -- | Reset server statistics |

### 2.16 Address Management

| Command | Parameters | Description |
|---------|-----------|-------------|
| `apiCreateMyAddress` | `userId` | Create SimpleX address |
| `apiDeleteMyAddress` | `userId` | Delete SimpleX address |
| `apiShowMyAddress` | `userId` | Show current address |
| `apiAddMyAddressShortLink` | `userId` | Add short link to address |
| `apiSetProfileAddress` | `userId, on: Bool` | Toggle address in profile |
| `apiSetAddressSettings` | `userId, addressSettings` | Configure address settings |

### 2.17 Connection Security

| Command | Parameters | Description |
|---------|-----------|-------------|
| `apiGetContactCode` | `contactId` | Get verification code |
| `apiGetGroupMemberCode` | `groupId, groupMemberId` | Get member verification code |
| `apiVerifyContact` | `contactId, connectionCode` | Verify contact identity |
| `apiVerifyGroupMember` | `groupId, groupMemberId, connectionCode` | Verify group member identity |
| `apiSwitchContact` | `contactId` | Switch contact connection (key rotation) |
| `apiSwitchGroupMember` | `groupId, groupMemberId` | Switch group member connection |
| `apiAbortSwitchContact` | `contactId` | Abort contact switch |
| `apiAbortSwitchGroupMember` | `groupId, groupMemberId` | Abort member switch |
| `apiSyncContactRatchet` | `contactId, force` | Sync double-ratchet state |
| `apiSyncGroupMemberRatchet` | `groupId, groupMemberId, force` | Sync member ratchet |

---

## 3. Response Types

Responses are split across three enums due to Swift enum size limitations:

### ChatResponse0

Synchronous query responses:

| Response | Key Fields | Description |
|----------|-----------|-------------|
| `activeUser` | `user: User` | Current active user |
| `usersList` | `users: [UserInfo]` | All user profiles |
| `chatStarted` | -- | Chat engine started |
| `chatRunning` | -- | Chat is already running |
| `chatStopped` | -- | Chat engine stopped |
| `apiChats` | `user, chats: [ChatData]` | All chat previews |
| `apiChat` | `user, chat: ChatData, navInfo` | Single chat with messages |
| `chatTags` | `user, userTags: [ChatTag]` | User's chat tags |
| `chatItemInfo` | `user, chatItem, chatItemInfo` | Message detail info |
| `serverTestResult` | `user, testServer, testFailure` | Server test result |
| `networkConfig` | `networkConfig: NetCfg` | Current network config |
| `contactInfo` | `user, contact, connectionStats, customUserProfile` | Contact details |
| `groupMemberInfo` | `user, groupInfo, member, connectionStats` | Member details |
| `connectionVerified` | `verified, expectedCode` | Verification result |
| `tagsUpdated` | `user, userTags, chatTags` | Tags changed |

### ChatResponse1

Contact, message, and profile responses:

| Response | Key Fields | Description |
|----------|-----------|-------------|
| `invitation` | `user, connLinkInvitation, connection` | Created invitation link |
| `connectionPlan` | `user, connLink, connectionPlan` | Connection plan preview |
| `newPreparedChat` | `user, chat: ChatData` | Prepared contact/group |
| `contactDeleted` | `user, contact` | Contact deleted |
| `newChatItems` | `user, chatItems: [AChatItem]` | New messages sent/received |
| `chatItemUpdated` | `user, chatItem: AChatItem` | Message edited |
| `chatItemReaction` | `user, added, reaction` | Reaction change |
| `chatItemsDeleted` | `user, chatItemDeletions, byUser` | Messages deleted |
| `contactsList` | `user, contacts: [Contact]` | All contacts list |
| `userProfileUpdated` | `user, fromProfile, toProfile` | Profile changed |
| `userContactLinkCreated` | `user, connLinkContact` | Address created |
| `forwardPlan` | `user, chatItemIds, forwardConfirmation` | Forward plan result |
| `groupChatItemsDeleted` | `user, groupInfo, chatItemIDs, byUser, member_` | Group items deleted |

### ChatResponse2

Group, file, call, notification, and misc responses:

| Response | Key Fields | Description |
|----------|-----------|-------------|
| `groupCreated` | `user, groupInfo` | New group created |
| `sentGroupInvitation` | `user, groupInfo, contact, member` | Group invitation sent |
| `groupMembers` | `user, group: Group` | Group member list |
| `membersRoleUser` | `user, groupInfo, members, toRole` | Role changed |
| `groupUpdated` | `user, toGroup: GroupInfo` | Group profile updated |
| `groupLinkCreated` | `user, groupInfo, groupLink` | Group link created |
| `rcvFileAccepted` | `user, chatItem` | File download started |
| `callInvitations` | `callInvitations: [RcvCallInvitation]` | Pending calls |
| `ntfToken` | `token, status, ntfMode, ntfServer` | Notification token info |
| `versionInfo` | `versionInfo, chatMigrations, agentMigrations` | Core version |
| `cmdOk` | `user_` | Generic success |
| `archiveExported` | `archiveErrors: [ArchiveError]` | Export result |
| `archiveImported` | `archiveErrors: [ArchiveError]` | Import result |
| `appSettings` | `appSettings: AppSettings` | Retrieved settings |

---

## 4. Event Types

The `ChatEvent` enum (`Shared/Model/AppAPITypes.swift`) represents async events from the Haskell core. These arrive via `chat_recv_msg_wait` polling, not as responses to commands.

### Connection Events

| Event | Key Fields | Description |
|-------|-----------|-------------|
| `contactConnected` | `user, contact, userCustomProfile` | Contact connection established |
| `contactConnecting` | `user, contact` | Contact connecting in progress |
| `contactSndReady` | `user, contact` | Ready to send to contact |
| `contactDeletedByContact` | `user, contact` | Contact deleted by other party |
| `contactUpdated` | `user, toContact` | Contact profile updated |
| `receivedContactRequest` | `user, contactRequest, chat_` | Incoming contact request |
| `subscriptionStatus` | `subscriptionStatus, connections` | Connection subscription change |

### Message Events

| Event | Key Fields | Description |
|-------|-----------|-------------|
| `newChatItems` | `user, chatItems: [AChatItem]` | New messages received |
| `chatItemUpdated` | `user, chatItem: AChatItem` | Message edited remotely |
| `chatItemReaction` | `user, added, reaction: ACIReaction` | Reaction added/removed |
| `chatItemsDeleted` | `user, chatItemDeletions, byUser` | Messages deleted |
| `chatItemsStatusesUpdated` | `user, chatItems: [AChatItem]` | Delivery status changed |
| `groupChatItemsDeleted` | `user, groupInfo, chatItemIDs, byUser, member_` | Group items deleted |
| `chatInfoUpdated` | `user, chatInfo` | Chat metadata changed |

### Group Events

| Event | Key Fields | Description |
|-------|-----------|-------------|
| `receivedGroupInvitation` | `user, groupInfo, contact, memberRole` | Group invitation received |
| `userAcceptedGroupSent` | `user, groupInfo, hostContact` | Joined group |
| `groupLinkConnecting` | `user, groupInfo, hostMember` | Connecting via group link |
| `joinedGroupMemberConnecting` | `user, groupInfo, hostMember, member` | Member joining |
| `memberRole` | `user, groupInfo, byMember, member, fromRole, toRole` | Role changed |
| `memberBlockedForAll` | `user, groupInfo, byMember, member, blocked` | Member blocked |
| `deletedMemberUser` | `user, groupInfo, member, withMessages` | Current user removed |
| `deletedMember` | `user, groupInfo, byMember, deletedMember` | Member removed |
| `leftMember` | `user, groupInfo, member` | Member left |
| `groupDeleted` | `user, groupInfo, member` | Group deleted |
| `userJoinedGroup` | `user, groupInfo` | Successfully joined |
| `joinedGroupMember` | `user, groupInfo, member` | New member joined |
| `connectedToGroupMember` | `user, groupInfo, member, memberContact` | E2E session established with member |
| `groupUpdated` | `user, toGroup: GroupInfo` | Group profile changed |
| `groupMemberUpdated` | `user, groupInfo, fromMember, toMember` | Member info updated |

### File Transfer Events

| Event | Key Fields | Description |
|-------|-----------|-------------|
| `rcvFileStart` | `user, chatItem` | Download started |
| `rcvFileProgressXFTP` | `user, chatItem_, receivedSize, totalSize` | Download progress |
| `rcvFileComplete` | `user, chatItem` | Download complete |
| `rcvFileSndCancelled` | `user, chatItem, rcvFileTransfer` | Sender cancelled |
| `rcvFileError` | `user, chatItem_, agentError, rcvFileTransfer` | Download error |
| `sndFileStart` | `user, chatItem, sndFileTransfer` | Upload started |
| `sndFileComplete` | `user, chatItem, sndFileTransfer` | Upload complete (inline) |
| `sndFileProgressXFTP` | `user, chatItem_, fileTransferMeta, sentSize, totalSize` | Upload progress |
| `sndFileCompleteXFTP` | `user, chatItem, fileTransferMeta` | XFTP upload complete |
| `sndFileError` | `user, chatItem_, fileTransferMeta, errorMessage` | Upload error |

### Call Events

| Event | Key Fields | Description |
|-------|-----------|-------------|
| `callInvitation` | `callInvitation: RcvCallInvitation` | Incoming call |
| `callOffer` | `user, contact, callType, offer, sharedKey, askConfirmation` | SDP offer received |
| `callAnswer` | `user, contact, answer` | SDP answer received |
| `callExtraInfo` | `user, contact, extraInfo` | ICE candidates received |
| `callEnded` | `user, contact` | Call ended by remote |

### Connection Security Events

| Event | Key Fields | Description |
|-------|-----------|-------------|
| `contactSwitch` | `user, contact, switchProgress` | Key rotation progress |
| `groupMemberSwitch` | `user, groupInfo, member, switchProgress` | Member key rotation |
| `contactRatchetSync` | `user, contact, ratchetSyncProgress` | Ratchet sync progress |
| `groupMemberRatchetSync` | `user, groupInfo, member, ratchetSyncProgress` | Member ratchet sync |

### System Events

| Event | Key Fields | Description |
|-------|-----------|-------------|
| `chatSuspended` | -- | Core suspended |

---

## 5. Error Types

Defined in `SimpleXChat/APITypes.swift`:

```swift
public enum ChatError: Decodable, Hashable {
    case error(errorType: ChatErrorType)
    case errorAgent(agentError: AgentErrorType)
    case errorStore(storeError: StoreError)
    case errorDatabase(databaseError: DatabaseError)
    case errorRemoteCtrl(remoteCtrlError: RemoteCtrlError)
    case invalidJSON(json: String)
    case unexpectedResult(type: String)
}
```

### Error Categories

| Category | Enum | Description |
|----------|------|-------------|
| Chat logic | `ChatErrorType` | Business logic errors (e.g., invalid state, permission denied) |
| SMP Agent | `AgentErrorType` | Protocol/network errors from the SMP agent layer |
| Database store | `StoreError` | SQLite query/constraint errors |
| Database engine | `DatabaseError` | DB open/migration/encryption errors |
| Remote control | `RemoteCtrlError` | Remote desktop session errors |
| Parse failure | `invalidJSON` | Failed to decode response JSON |
| Unexpected | `unexpectedResult` | Response type does not match expected |

---

## 6. FFI Bridge Functions

Defined in `Shared/Model/SimpleXAPI.swift`:

### Synchronous (blocking current thread)

```swift
// Throws on error, returns typed result
func chatSendCmdSync<R: ChatAPIResult>(
    _ cmd: ChatCommand,
    bgTask: Bool = true,
    bgDelay: Double? = nil,
    ctrl: chat_ctrl? = nil,
    log: Bool = true
) throws -> R

// Returns APIResult (caller handles error)
func chatApiSendCmdSync<R: ChatAPIResult>(
    _ cmd: ChatCommand,
    bgTask: Bool = true,
    bgDelay: Double? = nil,
    ctrl: chat_ctrl? = nil,
    retryNum: Int32 = 0,
    log: Bool = true
) -> APIResult<R>
```

### Asynchronous (Swift concurrency)

```swift
// Throws on error, returns typed result
func chatSendCmd<R: ChatAPIResult>(
    _ cmd: ChatCommand,
    bgTask: Bool = true,
    bgDelay: Double? = nil,
    ctrl: chat_ctrl? = nil,
    log: Bool = true
) async throws -> R

// Returns APIResult with optional retry on network errors
func chatApiSendCmdWithRetry<R: ChatAPIResult>(
    _ cmd: ChatCommand,
    bgTask: Bool = true,
    bgDelay: Double? = nil,
    inProgress: BoxedValue<Bool>? = nil,
    retryNum: Int32 = 0
) async -> APIResult<R>?
```

### Low-Level FFI

```swift
// Direct C FFI call -- serializes cmd.cmdString, calls chat_send_cmd_retry, decodes response
func sendSimpleXCmd<R: ChatAPIResult>(
    _ cmd: ChatCmdProtocol,
    _ ctrl: chat_ctrl?,
    retryNum: Int32 = 0
) -> APIResult<R>
```

---

## 7. Result Type

Defined in `SimpleXChat/APITypes.swift`:

```swift
public enum APIResult<R>: Decodable where R: Decodable, R: ChatAPIResult {
    case result(R)           // Successful response
    case error(ChatError)    // Error response from core
    case invalid(type: String, json: Data)  // Undecodable response

    public var responseType: String { ... }
    public var unexpected: ChatError { ... }
}

public protocol ChatAPIResult: Decodable {
    var responseType: String { get }
    var details: String { get }
    static func fallbackResult(_ type: String, _ json: NSDictionary) -> Self?
}
```

The `decodeAPIResult<R>` function handles JSON decoding with fallback logic:
1. Try standard `JSONDecoder.decode(APIResult<R>.self, from: data)`
2. If that fails, try manual JSON parsing via `JSONSerialization`
3. Check for `"error"` key -- return `.error`
4. Check for `"result"` key -- try `R.fallbackResult` or return `.invalid`
5. Last resort: return `.invalid(type: "invalid", json: ...)`

---

## Source Files

| File | Path |
|------|------|
| ChatCommand enum | `Shared/Model/AppAPITypes.swift` |
| ChatResponse0/1/2 enums | `Shared/Model/AppAPITypes.swift` |
| ChatEvent enum | `Shared/Model/AppAPITypes.swift` |
| APIResult, ChatError | `SimpleXChat/APITypes.swift` |
| FFI bridge functions | `Shared/Model/SimpleXAPI.swift` |
| Data types | `SimpleXChat/ChatTypes.swift` |
| C header | `SimpleXChat/SimpleX.h` |
| Haskell controller | `../../src/Simplex/Chat/Controller.hs` |
