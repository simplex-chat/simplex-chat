# Domain Term Glossary -- SimpleX Chat (Android & Desktop, Kotlin Multiplatform)

This glossary is self-contained and covers the Android and Desktop (Kotlin/Compose Multiplatform) codebase only.

---

## Table of Contents

1. [Protocols & Cryptography](#1-protocols--cryptography)
2. [Core Data Types](#2-core-data-types)
3. [Commands & Events](#3-commands--events)
4. [Connection & Identity](#4-connection--identity)
5. [Messaging Features](#5-messaging-features)
6. [Calling & Media](#6-calling--media)
7. [Notifications & Background](#7-notifications--background)
8. [Application Architecture](#8-application-architecture)
9. [Configuration & Preferences](#9-configuration--preferences)

---

## 1. Protocols & Cryptography

### SMP (SimpleX Messaging Protocol)
The core message-relay protocol. Clients send and receive messages through SMP relay servers without exposing sender/receiver identity correlation. The protocol uses unidirectional queues -- each contact pair maintains separate send and receive queues on potentially different servers.

*See:* `common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt` -- `SMPErrorType`, `SMPProxyMode`, `SMPProxyFallback`, `SMPWebPortServers`

### XFTP (SimpleX File Transfer Protocol)
Protocol for transferring files through relay servers. Files are chunked, encrypted, and uploaded to XFTP relays. Recipients download chunks and reassemble locally. Supports inline transfer for small files.

*See:* `common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt` -- `CC.ApiUploadStandaloneFile`, `CC.ApiDownloadStandaloneFile`, `CC.ApiStandaloneFileInfo`

### E2E Encryption (End-to-End Encryption)
All messages are encrypted end-to-end. The app never transmits plaintext to relay servers. Encryption keys are negotiated during connection establishment using X3DH-like key agreement and then maintained via the double-ratchet algorithm.

### Double Ratchet
The core key-management algorithm. After initial key agreement, each message derives a new symmetric key, providing forward secrecy per message. Ratchet state can be re-synchronized via `APISyncContactRatchet` / `APISyncGroupMemberRatchet` commands.

*See:* `SimpleXAPI.kt` -- `CC.APISyncContactRatchet(contactId, force)`, `CC.APISyncGroupMemberRatchet(groupId, groupMemberId, force)`, `CR.ContactRatchetSync`, `CR.GroupMemberRatchetSync`

### PQ (Post-Quantum)
Post-quantum key exchange support. Connections track PQ state via `Connection.pqSupport`, `Connection.pqEncryption`, `Connection.pqSndEnabled`, and `Connection.pqRcvEnabled` fields. When both peers support PQ, the key exchange incorporates a post-quantum KEM to resist future quantum attacks.

*See:* `ChatModel.kt` -- `Connection.pqSupport`, `Connection.pqEncryption`; `SimpleXAPI.kt` -- `SHARED_PREFS_PQ_EXPERIMENTAL_ENABLED` (legacy, no longer used)

### SMP Proxy / Private Routing
Messages can be sent through an intermediate SMP proxy relay to hide the sender's IP from the destination relay. Controlled by `SMPProxyMode` (Always, Unknown, Unprotected, Never) and `SMPProxyFallback` (Allow, AllowProtected, Prohibit).

*See:* `SimpleXAPI.kt` -- `AppPreferences.networkSMPProxyMode`, `AppPreferences.networkSMPProxyFallback`

### Transport Session Mode
Controls how TCP sessions to SMP relays are multiplexed. Options: `User` (one session per user profile), `Session` (single shared session), `Server` (one per server), `Entity` (one per queue/entity -- maximum metadata protection).

*See:* `SimpleXAPI.kt` -- `AppPreferences.networkSessionMode`, `TransportSessionMode`

---

## 2. Core Data Types

### ChatItem
A single item in a conversation -- a sent or received message, call event, group event, connection event, feature change, or moderation action. Contains direction (`CIDirection`), metadata (`CIMeta`), content (`CIContent`), optional formatted text, mentions, quoted item, reactions, and file attachment.

*See:* `ChatModel.kt:2715` -- `data class ChatItem`

### ChatInfo
The top-level discriminated union representing a conversation. Variants:
- `ChatInfo.Direct` -- wraps a `Contact`
- `ChatInfo.Group` -- wraps a `GroupInfo`
- `ChatInfo.Local` -- wraps a `NoteFolder` (saved messages / notes to self)
- `ChatInfo.ContactRequest` -- wraps a `UserContactRequest`
- `ChatInfo.ContactConnection` -- wraps a `PendingContactConnection`
- `ChatInfo.InvalidJSON` -- fallback for unrecognized data

*See:* `ChatModel.kt:1386` -- `sealed class ChatInfo`

### CIContent (Chat Item Content)
The content payload of a `ChatItem`. Over 30 variants including:
- `SndMsgContent` / `RcvMsgContent` -- regular message with `MsgContent`
- `SndCall` / `RcvCall` -- call event with status and duration
- `RcvIntegrityError` -- message integrity violation
- `RcvDecryptionError` -- decryption failure with error type and count
- `RcvGroupInvitation` / `SndGroupInvitation` -- group invite
- `RcvGroupEventContent` / `SndGroupEventContent` -- group lifecycle events
- `RcvChatFeature` / `SndChatFeature` -- per-chat feature toggle notifications
- `SndModerated` / `RcvModerated` / `RcvBlocked` -- moderation events
- `RcvDirectEventContent` -- direct chat lifecycle events

*See:* `ChatModel.kt:3549` -- `sealed class CIContent`

### MsgContent
The wire-format message body. Variants: `MCText`, `MCLink`, `MCImage`, `MCVideo`, `MCVoice`, `MCFile`, `MCReport`, `MCUnknown`. Each carries text plus optional media/file metadata.

*See:* `ChatModel.kt` -- `sealed class MsgContent`

### User
The local user profile. Fields: `userId`, `userContactId`, `localDisplayName`, `profile` (LocalProfile), `fullPreferences` (FullChatPreferences), `activeUser`, `activeOrder`, `showNtfs`, `sendRcptsContacts`, `sendRcptsSmallGroups`, `viewPwdHash` (for hidden profiles), `uiThemes`.

*See:* `ChatModel.kt:1206` -- `data class User`

### Contact
A remote contact. Fields: `contactId`, `localDisplayName`, `profile` (LocalProfile), `activeConn` (Connection?), `viaGroup`, `contactUsed`, `contactStatus`, `chatSettings`, `userPreferences`, `mergedPreferences`, `preparedContact`, `contactRequestId`, `contactGroupMemberId`, `chatTags`, `chatItemTTL`.

*See:* `ChatModel.kt:1706` -- `data class Contact`

### GroupInfo
Metadata for a group conversation. Fields: `groupId`, `localDisplayName`, `groupProfile` (GroupProfile), `businessChat` (BusinessChatInfo?), `fullGroupPreferences`, `membership` (GroupMember -- the local user's membership), `chatSettings`, `preparedGroup`, `membersRequireAttention`, `chatTags`, `chatItemTTL`.

*See:* `ChatModel.kt:1999` -- `data class GroupInfo`

### GroupMember
A member of a group. Fields: `groupMemberId`, `groupId`, `memberId`, `memberRole` (GroupMemberRole), `memberCategory` (GroupMemberCategory), `memberStatus` (GroupMemberStatus), `memberSettings` (GroupMemberSettings), `blockedByAdmin`, `invitedBy`, `localDisplayName`, `memberProfile`, `memberContactId`, `memberContactProfileId`, `activeConn` (Connection?), `supportChat` (GroupSupportChat?).

*See:* `ChatModel.kt:2172` -- `data class GroupMember`

### GroupMemberRole
Enumeration of group roles, ordered for comparison: `Observer` < `Author` < `Member` < `Moderator` < `Admin` < `Owner`. Selectable roles for assignment: Observer, Member, Moderator, Admin, Owner.

*See:* `ChatModel.kt:2364` -- `enum class GroupMemberRole`

### Connection
An active or pending cryptographic connection to a peer. Fields: `connId`, `agentConnId`, `peerChatVRange` (VersionRange), `connStatus` (ConnStatus), `connLevel`, `viaGroupLink`, `customUserProfileId`, `connectionCode` (SecurityCode?), `pqSupport`, `pqEncryption`, `pqSndEnabled`, `pqRcvEnabled`, `connectionStats`, `authErrCounter`, `quotaErrCounter`.

*See:* `ChatModel.kt:1877` -- `data class Connection`

### Chat
A composite type holding `chatInfo` (ChatInfo), `chatItems` (list of ChatItem), and `chatStats` (ChatStats -- unread count, min unread item ID, etc.). Represents a full conversation for the chat list.

*See:* `ChatModel.kt` -- `data class Chat`

### PendingContactConnection
Represents an in-progress connection that has not yet been established. Contains the connection link and state but no contact profile yet.

*See:* `ChatModel.kt` -- referenced in `ChatInfo.ContactConnection`

### CryptoFile
A file reference that optionally carries `CryptoFileArgs` (key + nonce) for local encryption. `CryptoFile.plain(path)` creates an unencrypted reference.

*See:* `ChatModel.kt` -- `data class CryptoFile`

---

## 3. Commands & Events

The codebase uses short type names for the command/event protocol: `CC` (Chat Command), `CR` (Chat Response -- also carries asynchronous events), `API` (top-level response wrapper), and `ChatError` (error hierarchy). There is no separate "ChatEvent" class; asynchronous events from the core (new messages, connection changes, call signaling) are all `CR` subclasses received via the `recvMsg` loop.

### CC (Chat Command)
The sealed class representing all commands the app can send to the Haskell core library. Over 140 command variants organized by domain:

**User management:** `ShowActiveUser`, `CreateActiveUser`, `ListUsers`, `ApiSetActiveUser`, `ApiHideUser`, `ApiUnhideUser`, `ApiMuteUser`, `ApiUnmuteUser`, `ApiDeleteUser`

**Chat lifecycle:** `StartChat`, `CheckChatRunning`, `ApiStopChat`, `ApiSetAppFilePaths`, `ApiSetEncryptLocalFiles`

**Database:** `ApiExportArchive`, `ApiImportArchive`, `ApiDeleteStorage`, `ApiStorageEncryption`, `TestStorageEncryption`

**Messaging:** `ApiSendMessages`, `ApiUpdateChatItem`, `ApiDeleteChatItem`, `ApiDeleteMemberChatItem`, `ApiChatItemReaction`, `ApiForwardChatItems`, `ApiPlanForwardChatItems`, `ApiReportMessage`

**Groups:** `ApiNewGroup`, `ApiAddMember`, `ApiJoinGroup`, `ApiAcceptMember`, `ApiMembersRole`, `ApiBlockMembersForAll`, `ApiRemoveMembers`, `ApiLeaveGroup`, `ApiListMembers`, `ApiUpdateGroupProfile`, `APICreateGroupLink`, `APIDeleteGroupLink`, `APIGetGroupLink`, `ApiAddGroupShortLink`

**Connections:** `APIAddContact`, `APIConnect`, `APIConnectPlan`, `APIPrepareContact`, `APIPrepareGroup`, `APIConnectPreparedContact`, `APIConnectPreparedGroup`, `ApiConnectContactViaAddress`

**Contacts:** `ApiDeleteChat`, `ApiClearChat`, `ApiListContacts`, `ApiUpdateProfile`, `ApiSetContactPrefs`, `ApiSetContactAlias`

**Address:** `ApiCreateMyAddress`, `ApiDeleteMyAddress`, `ApiShowMyAddress`, `ApiAddMyAddressShortLink`, `ApiSetProfileAddress`, `ApiSetAddressSettings`

**Calls:** `ApiGetCallInvitations`, `ApiSendCallInvitation`, `ApiRejectCall`, `ApiSendCallOffer`, `ApiSendCallAnswer`, `ApiSendCallExtraInfo`, `ApiEndCall`, `ApiCallStatus`

**Server config:** `ApiGetServerOperators`, `ApiSetServerOperators`, `ApiGetUserServers`, `ApiSetUserServers`, `ApiValidateServers`, `APITestProtoServer`

**Network:** `APISetNetworkConfig`, `APIGetNetworkConfig`, `APISetNetworkInfo`, `ReconnectServer`, `ReconnectAllServers`

**Files:** `ReceiveFile`, `CancelFile`, `ApiUploadStandaloneFile`, `ApiDownloadStandaloneFile`, `ApiStandaloneFileInfo`

**Remote access:** `SetLocalDeviceName`, `ListRemoteHosts`, `StartRemoteHost`, `SwitchRemoteHost`, `StopRemoteHost`, `DeleteRemoteHost`, `StoreRemoteFile`, `GetRemoteFile`, `ConnectRemoteCtrl`, `FindKnownRemoteCtrl`, `ConfirmRemoteCtrl`, `VerifyRemoteCtrlSession`, `ListRemoteCtrls`, `StopRemoteCtrl`, `DeleteRemoteCtrl`

**Read status:** `ApiChatRead`, `ApiChatItemsRead`, `ApiChatUnread`

**Settings:** `APISetChatSettings`, `ApiSetMemberSettings`, `APISetChatItemTTL`, `APIGetChatItemTTL`, `APISetChatTTL`, `ApiSaveSettings`, `ApiGetSettings`

**Ratchet & verification:** `APISwitchContact`, `APISwitchGroupMember`, `APIAbortSwitchContact`, `APIAbortSwitchGroupMember`, `APISyncContactRatchet`, `APISyncGroupMemberRatchet`, `APIGetContactCode`, `APIGetGroupMemberCode`, `APIVerifyContact`, `APIVerifyGroupMember`

Each command variant has a `cmdString` property that serializes it to the text protocol consumed by the Haskell FFI.

*See:* `SimpleXAPI.kt:3522` -- `sealed class CC`

### CR (Chat Response)
The sealed class representing all responses / events received from the Haskell core. Over 130 response types. Examples:

- `ActiveUser`, `UsersList` -- user management results
- `ChatStarted`, `ChatRunning`, `ChatStopped` -- lifecycle
- `ApiChats`, `ApiChat` -- chat list data
- `NewChatItems`, `ChatItemUpdated`, `ChatItemsDeleted` -- message events
- `ContactConnected`, `ContactConnecting`, `ContactSndReady` -- connection lifecycle
- `GroupCreated`, `ReceivedGroupInvitation`, `JoinedGroupMemberConnecting`, `MemberAccepted` -- group events
- `RcvFileStart`, `RcvFileComplete`, `SndFileComplete` -- file transfer progress
- `CallInvitation`, `CallOffer`, `CallAnswer`, `CallExtraInfo`, `CallEnded` -- call signaling
- `ChatError` -- error wrapper

*See:* `SimpleXAPI.kt:6103` -- `sealed class CR`

### API
The top-level response wrapper. Two variants:
- `API.Result(remoteHostId, res: CR)` -- successful response
- `API.Error(remoteHostId, err: ChatError)` -- error response

Properties: `ok` (Boolean -- true if `CR.CmdOk`), `result` (CR?), `rhId` (Long? -- remote host ID).

*See:* `SimpleXAPI.kt:5965` -- `sealed class API`

### ChatError
The error hierarchy returned from the Haskell core:
- `ChatErrorChat(errorType: ChatErrorType)` -- application-level errors (NoActiveUser, UserUnknown, DifferentActiveUser, etc.)
- `ChatErrorAgent(agentError: AgentErrorType)` -- SMP agent errors (BROKER, SMP, PROXY, etc.)
- `ChatErrorStore(storeError: StoreError)` -- database/store errors
- `ChatErrorDatabase(databaseError: DatabaseError)` -- database migration/encryption errors
- `ChatErrorRemoteHost(remoteHostError)` -- remote host control errors
- `ChatErrorRemoteCtrl(remoteCtrlError)` -- remote controller errors
- `ChatErrorInvalidJSON(json)` -- parse failure

*See:* `SimpleXAPI.kt:6962` -- `sealed class ChatError`

### sendCmd / recvMsg
The core FFI bridge. `sendCmd(rhId, cmd)` serializes a `CC` command and sends it to the Haskell backend via `chatSendCmd`. `recvMsg(ctrl)` blocks on `chatRecvMsg` to receive the next `API` response/event. The receiver loop runs in `ChatController.startReceiver()` on `Dispatchers.IO`.

*See:* `SimpleXAPI.kt` -- `ChatController.sendCmd()`, `ChatController.startReceiver()`

---

## 4. Connection & Identity

### SimpleX Address (User Address)
A long-lived contact address that others can use to send connection requests. Created via `ApiCreateMyAddress`, retrieved via `ApiShowMyAddress`, deleted via `ApiDeleteMyAddress`. Can optionally include a short link (`ApiAddMyAddressShortLink`). Stored as `ChatModel.userAddress` (`UserContactLinkRec`).

### Contact Link / Connection Link
A one-time or reusable invitation link. The `CreatedConnLink` type wraps the link string. Contact links can be one-time (single use) or long-lived (user address). Created via `APIAddContact` (one-time) or `ApiCreateMyAddress` (reusable).

### Group Link
A reusable invitation link for joining a group. Created via `APICreateGroupLink(groupId, memberRole)`. The default role for new members joining via the link is configurable. Can also have a short link variant via `ApiAddGroupShortLink`.

### Short Link
A compact form of a contact or group link. Created via `ApiAddMyAddressShortLink` (for user addresses) or `ApiAddGroupShortLink` (for groups). Short links resolve to the full connection link data including `ContactShortLinkData` or `GroupShortLinkData`.

### Incognito Mode
When enabled (`AppPreferences.incognito`), the app generates a random profile name for new connections instead of using the user's real profile. Each connection gets a unique random identity. The `customUserProfileId` on a `Connection` tracks which incognito profile is used for that connection.

*See:* `SimpleXAPI.kt` -- `AppPreferences.incognito`; `ChatModel.kt` -- `Connection.customUserProfileId`

### Hidden Profile
A user profile protected by a password (`viewPwdHash`). Hidden profiles do not appear in the profile list unless unlocked with the password. Created via `ApiHideUser(userId, viewPwd)`, revealed via `ApiUnhideUser(userId, viewPwd)`. When switching away from a hidden profile, its notifications are cancelled.

*See:* `SimpleXAPI.kt` -- `CC.ApiHideUser`, `CC.ApiUnhideUser`; `ChatModel.kt` -- `User.viewPwdHash`

### Connection Verification (Security Code)
Each connection has an optional `SecurityCode` (`Connection.connectionCode`). Users can verify connections out-of-band by comparing security codes displayed via `APIGetContactCode` / `APIGetGroupMemberCode` and confirming via `APIVerifyContact` / `APIVerifyGroupMember`.

### Connection Plan
Before connecting via a link, `APIConnectPlan` analyzes the link and returns a `ConnectionPlan` indicating whether the link leads to an existing contact, a new contact, a group join, etc. This prevents duplicate connections.

*See:* `SimpleXAPI.kt` -- `CC.APIConnectPlan`, `CR.CRConnectionPlan`

### Prepared Contact / Prepared Group
An intermediate state in the connection flow. `APIPrepareContact` / `APIPrepareGroup` creates the local record and displays the contact/group preview before the user confirms the connection. The user can then change the active profile (`APIChangePreparedContactUser` / `APIChangePreparedGroupUser`) and finally confirm via `APIConnectPreparedContact` / `APIConnectPreparedGroup`.

---

## 5. Messaging Features

### Delivery Receipt
Confirmation that a message was delivered to the recipient's device. Controlled per-user via `sendRcptsContacts` and `sendRcptsSmallGroups` on `User`. The global setting flow is triggered by `ChatModel.setDeliveryReceipts`. Individual overrides per-contact are managed via `ApiSetUserContactReceipts` / `ApiSetUserGroupReceipts`.

*See:* `SimpleXAPI.kt` -- `CC.SetAllContactReceipts`, `CC.ApiSetUserContactReceipts`, `CC.ApiSetUserGroupReceipts`; `AppPreferences.privacyDeliveryReceiptsSet`

### Timed Message (Disappearing Message)
Messages with a time-to-live after which they are automatically deleted. Configured as a `ChatFeature` / `GroupFeature` with a TTL parameter in seconds. The `customDisappearingMessageTime` preference stores the last custom duration used. Per-chat TTL can be set via `APISetChatTTL`. Global TTL via `APISetChatItemTTL`.

*See:* `SimpleXAPI.kt` -- `CC.APISetChatItemTTL`, `CC.APISetChatTTL`; `AppPreferences.customDisappearingMessageTime`

### Live Message
A message that updates in real-time as the sender types. Controlled by `CC.ApiSendMessages` with `live=true`. The `ComposeState.liveMessage` tracks the current live message being composed. An alert is shown on first use (`AppPreferences.liveMessageAlertShown`).

### Message Reactions
Emoji reactions on messages. Added/removed via `ApiChatItemReaction(type, id, scope, itemId, add, reaction)`. Reaction members in groups can be queried via `ApiGetReactionMembers`. Each `ChatItem` carries a `reactions: List<CIReactionCount>`.

### Message Forwarding
Messages can be forwarded between chats. `ApiPlanForwardChatItems` checks feasibility (e.g., file availability), and `ApiForwardChatItems` performs the forward. A `ForwardConfirmation` may be required if files need downloading first.

### Message Reports
Users can report messages in groups via `ApiReportMessage(groupId, chatItemId, reportReason, reportText)`. Admins can archive (`ApiArchiveReceivedReports`) or delete (`ApiDeleteReceivedReports`) reports.

### Mentions
In-message mentions of group members. Stored as `mentions: Map<String, CIMention>` on `ChatItem` and `mentions: MentionedMembers` on `ComposeState`.

### Link Previews
Automatic preview generation for URLs in messages. Controlled by `AppPreferences.privacyLinkPreviews`. An alert is shown on first use (`privacyLinkPreviewsShowAlert`).

### Local File Encryption
Files stored on device can be encrypted. Controlled by `AppPreferences.privacyEncryptLocalFiles` and toggled via `CC.ApiSetEncryptLocalFiles(enable)`.

### Chat Tags
User-defined tags for organizing conversations. CRUD via `ApiCreateChatTag`, `ApiUpdateChatTag`, `ApiDeleteChatTag`, `ApiReorderChatTags`. Assignment via `ApiSetChatTags`. The model tracks `userTags`, `presetTags` (system-defined categories), `unreadTags`, and the active filter (`activeChatTagFilter`).

---

## 6. Calling & Media

### WebRTC
The real-time communication framework used for audio and video calls. The app uses WebRTC for peer-to-peer media streams, with SMP used only for call signaling (offer/answer/ICE candidates).

### Call (data class)
Represents an active call session. Fields: `remoteHostId`, `userProfile`, `contact`, `callUUID`, `callState` (CallState enum), `initialCallType` (Audio/Video), `localMediaSources`, `localCapabilities`, `peerMediaSources`, `sharedKey` (for E2E call encryption), `connectionInfo`, `connectedAt`.

*See:* `common/src/commonMain/kotlin/chat/simplex/common/views/call/WebRTC.kt:14`

### CallState
Enum tracking call progression: `WaitCapabilities` -> `InvitationSent` / `InvitationAccepted` -> `OfferSent` / `OfferReceived` -> `Negotiated` -> `Connected` -> `Ended`.

### WCallCommand / WCallResponse
The command/response protocol between the Kotlin app and the WebRTC JavaScript layer:
- **Commands:** `Capabilities`, `Permission`, `Start`, `Offer`, `Answer`, `Ice`, `Media`, `Camera`, `Description`, `Layout`, `End`
- **Responses:** `Capabilities`, `Offer`, `Answer`, `Ice`, `Connection`, `Connected`, `PeerMedia`, `End`, `Ended`, `Ok`, `Error`

*See:* `WebRTC.kt:87` -- `sealed class WCallCommand`; `WebRTC.kt:102` -- `sealed class WCallResponse`

### CallManager
Manages incoming call invitations and the active call lifecycle. Handles reporting new incoming calls, accepting calls, switching between calls, and ending calls. Interacts with `ChatModel.callInvitations`, `ChatModel.activeCall`, and the platform notification manager.

*See:* `common/src/commonMain/kotlin/chat/simplex/common/views/call/CallManager.kt`

### Android: CallActivity
A dedicated Android `Activity` that displays the call UI. Launched when accepting an incoming call or initiating an outgoing call. Uses an Android `WebView` to host the WebRTC JavaScript.

*See:* `android/src/main/java/chat/simplex/app/views/call/CallActivity.kt`

### Android: CallService
An Android foreground `Service` that keeps the call alive when the app is in the background. Holds a `WakeLock`, displays an ongoing call notification, and manages the call lifecycle. Uses notification channel `CALL_SERVICE_NOTIFICATION`.

*See:* `android/src/main/java/chat/simplex/app/CallService.kt`

### Desktop: Browser-based WebRTC via NanoWSD
On Desktop, calls are implemented by opening the system browser to a locally-hosted WebSocket server. A `NanoHTTPD`/`NanoWSD` server runs on `localhost:50395`, serving the WebRTC call page and communicating with the Kotlin app via WebSocket messages. Commands are sent as JSON-serialized `WVAPICall` objects; responses are parsed as `WVAPIMessage` objects.

*See:* `common/src/desktopMain/kotlin/chat/simplex/common/views/call/CallView.desktop.kt`

### ICE Servers
STUN/TURN servers used for WebRTC NAT traversal. Configurable via `AppPreferences.webrtcIceServers`. The relay policy (`AppPreferences.webrtcPolicyRelay`) controls whether calls must use TURN relays (for IP privacy) or can attempt direct connections.

### CallMediaType
Enum: `Video`, `Audio`. Determines the initial media type of the call.

### CallMediaSource
Enum: `Mic`, `Camera`, `ScreenAudio`, `ScreenVideo`. Used in `WCallCommand.Media` to toggle individual media streams.

---

## 7. Notifications & Background

### Android: SimplexService
A foreground `Service` that keeps the chat backend running in the background. Uses a `WakeLock` and displays a persistent notification ("SimpleX Chat service" channel). Started with `START_STICKY` for automatic restart. Manages the `chatRecvMsg` loop indirectly by keeping the process alive.

Notification channel: `chat.simplex.app.SIMPLEX_SERVICE_NOTIFICATION` ("SimpleX Chat service")

*See:* `android/src/main/java/chat/simplex/app/SimplexService.kt`

### Android: MessagesFetcherWorker
A `WorkManager` periodic worker that wakes the app to fetch new messages when the foreground service is not running (i.e., when `NotificationsMode` is `PERIODIC`). Provides a battery-friendly alternative to the always-on service.

*See:* `android/src/main/java/chat/simplex/app/MessagesFetcherWorker.kt`

### Android: NotificationsMode
Enum controlling background message fetching:
- `OFF` -- no background activity; messages received only when app is open
- `PERIODIC` -- uses `MessagesFetcherWorker` for periodic fetches
- `SERVICE` -- uses `SimplexService` foreground service (default)

*See:* `SimpleXAPI.kt:7725` -- `enum class NotificationsMode`

### Android: Notification Channels
Android notification channels registered by the app:
- **Messages:** `chat.simplex.app.MESSAGE_NOTIFICATION` -- high importance, for incoming messages
- **Calls:** `chat.simplex.app.CALL_NOTIFICATION_2` -- high importance, for incoming call alerts with custom sound
- **Service:** `chat.simplex.app.SIMPLEX_SERVICE_NOTIFICATION` -- low importance, persistent foreground service indicator
- **Call Service:** `chat.simplex.app.CALL_SERVICE_NOTIFICATION` -- default importance, ongoing call indicator

*See:* `android/src/main/java/chat/simplex/app/model/NtfManager.android.kt`, `SimplexService.kt`, `CallService.kt`

### Android: NtfManager
The Android-specific notification manager. Handles creating notification channels, displaying message notifications (with grouping via `MessageGroup`), displaying incoming call notifications (with full-screen intent for lock-screen calls), and managing notification actions (accept/reject call, open chat).

*See:* `android/src/main/java/chat/simplex/app/model/NtfManager.android.kt`

### Desktop: System Notifications
On Desktop, notifications use the system notification mechanism (typically via the JVM's `SystemTray` or platform-specific notification APIs). The notification manager interface is shared (`ntfManager`) but the implementation is platform-specific.

### NotificationPreviewMode
Controls what information appears in notifications:
- `HIDDEN` -- no message content
- `CONTACT` -- shows sender name only
- `MESSAGE` -- shows sender name and message preview (default)

*See:* `ChatModel.kt:4818` -- `enum class NotificationPreviewMode`

### Wake Lock Management
In `ChatController.startReceiver()`, each received message acquires a wake lock (via `getWakeLock(timeout=60000)`) that is released after 30 seconds. This ensures the device stays awake long enough to process incoming messages and display notifications, particularly for incoming calls.

---

## 8. Application Architecture

### ChatController
The singleton controller that bridges the Kotlin UI layer and the Haskell core library. Responsibilities:
- Manages the `chatCtrl` (FFI handle to the Haskell runtime)
- Sends commands via `sendCmd()` and receives events via the `startReceiver()` coroutine loop
- Processes received messages in `processReceivedMsg()`
- Holds a reference to `AppPreferences` and `ChatModel`
- Provides the `messagesChannel` (Kotlin coroutine `Channel<API>`) for consumers to observe events
- Manages retry logic for transient network errors (`sendCmdWithRetry`)

*See:* `SimpleXAPI.kt:492` -- `object ChatController`

### ChatModel
The singleton reactive state container for the entire app. Uses Compose `mutableStateOf` and `mutableStateListOf` for reactive UI updates. Key state:
- `currentUser` -- the active user profile
- `users` -- list of all user profiles (`UserInfo`)
- `chatsContext` / `secondaryChatsContext` -- `ChatsContext` holding the chat list
- `chatId` -- currently open chat
- `groupMembers` -- members of the currently viewed group
- `callInvitations` -- pending incoming call invitations
- `activeCall` -- the currently active call
- `userAddress` -- the user's SimpleX address
- `chatItemTTL` -- global message TTL setting
- `userTags` -- chat tags
- `terminalItems` -- debug terminal log items
- Various UI state flags (`showCallView`, `switchingUsersAndHosts`, `clearOverlays`, etc.)

*See:* `ChatModel.kt:85` -- `object ChatModel`

### AppPreferences
A class wrapping platform-specific key-value storage (`Settings` from `com.russhwolf.settings`). On Android, backed by `SharedPreferences`. On Desktop, backed by Java `Properties` files. Provides type-safe accessors for all user preferences.

*See:* `SimpleXAPI.kt:93` -- `class AppPreferences`

### ComposeState
Data class holding the state of the message composition area. Fields: `message` (ComposeMessage), `parsedMessage` (formatted text), `liveMessage`, `preview` (ComposePreview), `contextItem` (ComposeContextItem -- reply/edit context), `inProgress`, `progressByTimeout`, `useLinkPreviews`, `mentions`.

*See:* `common/src/commonMain/kotlin/chat/simplex/common/views/chat/ComposeView.kt:96`

### ModalManager
Manages the modal/sheet presentation stack. Supports multiple placements (default, center, fullscreen, end). Holds an ordered list of `ModalViewHolder` items and exposes `showModal`, `showCustomModal`, `showModalCloseable`, `closeModal`. Uses Compose state (`modalCount`) to trigger recomposition.

*See:* `common/src/commonMain/kotlin/chat/simplex/common/views/helpers/ModalView.kt:92`

### AlertManager
Singleton for displaying alert dialogs. Provides `showAlertMsg`, `showAlertDialog`, `showAlertDialogButtons`, etc. Works with `AlertManager.shared` for the default instance.

### ChatsContext
Holds the chat list state for a particular scope (main or secondary). Manages `chats` (State<List<Chat>>), provides `updateChats()` to refresh, and supports filtering/keeping specific chats during updates.

### ConnectProgressManager
Tracks and displays connection progress in the UI. Methods: `startConnectProgress(text, onCancel)`, `stopConnectProgress()`, `cancelConnectProgress()`. Exposes `showConnectProgress` (nullable string indicating active progress text).

*See:* `ChatModel.kt:48` -- `object ConnectProgressManager`

### withBGApi / withLongRunningApi
Utility functions for launching coroutines on background threads. Used throughout the codebase to perform API calls without blocking the UI thread.

---

## 9. Configuration & Preferences

### AppPreferences (Storage)
All preferences are accessed through `ChatController.appPrefs`, which is a lazy-initialized `AppPreferences` instance. The underlying storage is:
- **Android:** `SharedPreferences` with ID `chat.simplex.app.SIMPLEX_APP_PREFS`
- **Desktop:** Java `Properties` files via `com.russhwolf.settings`

Theme overrides have separate storage (`SHARED_PREFS_THEMES_ID`).

### SharedPreference<T>
A generic wrapper providing `get()` and `set(value)` for a single preference. All `AppPreferences` fields are `SharedPreference<T>` instances created by factory methods (`mkBoolPreference`, `mkStrPreference`, `mkIntPreference`, `mkLongPreference`, `mkFloatPreference`, `mkEnumPreference`, `mkSafeEnumPreference`, `mkDatePreference`, `mkMapPreference`, `mkTimeoutPreference`).

### Key Preference Categories

**Notifications:**
- `notificationsMode` -- OFF / PERIODIC / SERVICE
- `notificationPreviewMode` -- HIDDEN / CONTACT / MESSAGE
- `canAskToEnableNotifications` -- gate for the notification prompt

**Privacy:**
- `privacyProtectScreen` -- prevents screenshots (Android FLAG_SECURE)
- `privacyAcceptImages` -- auto-accept inline images
- `privacyLinkPreviews` -- generate URL previews
- `privacySanitizeLinks` -- strip tracking parameters from URLs
- `privacyShowChatPreviews` -- show message preview in chat list
- `privacySaveLastDraft` -- persist draft messages
- `privacyEncryptLocalFiles` -- encrypt files at rest
- `privacyAskToApproveRelays` -- prompt before using relays suggested by contacts
- `privacyMediaBlurRadius` -- blur radius for media in notifications/previews

**Security:**
- `performLA` -- require local authentication (biometric/PIN)
- `laMode` -- local authentication mode
- `laLockDelay` -- seconds before re-locking
- `storeDBPassphrase` -- whether to persist the DB passphrase
- `initialRandomDBPassphrase` -- indicates the DB uses a random (non-user-chosen) passphrase
- `selfDestruct` -- enable self-destruct profile
- `selfDestructDisplayName` -- display name for the self-destruct profile

**Network:**
- `networkUseSocksProxy` -- route traffic through SOCKS proxy
- `networkProxy` -- SOCKS proxy host/port configuration
- `networkSessionMode` -- transport session multiplexing mode
- `networkSMPProxyMode` -- SMP proxy / private routing mode
- `networkSMPProxyFallback` -- fallback behavior when proxy fails
- `networkHostMode` -- onion/public host preference
- `networkRequiredHostMode` -- enforce host mode strictly
- Various TCP timeout settings (background, interactive, per-KB)
- Keep-alive settings (idle, interval, count)

**Calls:**
- `webrtcPolicyRelay` -- force TURN relay usage
- `callOnLockScreen` -- DISABLE / SHOW / ACCEPT calls on lock screen
- `webrtcIceServers` -- custom ICE server configuration
- `experimentalCalls` -- enable experimental call features

**Appearance:**
- `currentTheme` -- active theme name
- `systemDarkTheme` -- theme for system dark mode
- `themeOverrides` -- per-theme customizations
- `profileImageCornerRadius` -- avatar rounding
- `chatItemRoundness` -- message bubble rounding
- `chatItemTail` -- show/hide message bubble tail
- `fontScale` -- text size scaling
- `densityScale` -- UI density scaling
- `inAppBarsAlpha` -- toolbar transparency
- `appearanceBarsBlurRadius` -- toolbar blur effect

**UI:**
- `oneHandUI` -- one-handed UI mode (bottom-aligned navigation)
- `chatBottomBar` -- show bottom bar in chat view
- `simplexLinkMode` -- how SimpleX links are displayed (DESCRIPTION / FULL / BROWSER)
- `showUnreadAndFavorites` -- filter chat list to unread/favorites
- `developerTools` -- enable developer tools (terminal, etc.)

**Database:**
- `encryptedDBPassphrase` -- encrypted form of the DB passphrase
- `initializationVectorDBPassphrase` -- IV for DB passphrase encryption
- `encryptionStartedAt` -- timestamp of encryption operation start (for crash recovery)
- `confirmDBUpgrades` -- prompt before database migrations
- `newDatabaseInitialized` -- flag for incomplete initialization recovery

**Remote Access:**
- `deviceNameForRemoteAccess` -- device display name for remote control
- `confirmRemoteSessions` -- require confirmation for remote sessions
- `connectRemoteViaMulticast` -- use multicast discovery
- `connectRemoteViaMulticastAuto` -- auto-connect via multicast
- `desktopWindowState` -- persisted window position/size (Desktop only)

**Migration:**
- `migrationToStage` / `migrationFromStage` -- track migration progress
- `onboardingStage` -- current onboarding step
- `lastMigratedVersionCode` -- last app version that ran migrations

*See:* `SimpleXAPI.kt:93-488` -- `class AppPreferences` with all `SHARED_PREFS_*` constants
