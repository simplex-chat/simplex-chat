# State Management

## Table of Contents

1. [Overview](#1-overview)
2. [ChatModel](#2-chatmodel)
3. [ChatsContext](#3-chatscontext)
4. [Chat](#4-chat)
5. [AppPreferences](#5-apppreferences)
6. [Source Files](#6-source-files)

---

## 1. Overview

SimpleX Chat uses a **singleton-based, Compose-reactive state model**. The primary state holder is `ChatModel`, a Kotlin `object` annotated with `@Stable`. All mutable fields are Compose `MutableState`, `MutableStateFlow`, or `SnapshotStateList`/`SnapshotStateMap` instances, which trigger Compose recomposition on mutation.

There is no ViewModel layer, no dependency injection framework, and no Redux/MVI pattern. The architecture is:

```
ChatModel (singleton, global Compose state)
    |
    +-- ChatController (command dispatch + event processing)
    |       |
    |       +-- sendCmd() -> chatSendCmdRetry() [JNI]
    |       +-- recvMsg() -> chatRecvMsgWait() [JNI]
    |       +-- processReceivedMsg() -> mutates ChatModel fields
    |
    +-- AppPreferences (150+ SharedPreferences via multiplatform-settings)
    |
    +-- ChatsContext (primary) -- chat list + current chat items
    +-- ChatsContext? (secondary) -- optional second context for dual-pane/support chat
```

State mutations originate from two sources:
1. **User actions**: Compose UI handlers call `api*()` suspend functions on `ChatController`, which send commands to the Haskell core, receive responses, and update `ChatModel`.
2. **Core events**: The receiver coroutine (`startReceiver`) calls `processReceivedMsg()`, which updates `ChatModel` fields on `Dispatchers.Main`.

---

<a id="ChatModel"></a>

## 2. ChatModel

Defined at [`ChatModel.kt line 86`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L86) as `@Stable object ChatModel`.

### Controller Reference

| Field | Type | Line | Purpose |
|---|---|---|---|
| [`controller`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L87) | `ChatController` | 86 | Reference to the `ChatController` singleton |

### User State

| Field | Type | Line | Purpose |
|---|---|---|---|
| [`currentUser`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L89) | `MutableState<User?>` | 88 | Currently active user profile |
| [`users`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L90) | `SnapshotStateList<UserInfo>` | 89 | All user profiles (multi-account) |
| [`localUserCreated`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L91) | `MutableState<Boolean?>` | 90 | Whether a local user has been created (null = unknown during init) |
| [`setDeliveryReceipts`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L88) | `MutableState<Boolean>` | 87 | Trigger for delivery receipts setup dialog |
| [`switchingUsersAndHosts`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L100) | `MutableState<Boolean>` | 99 | True while switching active user/remote host |
| [`changingActiveUserMutex`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L193) | `Mutex` | 192 | Prevents concurrent user switches |

### Chat Runtime State

| Field | Type | Line | Purpose |
|---|---|---|---|
| [`chatRunning`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L92) | `MutableState<Boolean?>` | 91 | `null` = initializing, `true` = running, `false` = stopped |
| [`chatDbChanged`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L93) | `MutableState<Boolean>` | 92 | Database was changed externally (needs restart) |
| [`chatDbEncrypted`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L94) | `MutableState<Boolean?>` | 93 | Whether database is encrypted |
| [`chatDbStatus`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L95) | `MutableState<DBMigrationResult?>` | 94 | Result of database migration attempt |
| [`ctrlInitInProgress`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L96) | `MutableState<Boolean>` | 95 | Controller initialization in progress |
| [`dbMigrationInProgress`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L97) | `MutableState<Boolean>` | 96 | Database migration in progress |
| [`incompleteInitializedDbRemoved`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L98) | `MutableState<Boolean>` | 97 | Tracks if incomplete DB files were removed (prevents infinite retry) |

### Current Chat State

| Field | Type | Line | Purpose |
|---|---|---|---|
| [`chatId`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L103) | `MutableState<String?>` | 102 | ID of the currently open chat (null = chat list shown) |
| [`chatAgentConnId`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L104) | `MutableState<String?>` | 103 | Agent connection ID for current chat |
| [`chatSubStatus`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L105) | `MutableState<SubscriptionStatus?>` | 104 | Subscription status for current chat |
| [`openAroundItemId`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L106) | `MutableState<Long?>` | 105 | Item ID to scroll to when opening chat |
| [`chatsContext`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L107) | `ChatsContext` | 106 | Primary chat context (see [ChatsContext](#3-chatscontext)) |
| [`secondaryChatsContext`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L108) | `MutableState<ChatsContext?>` | 107 | Optional secondary context for dual-pane views |
| [`chats`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L110) | `State<List<Chat>>` | 109 | Derived from `chatsContext.chats` |
| [`deletedChats`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L112) | `MutableState<List<Pair<Long?, String>>>` | 111 | Recently deleted chats (rhId, chatId) |

### Group Members

| Field | Type | Line | Purpose |
|---|---|---|---|
| [`groupMembers`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L113) | `MutableState<List<GroupMember>>` | 112 | Members of currently viewed group |
| [`groupMembersIndexes`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L114) | `MutableState<Map<Long, Int>>` | 113 | Index lookup by `groupMemberId` |
| [`membersLoaded`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L115) | `MutableState<Boolean>` | 114 | Whether group members have been loaded |

### Chat Tags and Filters

| Field | Type | Line | Purpose |
|---|---|---|---|
| [`userTags`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L118) | `MutableState<List<ChatTag>>` | 117 | User-defined chat tags |
| [`activeChatTagFilter`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L119) | `MutableState<ActiveFilter?>` | 118 | Currently active filter in chat list |
| [`presetTags`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L120) | `SnapshotStateMap<PresetTagKind, Int>` | 119 | Counts for preset tag categories (favorites, groups, contacts, etc.) |
| [`unreadTags`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L121) | `SnapshotStateMap<Long, Int>` | 120 | Unread counts per user-defined tag |

### Terminal and Developer

| Field | Type | Line | Purpose |
|---|---|---|---|
| [`terminalsVisible`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L125) | `Set<Boolean>` | 124 | Tracks which terminal views are visible (default vs floating) |
| [`terminalItems`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L126) | `MutableState<List<TerminalItem>>` | 125 | Command/response log for developer terminal |

### Calls (WebRTC)

| Field | Type | Line | Purpose |
|---|---|---|---|
| [`callManager`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L161) | `CallManager` | 160 | WebRTC call lifecycle manager |
| [`callInvitations`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L162) | `SnapshotStateMap<String, RcvCallInvitation>` | 161 | Pending incoming call invitations keyed by chatId |
| [`activeCallInvitation`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L163) | `MutableState<RcvCallInvitation?>` | 162 | Currently displayed incoming call invitation |
| [`activeCall`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L164) | `MutableState<Call?>` | 163 | Currently active call |
| [`activeCallViewIsVisible`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L165) | `MutableState<Boolean>` | 164 | Whether call UI is showing |
| [`activeCallViewIsCollapsed`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L166) | `MutableState<Boolean>` | 165 | Whether call UI is in PiP/collapsed mode |
| [`callCommand`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L167) | `SnapshotStateList<WCallCommand>` | 166 | Pending WebRTC commands |
| [`showCallView`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L168) | `MutableState<Boolean>` | 167 | Call view visibility toggle |
| [`switchingCall`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L169) | `MutableState<Boolean>` | 168 | True during call switching |

### Compose Draft and Sharing

| Field | Type | Line | Purpose |
|---|---|---|---|
| [`draft`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L176) | `MutableState<ComposeState?>` | 175 | Saved compose draft for current chat |
| [`draftChatId`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L177) | `MutableState<String?>` | 176 | Chat ID the draft belongs to |
| [`sharedContent`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L180) | `MutableState<SharedContent?>` | 179 | Content received via share intent or internal forwarding |

### Remote Hosts

| Field | Type | Line | Purpose |
|---|---|---|---|
| [`remoteHosts`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L199) | `SnapshotStateList<RemoteHostInfo>` | 198 | Connected remote hosts (for desktop-mobile pairing) |
| [`currentRemoteHost`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L200) | `MutableState<RemoteHostInfo?>` | 199 | Currently selected remote host |
| [`remoteHostPairing`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L203) | `MutableState<Pair<RemoteHostInfo?, RemoteHostSessionState>?>` | 202 | Remote host pairing state |
| [`remoteCtrlSession`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L204) | `MutableState<RemoteCtrlSession?>` | 203 | Remote controller session |

### Miscellaneous UI State

| Field | Type | Line | Purpose |
|---|---|---|---|
| [`userAddress`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L127) | `MutableState<UserContactLinkRec?>` | 126 | User's public contact address |
| [`chatItemTTL`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L128) | `MutableState<ChatItemTTL>` | 127 | Chat item time-to-live setting |
| [`clearOverlays`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L131) | `MutableState<Boolean>` | 130 | Signal to close all overlays/modals |
| [`appOpenUrl`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L137) | `MutableState<Pair<Long?, String>?>` | 136 | URL opened via deep link (rhId, uri) |
| [`appOpenUrlConnecting`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L138) | `MutableState<Boolean>` | 137 | Whether a deep link connection is in progress |
| [`newChatSheetVisible`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L141) | `MutableState<Boolean>` | 140 | Whether new chat bottom sheet is visible |
| [`fullscreenGalleryVisible`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L144) | `MutableState<Boolean>` | 143 | Fullscreen gallery mode |
| [`notificationPreviewMode`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L147) | `MutableState<NotificationPreviewMode>` | 146 | Notification content preview level |
| [`showAuthScreen`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L156) | `MutableState<Boolean>` | 155 | Whether to show authentication screen |
| [`showChatPreviews`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L158) | `MutableState<Boolean>` | 157 | Whether to show chat preview text in list |
| [`clipboardHasText`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L185) | `MutableState<Boolean>` | 184 | System clipboard has text content |
| [`networkInfo`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L186) | `MutableState<UserNetworkInfo>` | 185 | Network type and online status |
| [`conditions`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L188) | `MutableState<ServerOperatorConditionsDetail>` | 187 | Server operator terms/conditions |
| [`updatingProgress`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L190) | `MutableState<Float?>` | 189 | Progress indicator for app updates |
| [`simplexLinkMode`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L183) | `MutableState<SimplexLinkMode>` | 182 | How SimpleX links are displayed |
| [`migrationState`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L174) | `MutableState<MigrationToState?>` | 173 | Database migration to new device state |
| [`showingInvitation`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L172) | `MutableState<ShowingInvitation?>` | 171 | Currently displayed invitation |
| [`desktopOnboardingRandomPassword`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L134) | `MutableState<Boolean>` | 133 | Desktop: user skipped password setup |
| [`filesToDelete`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L182) | `MutableSet<File>` | 181 | Temporary files pending cleanup |

---

<a id="ChatsContext"></a>

## 3. ChatsContext

Defined as inner class at [`ChatModel.kt line 339`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L339):

```kotlin
class ChatsContext(val secondaryContextFilter: SecondaryContextFilter?)
```

`ChatsContext` holds the chat list and current chat items for a given context. The `ChatModel` maintains a **primary** context (`chatsContext` at line 106) and an optional **secondary** context (`secondaryChatsContext` at line 107).

The secondary context is used for:
- **Group support chat scope** (`SecondaryContextFilter.GroupChatScopeContext`) -- viewing member support threads alongside the main group chat
- **Message content tag filtering** (`SecondaryContextFilter.MsgContentTagContext`) -- filtering messages by content type

### Fields

| Field | Type | Line | Purpose |
|---|---|---|---|
| [`secondaryContextFilter`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L339) | `SecondaryContextFilter?` | 337 | Filter type: null = primary, GroupChatScope or MsgContentTag |
| [`chats`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L340) | `MutableState<SnapshotStateList<Chat>>` | 338 | List of all chats in this context |
| [`chatItems`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L345) | `MutableState<SnapshotStateList<ChatItem>>` | 343 | Items for the currently open chat in this context |
| [`chatState`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L347) | `ActiveChatState` | 345 | Tracks unread counts, splits, scroll state |

### Derived Properties

| Property | Line | Purpose |
|---|---|---|
| [`contentTag`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L353) | 351 | `MsgContentTag?` -- content filter tag if context is MsgContentTag |
| [`groupScopeInfo`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L360) | 358 | `GroupChatScopeInfo?` -- group scope if context is GroupChatScope |
| [`isUserSupportChat`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L367) | 365 | True when viewing own support chat (no specific member) |

### Key Operations

- `addChat(chat)` -- adds chat at index 0, triggers pop animation
- `reorderChat(chat, toIndex)` -- reorders chat list (e.g., when a chat receives a new message)
- `updateChatInfo(rhId, cInfo)` -- updates chat metadata while preserving connection stats
- `hasChat(rhId, id)` / `getChat(id)` -- lookup methods

### ActiveChatState

Defined at [`ChatItemsMerger.kt line 196`](../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatItemsMerger.kt#L196):

```kotlin
data class ActiveChatState(
    val splits: MutableStateFlow<List<Long>>,
    val unreadAfterItemId: MutableStateFlow<Long>,
    val totalAfter: MutableStateFlow<Int>,
    val unreadTotal: MutableStateFlow<Int>,
    val unreadAfter: MutableStateFlow<Int>,
    val unreadAfterNewestLoaded: MutableStateFlow<Int>
)
```

This tracks the scroll position and unread item accounting for the lazy-loaded chat item list:

| Field | Purpose |
|---|---|
| `splits` | List of item IDs where pagination gaps exist (items not yet loaded) |
| `unreadAfterItemId` | The item ID that marks the boundary of "read" vs "unread after" |
| `totalAfter` | Total items after the unread boundary |
| `unreadTotal` | Total unread items in the chat |
| `unreadAfter` | Unread items after the boundary (exclusive) |
| `unreadAfterNewestLoaded` | Unread items after the newest loaded batch |

---

<a id="Chat"></a>

## 4. Chat

Defined at [`ChatModel.kt line 1328`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L1328):

```kotlin
@Serializable @Stable
data class Chat(
    val remoteHostId: Long?,
    val chatInfo: ChatInfo,
    val chatItems: List<ChatItem>,
    val chatStats: ChatStats = ChatStats()
)
```

### Fields

| Field | Type | Purpose |
|---|---|---|
| `remoteHostId` | `Long?` | Remote host ID (null = local) |
| `chatInfo` | `ChatInfo` | Sealed class: `Direct`, `Group`, `Local`, `ContactRequest`, `ContactConnection`, `InvalidJSON` |
| `chatItems` | `List<ChatItem>` | Latest chat items (summary; full list is in `ChatsContext.chatItems`) |
| `chatStats` | `ChatStats` | Unread counts and stats |

<a id="ChatStats"></a>

### ChatStats

Defined at [`ChatModel.kt line 1370`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L1370):

```kotlin
data class ChatStats(
    val unreadCount: Int = 0,
    val unreadMentions: Int = 0,
    val reportsCount: Int = 0,
    val minUnreadItemId: Long = 0,
    val unreadChat: Boolean = false
)
```

### Derived Properties

| Property | Line | Purpose |
|---|---|---|
| `id` | 1346 | Chat ID derived from `chatInfo.id` |
| `unreadTag` | 1340 | Whether chat counts as "unread" for tag filtering (considers notification settings) |
| `supportUnreadCount` | 1348 | Unread count in support/moderation context |
| `nextSendGrpInv` | 1334 | Whether next message should send group invitation |

<a id="ChatInfo"></a>

### ChatInfo Variants

`ChatInfo` is a sealed class at [`ChatModel.kt line 1391`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L1391):

| Variant | SerialName | Key Data |
|---|---|---|
| `ChatInfo.Direct` | `"direct"` | `contact: Contact` |
| `ChatInfo.Group` | `"group"` | `groupInfo: GroupInfo` |
| `ChatInfo.Local` | `"local"` | `noteFolder: NoteFolder` |
| `ChatInfo.ContactRequest` | `"contactRequest"` | `contactRequest: UserContactRequest` |
| `ChatInfo.ContactConnection` | `"contactConnection"` | `contactConnection: PendingContactConnection` |
| `ChatInfo.InvalidJSON` | `"invalidJSON"` | `json: String` |

---

<a id="AppPreferences"></a>
<a id="appPrefs"></a>

## 5. AppPreferences

Defined at [`SimpleXAPI.kt line 94`](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L94) as `class AppPreferences`.

Uses the `multiplatform-settings` library (`com.russhwolf.settings.Settings`) for cross-platform key-value storage (Android `SharedPreferences` / Desktop `java.util.prefs.Preferences`).

The `AppPreferences` instance is created lazily in `ChatController` at [`SimpleXAPI.kt line 496`](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L496):
```kotlin
val appPrefs: AppPreferences by lazy { AppPreferences() }
```

### Preference Categories

#### Notifications (lines 96-103)

| Key | Type | Default | Purpose |
|---|---|---|---|
| `notificationsMode` | `NotificationsMode` | `SERVICE` (if previously enabled) | OFF / SERVICE / PERIODIC |
| `notificationPreviewMode` | `String` | `"message"` | message / contact / hidden |
| `canAskToEnableNotifications` | `Boolean` | `true` | Whether to show notification enable prompt |
| `backgroundServiceNoticeShown` | `Boolean` | `false` | Background service notice already shown |
| `backgroundServiceBatteryNoticeShown` | `Boolean` | `false` | Battery notice already shown |
| `autoRestartWorkerVersion` | `Int` | `0` | Worker version for periodic restart |

#### Calls (lines 105-111)

| Key | Type | Default | Purpose |
|---|---|---|---|
| `webrtcPolicyRelay` | `Boolean` | `true` | Use TURN relay for WebRTC |
| `callOnLockScreen` | `CallOnLockScreen` | `SHOW` | DISABLE / SHOW / ACCEPT |
| `webrtcIceServers` | `String?` | `null` | Custom ICE servers |
| `experimentalCalls` | `Boolean` | `false` | Enable experimental call features |

#### Authentication (lines 107-110)

| Key | Type | Default | Purpose |
|---|---|---|---|
| `performLA` | `Boolean` | `false` | Enable local authentication |
| `laMode` | `LAMode` | default | Authentication mode |
| `laLockDelay` | `Int` | `30` | Seconds before re-auth required |
| `laNoticeShown` | `Boolean` | `false` | LA notice shown |

#### Privacy (lines 112-128)

| Key | Type | Default | Purpose |
|---|---|---|---|
| `privacyProtectScreen` | `Boolean` | `true` | FLAG_SECURE on Android |
| `privacyAcceptImages` | `Boolean` | `true` | Auto-accept images |
| `privacyLinkPreviews` | `Boolean` | `true` | Generate link previews |
| `privacySanitizeLinks` | `Boolean` | `false` | Remove tracking params from links |
| `simplexLinkMode` | `SimplexLinkMode` | `DESCRIPTION` | DESCRIPTION / FULL / BROWSER |
| `privacyShowChatPreviews` | `Boolean` | `true` | Show chat previews in list |
| `privacySaveLastDraft` | `Boolean` | `true` | Save compose draft |
| `privacyDeliveryReceiptsSet` | `Boolean` | `false` | Delivery receipts configured |
| `privacyEncryptLocalFiles` | `Boolean` | `true` | Encrypt local files |
| `privacyAskToApproveRelays` | `Boolean` | `true` | Ask before using relays |
| `privacyMediaBlurRadius` | `Int` | `0` | Blur radius for media |

#### Network (lines 140-175)

| Key | Type | Default | Purpose |
|---|---|---|---|
| `networkUseSocksProxy` | `Boolean` | `false` | Enable SOCKS proxy |
| `networkProxy` | `NetworkProxy` | localhost:9050 | Proxy host/port |
| `networkSessionMode` | `TransportSessionMode` | default | Session mode |
| `networkSMPProxyMode` | `SMPProxyMode` | default | SMP proxy mode |
| `networkSMPProxyFallback` | `SMPProxyFallback` | default | Proxy fallback policy |
| `networkHostMode` | `HostMode` | default | Host mode (onion routing) |
| `networkRequiredHostMode` | `Boolean` | `false` | Enforce host mode |
| `networkSMPWebPortServers` | `SMPWebPortServers` | default | Web port server config |
| `networkShowSubscriptionPercentage` | `Boolean` | `false` | Show subscription stats |
| `networkTCPConnectTimeout*` | `Long` | varies | TCP connect timeouts (background/interactive) |
| `networkTCPTimeout*` | `Long` | varies | TCP operation timeouts |
| `networkTCPTimeoutPerKb` | `Long` | varies | Per-KB timeout |
| `networkRcvConcurrency` | `Int` | default | Receive concurrency |
| `networkSMPPingInterval` | `Long` | default | SMP ping interval |
| `networkSMPPingCount` | `Int` | default | SMP ping count |
| `networkEnableKeepAlive` | `Boolean` | default | TCP keep-alive |
| `networkTCPKeepIdle` | `Int` | default | Keep-alive idle time |
| `networkTCPKeepIntvl` | `Int` | default | Keep-alive interval |
| `networkTCPKeepCnt` | `Int` | default | Keep-alive count |

#### Appearance (lines 213-233)

| Key | Type | Default | Purpose |
|---|---|---|---|
| `currentTheme` | `String` | `"SYSTEM"` | Active theme name |
| `systemDarkTheme` | `String` | `"SIMPLEX"` | Theme for system dark mode |
| `currentThemeIds` | `Map<String, String>` | empty | Theme ID per base theme |
| `themeOverrides` | `List<ThemeOverrides>` | empty | Custom theme overrides |
| `profileImageCornerRadius` | `Float` | `22.5f` | Avatar corner radius |
| `chatItemRoundness` | `Float` | `0.75f` | Message bubble roundness |
| `chatItemTail` | `Boolean` | `true` | Show bubble tail |
| `fontScale` | `Float` | `1f` | Font scale factor |
| `densityScale` | `Float` | `1f` | UI density scale |
| `inAppBarsAlpha` | `Float` | varies | Bar transparency |
| `appearanceBarsBlurRadius` | `Int` | 50 or 0 | Bar blur radius (device-dependent) |

#### Developer (lines 135-139)

| Key | Type | Default | Purpose |
|---|---|---|---|
| `developerTools` | `Boolean` | `false` | Enable developer tools |
| `logLevel` | `LogLevel` | `WARNING` | Log level |
| `showInternalErrors` | `Boolean` | `false` | Show internal errors to user |
| `showSlowApiCalls` | `Boolean` | `false` | Alert on slow API calls |
| `terminalAlwaysVisible` | `Boolean` | `false` | Floating terminal window (desktop) |

#### Database (lines 188-208)

| Key | Type | Default | Purpose |
|---|---|---|---|
| `onboardingStage` | `OnboardingStage` | `OnboardingComplete` | Current onboarding step |
| `storeDBPassphrase` | `Boolean` | `true` | Store DB passphrase in keystore |
| `initialRandomDBPassphrase` | `Boolean` | `false` | DB was created with random passphrase |
| `encryptedDBPassphrase` | `String?` | null | Encrypted DB passphrase |
| `confirmDBUpgrades` | `Boolean` | `false` | Confirm DB migrations |
| `chatStopped` | `Boolean` | `false` | Chat was explicitly stopped |
| `chatLastStart` | `Instant?` | null | Last chat start timestamp |
| `newDatabaseInitialized` | `Boolean` | `false` | DB successfully initialized at least once |
| `shouldImportAppSettings` | `Boolean` | `false` | Import settings after DB import |
| `selfDestruct` | `Boolean` | `false` | Self-destruct enabled |
| `selfDestructDisplayName` | `String?` | null | Display name for self-destruct profile |

#### UI Preferences (lines 255-257)

| Key | Type | Default | Purpose |
|---|---|---|---|
| `oneHandUI` | `Boolean` | `true` | One-hand mode |
| `chatBottomBar` | `Boolean` | `true` | Bottom bar in chat |

#### Remote Access (lines 238-243)

| Key | Type | Default | Purpose |
|---|---|---|---|
| `deviceNameForRemoteAccess` | `String` | device model | Device name shown to paired devices |
| `confirmRemoteSessions` | `Boolean` | `false` | Confirm remote sessions |
| `connectRemoteViaMulticast` | `Boolean` | `false` | Use multicast for discovery |
| `connectRemoteViaMulticastAuto` | `Boolean` | `true` | Auto-connect via multicast |
| `offerRemoteMulticast` | `Boolean` | `true` | Offer multicast connection |

#### Migration (lines 189-190)

| Key | Type | Default | Purpose |
|---|---|---|---|
| `migrationToStage` | `String?` | null | Migration-to-device progress |
| `migrationFromStage` | `String?` | null | Migration-from-device progress |

#### Updates and Versioning (lines 184-186, 235-237)

| Key | Type | Default | Purpose |
|---|---|---|---|
| `appUpdateChannel` | `AppUpdatesChannel` | `DISABLED` | DISABLED / STABLE / BETA |
| `appSkippedUpdate` | `String` | `""` | Skipped update version |
| `appUpdateNoticeShown` | `Boolean` | `false` | Update notice shown |
| `whatsNewVersion` | `String?` | null | Last "What's New" version seen |
| `lastMigratedVersionCode` | `Int` | `0` | Last app version code for data migrations |
| `customDisappearingMessageTime` | `Int` | `300` | Custom disappearing message time (seconds) |

### Preference Utility Types

The `SharedPreference<T>` wrapper (defined in SimpleXAPI.kt) provides:
- `get(): T` -- read current value
- `set(value: T)` -- write value
- `state: MutableState<T>` -- Compose-observable state (derived lazily)

Factory methods: `mkBoolPreference`, `mkIntPreference`, `mkLongPreference`, `mkFloatPreference`, `mkStrPreference`, `mkEnumPreference`, `mkSafeEnumPreference`, `mkDatePreference`, `mkMapPreference`, `mkTimeoutPreference`.

---

## 6. Source Files

| File | Path | Key Contents |
|---|---|---|
| ChatModel.kt | [`common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt) | `ChatModel` singleton (line 85), `ChatsContext` (line 337), `Chat` (line 1325), `ChatInfo` (line 1386), `ChatStats` (line 1366), helper methods |
| SimpleXAPI.kt | [`common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt`](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt) | `AppPreferences` (line 93), `ChatController` (line 492), `startReceiver` (line 657), `sendCmd` (line 800), `recvMsg` (line 824), `processReceivedMsg` (line 2568) |
| ChatItemsMerger.kt | [`common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatItemsMerger.kt`](../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatItemsMerger.kt) | `ActiveChatState` (line 196), chat item merge/diff logic |
| Core.kt | [`common/src/commonMain/kotlin/chat/simplex/common/platform/Core.kt`](../common/src/commonMain/kotlin/chat/simplex/common/platform/Core.kt) | `initChatController` (line 58), state initialization flow |
| App.kt | [`common/src/commonMain/kotlin/chat/simplex/common/App.kt`](../common/src/commonMain/kotlin/chat/simplex/common/App.kt) | `AppScreen` (line 46), `MainScreen` (line 82), top-level UI state reads |
