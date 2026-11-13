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

Defined at [`ChatModel.kt line 153`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L153) as `@Stable object ChatModel`.

### Controller Reference

| Field | Type | Line | Purpose |
|---|---|---|---|
| [`controller`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L154) | `ChatController` | 154 | Reference to the `ChatController` singleton |

### User State

| Field | Type | Line | Purpose |
|---|---|---|---|
| [`currentUser`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L156) | `MutableState<User?>` | 156 | Currently active user profile |
| [`users`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L157) | `SnapshotStateList<UserInfo>` | 157 | All user profiles (multi-account) |
| [`localUserCreated`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L158) | `MutableState<Boolean?>` | 158 | Whether a local user has been created (null = unknown during init) |
| [`setDeliveryReceipts`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L155) | `MutableState<Boolean>` | 155 | Trigger for delivery receipts setup dialog |
| [`switchingUsersAndHosts`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L167) | `MutableState<Boolean>` | 167 | True while switching active user/remote host |
| [`changingActiveUserMutex`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L283) | `Mutex` | 283 | Prevents concurrent user switches |

### Chat Runtime State

| Field | Type | Line | Purpose |
|---|---|---|---|
| [`chatRunning`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L159) | `MutableState<Boolean?>` | 159 | `null` = initializing, `true` = running, `false` = stopped |
| [`chatDbChanged`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L160) | `MutableState<Boolean>` | 160 | Database was changed externally (needs restart) |
| [`chatDbEncrypted`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L161) | `MutableState<Boolean?>` | 161 | Whether database is encrypted |
| [`chatDbStatus`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L162) | `MutableState<DBMigrationResult?>` | 162 | Result of database migration attempt |
| [`ctrlInitInProgress`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L163) | `MutableState<Boolean>` | 163 | Controller initialization in progress |
| [`dbMigrationInProgress`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L164) | `MutableState<Boolean>` | 164 | Database migration in progress |
| [`incompleteInitializedDbRemoved`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L165) | `MutableState<Boolean>` | 165 | Tracks if incomplete DB files were removed (prevents infinite retry) |

### Current Chat State

| Field | Type | Line | Purpose |
|---|---|---|---|
| [`chatId`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L170) | `MutableState<String?>` | 170 | ID of the currently open chat (null = chat list shown) |
| [`chatAgentConnId`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L171) | `MutableState<String?>` | 171 | Agent connection ID for current chat |
| [`chatSubStatus`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L172) | `MutableState<SubscriptionStatus?>` | 172 | Subscription status for current chat |
| [`openAroundItemId`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L173) | `MutableState<Long?>` | 173 | Item ID to scroll to when opening chat |
| [`chatsContext`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L174) | `ChatsContext` | 174 | Primary chat context (see [ChatsContext](#3-chatscontext)) |
| [`secondaryChatsContext`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L175) | `MutableState<ChatsContext?>` | 175 | Optional secondary context for dual-pane views |
| [`chats`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L177) | `State<List<Chat>>` | 177 | Derived from `chatsContext.chats` |
| [`deletedChats`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L179) | `MutableState<List<Pair<Long?, String>>>` | 179 | Recently deleted chats (rhId, chatId) |

### Group Members

| Field | Type | Line | Purpose |
|---|---|---|---|
| [`groupMembers`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L181) | `MutableState<List<GroupMember>>` | 181 | Members of currently viewed group |
| [`groupMembersIndexes`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L182) | `MutableState<Map<Long, Int>>` | 182 | Index lookup by `groupMemberId` |
| [`membersLoaded`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L183) | `MutableState<Boolean>` | 183 | Whether group members have been loaded |

### Chat Tags and Filters

| Field | Type | Line | Purpose |
|---|---|---|---|
| [`userTags`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L189) | `MutableState<List<ChatTag>>` | 189 | User-defined chat tags |
| [`activeChatTagFilter`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L190) | `MutableState<ActiveFilter?>` | 190 | Currently active filter in chat list |
| [`presetTags`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L191) | `SnapshotStateMap<PresetTagKind, Int>` | 191 | Counts for preset tag categories (favorites, groups, contacts, etc.) |
| [`unreadTags`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L192) | `SnapshotStateMap<Long, Int>` | 192 | Unread counts per user-defined tag |

### Terminal and Developer

| Field | Type | Line | Purpose |
|---|---|---|---|
| [`terminalsVisible`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L196) | `Set<Boolean>` | 196 | Tracks which terminal views are visible (default vs floating) |
| [`terminalItems`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L197) | `MutableState<List<TerminalItem>>` | 197 | Command/response log for developer terminal |

### Calls (WebRTC)

| Field | Type | Line | Purpose |
|---|---|---|---|
| [`callManager`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L251) | `CallManager` | 251 | WebRTC call lifecycle manager |
| [`callInvitations`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L252) | `SnapshotStateMap<String, RcvCallInvitation>` | 252 | Pending incoming call invitations keyed by chatId |
| [`activeCallInvitation`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L253) | `MutableState<RcvCallInvitation?>` | 253 | Currently displayed incoming call invitation |
| [`activeCall`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L254) | `MutableState<Call?>` | 254 | Currently active call |
| [`activeCallViewIsVisible`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L255) | `MutableState<Boolean>` | 255 | Whether call UI is showing |
| [`activeCallViewIsCollapsed`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L256) | `MutableState<Boolean>` | 256 | Whether call UI is in PiP/collapsed mode |
| [`callCommand`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L257) | `SnapshotStateList<WCallCommand>` | 257 | Pending WebRTC commands |
| [`showCallView`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L258) | `MutableState<Boolean>` | 258 | Call view visibility toggle |
| [`switchingCall`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L259) | `MutableState<Boolean>` | 259 | True during call switching |

### Compose Draft and Sharing

| Field | Type | Line | Purpose |
|---|---|---|---|
| [`draft`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L266) | `MutableState<ComposeState?>` | 266 | Saved compose draft for current chat |
| [`draftChatId`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L268) | `MutableState<String?>` | 268 | Chat ID the draft belongs to |
| [`sharedContent`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L271) | `MutableState<SharedContent?>` | 271 | Content received via share intent or internal forwarding |

### Remote Hosts

| Field | Type | Line | Purpose |
|---|---|---|---|
| [`remoteHosts`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L289) | `SnapshotStateList<RemoteHostInfo>` | 289 | Connected remote hosts (for desktop-mobile pairing) |
| [`currentRemoteHost`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L290) | `MutableState<RemoteHostInfo?>` | 290 | Currently selected remote host |
| [`remoteHostPairing`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L293) | `MutableState<Pair<RemoteHostInfo?, RemoteHostSessionState>?>` | 293 | Remote host pairing state |
| [`remoteCtrlSession`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L294) | `MutableState<RemoteCtrlSession?>` | 294 | Remote controller session |

### Miscellaneous UI State

| Field | Type | Line | Purpose |
|---|---|---|---|
| [`userAddress`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L198) | `MutableState<UserContactLinkRec?>` | 198 | User's public contact address |
| [`chatItemTTL`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L199) | `MutableState<ChatItemTTL>` | 199 | Chat item time-to-live setting |
| [`clearOverlays`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L202) | `MutableState<Boolean>` | 202 | Signal to close all overlays/modals |
| [`appOpenUrl`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L208) | `MutableState<Pair<Long?, String>?>` | 208 | URL opened via deep link (rhId, uri) |
| [`appOpenUrlConnecting`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L209) | `MutableState<Boolean>` | 209 | Whether a deep link connection is in progress |
| [`newChatSheetVisible`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L212) | `MutableState<Boolean>` | 212 | Whether new chat bottom sheet is visible |
| [`fullscreenGalleryVisible`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L215) | `MutableState<Boolean>` | 215 | Fullscreen gallery mode |
| [`chatListBanner`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L219) | `ChatListBanner?` (plain var) | 219 | The banner kind the chat list showed this app session; `bannerSlotFree(banner)` tells whether a kind may take the slot (see [chat-list.md](client/chat-list.md)) |
| [`notificationPreviewMode`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L237) | `MutableState<NotificationPreviewMode>` | 237 | Notification content preview level |
| [`showAuthScreen`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L246) | `MutableState<Boolean>` | 246 | Whether to show authentication screen |
| [`showChatPreviews`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L248) | `MutableState<Boolean>` | 248 | Whether to show chat preview text in list |
| [`networkInfo`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L276) | `MutableState<UserNetworkInfo>` | 276 | Network type and online status |
| [`conditions`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L278) | `MutableState<ServerOperatorConditionsDetail>` | 278 | Server operator terms/conditions |
| [`updatingProgress`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L280) | `MutableState<Float?>` | 280 | Progress indicator for app updates |
| [`simplexLinkMode`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L274) | `MutableState<SimplexLinkMode>` | 274 | How SimpleX links are displayed |
| [`migrationState`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L264) | `MutableState<MigrationToState?>` | 264 | Database migration to new device state |
| [`showingInvitation`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L262) | `MutableState<ShowingInvitation?>` | 262 | Currently displayed invitation |
| [`desktopOnboardingRandomPassword`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L205) | `MutableState<Boolean>` | 205 | Desktop: user skipped password setup |
| [`filesToDelete`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L273) | `MutableSet<File>` | 273 | Temporary files pending cleanup |

---

<a id="ChatsContext"></a>

## 3. ChatsContext

Defined as inner class at [`ChatModel.kt line 436`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L436):

```kotlin
class ChatsContext(val secondaryContextFilter: SecondaryContextFilter?)
```

`ChatsContext` holds the chat list and current chat items for a given context. The `ChatModel` maintains a **primary** context (`chatsContext` at line 176) and an optional **secondary** context (`secondaryChatsContext` at line 177).

The secondary context is used for:
- **Group support chat scope** (`SecondaryContextFilter.GroupChatScopeContext`) -- viewing member support threads alongside the main group chat
- **Message content tag filtering** (`SecondaryContextFilter.MsgContentTagContext`) -- filtering messages by content type

### Fields

| Field | Type | Line | Purpose |
|---|---|---|---|
| [`secondaryContextFilter`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L436) | `SecondaryContextFilter?` | 436 | Filter type: null = primary, GroupChatScope or MsgContentTag |
| [`chats`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L437) | `MutableState<SnapshotStateList<Chat>>` | 437 | List of all chats in this context |
| [`chatItems`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L442) | `MutableState<SnapshotStateList<ChatItem>>` | 442 | Items for the currently open chat in this context |
| [`chatState`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L444) | `ActiveChatState` | 444 | Tracks unread counts, splits, scroll state |

### Derived Properties

| Property | Line | Purpose |
|---|---|---|
| [`contentTag`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L450) | 450 | `MsgContentTag?` -- content filter tag if context is MsgContentTag |
| [`groupScopeInfo`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L457) | 457 | `GroupChatScopeInfo?` -- group scope if context is GroupChatScope |
| [`isUserSupportChat`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L464) | 464 | True when viewing own support chat (no specific member) |

### Key Operations

- `addChat(chat)` -- adds chat at index 0, triggers pop animation
- `reorderChat(chat, toIndex)` -- reorders chat list (e.g., when a chat receives a new message)
- `updateChatInfo(rhId, cInfo)` -- updates chat metadata while preserving connection stats
- `hasChat(rhId, id)` / `getChat(id)` -- lookup methods

### ActiveChatState

Defined at [`ChatItemsMerger.kt line 196`](../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatItemsMerger.kt#L196):

```kotlin
data class ActiveChatState(
    val splits: MutableStateFlow<List<Long>> = MutableStateFlow(emptyList()),
    val unreadAfterItemId: MutableStateFlow<Long> = MutableStateFlow(-1L),
    val totalAfter: MutableStateFlow<Int> = MutableStateFlow(0),
    val unreadTotal: MutableStateFlow<Int> = MutableStateFlow(0),
    val unreadAfter: MutableStateFlow<Int> = MutableStateFlow(0),
    val unreadAfterNewestLoaded: MutableStateFlow<Int> = MutableStateFlow(0)
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

Defined at [`ChatModel.kt line 1479`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L1479):

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

Defined at [`ChatModel.kt line 1521`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L1521):

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
| `id` | 1349 | Chat ID derived from `chatInfo.id` |
| `unreadTag` | 1343 | Whether chat counts as "unread" for tag filtering (considers notification settings) |
| `supportUnreadCount` | 1351 | Unread count in support/moderation context |
| `nextSendGrpInv` | 1337 | Whether next message should send group invitation |

<a id="ChatInfo"></a>

### ChatInfo Variants

`ChatInfo` is a sealed class at [`ChatModel.kt line 1542`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L1542):

| Variant | SerialName | Key Data |
|---|---|---|
| `ChatInfo.Direct` | `"direct"` | `contact: Contact` |
| `ChatInfo.Group` | `"group"` | `groupInfo: GroupInfo, groupChatScope: GroupChatScopeInfo?` |
| `ChatInfo.Local` | `"local"` | `noteFolder: NoteFolder` |
| `ChatInfo.ContactRequest` | `"contactRequest"` | `contactRequest: UserContactRequest` |
| `ChatInfo.ContactConnection` | `"contactConnection"` | `contactConnection: PendingContactConnection` |
| `ChatInfo.InvalidJSON` | `"invalidJSON"` | `json: String` |

[`ChatInfo.activeChatLayout(user)`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L1895) is computed in the app, skipping `ChatLayoutMode.Unknown` values:

```
chat choice        ChatInfo.uiThemes.chatLayout (contact or group theme record)
chat layout        ChatInfo.chatLayout: groupProfile.chatLayout of a group; Contact.chatLayout (address link data, stored when the contact is prepared)
profile choice     User.uiThemes.chatLayout (profileChatLayout)
app choice         appPrefs.chatLayout
otherwise          ChatLayoutMode.Classic
```

[`defaultChatLayout(user)`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L1892) is the same without the chat choice. Note folders have no chat values, so the profile and app choices apply. `ChatView` reads `chatModel.currentUser`, and `profileChatLayout` reads `appPrefs.chatLayout.state`, so a change of either recomposes the open chat; the chat list is not reloaded. The chat choice is written with `apiSetChatUIThemes` (`setChatLayout`, `ChatInfoView.kt`), the profile choice with `apiSetUserUIThemes` (`ChatLayoutRow`, `Appearance.kt`), together with the colours of the same record.

### RelayStatus (Channels)

`RelayStatus` is an `enum class` at [`ChatModel.kt line 2919`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L2919) modelling a relay's lifecycle for a channel on the owner's side. Serialized as a lowercase string via `@SerialName`.

| Case | SerialName | Meaning |
|---|---|---|
| `RsNew` | `"new"` | Allocated locally; not yet sent |
| `RsInvited` | `"invited"` | `XGrpRelayInv` sent, awaiting `XGrpRelayAcpt` |
| `RsAccepted` | `"accepted"` | Accepted, link-data update pending |
| `RsActive` | `"active"` | Listed in channel link data; forwarding |
| `RsInactive` | `"inactive"` | No longer in link data or backend reports it removed |
| `RsRejected` | `"rejected"` | Relay sent `XGrpRelayReject` for the channel link; final on the owner side. Clearable only by the relay operator running `/group allow #<channel>`. The owner-side `GroupMember.memberStatus` is also set to `MemLeft` so the relay renders identically to one that explicitly left (`MemRejected` is reserved for the knocking-admission flow). |

The `text` extension on the enum returns the localized status string (resource key `relay_status_*`, with `relay_status_rejected` = "rejected").

---

<a id="AppPreferences"></a>
<a id="appPrefs"></a>

## 5. AppPreferences

Defined at [`SimpleXAPI.kt line 107`](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L107) as `class AppPreferences`.

Uses the `multiplatform-settings` library (`com.russhwolf.settings.Settings`) for cross-platform key-value storage (Android `SharedPreferences` / Desktop `java.util.prefs.Preferences`).

The `AppPreferences` instance is created lazily in `ChatController` at [`SimpleXAPI.kt line 550`](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L550):
```kotlin
val appPrefs: AppPreferences by lazy { AppPreferences() }
```

### Preference Categories

#### Notifications (lines 105-113)

| Key | Type | Default | Purpose |
|---|---|---|---|
| `notificationsMode` | `NotificationsMode` | `SERVICE` (if previously enabled) | OFF / SERVICE / PERIODIC |
| `notificationPreviewMode` | `String` | `"message"` | message / contact / hidden |
| `canAskToEnableNotifications` | `Boolean` | `true` | Whether to show notification enable prompt |
| `backgroundServiceNoticeShown` | `Boolean` | `false` | Background service notice already shown |
| `backgroundServiceBatteryNoticeShown` | `Boolean` | `false` | Battery notice already shown |
| `autoRestartWorkerVersion` | `Int` | `0` | Worker version for periodic restart |

#### Calls (lines 115-121)

| Key | Type | Default | Purpose |
|---|---|---|---|
| `webrtcPolicyRelay` | `Boolean` | `true` | Use TURN relay for WebRTC |
| `callOnLockScreen` | `CallOnLockScreen` | `SHOW` | DISABLE / SHOW / ACCEPT |
| `webrtcIceServers` | `String?` | `null` | Custom ICE servers |
| `experimentalCalls` | `Boolean` | `false` | Enable experimental call features |

#### Authentication (lines 117-120)

| Key | Type | Default | Purpose |
|---|---|---|---|
| `performLA` | `Boolean` | `false` | Enable local authentication |
| `laMode` | `LAMode` | default | Authentication mode |
| `laLockDelay` | `Int` | `30` | Seconds before re-auth required |
| `laNoticeShown` | `Boolean` | `false` | LA notice shown |

#### Privacy (lines 122-141)

| Key | Type | Default | Purpose |
|---|---|---|---|
| `privacyProtectScreen` | `Boolean` | `true` | FLAG_SECURE on Android |
| `privacyAcceptImages` | `Boolean` | `true` | Auto-accept images |
| `privacyLinkPreviews` | `Boolean` | `true` | Generate link previews |
| [`privacyDirectorySearch`](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L130) (`"PrivacyDirectorySearch"`) | `Boolean` | `false` | "Directory search" in Privacy settings; while it is off, "Enable Directory search?" is shown before every online search, and Enable turns it on (see [Chat List, Online Search](client/chat-list.md#11-online-search)) |
| `privacySanitizeLinks` | `Boolean` | `false` | Remove tracking params from links |
| `simplexLinkMode` | `SimplexLinkMode` | `DESCRIPTION` | DESCRIPTION / FULL / BROWSER |
| `privacyShowChatPreviews` | `Boolean` | `true` | Show chat previews in list |
| `privacySaveLastDraft` | `Boolean` | `true` | Save compose draft |
| `privacyDeliveryReceiptsSet` | `Boolean` | `false` | Delivery receipts configured |
| `privacyEncryptLocalFiles` | `Boolean` | `true` | Encrypt local files |
| `privacyAskToApproveRelays` | `Boolean` | `true` | Ask before using relays |
| `privacyMediaBlurRadius` | `Int` | `0` | Blur radius for media |

#### Network (lines 153-188)

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

#### Appearance (lines 233-253)

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

#### Developer (lines 148-152)

| Key | Type | Default | Purpose |
|---|---|---|---|
| `developerTools` | `Boolean` | `false` | Enable developer tools |
| `logLevel` | `LogLevel` | `WARNING` | Log level |
| `showInternalErrors` | `Boolean` | `false` | Show internal errors to user |
| `showSlowApiCalls` | `Boolean` | `false` | Alert on slow API calls |
| `terminalAlwaysVisible` | `Boolean` | `false` | Floating terminal window (desktop) |

#### Database (lines 207-228)

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

#### UI Preferences (lines 275-277)

| Key | Type | Default | Purpose |
|---|---|---|---|
| `oneHandUI` | `Boolean` | `true` | One-hand mode |
| `chatBottomBar` | `Boolean` | `true` | Bottom bar in chat |
| `chatLayout` | `ChatLayoutMode` | `Classic` | App chat layout (`activeChatLayout`) |

#### Hints

Preferences listed in [`hintPreferences`](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L288-L305) are restored to their defaults by "Reset all hints" (`resetHintPreferences()` in `views/usersettings/SettingsView.kt`). The online search's consent is not among them: `privacyDirectorySearch` (Privacy, above) is a setting, so "Reset all hints" leaves it as it is.

#### Remote Access (lines 258-263)

| Key | Type | Default | Purpose |
|---|---|---|---|
| `deviceNameForRemoteAccess` | `String` | device model | Device name shown to paired devices |
| `confirmRemoteSessions` | `Boolean` | `false` | Confirm remote sessions |
| `connectRemoteViaMulticast` | `Boolean` | `false` | Use multicast for discovery |
| `connectRemoteViaMulticastAuto` | `Boolean` | `true` | Auto-connect via multicast |
| `offerRemoteMulticast` | `Boolean` | `true` | Offer multicast connection |

#### Migration (lines 208-209)

| Key | Type | Default | Purpose |
|---|---|---|---|
| `migrationToStage` | `String?` | null | Migration-to-device progress |
| `migrationFromStage` | `String?` | null | Migration-from-device progress |

#### Updates and Versioning (lines 203-205, 255-257)

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
| ChatModel.kt | [`common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt) | `ChatModel` singleton (line 155), `ChatsContext` (line 418), `Chat` (line 1460), `ChatInfo` (line 1523), `ChatStats` (line 1502), helper methods |
| SimpleXAPI.kt | [`common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt`](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt) | `AppPreferences` (line 103), `ChatController` (line 537), `startReceiver` (line 785), `sendCmd` (line 939), `recvMsg` (line 964), `processReceivedMsg` (line 2920) |
| ChatItemsMerger.kt | [`common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatItemsMerger.kt`](../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatItemsMerger.kt) | `ActiveChatState` (line 196), chat item merge/diff logic |
| Core.kt | [`common/src/commonMain/kotlin/chat/simplex/common/platform/Core.kt`](../common/src/commonMain/kotlin/chat/simplex/common/platform/Core.kt) | `initChatController` (line 70), state initialization flow |
| App.kt | [`common/src/commonMain/kotlin/chat/simplex/common/App.kt`](../common/src/commonMain/kotlin/chat/simplex/common/App.kt) | `AppScreen` (line 48), `MainScreen` (line 85), top-level UI state reads |
