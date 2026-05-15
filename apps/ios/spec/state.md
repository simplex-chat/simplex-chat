# SimpleX Chat iOS -- State Management

**Source:** [`ChatModel.swift`](../Shared/Model/ChatModel.swift#L1-L1483) | [`ChatTypes.swift`](../SimpleXChat/ChatTypes.swift#L1-L5377)

> Technical specification for the app's state architecture: ChatModel, ItemsModel, Chat, ChatInfo, and preference storage.
>
> Related specs: [Architecture](architecture.md) | [API Reference](api.md) | [README](README.md)
> Related product: [Concept Index](../product/concepts.md)

---

## Table of Contents

1. [Overview](#1-overview)
2. [ChatModel -- Primary App State](#2-chatmodel)
3. [ItemsModel -- Per-Chat Message State](#3-itemsmodel)
4. [ChatTagsModel -- Tag Filtering State](#4-chattagsmodel)
5. [ChannelRelaysModel -- Channel Relay State](#5-channelrelaysmodel)
6. [Chat -- Single Conversation State](#6-chat)
7. [ChatInfo -- Conversation Metadata](#7-chatinfo)
8. [State Flow](#8-state-flow)
9. [Preference Storage](#9-preference-storage)

---

## 1. Overview

The app uses SwiftUI's `ObservableObject` pattern for reactive state management. The state hierarchy is:

```
ChatModel (singleton -- global app state)
├── currentUser: User?
├── users: [UserInfo]
├── chats: [Chat]                    (chat list)
├── chatId: String?                  (active chat ID)
├── im: ItemsModel.shared            (primary chat items)
├── secondaryIM: ItemsModel?         (secondary chat items, e.g. support scope)
├── activeCall: Call?
├── callInvitations: [ChatId: RcvCallInvitation]
├── deviceToken / savedToken / tokenStatus
├── notificationMode: NotificationsMode
├── onboardingStage: OnboardingStage?
├── migrationState: MigrationToState?
└── ... (50+ @Published properties)

ItemsModel (singleton + secondary instances -- per-chat message state)
├── reversedChatItems: [ChatItem]    (messages in reverse order)
├── chatState: ActiveChatState       (pagination/split state)
├── isLoading / showLoadingProgress
└── preloadState: PreloadState

Chat (per-conversation -- one per entry in chat list)
├── chatInfo: ChatInfo               (type + metadata)
├── chatItems: [ChatItem]            (preview items)
└── chatStats: ChatStats             (unread counts)

ChatTagsModel (singleton -- filter state)
├── userTags: [ChatTag]
├── activeFilter: ActiveFilter?
├── presetTags: [PresetTag: Int]
└── unreadTags: [Int64: Int]
```

---

## 2. [ChatModel](../Shared/Model/ChatModel.swift#L416-L1368)

**Class**: `final class ChatModel: ObservableObject`
**Singleton**: `ChatModel.shared`
**Source**: [`Shared/Model/ChatModel.swift`](../Shared/Model/ChatModel.swift#L416)

### Key Published Properties

#### App Lifecycle
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `onboardingStage` | `OnboardingStage?` | Current onboarding step | [L417](../Shared/Model/ChatModel.swift#L417) |
| `chatInitialized` | `Bool` | Whether chat has been initialized | [L426](../Shared/Model/ChatModel.swift#L426) |
| `chatRunning` | `Bool?` | Whether chat engine is running | [L427](../Shared/Model/ChatModel.swift#L427) |
| `chatDbChanged` | `Bool` | Whether DB was changed externally | [L428](../Shared/Model/ChatModel.swift#L428) |
| `chatDbEncrypted` | `Bool?` | Whether DB is encrypted | [L429](../Shared/Model/ChatModel.swift#L429) |
| `chatDbStatus` | `DBMigrationResult?` | DB migration status | [L430](../Shared/Model/ChatModel.swift#L430) |
| `ctrlInitInProgress` | `Bool` | Whether controller is initializing | [L431](../Shared/Model/ChatModel.swift#L431) |
| `migrationState` | `MigrationToState?` | Device migration state | [L481](../Shared/Model/ChatModel.swift#L481) |

#### User State
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `currentUser` | `User?` | Active user profile (triggers theme reapply on change) | [L420](../Shared/Model/ChatModel.swift#L420) |
| `users` | `[UserInfo]` | All user profiles | [L425](../Shared/Model/ChatModel.swift#L425) |
| `v3DBMigration` | `V3DBMigrationState` | Legacy DB migration state | [L419](../Shared/Model/ChatModel.swift#L419) |

#### Chat List
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `chats` | `[Chat]` (private set) | All conversations for current user | [L437](../Shared/Model/ChatModel.swift#L437) |
| `deletedChats` | `Set<String>` | Chat IDs pending deletion animation | [L438](../Shared/Model/ChatModel.swift#L438) |

#### Active Chat
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `chatId` | `String?` | Currently open chat ID | [L440](../Shared/Model/ChatModel.swift#L440) |
| `chatAgentConnId` | `String?` | Agent connection ID for active chat | [L441](../Shared/Model/ChatModel.swift#L441) |
| `chatSubStatus` | `SubscriptionStatus?` | Active chat subscription status | [L442](../Shared/Model/ChatModel.swift#L442) |
| `openAroundItemId` | `ChatItem.ID?` | Item to scroll to when opening | [L443](../Shared/Model/ChatModel.swift#L443) |
| `chatToTop` | `String?` | Chat to scroll to top | [L444](../Shared/Model/ChatModel.swift#L444) |
| `groupMembers` | `[GMember]` | Members of active group | [L446](../Shared/Model/ChatModel.swift#L446) |
| `groupMembersIndexes` | `[Int64: Int]` | Member ID to index mapping | [L447](../Shared/Model/ChatModel.swift#L447) |
| `membersLoaded` | `Bool` | Whether members have been loaded | [L448](../Shared/Model/ChatModel.swift#L448) |
| `secondaryIM` | `ItemsModel?` | Secondary items model (member support scope, channel comments thread, or content-type filter) | [L499](../Shared/Model/ChatModel.swift#L499) |

#### Authentication
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `contentViewAccessAuthenticated` | `Bool` | Whether user has passed authentication | [L434](../Shared/Model/ChatModel.swift#L434) |
| `laRequest` | `LocalAuthRequest?` | Pending authentication request | [L435](../Shared/Model/ChatModel.swift#L435) |

#### Notifications
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `deviceToken` | `DeviceToken?` | Current APNs device token | [L459](../Shared/Model/ChatModel.swift#L459) |
| `savedToken` | `DeviceToken?` | Previously saved token | [L460](../Shared/Model/ChatModel.swift#L460) |
| `tokenRegistered` | `Bool` | Whether token is registered with server | [L461](../Shared/Model/ChatModel.swift#L461) |
| `tokenStatus` | `NtfTknStatus?` | Token registration status | [L463](../Shared/Model/ChatModel.swift#L463) |
| `notificationMode` | `NotificationsMode` | Current notification mode (.off/.periodic/.instant) | [L464](../Shared/Model/ChatModel.swift#L464) |
| `notificationServer` | `String?` | Notification server URL | [L465](../Shared/Model/ChatModel.swift#L465) |
| `notificationPreview` | `NotificationPreviewMode` | What to show in notifications | [L466](../Shared/Model/ChatModel.swift#L466) |
| `notificationResponse` | `UNNotificationResponse?` | Pending notification action | [L432](../Shared/Model/ChatModel.swift#L432) |
| `ntfContactRequest` | `NTFContactRequest?` | Pending contact request from notification | [L468](../Shared/Model/ChatModel.swift#L468) |
| `ntfCallInvitationAction` | `(ChatId, NtfCallAction)?` | Pending call action from notification | [L469](../Shared/Model/ChatModel.swift#L469) |

#### Calls
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `callInvitations` | `[ChatId: RcvCallInvitation]` | Pending incoming call invitations | [L471](../Shared/Model/ChatModel.swift#L471) |
| `activeCall` | `Call?` | Currently active call | [L472](../Shared/Model/ChatModel.swift#L472) |
| `callCommand` | `WebRTCCommandProcessor` | WebRTC command queue | [L473](../Shared/Model/ChatModel.swift#L473) |
| `showCallView` | `Bool` | Whether to show full-screen call UI | [L474](../Shared/Model/ChatModel.swift#L474) |
| `activeCallViewIsCollapsed` | `Bool` | Whether call view is in PiP mode | [L475](../Shared/Model/ChatModel.swift#L475) |

#### Remote Desktop
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `remoteCtrlSession` | `RemoteCtrlSession?` | Active remote desktop session | [L478](../Shared/Model/ChatModel.swift#L478) |

#### Misc
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `userAddress` | `UserContactLink?` | User's SimpleX address | [L455](../Shared/Model/ChatModel.swift#L455) |
| `chatItemTTL` | `ChatItemTTL` | Global message TTL | [L456](../Shared/Model/ChatModel.swift#L456) |
| `appOpenUrl` | `URL?` | URL opened while app active | [L457](../Shared/Model/ChatModel.swift#L457) |
| `appOpenUrlLater` | `URL?` | URL opened while app inactive | [L458](../Shared/Model/ChatModel.swift#L458) |
| `showingInvitation` | `ShowingInvitation?` | Currently displayed invitation | [L480](../Shared/Model/ChatModel.swift#L480) |
| `draft` | `ComposeState?` | Saved compose draft | [L484](../Shared/Model/ChatModel.swift#L484) |
| `draftChatId` | `String?` | Chat ID for saved draft | [L485](../Shared/Model/ChatModel.swift#L485) |
| `networkInfo` | `UserNetworkInfo` | Current network type and status | [L486](../Shared/Model/ChatModel.swift#L486) |
| `conditions` | `ServerOperatorConditions` | Server usage conditions | [L488](../Shared/Model/ChatModel.swift#L488) |
| `stopPreviousRecPlay` | `URL?` | Currently playing audio source | [L483](../Shared/Model/ChatModel.swift#L483) |

### Non-Published Properties

| Property | Type | Description | Line |
|----------|------|-------------|------|
| `messageDelivery` | `[Int64: () -> Void]` | Pending delivery confirmation callbacks | [L490](../Shared/Model/ChatModel.swift#L490) |
| `filesToDelete` | `Set<URL>` | Files queued for deletion | [L492](../Shared/Model/ChatModel.swift#L492) |
| `im` | `ItemsModel` | Reference to `ItemsModel.shared` | [L496](../Shared/Model/ChatModel.swift#L496) |

### Key Methods

| Method | Description | Line |
|--------|-------------|------|
| `getUser(_ userId:)` | Find user by ID | [L519](../Shared/Model/ChatModel.swift#L519) |
| `updateUser(_ user:)` | Update user in list and current | [L530](../Shared/Model/ChatModel.swift#L530) |
| `removeUser(_ user:)` | Remove user from list | [L540](../Shared/Model/ChatModel.swift#L540) |
| `getChat(_ id:)` | Find chat by ID | [L551](../Shared/Model/ChatModel.swift#L551) |
| `addChat(_ chat:)` | Add chat to list | [L606](../Shared/Model/ChatModel.swift#L606) |
| `updateChatInfo(_ cInfo:)` | Update chat metadata | [L620](../Shared/Model/ChatModel.swift#L620) |
| `replaceChat(_ id:, _ chat:)` | Replace chat in list | [L672](../Shared/Model/ChatModel.swift#L672) |
| `getCIItemsModel(_ cInfo:_ ci:)` | Resolve which `ItemsModel` an incoming item belongs to (primary `im`, secondary `secondaryIM`, or `nil`). For the channel-comments thread it routes via `ci.parentChatItemId == parent.id`. | [L747](../Shared/Model/ChatModel.swift#L747) |
| `removeChat(_ id:)` | Remove chat from list | [L1290](../Shared/Model/ChatModel.swift#L1290) |
| `popChat(_ id:)` | Move chat to top of list | [L1266](../Shared/Model/ChatModel.swift#L1266) |
| `totalUnreadCountForAllUsers()` | Sum unread across all users | [L1166](../Shared/Model/ChatModel.swift#L1166) |

---

## 3. [ItemsModel](../Shared/Model/ChatModel.swift#L80-L227)

**Class**: `class ItemsModel: ObservableObject`
**Primary singleton**: `ItemsModel.shared`
**Secondary instances**: Created via `ItemsModel.loadSecondaryChat()` for scope-based views (member support chat, channel comments thread, reports filter).
**Source**: [`Shared/Model/ChatModel.swift`](../Shared/Model/ChatModel.swift#L80)

### Properties

| Property | Type | Description | Line |
|----------|------|-------------|------|
| `reversedChatItems` | `[ChatItem]` | Messages in reverse chronological order (newest first) | [L86](../Shared/Model/ChatModel.swift#L86) |
| `itemAdded` | `Bool` | Flag indicating a new item was added | [L89](../Shared/Model/ChatModel.swift#L89) |
| `chatState` | `ActiveChatState` | Pagination splits and loaded ranges | [L93](../Shared/Model/ChatModel.swift#L93) |
| `isLoading` | `Bool` | Whether messages are currently loading | [L97](../Shared/Model/ChatModel.swift#L97) |
| `showLoadingProgress` | `ChatId?` | Chat ID showing loading spinner | [L98](../Shared/Model/ChatModel.swift#L98) |
| `preloadState` | `PreloadState` | State for infinite-scroll preloading | [L83](../Shared/Model/ChatModel.swift#L83) |
| `secondaryIMFilter` | `SecondaryItemsModelFilter?` | Filter for secondary instances | [L82](../Shared/Model/ChatModel.swift#L82) |

### Computed Properties

| Property | Type | Description | Line |
|----------|------|-------------|------|
| `lastItemsLoaded` | `Bool` | Whether the oldest messages have been loaded | [L103](../Shared/Model/ChatModel.swift#L103) |
| `contentTag` | `MsgContentTag?` | Content type filter (if secondary) | [L212](../Shared/Model/ChatModel.swift#L212) |
| `groupScopeInfo` | `GroupChatScopeInfo?` | Group scope filter (if secondary) | [L220](../Shared/Model/ChatModel.swift#L220) |

### Throttling

`ItemsModel` uses a custom publisher throttle (0.2 seconds) to batch rapid updates to `reversedChatItems` and prevent excessive SwiftUI re-renders:

```swift
publisher
    .throttle(for: 0.2, scheduler: DispatchQueue.main, latest: true)
    .sink { self.objectWillChange.send() }
    .store(in: &bag)
```

Direct `@Published` properties (`isLoading`, `showLoadingProgress`) bypass throttling for immediate UI response.

### Key Methods

| Method | Description | Line |
|--------|-------------|------|
| `loadOpenChat(_ chatId:)` | Load chat with 250ms navigation delay | [L170](../Shared/Model/ChatModel.swift#L170) |
| `loadOpenChatNoWait(_ chatId:, _ openAroundItemId:)` | Load chat without delay | [L196](../Shared/Model/ChatModel.swift#L196) |
| `loadSecondaryChat(_ chatId:, chatFilter:)` | Create secondary `ItemsModel` instance. For `.groupChannelMsgContext` it fetches via `apiGetChat(parentItemId:)` and injects a local-only `ChannelMsgInfo` into the returned `ChatInfo.group`. | [L116](../Shared/Model/ChatModel.swift#L116) |

### [SecondaryItemsModelFilter](../Shared/Model/ChatModel.swift#L58-L76)

Used for secondary chat views (e.g., group member support scope, content type filter, channel comments thread):

```swift
enum SecondaryItemsModelFilter {
    case groupChatScopeContext(groupScopeInfo: GroupChatScopeInfo)
    case msgContentTagContext(contentTag: MsgContentTag)
    case groupChannelMsgContext(parent: ChatItem)
}
```

`.groupChannelMsgContext` is the channel-comments-thread scope. The associated `parent` carries the channel post being commented on; the comments-thread view routes inbound items via `ci.parentChatItemId == parent.id`. The parent's `meta.itemSharedMsgId` is required before opening the thread (the owner-side parent may briefly lack a shared id during send).

### Channel comments thread

Opening a comments thread bypasses the standard `loadOpenChat` flow because the comments scope is not represented on the wire as a `GroupChatScope`. Instead:

1. The caller invokes [`ItemsModel.loadSecondaryChat`](../Shared/Model/ChatModel.swift#L116-L153) with `chatFilter: .groupChannelMsgContext(parent:)`.
2. Items are fetched via `apiGetChat(..., parentItemId: parent.id)` (see `apiGetChat` in [spec/api.md §2.3](api.md#23-chat--message-operations)).
3. The returned `ChatInfo.group` has its third associated value rewritten by [`injectChannelMsgInfo`](../Shared/Model/ChatModel.swift#L158-L167) to embed a local-only [`ChannelMsgInfo`](../SimpleXChat/ChatTypes.swift#L2028-L2036) carrier so toolbar and routing logic can read the parent without a second lookup.
4. [`getCIItemsModel`](../Shared/Model/ChatModel.swift#L747-L777) gains a new branch (`cInfo.channelMsgInfo() != nil`) that routes events into the secondary `ItemsModel` when `ci.parentChatItemId == parent.id`.
5. Gating sites that previously checked `cInfo.groupChatScope() == nil` to decide whether to update the main chat preview now also check `cInfo.channelMsgInfo() == nil`, so comment items do not bubble into the main chat list.

The carrier is **not** serialized: `ChannelMsgInfo` is `Decodable` only so it nests cleanly into `ChatInfo.group`, but the server never sends this field — it is injected exclusively by `loadSecondaryChat`. Inbound `XMsgNew` items with a `parent` come back over the regular event stream; they are routed by `ci.parentChatItemId` alone.

---

## 4. [ChatTagsModel](../Shared/Model/ChatModel.swift#L242-L344)

**Class**: `class ChatTagsModel: ObservableObject`
**Singleton**: `ChatTagsModel.shared`
**Source**: [`Shared/Model/ChatModel.swift`](../Shared/Model/ChatModel.swift#L242)

### Properties

| Property | Type | Description | Line |
|----------|------|-------------|------|
| `userTags` | `[ChatTag]` | User-defined tags | [L245](../Shared/Model/ChatModel.swift#L245) |
| `activeFilter` | `ActiveFilter?` | Currently active filter tab | [L246](../Shared/Model/ChatModel.swift#L246) |
| `presetTags` | `[PresetTag: Int]` | Preset tag counts (groups, contacts, favorites, etc.) | [L247](../Shared/Model/ChatModel.swift#L247) |
| `unreadTags` | `[Int64: Int]` | Unread count per user tag | [L248](../Shared/Model/ChatModel.swift#L248) |

### [ActiveFilter](../Shared/Views/ChatList/ChatListView.swift#L52)

```swift
enum ActiveFilter {
    case presetTag(PresetTag)   // .favorites, .contacts, .groups, .business, .groupReports
    case userTag(ChatTag)       // User-defined tag
    case unread                 // Unread conversations
}
```

---

## 5. [ChannelRelaysModel](../Shared/Model/ChatModel.swift#L389-L413)

**Class**: `class ChannelRelaysModel: ObservableObject`
**Singleton**: `ChannelRelaysModel.shared`
**Source**: [`Shared/Model/ChatModel.swift`](../Shared/Model/ChatModel.swift#L389)

Holds runtime relay state for the currently viewed channel. Used by `ChannelRelaysView` to display and manage relays. Reset when the view is dismissed.

### Properties

| Property | Type | Description | Line |
|----------|------|-------------|------|
| `groupId` | `Int64?` | Group ID of the channel whose relays are loaded | [L391](../Shared/Model/ChatModel.swift#L391) |
| `groupRelays` | `[GroupRelay]` | Current relay instances for the channel | [L392](../Shared/Model/ChatModel.swift#L392) |

### Methods

| Method | Description | Line |
|--------|-------------|------|
| `set(groupId:groupRelays:)` | Populate all properties at once | [L394](../Shared/Model/ChatModel.swift#L394) |
| `reset()` | Clear all properties to nil/empty | [L409](../Shared/Model/ChatModel.swift#L409) |

---

## 6. [Chat](../Shared/Model/ChatModel.swift#L1380-L1432)

**Class**: `final class Chat: ObservableObject, Identifiable, ChatLike`
**Source**: [`Shared/Model/ChatModel.swift`](../Shared/Model/ChatModel.swift#L1380)

Represents a single conversation in the chat list. Each `Chat` is an independent observable object.

### Properties

| Property | Type | Description | Line |
|----------|------|-------------|------|
| `chatInfo` | `ChatInfo` | Conversation type and metadata | [L1381](../Shared/Model/ChatModel.swift#L1381) |
| `chatItems` | `[ChatItem]` | Preview items (typically last message) | [L1382](../Shared/Model/ChatModel.swift#L1382) |
| `chatStats` | `ChatStats` | Unread counts and min unread item ID | [L1383](../Shared/Model/ChatModel.swift#L1383) |
| `created` | `Date` | Creation timestamp | [L1384](../Shared/Model/ChatModel.swift#L1384) |

### [ChatStats](../SimpleXChat/ChatTypes.swift#L1966-L1988)

```swift
struct ChatStats: Decodable, Hashable {
    var unreadCount: Int = 0
    var unreadMentions: Int = 0
    var reportsCount: Int = 0
    var minUnreadItemId: Int64 = 0
    var unreadChat: Bool = false
}
```

### Computed Properties

| Property | Description | Line |
|----------|-------------|------|
| `id` | Chat ID from `chatInfo.id` | [L1415](../Shared/Model/ChatModel.swift#L1415) |
| `viewId` | Unique view identity including creation time | [L1417](../Shared/Model/ChatModel.swift#L1417) |
| `unreadTag` | Whether chat counts as "unread" based on notification settings | [L1407](../Shared/Model/ChatModel.swift#L1407) |
| `supportUnreadCount` | Unread count for group support scope | [L1419](../Shared/Model/ChatModel.swift#L1419) |

---

## 7. [ChatInfo](../SimpleXChat/ChatTypes.swift#L1435-L1941)

**Enum**: `public enum ChatInfo: Identifiable, Decodable, NamedChat, Hashable`
**Source**: [`SimpleXChat/ChatTypes.swift`](../SimpleXChat/ChatTypes.swift#L1435)

Represents the type and metadata of a conversation:

```swift
public enum ChatInfo: Identifiable, Decodable, NamedChat, Hashable {
    case direct(contact: Contact)
    case group(groupInfo: GroupInfo, groupChatScope: GroupChatScopeInfo?, channelMsgInfo: ChannelMsgInfo?)
    case local(noteFolder: NoteFolder)
    case contactRequest(contactRequest: UserContactRequest)
    case contactConnection(contactConnection: PendingContactConnection)
    case invalidJSON(json: Data?)
}
```

### Cases

| Case | Associated Value | Description |
|------|-----------------|-------------|
| `.direct` | `Contact` | One-to-one conversation |
| `.group` | `GroupInfo, GroupChatScopeInfo?, ChannelMsgInfo?` | Group conversation. Optional `GroupChatScopeInfo` selects a member support thread; optional `ChannelMsgInfo` (set client-side only) selects a channel-comments thread under a specific post. The two are mutually exclusive in practice. |
| `.local` | `NoteFolder` | Local notes (self-chat) |
| `.contactRequest` | `UserContactRequest` | Incoming contact request |
| `.contactConnection` | `PendingContactConnection` | Pending connection |
| `.invalidJSON` | `Data?` | Undecodable chat data |

### [ChannelMsgInfo](../SimpleXChat/ChatTypes.swift#L2028-L2036)

```swift
public struct ChannelMsgInfo: Decodable, Hashable {
    public var channelMsgItem: ChatItem
    public var channelMsgSharedId: String
}
```

Local-only carrier for the comments-thread view. Holds the parent channel post (`channelMsgItem`) and its shared message id (`channelMsgSharedId`, hoisted from `parent.meta.itemSharedMsgId`). The struct is `Decodable` so it can sit inside the `ChatInfo.group` enum case, but the Haskell core never emits this field — `ItemsModel.loadSecondaryChat` injects it after fetching the thread by `parentItemId=`. See [§3 Channel comments thread](#channel-comments-thread).

### Key Computed Properties on ChatInfo

| Property | Type | Description |
|----------|------|-------------|
| `chatType` | `ChatType` | `.direct`, `.group`, `.local`, `.contactRequest`, `.contactConnection` |
| `id` | `ChatId` | Prefixed ID (e.g., `"@1"` for direct, `"#5"` for group) |
| `displayName` | `String` | Contact/group name |
| `image` | `String?` | Profile image (base64) |
| `chatSettings` | `ChatSettings?` | Notification/favorite settings |
| `chatTags` | `[Int64]?` | Assigned tag IDs |

### Relay-Related Data Model (Channels)

A **channel** is a group with `groupInfo.useRelays == true`. These types support the relay/channel infrastructure:

#### New Fields on Existing Types

| Type | Field | Type | Description | Line |
|------|-------|------|-------------|------|
| `User` | `userChatRelay` | `Bool` | Whether user acts as a chat relay | [L46](../SimpleXChat/ChatTypes.swift#L46) |
| `GroupInfo` | `useRelays` | `Bool` | Whether group uses relay infrastructure (channel mode) | [L2448](../SimpleXChat/ChatTypes.swift#L2448) |
| `GroupInfo` | `relayOwnStatus` | `RelayStatus?` | Current user's relay status in this group | [L2449](../SimpleXChat/ChatTypes.swift#L2449) |
| `GroupProfile` | `publicGroup` | `PublicGroupProfile?` | Channel-specific profile data (type, link, ID) | [L2588](../SimpleXChat/ChatTypes.swift#L2588) |

#### New Types

| Type | Kind | Description | Line |
|------|------|-------------|------|
| `RelayStatus` | `enum` | Relay lifecycle: `.rsNew`, `.rsInvited`, `.rsAccepted`, `.rsActive` | [L2659](../SimpleXChat/ChatTypes.swift#L2659) |
| `RelayStatus.text` | `extension` | Localized display text: New/Invited/Accepted/Active | [L2730](../SimpleXChat/ChatTypes.swift#L2730) |
| `GroupRelay` | `struct` | Relay instance for a group (ID, member ID, relay status). Fetched at runtime via `apiGetGroupRelays` (owner only) | [L2721](../SimpleXChat/ChatTypes.swift#L2721) |
| `UserChatRelay` | `struct` | User's chat relay configuration (ID, SMP address, name, domains, preset/tested/enabled/deleted flags) | [L2674](../SimpleXChat/ChatTypes.swift#L2674) |

#### New Enum Cases

| Enum | Case | Description | Line |
|------|------|-------------|------|
| `GroupMemberRole` | `.relay` | Role for relay members (below `.observer`) | [L2974](../SimpleXChat/ChatTypes.swift#L2974) |
| `CIDirection` | `.channelRcv` | Message direction for channel-received messages (via relay) | [L3737](../SimpleXChat/ChatTypes.swift#L3737) |

### Comments-Related Data Model (Channel Comments)

A **comment** is a `ChatItem` whose `parentChatItemId` references a channel post. The parent post is the only `ChatItem` that carries `commentsTotal`/`commentsDisabled`. Comments live in a secondary `ItemsModel` keyed by [`.groupChannelMsgContext(parent:)`](#secondaryitemsmodelfilter); the channel-post-list view (slice 4) uses `commentsTotal` to render the comments-button counter.

#### New Fields on Existing Types

| Type | Field | Type | Description | Line |
|------|-------|------|-------------|------|
| `ChatItem` | `parentChatItemId` | `Int64?` | Parent post id if this item is a comment. Hoisted from `CIMeta.parentChatItemId` on decode for caller ergonomics; `nil` means the item is not a comment. | [L3289](../SimpleXChat/ChatTypes.swift#L3289) |
| `ChatItem` | `commentsTotal` | `Int` | Total comments under this item (only meaningful when this item is a channel post). Defaults to `0` via `decodeIfPresent ?? 0`, mirroring Haskell `omittedField`. | [L3290](../SimpleXChat/ChatTypes.swift#L3290) |
| `ChatItem` | `commentsDisabled` | `Bool` | Whether the channel owner has disabled comments under this post. Defaults to `false` via `decodeIfPresent ?? false`. | [L3291](../SimpleXChat/ChatTypes.swift#L3291) |
| `CIMeta` | `itemSharedMsgId` | `String?` | Shared message id (`SharedMsgId`) used to address this item across members. Needed to open a comments thread under an owner-side parent that has just been sent. | [L3783](../SimpleXChat/ChatTypes.swift#L3783) |

The three `ChatItem` fields are wire-encoded inside the nested `meta` object (Haskell `CIMeta`). The iOS [`ChatItem.init(from decoder:)`](../SimpleXChat/ChatTypes.swift#L3304-L3320) hoists them to `ChatItem` level so that view code can write `ci.parentChatItemId` and `ci.commentsTotal` directly without descending into `ci.meta`. Encoding round-trips remain wire-compatible because the iOS `ChatItem` is `Decodable`-only.

#### New Types

| Type | Kind | Description | Line |
|------|------|-------------|------|
| `ChannelMsgInfo` | `struct` | Local-only carrier for the comments-thread view. See [§7 ChannelMsgInfo](#channelmsginfo). | [L2028](../SimpleXChat/ChatTypes.swift#L2028) |

---

## 8. State Flow

### App Start
```
SimpleXApp.init()
    → haskell_init()
    → initChatAndMigrate()
        → chat_migrate_init_key() -- creates/opens DB
        → startChat(mainApp: true) -- starts core
        → apiGetChats(userId) -- populates ChatModel.chats
        → UI renders ChatListView
```

### Opening a Chat
```
User taps chat in ChatListView
    → ItemsModel.loadOpenChat(chatId)
        → 250ms delay for navigation animation
        → ChatModel.chatId = chatId
        → loadChat(chatId:, im:)
            → apiGetChat(chatId, pagination: .last(count: 50))
            → ItemsModel.reversedChatItems = [ChatItem]
        → ChatView renders messages
```

### Receiving a Message (Event)
```
Haskell core generates ChatEvent.newChatItems
    → Event loop calls chat_recv_msg_wait
    → Decoded as ChatEvent.newChatItems(user, chatItems)
    → ChatModel updates:
        1. Insert new Chat items into ChatModel.chats (preview)
        2. If chat is open: insert into ItemsModel.reversedChatItems
        3. Update ChatStats (unread counts)
        4. Update ChatTagsModel (tag unread counts)
    → SwiftUI re-renders affected views via @Published observation
```

### Sending a Message
```
User taps send in ComposeView
    → apiSendMessages(type, id, scope, live, ttl, composedMessages)
    → Haskell processes, returns ChatResponse1.newChatItems
    → ChatModel.chats updated with new preview
    → ItemsModel.reversedChatItems gets new item
    → ChatView scrolls to bottom, shows sent message
```

---

## 9. Preference Storage

### UserDefaults (via @AppStorage)

App-level UI settings stored in `UserDefaults.standard`:

| Key Constant | Type | Description |
|--------------|------|-------------|
| `DEFAULT_PERFORM_LA` | `Bool` | Enable local authentication |
| `DEFAULT_PRIVACY_PROTECT_SCREEN` | `Bool` | Hide screen in app switcher |
| `DEFAULT_SHOW_LA_NOTICE` | `Bool` | Show LA setup notice |
| `DEFAULT_NOTIFICATION_ALERT_SHOWN` | `Bool` | Notification permission alert shown |
| `DEFAULT_CALL_KIT_CALLS_IN_RECENTS` | `Bool` | Show CallKit calls in recents |

### GroupDefaults

Settings shared between main app and extensions (NSE, SE) via app group `UserDefaults`:

| Key | Description |
|-----|-------------|
| `appStateGroupDefault` | Current app state (.active/.suspended/.stopped) |
| `dbContainerGroupDefault` | Database container location (.group/.documents) |
| `ntfPreviewModeGroupDefault` | Notification preview mode |
| `storeDBPassphraseGroupDefault` | Whether to store DB passphrase |
| `callKitEnabledGroupDefault` | Whether CallKit is enabled |
| `onboardingStageDefault` | Current onboarding stage |
| `currentThemeDefault` | Current theme name |
| `systemDarkThemeDefault` | Dark mode theme name |
| `themeOverridesDefault` | Custom theme overrides |
| `currentThemeIdsDefault` | Active theme override IDs |

### Keychain (KeyChain wrapper)

Sensitive data stored in iOS Keychain:

| Key | Description |
|-----|-------------|
| `kcDatabasePassword` | SQLite database encryption key |
| `kcAppPassword` | App lock password |
| `kcSelfDestructPassword` | Self-destruct trigger password |

### Haskell DB (via apiSaveSettings / apiGetSettings)

Chat-level preferences stored in the SQLite database (managed by Haskell core):

- Per-contact preferences (timed messages, voice, calls, etc.)
- Per-group preferences
- Per-user notification settings
- Network configuration
- Server lists

---

## Source Files

| File | Path |
|------|------|
| ChatModel, ItemsModel, Chat, ChatTagsModel, ChannelRelaysModel | [`Shared/Model/ChatModel.swift`](../Shared/Model/ChatModel.swift) |
| ChatInfo, User, Contact, GroupInfo, ChatItem | [`SimpleXChat/ChatTypes.swift`](../SimpleXChat/ChatTypes.swift) |
| ActiveFilter | [`Shared/Views/ChatList/ChatListView.swift`](../Shared/Views/ChatList/ChatListView.swift#L52) |
| Preference defaults | [`Shared/Model/ChatModel.swift`](../Shared/Model/ChatModel.swift), [`SimpleXChat/FileUtils.swift`](../SimpleXChat/FileUtils.swift) |
