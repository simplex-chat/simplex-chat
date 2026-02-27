# SimpleX Chat iOS -- State Management

**Source:** [`ChatModel.swift`](../Shared/Model/ChatModel.swift#L1-L1393) | [`ChatTypes.swift`](../SimpleXChat/ChatTypes.swift#L1-L5333)

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

## 2. [ChatModel](../Shared/Model/ChatModel.swift#L356-L1278)

**Class**: `final class ChatModel: ObservableObject`
**Singleton**: `ChatModel.shared`
**Source**: [`Shared/Model/ChatModel.swift`](../Shared/Model/ChatModel.swift#L356)

### Key Published Properties

#### App Lifecycle
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `onboardingStage` | `OnboardingStage?` | Current onboarding step | [L357](../Shared/Model/ChatModel.swift#L357) |
| `chatInitialized` | `Bool` | Whether chat has been initialized | [L366](../Shared/Model/ChatModel.swift#L366) |
| `chatRunning` | `Bool?` | Whether chat engine is running | [L367](../Shared/Model/ChatModel.swift#L367) |
| `chatDbChanged` | `Bool` | Whether DB was changed externally | [L368](../Shared/Model/ChatModel.swift#L368) |
| `chatDbEncrypted` | `Bool?` | Whether DB is encrypted | [L369](../Shared/Model/ChatModel.swift#L369) |
| `chatDbStatus` | `DBMigrationResult?` | DB migration status | [L370](../Shared/Model/ChatModel.swift#L370) |
| `ctrlInitInProgress` | `Bool` | Whether controller is initializing | [L371](../Shared/Model/ChatModel.swift#L371) |
| `migrationState` | `MigrationToState?` | Device migration state | [L417](../Shared/Model/ChatModel.swift#L417) |

#### User State
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `currentUser` | `User?` | Active user profile (triggers theme reapply on change) | [L360](../Shared/Model/ChatModel.swift#L360) |
| `users` | `[UserInfo]` | All user profiles | [L365](../Shared/Model/ChatModel.swift#L365) |
| `v3DBMigration` | `V3DBMigrationState` | Legacy DB migration state | [L359](../Shared/Model/ChatModel.swift#L359) |

#### Chat List
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `chats` | `[Chat]` (private set) | All conversations for current user | [L377](../Shared/Model/ChatModel.swift#L377) |
| `deletedChats` | `Set<String>` | Chat IDs pending deletion animation | [L378](../Shared/Model/ChatModel.swift#L378) |

#### Active Chat
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `chatId` | `String?` | Currently open chat ID | [L380](../Shared/Model/ChatModel.swift#L380) |
| `chatAgentConnId` | `String?` | Agent connection ID for active chat | [L381](../Shared/Model/ChatModel.swift#L381) |
| `chatSubStatus` | `SubscriptionStatus?` | Active chat subscription status | [L382](../Shared/Model/ChatModel.swift#L382) |
| `openAroundItemId` | `ChatItem.ID?` | Item to scroll to when opening | [L383](../Shared/Model/ChatModel.swift#L383) |
| `chatToTop` | `String?` | Chat to scroll to top | [L384](../Shared/Model/ChatModel.swift#L384) |
| `groupMembers` | `[GMember]` | Members of active group | [L385](../Shared/Model/ChatModel.swift#L385) |
| `groupMembersIndexes` | `[Int64: Int]` | Member ID to index mapping | [L386](../Shared/Model/ChatModel.swift#L386) |
| `membersLoaded` | `Bool` | Whether members have been loaded | [L387](../Shared/Model/ChatModel.swift#L387) |
| `secondaryIM` | `ItemsModel?` | Secondary items model (e.g. support chat scope) | [L435](../Shared/Model/ChatModel.swift#L435) |

#### Authentication
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `contentViewAccessAuthenticated` | `Bool` | Whether user has passed authentication | [L374](../Shared/Model/ChatModel.swift#L374) |
| `laRequest` | `LocalAuthRequest?` | Pending authentication request | [L375](../Shared/Model/ChatModel.swift#L375) |

#### Notifications
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `deviceToken` | `DeviceToken?` | Current APNs device token | [L395](../Shared/Model/ChatModel.swift#L395) |
| `savedToken` | `DeviceToken?` | Previously saved token | [L396](../Shared/Model/ChatModel.swift#L396) |
| `tokenRegistered` | `Bool` | Whether token is registered with server | [L397](../Shared/Model/ChatModel.swift#L397) |
| `tokenStatus` | `NtfTknStatus?` | Token registration status | [L399](../Shared/Model/ChatModel.swift#L399) |
| `notificationMode` | `NotificationsMode` | Current notification mode (.off/.periodic/.instant) | [L400](../Shared/Model/ChatModel.swift#L400) |
| `notificationServer` | `String?` | Notification server URL | [L401](../Shared/Model/ChatModel.swift#L401) |
| `notificationPreview` | `NotificationPreviewMode` | What to show in notifications | [L402](../Shared/Model/ChatModel.swift#L402) |
| `notificationResponse` | `UNNotificationResponse?` | Pending notification action | [L372](../Shared/Model/ChatModel.swift#L372) |
| `ntfContactRequest` | `NTFContactRequest?` | Pending contact request from notification | [L404](../Shared/Model/ChatModel.swift#L404) |
| `ntfCallInvitationAction` | `(ChatId, NtfCallAction)?` | Pending call action from notification | [L405](../Shared/Model/ChatModel.swift#L405) |

#### Calls
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `callInvitations` | `[ChatId: RcvCallInvitation]` | Pending incoming call invitations | [L407](../Shared/Model/ChatModel.swift#L407) |
| `activeCall` | `Call?` | Currently active call | [L408](../Shared/Model/ChatModel.swift#L408) |
| `callCommand` | `WebRTCCommandProcessor` | WebRTC command queue | [L409](../Shared/Model/ChatModel.swift#L409) |
| `showCallView` | `Bool` | Whether to show full-screen call UI | [L410](../Shared/Model/ChatModel.swift#L410) |
| `activeCallViewIsCollapsed` | `Bool` | Whether call view is in PiP mode | [L411](../Shared/Model/ChatModel.swift#L411) |

#### Remote Desktop
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `remoteCtrlSession` | `RemoteCtrlSession?` | Active remote desktop session | [L414](../Shared/Model/ChatModel.swift#L414) |

#### Misc
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `userAddress` | `UserContactLink?` | User's SimpleX address | [L391](../Shared/Model/ChatModel.swift#L391) |
| `chatItemTTL` | `ChatItemTTL` | Global message TTL | [L392](../Shared/Model/ChatModel.swift#L392) |
| `appOpenUrl` | `URL?` | URL opened while app active | [L393](../Shared/Model/ChatModel.swift#L393) |
| `appOpenUrlLater` | `URL?` | URL opened while app inactive | [L394](../Shared/Model/ChatModel.swift#L394) |
| `showingInvitation` | `ShowingInvitation?` | Currently displayed invitation | [L416](../Shared/Model/ChatModel.swift#L416) |
| `draft` | `ComposeState?` | Saved compose draft | [L420](../Shared/Model/ChatModel.swift#L420) |
| `draftChatId` | `String?` | Chat ID for saved draft | [L421](../Shared/Model/ChatModel.swift#L421) |
| `networkInfo` | `UserNetworkInfo` | Current network type and status | [L422](../Shared/Model/ChatModel.swift#L422) |
| `conditions` | `ServerOperatorConditions` | Server usage conditions | [L424](../Shared/Model/ChatModel.swift#L424) |
| `stopPreviousRecPlay` | `URL?` | Currently playing audio source | [L419](../Shared/Model/ChatModel.swift#L419) |

### Non-Published Properties

| Property | Type | Description | Line |
|----------|------|-------------|------|
| `messageDelivery` | `[Int64: () -> Void]` | Pending delivery confirmation callbacks | [L426](../Shared/Model/ChatModel.swift#L426) |
| `filesToDelete` | `Set<URL>` | Files queued for deletion | [L428](../Shared/Model/ChatModel.swift#L428) |
| `im` | `ItemsModel` | Reference to `ItemsModel.shared` | [L432](../Shared/Model/ChatModel.swift#L432) |

### Key Methods

| Method | Description | Line |
|--------|-------------|------|
| `getUser(_ userId:)` | Find user by ID | [L455](../Shared/Model/ChatModel.swift#L455) |
| `updateUser(_ user:)` | Update user in list and current | [L466](../Shared/Model/ChatModel.swift#L466) |
| `removeUser(_ user:)` | Remove user from list | [L476](../Shared/Model/ChatModel.swift#L476) |
| `getChat(_ id:)` | Find chat by ID | [L487](../Shared/Model/ChatModel.swift#L487) |
| `addChat(_ chat:)` | Add chat to list | [L542](../Shared/Model/ChatModel.swift#L542) |
| `updateChatInfo(_ cInfo:)` | Update chat metadata | [L556](../Shared/Model/ChatModel.swift#L556) |
| `replaceChat(_ id:, _ chat:)` | Replace chat in list | [L608](../Shared/Model/ChatModel.swift#L608) |
| `removeChat(_ id:)` | Remove chat from list | [L1217](../Shared/Model/ChatModel.swift#L1217) |
| `popChat(_ id:, _ ts:)` | Move chat to top of list | [L1193](../Shared/Model/ChatModel.swift#L1193) |
| `totalUnreadCountForAllUsers()` | Sum unread across all users | [L1093](../Shared/Model/ChatModel.swift#L1093) |

---

## 3. [ItemsModel](../Shared/Model/ChatModel.swift#L74-L174)

**Class**: `class ItemsModel: ObservableObject`
**Primary singleton**: `ItemsModel.shared`
**Secondary instances**: Created via `ItemsModel.loadSecondaryChat()` for scope-based views (e.g., group member support chat)
**Source**: [`Shared/Model/ChatModel.swift`](../Shared/Model/ChatModel.swift#L74)

### Properties

| Property | Type | Description | Line |
|----------|------|-------------|------|
| `reversedChatItems` | `[ChatItem]` | Messages in reverse chronological order (newest first) | [L78](../Shared/Model/ChatModel.swift#L80) |
| `itemAdded` | `Bool` | Flag indicating a new item was added | [L81](../Shared/Model/ChatModel.swift#L83) |
| `chatState` | `ActiveChatState` | Pagination splits and loaded ranges | [L85](../Shared/Model/ChatModel.swift#L87) |
| `isLoading` | `Bool` | Whether messages are currently loading | [L89](../Shared/Model/ChatModel.swift#L91) |
| `showLoadingProgress` | `ChatId?` | Chat ID showing loading spinner | [L90](../Shared/Model/ChatModel.swift#L92) |
| `preloadState` | `PreloadState` | State for infinite-scroll preloading | [L75](../Shared/Model/ChatModel.swift#L77) |
| `secondaryIMFilter` | `SecondaryItemsModelFilter?` | Filter for secondary instances | [L74](../Shared/Model/ChatModel.swift#L76) |

### Computed Properties

| Property | Type | Description | Line |
|----------|------|-------------|------|
| `lastItemsLoaded` | `Bool` | Whether the oldest messages have been loaded | [L95](../Shared/Model/ChatModel.swift#L97) |
| `contentTag` | `MsgContentTag?` | Content type filter (if secondary) | [L154](../Shared/Model/ChatModel.swift#L159) |
| `groupScopeInfo` | `GroupChatScopeInfo?` | Group scope filter (if secondary) | [L162](../Shared/Model/ChatModel.swift#L167) |

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
| `loadOpenChat(_ chatId:)` | Load chat with 250ms navigation delay | [L113](../Shared/Model/ChatModel.swift#L117) |
| `loadOpenChatNoWait(_ chatId:, _ openAroundItemId:)` | Load chat without delay | [L138](../Shared/Model/ChatModel.swift#L143) |
| `loadSecondaryChat(_ chatId:, chatFilter:)` | Create secondary ItemsModel instance | [L107](../Shared/Model/ChatModel.swift#L110) |

### [SecondaryItemsModelFilter](../Shared/Model/ChatModel.swift#L58-L70)

Used for secondary chat views (e.g., group member support scope, content type filter):

```swift
enum SecondaryItemsModelFilter {
    case groupChatScopeContext(groupScopeInfo: GroupChatScopeInfo)
    case msgContentTagContext(contentTag: MsgContentTag)
}
```

---

## 4. [ChatTagsModel](../Shared/Model/ChatModel.swift#L189-L291)

**Class**: `class ChatTagsModel: ObservableObject`
**Singleton**: `ChatTagsModel.shared`
**Source**: [`Shared/Model/ChatModel.swift`](../Shared/Model/ChatModel.swift#L189)

### Properties

| Property | Type | Description | Line |
|----------|------|-------------|------|
| `userTags` | `[ChatTag]` | User-defined tags | [L186](../Shared/Model/ChatModel.swift#L192) |
| `activeFilter` | `ActiveFilter?` | Currently active filter tab | [L187](../Shared/Model/ChatModel.swift#L193) |
| `presetTags` | `[PresetTag: Int]` | Preset tag counts (groups, contacts, favorites, etc.) | [L188](../Shared/Model/ChatModel.swift#L194) |
| `unreadTags` | `[Int64: Int]` | Unread count per user tag | [L189](../Shared/Model/ChatModel.swift#L195) |

### [ActiveFilter](../Shared/Views/ChatList/ChatListView.swift#L52)

```swift
enum ActiveFilter {
    case presetTag(PresetTag)   // .favorites, .contacts, .groups, .business, .groupReports
    case userTag(ChatTag)       // User-defined tag
    case unread                 // Unread conversations
}
```

---

## 5. [ChannelRelaysModel](../Shared/Model/ChatModel.swift#L336-L353)

**Class**: `class ChannelRelaysModel: ObservableObject`
**Singleton**: `ChannelRelaysModel.shared`
**Source**: [`Shared/Model/ChatModel.swift`](../Shared/Model/ChatModel.swift#L336)

Holds runtime relay state for the currently viewed channel. Used by `ChannelRelaysView` to display and manage relays. Reset when the view is dismissed.

### Properties

| Property | Type | Description | Line |
|----------|------|-------------|------|
| `groupId` | `Int64?` | Group ID of the channel whose relays are loaded | [L338](../Shared/Model/ChatModel.swift#L338) |
| `groupLink` | `GroupLink?` | Channel's group link (for relay invitation) | [L339](../Shared/Model/ChatModel.swift#L339) |
| `groupRelays` | `[GroupRelay]` | Current relay instances for the channel | [L340](../Shared/Model/ChatModel.swift#L340) |

### Methods

| Method | Description | Line |
|--------|-------------|------|
| `update(groupId:groupLink:groupRelays:)` | Populate all properties at once | [L342](../Shared/Model/ChatModel.swift#L342) |
| `reset()` | Clear all properties to nil/empty | [L348](../Shared/Model/ChatModel.swift#L348) |

---

## 6. [Chat](../Shared/Model/ChatModel.swift#L1290-L1342)

**Class**: `final class Chat: ObservableObject, Identifiable, ChatLike`
**Source**: [`Shared/Model/ChatModel.swift`](../Shared/Model/ChatModel.swift#L1290)

Represents a single conversation in the chat list. Each `Chat` is an independent observable object.

### Properties

| Property | Type | Description | Line |
|----------|------|-------------|------|
| `chatInfo` | `ChatInfo` | Conversation type and metadata | [L1291](../Shared/Model/ChatModel.swift#L1291) |
| `chatItems` | `[ChatItem]` | Preview items (typically last message) | [L1292](../Shared/Model/ChatModel.swift#L1292) |
| `chatStats` | `ChatStats` | Unread counts and min unread item ID | [L1293](../Shared/Model/ChatModel.swift#L1293) |
| `created` | `Date` | Creation timestamp | [L1294](../Shared/Model/ChatModel.swift#L1294) |

### [ChatStats](../SimpleXChat/ChatTypes.swift#L1880-L1902)

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
| `id` | Chat ID from `chatInfo.id` | [L1325](../Shared/Model/ChatModel.swift#L1325) |
| `viewId` | Unique view identity including creation time | [L1327](../Shared/Model/ChatModel.swift#L1327) |
| `unreadTag` | Whether chat counts as "unread" based on notification settings | [L1317](../Shared/Model/ChatModel.swift#L1317) |
| `supportUnreadCount` | Unread count for group support scope | [L1329](../Shared/Model/ChatModel.swift#L1329) |

---

## 7. [ChatInfo](../SimpleXChat/ChatTypes.swift#L1374-L1855)

**Enum**: `public enum ChatInfo: Identifiable, Decodable, NamedChat, Hashable`
**Source**: [`SimpleXChat/ChatTypes.swift`](../SimpleXChat/ChatTypes.swift#L1374)

Represents the type and metadata of a conversation:

```swift
public enum ChatInfo: Identifiable, Decodable, NamedChat, Hashable {
    case direct(contact: Contact)
    case group(groupInfo: GroupInfo, groupChatScope: GroupChatScopeInfo?)
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
| `.group` | `GroupInfo, GroupChatScopeInfo?` | Group conversation (optional scope for member support threads) |
| `.local` | `NoteFolder` | Local notes (self-chat) |
| `.contactRequest` | `UserContactRequest` | Incoming contact request |
| `.contactConnection` | `PendingContactConnection` | Pending connection |
| `.invalidJSON` | `Data?` | Undecodable chat data |

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
| `GroupInfo` | `useRelays` | `Bool` | Whether group uses relay infrastructure (channel mode) | [L2342](../SimpleXChat/ChatTypes.swift#L2342) |
| `GroupInfo` | `relayOwnStatus` | `RelayStatus?` | Current user's relay status in this group | [L2343](../SimpleXChat/ChatTypes.swift#L2343) |
| `GroupProfile` | `groupLink` | `String?` | Group's short link | [L2451](../SimpleXChat/ChatTypes.swift#L2451) |

#### New Types

| Type | Kind | Description | Line |
|------|------|-------------|------|
| `RelayStatus` | `enum` | Relay lifecycle: `.rsNew`, `.rsInvited`, `.rsAccepted`, `.rsActive` | [L2505](../SimpleXChat/ChatTypes.swift#L2505) |
| `RelayStatus.text` | `extension` | Localized display text: New/Connecting/Accepted/Active | [L2521](../SimpleXChat/ChatTypes.swift#L2521) |
| `GroupRelay` | `struct` | Relay instance for a group (ID, member ID, relay status). Fetched at runtime via `apiGetGroupRelays` (owner only) | [L2512](../SimpleXChat/ChatTypes.swift#L2512) |

#### New Enum Cases

| Enum | Case | Description | Line |
|------|------|-------------|------|
| `GroupMemberRole` | `.relay` | Role for relay members (below `.observer`) | [L2763](../SimpleXChat/ChatTypes.swift#L2763) |
| `CIDirection` | `.channelRcv` | Message direction for channel-received messages (via relay) | [L3485](../SimpleXChat/ChatTypes.swift#L3485) |

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
