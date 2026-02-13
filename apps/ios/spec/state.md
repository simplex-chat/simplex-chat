# SimpleX Chat iOS -- State Management

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
5. [Chat -- Single Conversation State](#5-chat)
6. [ChatInfo -- Conversation Metadata](#6-chatinfo)
7. [State Flow](#7-state-flow)
8. [Preference Storage](#8-preference-storage)

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

## 2. ChatModel

**Class**: `final class ChatModel: ObservableObject`
**Singleton**: `ChatModel.shared`
**Source**: `Shared/Model/ChatModel.swift`

### Key Published Properties

#### App Lifecycle
| Property | Type | Description |
|----------|------|-------------|
| `onboardingStage` | `OnboardingStage?` | Current onboarding step |
| `chatInitialized` | `Bool` | Whether chat has been initialized |
| `chatRunning` | `Bool?` | Whether chat engine is running |
| `chatDbChanged` | `Bool` | Whether DB was changed externally |
| `chatDbEncrypted` | `Bool?` | Whether DB is encrypted |
| `chatDbStatus` | `DBMigrationResult?` | DB migration status |
| `ctrlInitInProgress` | `Bool` | Whether controller is initializing |
| `migrationState` | `MigrationToState?` | Device migration state |

#### User State
| Property | Type | Description |
|----------|------|-------------|
| `currentUser` | `User?` | Active user profile (triggers theme reapply on change) |
| `users` | `[UserInfo]` | All user profiles |
| `v3DBMigration` | `V3DBMigrationState` | Legacy DB migration state |

#### Chat List
| Property | Type | Description |
|----------|------|-------------|
| `chats` | `[Chat]` (private set) | All conversations for current user |
| `deletedChats` | `Set<String>` | Chat IDs pending deletion animation |

#### Active Chat
| Property | Type | Description |
|----------|------|-------------|
| `chatId` | `String?` | Currently open chat ID |
| `chatAgentConnId` | `String?` | Agent connection ID for active chat |
| `chatSubStatus` | `SubscriptionStatus?` | Active chat subscription status |
| `openAroundItemId` | `ChatItem.ID?` | Item to scroll to when opening |
| `chatToTop` | `String?` | Chat to scroll to top |
| `groupMembers` | `[GMember]` | Members of active group |
| `groupMembersIndexes` | `[Int64: Int]` | Member ID to index mapping |
| `membersLoaded` | `Bool` | Whether members have been loaded |
| `secondaryIM` | `ItemsModel?` | Secondary items model (e.g. support chat scope) |

#### Authentication
| Property | Type | Description |
|----------|------|-------------|
| `contentViewAccessAuthenticated` | `Bool` | Whether user has passed authentication |
| `laRequest` | `LocalAuthRequest?` | Pending authentication request |

#### Notifications
| Property | Type | Description |
|----------|------|-------------|
| `deviceToken` | `DeviceToken?` | Current APNs device token |
| `savedToken` | `DeviceToken?` | Previously saved token |
| `tokenRegistered` | `Bool` | Whether token is registered with server |
| `tokenStatus` | `NtfTknStatus?` | Token registration status |
| `notificationMode` | `NotificationsMode` | Current notification mode (.off/.periodic/.instant) |
| `notificationServer` | `String?` | Notification server URL |
| `notificationPreview` | `NotificationPreviewMode` | What to show in notifications |
| `notificationResponse` | `UNNotificationResponse?` | Pending notification action |
| `ntfContactRequest` | `NTFContactRequest?` | Pending contact request from notification |
| `ntfCallInvitationAction` | `(ChatId, NtfCallAction)?` | Pending call action from notification |

#### Calls
| Property | Type | Description |
|----------|------|-------------|
| `callInvitations` | `[ChatId: RcvCallInvitation]` | Pending incoming call invitations |
| `activeCall` | `Call?` | Currently active call |
| `callCommand` | `WebRTCCommandProcessor` | WebRTC command queue |
| `showCallView` | `Bool` | Whether to show full-screen call UI |
| `activeCallViewIsCollapsed` | `Bool` | Whether call view is in PiP mode |

#### Remote Desktop
| Property | Type | Description |
|----------|------|-------------|
| `remoteCtrlSession` | `RemoteCtrlSession?` | Active remote desktop session |

#### Misc
| Property | Type | Description |
|----------|------|-------------|
| `userAddress` | `UserContactLink?` | User's SimpleX address |
| `chatItemTTL` | `ChatItemTTL` | Global message TTL |
| `appOpenUrl` | `URL?` | URL opened while app active |
| `appOpenUrlLater` | `URL?` | URL opened while app inactive |
| `showingInvitation` | `ShowingInvitation?` | Currently displayed invitation |
| `draft` | `ComposeState?` | Saved compose draft |
| `draftChatId` | `String?` | Chat ID for saved draft |
| `networkInfo` | `UserNetworkInfo` | Current network type and status |
| `conditions` | `ServerOperatorConditions` | Server usage conditions |
| `stopPreviousRecPlay` | `URL?` | Currently playing audio source |

### Non-Published Properties

| Property | Type | Description |
|----------|------|-------------|
| `messageDelivery` | `[Int64: () -> Void]` | Pending delivery confirmation callbacks |
| `filesToDelete` | `Set<URL>` | Files queued for deletion |
| `im` | `ItemsModel` | Reference to `ItemsModel.shared` |

### Key Methods

| Method | Description |
|--------|-------------|
| `getUser(_ userId:)` | Find user by ID |
| `updateUser(_ user:)` | Update user in list and current |
| `removeUser(_ user:)` | Remove user from list |
| `getChat(_ id:)` | Find chat by ID |
| `addChat(_ chat:)` | Add chat to list |
| `updateChatInfo(_ cInfo:)` | Update chat metadata |
| `replaceChat(_ id:, _ chat:)` | Replace chat in list |
| `removeChat(_ id:)` | Remove chat from list |
| `popChat(_ id:, _ ts:)` | Move chat to top of list |
| `totalUnreadCountForAllUsers()` | Sum unread across all users |

---

## 3. ItemsModel

**Class**: `class ItemsModel: ObservableObject`
**Primary singleton**: `ItemsModel.shared`
**Secondary instances**: Created via `ItemsModel.loadSecondaryChat()` for scope-based views (e.g., group member support chat)
**Source**: `Shared/Model/ChatModel.swift`

### Properties

| Property | Type | Description |
|----------|------|-------------|
| `reversedChatItems` | `[ChatItem]` | Messages in reverse chronological order (newest first) |
| `itemAdded` | `Bool` | Flag indicating a new item was added |
| `chatState` | `ActiveChatState` | Pagination splits and loaded ranges |
| `isLoading` | `Bool` | Whether messages are currently loading |
| `showLoadingProgress` | `ChatId?` | Chat ID showing loading spinner |
| `preloadState` | `PreloadState` | State for infinite-scroll preloading |
| `secondaryIMFilter` | `SecondaryItemsModelFilter?` | Filter for secondary instances |

### Computed Properties

| Property | Type | Description |
|----------|------|-------------|
| `lastItemsLoaded` | `Bool` | Whether the oldest messages have been loaded |
| `contentTag` | `MsgContentTag?` | Content type filter (if secondary) |
| `groupScopeInfo` | `GroupChatScopeInfo?` | Group scope filter (if secondary) |

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

| Method | Description |
|--------|-------------|
| `loadOpenChat(_ chatId:)` | Load chat with 250ms navigation delay |
| `loadOpenChatNoWait(_ chatId:, _ openAroundItemId:)` | Load chat without delay |
| `loadSecondaryChat(_ chatId:, chatFilter:)` | Create secondary ItemsModel instance |

### SecondaryItemsModelFilter

Used for secondary chat views (e.g., group member support scope, content type filter):

```swift
enum SecondaryItemsModelFilter {
    case groupChatScopeContext(groupScopeInfo: GroupChatScopeInfo)
    case msgContentTagContext(contentTag: MsgContentTag)
}
```

---

## 4. ChatTagsModel

**Class**: `class ChatTagsModel: ObservableObject`
**Singleton**: `ChatTagsModel.shared`
**Source**: `Shared/Model/ChatModel.swift`

### Properties

| Property | Type | Description |
|----------|------|-------------|
| `userTags` | `[ChatTag]` | User-defined tags |
| `activeFilter` | `ActiveFilter?` | Currently active filter tab |
| `presetTags` | `[PresetTag: Int]` | Preset tag counts (groups, contacts, favorites, etc.) |
| `unreadTags` | `[Int64: Int]` | Unread count per user tag |

### ActiveFilter

```swift
enum ActiveFilter {
    case presetTag(PresetTag)   // .favorites, .contacts, .groups, .business, .groupReports
    case userTag(ChatTag)       // User-defined tag
    case unread                 // Unread conversations
}
```

---

## 5. Chat

**Class**: `final class Chat: ObservableObject, Identifiable, ChatLike`
**Source**: `Shared/Model/ChatModel.swift` (line 1251)

Represents a single conversation in the chat list. Each `Chat` is an independent observable object.

### Properties

| Property | Type | Description |
|----------|------|-------------|
| `chatInfo` | `ChatInfo` | Conversation type and metadata |
| `chatItems` | `[ChatItem]` | Preview items (typically last message) |
| `chatStats` | `ChatStats` | Unread counts and min unread item ID |
| `created` | `Date` | Creation timestamp |

### ChatStats

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

| Property | Description |
|----------|-------------|
| `id` | Chat ID from `chatInfo.id` |
| `viewId` | Unique view identity including creation time |
| `unreadTag` | Whether chat counts as "unread" based on notification settings |
| `supportUnreadCount` | Unread count for group support scope |

---

## 6. ChatInfo

**Enum**: `public enum ChatInfo: Identifiable, Decodable, NamedChat, Hashable`
**Source**: `SimpleXChat/ChatTypes.swift`

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

---

## 7. State Flow

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

## 8. Preference Storage

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
| ChatModel, ItemsModel, Chat, ChatTagsModel | `Shared/Model/ChatModel.swift` |
| ChatInfo, User, Contact, GroupInfo, ChatItem | `SimpleXChat/ChatTypes.swift` |
| Preference defaults | `Shared/Model/ChatModel.swift`, `SimpleXChat/FileUtils.swift` |
