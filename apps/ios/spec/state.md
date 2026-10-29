# SimpleX Chat iOS -- State Management

**Source:** [`ChatModel.swift`](../Shared/Model/ChatModel.swift#L1-L1538) | [`ChatTypes.swift`](../SimpleXChat/ChatTypes.swift#L1-L6520)

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

## 2. [ChatModel](../Shared/Model/ChatModel.swift#L412-L1406)

**Class**: `final class ChatModel: ObservableObject`
**Singleton**: `ChatModel.shared`
**Source**: [`Shared/Model/ChatModel.swift`](../Shared/Model/ChatModel.swift#L412)

### Key Published Properties

#### App Lifecycle
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `onboardingStage` | `OnboardingStage?` | Current onboarding step | [L413](../Shared/Model/ChatModel.swift#L413) |
| `chatInitialized` | `Bool` | Whether chat has been initialized | [L422](../Shared/Model/ChatModel.swift#L422) |
| `chatRunning` | `Bool?` | Whether chat engine is running | [L423](../Shared/Model/ChatModel.swift#L423) |
| `chatDbChanged` | `Bool` | Whether DB was changed externally | [L424](../Shared/Model/ChatModel.swift#L424) |
| `chatDbEncrypted` | `Bool?` | Whether DB is encrypted | [L425](../Shared/Model/ChatModel.swift#L425) |
| `chatDbStatus` | `DBMigrationResult?` | DB migration status | [L426](../Shared/Model/ChatModel.swift#L426) |
| `ctrlInitInProgress` | `Bool` | Whether controller is initializing | [L427](../Shared/Model/ChatModel.swift#L427) |
| `migrationState` | `MigrationToState?` | Device migration state | [L477](../Shared/Model/ChatModel.swift#L477) |

#### User State
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `currentUser` | `User?` | Active user profile (triggers theme reapply on change) | [L416](../Shared/Model/ChatModel.swift#L416) |
| `users` | `[UserInfo]` | All user profiles | [L421](../Shared/Model/ChatModel.swift#L421) |
| `v3DBMigration` | `V3DBMigrationState` | Legacy DB migration state | [L415](../Shared/Model/ChatModel.swift#L415) |

#### Chat List
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `chats` | `[Chat]` (private set) | All conversations for current user | [L433](../Shared/Model/ChatModel.swift#L433) |
| `deletedChats` | `Set<String>` | Chat IDs pending deletion animation | [L434](../Shared/Model/ChatModel.swift#L434) |

#### Active Chat
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `chatId` | `String?` | Currently open chat ID | [L436](../Shared/Model/ChatModel.swift#L436) |
| `chatAgentConnId` | `String?` | Agent connection ID for active chat | [L437](../Shared/Model/ChatModel.swift#L437) |
| `chatSubStatus` | `SubscriptionStatus?` | Active chat subscription status | [L438](../Shared/Model/ChatModel.swift#L438) |
| `openAroundItemId` | `ChatItem.ID?` | Item to scroll to when opening | [L439](../Shared/Model/ChatModel.swift#L439) |
| `chatToTop` | `String?` | Chat to scroll to top | [L440](../Shared/Model/ChatModel.swift#L440) |
| `groupMembers` | `[GMember]` | Members of active group | [L442](../Shared/Model/ChatModel.swift#L442) |
| `groupMembersIndexes` | `[Int64: Int]` | Member ID to index mapping | [L443](../Shared/Model/ChatModel.swift#L443) |
| `membersLoaded` | `Bool` | Whether members have been loaded | [L444](../Shared/Model/ChatModel.swift#L444) |
| `secondaryIM` | `ItemsModel?` | Secondary items model (e.g. support chat scope) | [L521](../Shared/Model/ChatModel.swift#L521) |

#### Authentication
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `contentViewAccessAuthenticated` | `Bool` | Whether user has passed authentication | [L430](../Shared/Model/ChatModel.swift#L430) |
| `laRequest` | `LocalAuthRequest?` | Pending authentication request | [L431](../Shared/Model/ChatModel.swift#L431) |

#### Notifications
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `deviceToken` | `DeviceToken?` | Current APNs device token | [L455](../Shared/Model/ChatModel.swift#L455) |
| `savedToken` | `DeviceToken?` | Previously saved token | [L456](../Shared/Model/ChatModel.swift#L456) |
| `tokenRegistered` | `Bool` | Whether token is registered with server | [L457](../Shared/Model/ChatModel.swift#L457) |
| `tokenStatus` | `NtfTknStatus?` | Token registration status | [L459](../Shared/Model/ChatModel.swift#L459) |
| `notificationMode` | `NotificationsMode` | Current notification mode (.off/.periodic/.instant) | [L460](../Shared/Model/ChatModel.swift#L460) |
| `notificationServer` | `String?` | Notification server URL | [L461](../Shared/Model/ChatModel.swift#L461) |
| `notificationPreview` | `NotificationPreviewMode` | What to show in notifications | [L462](../Shared/Model/ChatModel.swift#L462) |
| `notificationResponse` | `UNNotificationResponse?` | Pending notification action | [L428](../Shared/Model/ChatModel.swift#L428) |
| `ntfContactRequest` | `NTFContactRequest?` | Pending contact request from notification | [L464](../Shared/Model/ChatModel.swift#L464) |
| `ntfCallInvitationAction` | `(ChatId, NtfCallAction)?` | Pending call action from notification | [L465](../Shared/Model/ChatModel.swift#L465) |

#### Calls
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `callInvitations` | `[ChatId: RcvCallInvitation]` | Pending incoming call invitations | [L467](../Shared/Model/ChatModel.swift#L467) |
| `activeCall` | `Call?` | Currently active call | [L468](../Shared/Model/ChatModel.swift#L468) |
| `callCommand` | `WebRTCCommandProcessor` | WebRTC command queue | [L469](../Shared/Model/ChatModel.swift#L469) |
| `showCallView` | `Bool` | Whether to show full-screen call UI | [L470](../Shared/Model/ChatModel.swift#L470) |
| `activeCallViewIsCollapsed` | `Bool` | Whether call view is in PiP mode | [L471](../Shared/Model/ChatModel.swift#L471) |

#### Remote Desktop
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `remoteCtrlSession` | `RemoteCtrlSession?` | Active remote desktop session | [L474](../Shared/Model/ChatModel.swift#L474) |

#### Misc
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `userAddress` | `UserContactLink?` | User's SimpleX address | [L451](../Shared/Model/ChatModel.swift#L451) |
| `chatItemTTL` | `ChatItemTTL` | Global message TTL | [L452](../Shared/Model/ChatModel.swift#L452) |
| `appOpenUrl` | `URL?` | URL opened while app active | [L453](../Shared/Model/ChatModel.swift#L453) |
| `appOpenUrlLater` | `URL?` | URL opened while app inactive | [L454](../Shared/Model/ChatModel.swift#L454) |
| `showingInvitation` | `ShowingInvitation?` | Currently displayed invitation | [L476](../Shared/Model/ChatModel.swift#L476) |
| `draft` | `ComposeState?` | Saved compose draft | [L480](../Shared/Model/ChatModel.swift#L480) |
| `draftChatId` | `String?` | Chat ID for saved draft | [L482](../Shared/Model/ChatModel.swift#L482) |
| `networkInfo` | `UserNetworkInfo` | Current network type and status | [L483](../Shared/Model/ChatModel.swift#L483) |
| `conditions` | `ServerOperatorConditions` | Server usage conditions | [L485](../Shared/Model/ChatModel.swift#L485) |
| `stopPreviousRecPlay` | `URL?` | Currently playing audio source | [L479](../Shared/Model/ChatModel.swift#L479) |

### Non-Published Properties

| Property | Type | Description | Line |
|----------|------|-------------|------|
| `messageDelivery` | `[Int64: () -> Void]` | Pending delivery confirmation callbacks | [L487](../Shared/Model/ChatModel.swift#L487) |
| `filesToDelete` | `Set<URL>` | Files queued for deletion | [L489](../Shared/Model/ChatModel.swift#L489) |
| `chatListBanner` | `ChatListBanner?` | The banner kind the chat list showed this app session; `bannerSlotFree(for:)` tells whether a kind may take the slot (see [chat-list.md](client/chat-list.md)) | [L493](../Shared/Model/ChatModel.swift#L493) |
| `im` | `ItemsModel` | Reference to `ItemsModel.shared` | [L518](../Shared/Model/ChatModel.swift#L518) |

### Key Methods

| Method | Description | Line |
|--------|-------------|------|
| `getUser(_ userId:)` | Find user by ID | [L541](../Shared/Model/ChatModel.swift#L541) |
| `updateUser(_ user:)` | Update user in list and current | [L552](../Shared/Model/ChatModel.swift#L552) |
| `removeUser(_ user:)` | Remove user from list | [L562](../Shared/Model/ChatModel.swift#L562) |
| `getChat(_ id:)` | Find chat by ID | [L573](../Shared/Model/ChatModel.swift#L573) |
| `addChat(_ chat:)` | Add chat to list | [L628](../Shared/Model/ChatModel.swift#L628) |
| `updateChatInfo(_ cInfo:)` | Update chat metadata | [L642](../Shared/Model/ChatModel.swift#L642) |
| `replaceChat(_ id:, _ chat:)` | Replace chat in list | [L694](../Shared/Model/ChatModel.swift#L694) |
| `removeChat(_ id:)` | Remove chat from list | [L1345](../Shared/Model/ChatModel.swift#L1345) |
| `popChat(_ id:)` | Move chat to top of list | [L1312](../Shared/Model/ChatModel.swift#L1312) |
| `totalUnreadCountForAllUsers()` | Sum unread across all users | [L1212](../Shared/Model/ChatModel.swift#L1212) |

---

## 3. [ItemsModel](../Shared/Model/ChatModel.swift#L74-L174)

**Class**: `class ItemsModel: ObservableObject`
**Primary singleton**: `ItemsModel.shared`
**Secondary instances**: Created via `ItemsModel.loadSecondaryChat()` for scope-based views (e.g., group member support chat)
**Source**: [`Shared/Model/ChatModel.swift`](../Shared/Model/ChatModel.swift#L74)

### Properties

| Property | Type | Description | Line |
|----------|------|-------------|------|
| `reversedChatItems` | `[ChatItem]` | Messages in reverse chronological order (newest first) | [L80](../Shared/Model/ChatModel.swift#L80) |
| `itemAdded` | `Bool` | Flag indicating a new item was added | [L83](../Shared/Model/ChatModel.swift#L83) |
| `chatState` | `ActiveChatState` | Pagination splits and loaded ranges | [L87](../Shared/Model/ChatModel.swift#L87) |
| `isLoading` | `Bool` | Whether messages are currently loading | [L91](../Shared/Model/ChatModel.swift#L91) |
| `showLoadingProgress` | `ChatId?` | Chat ID showing loading spinner | [L92](../Shared/Model/ChatModel.swift#L92) |
| `preloadState` | `PreloadState` | State for infinite-scroll preloading | [L77](../Shared/Model/ChatModel.swift#L77) |
| `secondaryIMFilter` | `SecondaryItemsModelFilter?` | Filter for secondary instances | [L76](../Shared/Model/ChatModel.swift#L76) |

### Computed Properties

| Property | Type | Description | Line |
|----------|------|-------------|------|
| `lastItemsLoaded` | `Bool` | Whether the oldest messages have been loaded | [L97](../Shared/Model/ChatModel.swift#L97) |
| `contentTag` | `MsgContentTag?` | Content type filter (if secondary) | [L159](../Shared/Model/ChatModel.swift#L159) |
| `groupScopeInfo` | `GroupChatScopeInfo?` | Group scope filter (if secondary) | [L167](../Shared/Model/ChatModel.swift#L167) |

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
| `loadOpenChat(_ chatId:)` | Load chat with 250ms navigation delay | [L117](../Shared/Model/ChatModel.swift#L117) |
| `loadOpenChatNoWait(_ chatId:, _ openAroundItemId:)` | Load chat without delay | [L143](../Shared/Model/ChatModel.swift#L143) |
| `loadSecondaryChat(_ chatId:, chatFilter:)` | Create secondary ItemsModel instance | [L110](../Shared/Model/ChatModel.swift#L110) |

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
| `userTags` | `[ChatTag]` | User-defined tags | [L192](../Shared/Model/ChatModel.swift#L192) |
| `activeFilter` | `ActiveFilter?` | Currently active filter tab | [L193](../Shared/Model/ChatModel.swift#L193) |
| `presetTags` | `[PresetTag: Int]` | Preset tag counts (groups, contacts, favorites, etc.) | [L194](../Shared/Model/ChatModel.swift#L194) |
| `unreadTags` | `[Int64: Int]` | Unread count per user tag | [L195](../Shared/Model/ChatModel.swift#L195) |

### [ActiveFilter](../Shared/Views/ChatList/ChatListView.swift#L53)

```swift
enum ActiveFilter {
    case presetTag(PresetTag)   // .favorites, .contacts, .groups, .business, .groupReports
    case userTag(ChatTag)       // User-defined tag
    case unread                 // Unread conversations
}
```

---

## 5. [ChannelRelaysModel](../Shared/Model/ChatModel.swift#L354-L378)

**Class**: `class ChannelRelaysModel: ObservableObject`
**Singleton**: `ChannelRelaysModel.shared`
**Source**: [`Shared/Model/ChatModel.swift`](../Shared/Model/ChatModel.swift#L354)

Holds runtime relay state for the currently viewed channel. Used by `ChannelRelaysView` to display and manage relays. Reset when the view is dismissed.

### Properties

| Property | Type | Description | Line |
|----------|------|-------------|------|
| `groupId` | `Int64?` | Group ID of the channel whose relays are loaded | [L356](../Shared/Model/ChatModel.swift#L356) |
| `groupRelays` | `[GroupRelay]` | Current relay instances for the channel | [L357](../Shared/Model/ChatModel.swift#L357) |

### Methods

| Method | Description | Line |
|--------|-------------|------|
| `set(groupId:groupRelays:)` | Populate all properties at once | [L359](../Shared/Model/ChatModel.swift#L359) |
| `reset()` | Clear all properties to nil/empty | [L374](../Shared/Model/ChatModel.swift#L374) |

---

## 6. [Chat](../Shared/Model/ChatModel.swift#L1418-L1470)

**Class**: `final class Chat: ObservableObject, Identifiable, ChatLike`
**Source**: [`Shared/Model/ChatModel.swift`](../Shared/Model/ChatModel.swift#L1435)

Represents a single conversation in the chat list. Each `Chat` is an independent observable object.

### Properties

| Property | Type | Description | Line |
|----------|------|-------------|------|
| `chatInfo` | `ChatInfo` | Conversation type and metadata | [L1436](../Shared/Model/ChatModel.swift#L1436) |
| `chatItems` | `[ChatItem]` | Preview items (typically last message) | [L1437](../Shared/Model/ChatModel.swift#L1437) |
| `chatStats` | `ChatStats` | Unread counts and min unread item ID | [L1438](../Shared/Model/ChatModel.swift#L1438) |
| `created` | `Date` | Creation timestamp | [L1439](../Shared/Model/ChatModel.swift#L1439) |

### [ChatStats](../SimpleXChat/ChatTypes.swift#L2411-L2433)

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
| `id` | Chat ID from `chatInfo.id` | [L1470](../Shared/Model/ChatModel.swift#L1470) |
| `viewId` | Unique view identity including creation time | [L1472](../Shared/Model/ChatModel.swift#L1472) |
| `unreadTag` | Whether chat counts as "unread" based on notification settings | [L1462](../Shared/Model/ChatModel.swift#L1462) |
| `supportUnreadCount` | Unread count for group support scope | [L1474](../Shared/Model/ChatModel.swift#L1474) |

---

## 7. [ChatInfo](../SimpleXChat/ChatTypes.swift#L1867-L2386)

**Enum**: `public enum ChatInfo: Identifiable, Decodable, NamedChat, Hashable`
**Source**: [`SimpleXChat/ChatTypes.swift`](../SimpleXChat/ChatTypes.swift#L1867)

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
| `User` | `userChatRelay` | `Bool` | Whether user acts as a chat relay | [L48](../SimpleXChat/ChatTypes.swift#L48) |
| `GroupInfo` | `useRelays` | `Bool` | Whether group uses relay infrastructure (channel mode) | [L2890](../SimpleXChat/ChatTypes.swift#L2890) |
| `GroupInfo` | `relayOwnStatus` | `RelayStatus?` | Current user's relay status in this group | [L2891](../SimpleXChat/ChatTypes.swift#L2891) |
| `GroupProfile` | `publicGroup` | `PublicGroupProfile?` | Channel-specific profile data (type, link, ID) | [L3069](../SimpleXChat/ChatTypes.swift#L3069) |

#### New Types

| Type | Kind | Description | Line |
|------|------|-------------|------|
| `RelayStatus` | `enum` | Relay lifecycle: `.rsNew`, `.rsInvited`, `.rsAccepted`, `.rsActive`, `.rsInactive`, `.rsRejected` | [L3167](../SimpleXChat/ChatTypes.swift#L3167) |
| `RelayStatus.text` | `extension` | Localized display text: New/Invited/Accepted/Active/Inactive/Rejected | [L3241](../SimpleXChat/ChatTypes.swift#L3241) |
| `GroupRelay` | `struct` | Relay instance for a group (ID, member ID, relay status). Fetched at runtime via `apiGetGroupRelays` (owner only) | [L3231](../SimpleXChat/ChatTypes.swift#L3231) |
| `UserChatRelay` | `struct` | User's chat relay configuration (ID, SMP address, name, domains, preset/tested/enabled/deleted flags) | [L3184](../SimpleXChat/ChatTypes.swift#L3184) |

#### New Enum Cases

| Enum | Case | Description | Line |
|------|------|-------------|------|
| `GroupMemberRole` | `.relay` | Role for relay members (below `.observer`) | [L3502](../SimpleXChat/ChatTypes.swift#L3502) |
| `CIDirection` | `.channelRcv` | Message direction for channel-received messages (via relay) | [L4237](../SimpleXChat/ChatTypes.swift#L4237) |

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
| `DEFAULT_PRIVACY_DIRECTORY_SEARCH` | `Bool` | "Directory search" in Privacy settings, default false; while it is off, every online search asks "Enable Directory search?", and Enable turns it on; not in `hintDefaults` (see [Chat List, Online Search](client/chat-list.md#11-online-search)) |

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
| ActiveFilter | [`Shared/Views/ChatList/ChatListView.swift`](../Shared/Views/ChatList/ChatListView.swift#L53) |
| Preference defaults | [`Shared/Model/ChatModel.swift`](../Shared/Model/ChatModel.swift), [`SimpleXChat/FileUtils.swift`](../SimpleXChat/FileUtils.swift) |
