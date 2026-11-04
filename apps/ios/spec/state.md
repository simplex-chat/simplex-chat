# SimpleX Chat iOS -- State Management

**Source:** [`ChatModel.swift`](../Shared/Model/ChatModel.swift#L1-L1648) | [`ChatTypes.swift`](../SimpleXChat/ChatTypes.swift#L1-L6702)

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

## 2. [ChatModel](../Shared/Model/ChatModel.swift#L408-L1529)

**Class**: `final class ChatModel: ObservableObject`
**Singleton**: `ChatModel.shared`
**Source**: [`Shared/Model/ChatModel.swift`](../Shared/Model/ChatModel.swift#L408)

### Key Published Properties

#### App Lifecycle
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `onboardingStage` | `OnboardingStage?` | Current onboarding step | [L409](../Shared/Model/ChatModel.swift#L409) |
| `chatInitialized` | `Bool` | Whether chat has been initialized | [L418](../Shared/Model/ChatModel.swift#L418) |
| `chatRunning` | `Bool?` | Whether chat engine is running | [L419](../Shared/Model/ChatModel.swift#L419) |
| `chatDbChanged` | `Bool` | Whether DB was changed externally | [L420](../Shared/Model/ChatModel.swift#L420) |
| `chatDbEncrypted` | `Bool?` | Whether DB is encrypted | [L421](../Shared/Model/ChatModel.swift#L421) |
| `chatDbStatus` | `DBMigrationResult?` | DB migration status | [L422](../Shared/Model/ChatModel.swift#L422) |
| `ctrlInitInProgress` | `Bool` | Whether controller is initializing | [L423](../Shared/Model/ChatModel.swift#L423) |
| `migrationState` | `MigrationToState?` | Device migration state | [L473](../Shared/Model/ChatModel.swift#L473) |

#### User State
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `currentUser` | `User?` | Active user profile (triggers theme reapply on change) | [L412](../Shared/Model/ChatModel.swift#L412) |
| `users` | `[UserInfo]` | All user profiles | [L417](../Shared/Model/ChatModel.swift#L417) |
| `v3DBMigration` | `V3DBMigrationState` | Legacy DB migration state | [L411](../Shared/Model/ChatModel.swift#L411) |

#### Chat List
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `chats` | `[Chat]` (private set) | All conversations for current user | [L429](../Shared/Model/ChatModel.swift#L429) |
| `deletedChats` | `Set<String>` | Chat IDs pending deletion animation | [L430](../Shared/Model/ChatModel.swift#L430) |

#### Active Chat
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `chatId` | `String?` | Currently open chat ID | [L432](../Shared/Model/ChatModel.swift#L432) |
| `chatAgentConnId` | `String?` | Agent connection ID for active chat | [L433](../Shared/Model/ChatModel.swift#L433) |
| `chatSubStatus` | `SubscriptionStatus?` | Active chat subscription status | [L434](../Shared/Model/ChatModel.swift#L434) |
| `openAroundItemId` | `ChatItem.ID?` | Item to scroll to when opening | [L435](../Shared/Model/ChatModel.swift#L435) |
| `chatToTop` | `String?` | Chat to scroll to top | [L436](../Shared/Model/ChatModel.swift#L436) |
| `groupMembers` | `[GMember]` | Members of active group | [L438](../Shared/Model/ChatModel.swift#L438) |
| `groupMembersIndexes` | `[Int64: Int]` | Member ID to index mapping | [L439](../Shared/Model/ChatModel.swift#L439) |
| `membersLoaded` | `Bool` | Whether members have been loaded | [L440](../Shared/Model/ChatModel.swift#L440) |
| `secondaryIM` | `ItemsModel?` | Secondary items model (e.g. support chat scope) | [L517](../Shared/Model/ChatModel.swift#L517) |

#### Authentication
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `contentViewAccessAuthenticated` | `Bool` | Whether user has passed authentication | [L426](../Shared/Model/ChatModel.swift#L426) |
| `laRequest` | `LocalAuthRequest?` | Pending authentication request | [L427](../Shared/Model/ChatModel.swift#L427) |

#### Notifications
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `deviceToken` | `DeviceToken?` | Current APNs device token | [L451](../Shared/Model/ChatModel.swift#L451) |
| `savedToken` | `DeviceToken?` | Previously saved token | [L452](../Shared/Model/ChatModel.swift#L452) |
| `tokenRegistered` | `Bool` | Whether token is registered with server | [L453](../Shared/Model/ChatModel.swift#L453) |
| `tokenStatus` | `NtfTknStatus?` | Token registration status | [L455](../Shared/Model/ChatModel.swift#L455) |
| `notificationMode` | `NotificationsMode` | Current notification mode (.off/.periodic/.instant) | [L456](../Shared/Model/ChatModel.swift#L456) |
| `notificationServer` | `String?` | Notification server URL | [L457](../Shared/Model/ChatModel.swift#L457) |
| `notificationPreview` | `NotificationPreviewMode` | What to show in notifications | [L458](../Shared/Model/ChatModel.swift#L458) |
| `notificationResponse` | `UNNotificationResponse?` | Pending notification action | [L424](../Shared/Model/ChatModel.swift#L424) |
| `ntfContactRequest` | `NTFContactRequest?` | Pending contact request from notification | [L460](../Shared/Model/ChatModel.swift#L460) |
| `ntfCallInvitationAction` | `(ChatId, NtfCallAction)?` | Pending call action from notification | [L461](../Shared/Model/ChatModel.swift#L461) |

#### Calls
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `callInvitations` | `[ChatId: RcvCallInvitation]` | Pending incoming call invitations | [L463](../Shared/Model/ChatModel.swift#L463) |
| `activeCall` | `Call?` | Currently active call | [L464](../Shared/Model/ChatModel.swift#L464) |
| `callCommand` | `WebRTCCommandProcessor` | WebRTC command queue | [L465](../Shared/Model/ChatModel.swift#L465) |
| `showCallView` | `Bool` | Whether to show full-screen call UI | [L466](../Shared/Model/ChatModel.swift#L466) |
| `activeCallViewIsCollapsed` | `Bool` | Whether call view is in PiP mode | [L467](../Shared/Model/ChatModel.swift#L467) |

#### Remote Desktop
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `remoteCtrlSession` | `RemoteCtrlSession?` | Active remote desktop session | [L470](../Shared/Model/ChatModel.swift#L470) |

#### Misc
| Property | Type | Description | Line |
|----------|------|-------------|------|
| `userAddress` | `UserContactLink?` | User's SimpleX address | [L447](../Shared/Model/ChatModel.swift#L447) |
| `chatItemTTL` | `ChatItemTTL` | Global message TTL | [L448](../Shared/Model/ChatModel.swift#L448) |
| `appOpenUrl` | `URL?` | URL opened while app active | [L449](../Shared/Model/ChatModel.swift#L449) |
| `appOpenUrlLater` | `URL?` | URL opened while app inactive | [L450](../Shared/Model/ChatModel.swift#L450) |
| `showingInvitation` | `ShowingInvitation?` | Currently displayed invitation | [L472](../Shared/Model/ChatModel.swift#L472) |
| `draft` | `ComposeState?` | Saved compose draft | [L476](../Shared/Model/ChatModel.swift#L476) |
| `draftChatId` | `String?` | Chat ID for saved draft | [L478](../Shared/Model/ChatModel.swift#L478) |
| `networkInfo` | `UserNetworkInfo` | Current network type and status | [L479](../Shared/Model/ChatModel.swift#L479) |
| `conditions` | `ServerOperatorConditions` | Server usage conditions | [L481](../Shared/Model/ChatModel.swift#L481) |
| `stopPreviousRecPlay` | `URL?` | Currently playing audio source | [L475](../Shared/Model/ChatModel.swift#L475) |

### Non-Published Properties

| Property | Type | Description | Line |
|----------|------|-------------|------|
| `messageDelivery` | `[Int64: () -> Void]` | Pending delivery confirmation callbacks | [L483](../Shared/Model/ChatModel.swift#L483) |
| `filesToDelete` | `Set<URL>` | Files queued for deletion | [L485](../Shared/Model/ChatModel.swift#L485) |
| `chatListBanner` | `ChatListBanner?` | The banner kind the chat list showed this app session; `bannerSlotFree(for:)` tells whether a kind may take the slot (see [chat-list.md](client/chat-list.md)) | [L489](../Shared/Model/ChatModel.swift#L489) |
| `im` | `ItemsModel` | Reference to `ItemsModel.shared` | [L514](../Shared/Model/ChatModel.swift#L514) |

### Key Methods

| Method | Description | Line |
|--------|-------------|------|
| `getUser(_ userId:)` | Find user by ID | [L537](../Shared/Model/ChatModel.swift#L537) |
| `updateUser(_ user:)` | Update user in list and current | [L548](../Shared/Model/ChatModel.swift#L548) |
| `removeUser(_ user:)` | Remove user from list | [L558](../Shared/Model/ChatModel.swift#L558) |
| `getChat(_ id:)` | Find chat by ID | [L569](../Shared/Model/ChatModel.swift#L569) |
| `addChat(_ chat:)` | Add chat to list | [L624](../Shared/Model/ChatModel.swift#L624) |
| `updateChatInfo(_ cInfo:)` | Update chat metadata | [L638](../Shared/Model/ChatModel.swift#L638) |
| `replaceChat(_ id:, _ chat:)` | Replace chat in list | [L690](../Shared/Model/ChatModel.swift#L690) |
| `removeChat(_ id:)` | Remove chat from list | [L1451](../Shared/Model/ChatModel.swift#L1451) |
| `popChat(_ id:)` | Move chat to top of list | [L1418](../Shared/Model/ChatModel.swift#L1418) |
| `totalUnreadCountForAllUsers()` | Sum unread across all users | [L1318](../Shared/Model/ChatModel.swift#L1318) |

---

## 3. [ItemsModel](../Shared/Model/ChatModel.swift#L74-L189)

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
| `openingChatId` | `ChatId?` | Chat being opened by `loadOpenChat` (until its navigation timer sets `chatId`) or `loadOpenChatNoWait` (until its completion step) | [L96](../Shared/Model/ChatModel.swift#L96) |
| `openingItemUpdates` | `ChatModel.ItemUpdatesWhileLoading?` | Record of the installed opening page, applied at once until `chatId` is set | [L97](../Shared/Model/ChatModel.swift#L97) |

### Computed Properties

| Property | Type | Description | Line |
|----------|------|-------------|------|
| `lastItemsLoaded` | `Bool` | Whether the oldest messages have been loaded | [L99](../Shared/Model/ChatModel.swift#L99) |
| `contentTag` | `MsgContentTag?` | Content type filter (if secondary) | [L174](../Shared/Model/ChatModel.swift#L174) |
| `groupScopeInfo` | `GroupChatScopeInfo?` | Group scope filter (if secondary) | [L182](../Shared/Model/ChatModel.swift#L182) |

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
| `loadOpenChat(_ chatId:)` | Load chat with 250ms navigation delay | [L119](../Shared/Model/ChatModel.swift#L119) |
| `loadOpenChatNoWait(_ chatId:, _ openAroundItemId:)` | Load chat without delay | [L148](../Shared/Model/ChatModel.swift#L148) |
| `loadSecondaryChat(_ chatId:, chatFilter:)` | Create secondary ItemsModel instance | [L112](../Shared/Model/ChatModel.swift#L112) |
| `ChatModel.startRecordingItemUpdates(_ record:)` / `stopRecordingItemUpdates(_ record:)` | Record the list part of each item update while a page of items loads (`apiLoadMessages` in `ChatItemsLoader.swift`) | [L724](../Shared/Model/ChatModel.swift#L724) |
| `ChatModel.recordItemUpdate(_ cInfo:, _ itemId:, _ update:)` | Record an update of an item missing from the list | [L783](../Shared/Model/ChatModel.swift#L783) |
| `ChatModel.applyItemUpdatesWhileLoading(_ record:, _ loadingIM:, _ loadedChatId:)` | Re-run the recorded additions and then the other recorded updates on the items of `loadingIM`, each in order, resolved for the chat `loadedChatId` | [L732](../Shared/Model/ChatModel.swift#L732) |
| `ChatModel.applyItemUpdatesUntilChatIsShown(_ record:, _ loadingIM:, _ loadedChatId:)` | After the install of an opening page while `openingChatId` is the loaded chat, apply each newly recorded update to `loadingIM` at once, until `stopApplyingItemUpdates()` | [L737](../Shared/Model/ChatModel.swift#L737) |
| `stopApplyingItemUpdates()` | Stop the record of the opening page; called where `chatId` is set, when the next load starts and when a new opening record is kept | [L167](../Shared/Model/ChatModel.swift#L167) |

### [SecondaryItemsModelFilter](../Shared/Model/ChatModel.swift#L58-L70)

Used for secondary chat views (e.g., group member support scope, content type filter):

```swift
enum SecondaryItemsModelFilter {
    case groupChatScopeContext(groupScopeInfo: GroupChatScopeInfo)
    case msgContentTagContext(contentTag: MsgContentTag)
}
```

---

## 4. [ChatTagsModel](../Shared/Model/ChatModel.swift#L204-L306)

**Class**: `class ChatTagsModel: ObservableObject`
**Singleton**: `ChatTagsModel.shared`
**Source**: [`Shared/Model/ChatModel.swift`](../Shared/Model/ChatModel.swift#L204)

### Properties

| Property | Type | Description | Line |
|----------|------|-------------|------|
| `userTags` | `[ChatTag]` | User-defined tags | [L207](../Shared/Model/ChatModel.swift#L207) |
| `activeFilter` | `ActiveFilter?` | Currently active filter tab | [L208](../Shared/Model/ChatModel.swift#L208) |
| `presetTags` | `[PresetTag: Int]` | Preset tag counts (groups, contacts, favorites, etc.) | [L209](../Shared/Model/ChatModel.swift#L209) |
| `unreadTags` | `[Int64: Int]` | Unread count per user tag | [L210](../Shared/Model/ChatModel.swift#L210) |

### [ActiveFilter](../Shared/Views/ChatList/ChatListView.swift#L53)

```swift
enum ActiveFilter {
    case presetTag(PresetTag)   // .favorites, .contacts, .groups, .business, .groupReports
    case userTag(ChatTag)       // User-defined tag
    case unread                 // Unread conversations
}
```

---

## 5. [ChannelRelaysModel](../Shared/Model/ChatModel.swift#L351-L375)

**Class**: `class ChannelRelaysModel: ObservableObject`
**Singleton**: `ChannelRelaysModel.shared`
**Source**: [`Shared/Model/ChatModel.swift`](../Shared/Model/ChatModel.swift#L351)

Holds runtime relay state for the currently viewed channel. Used by `ChannelRelaysView` to display and manage relays. Reset when the view is dismissed.

### Properties

| Property | Type | Description | Line |
|----------|------|-------------|------|
| `groupId` | `Int64?` | Group ID of the channel whose relays are loaded | [L353](../Shared/Model/ChatModel.swift#L353) |
| `groupRelays` | `[GroupRelay]` | Current relay instances for the channel | [L354](../Shared/Model/ChatModel.swift#L354) |

### Methods

| Method | Description | Line |
|--------|-------------|------|
| `set(groupId:groupRelays:)` | Populate all properties at once | [L356](../Shared/Model/ChatModel.swift#L356) |
| `reset()` | Clear all properties to nil/empty | [L371](../Shared/Model/ChatModel.swift#L371) |

---

## 6. [Chat](../Shared/Model/ChatModel.swift#L1541-L1593)

**Class**: `final class Chat: ObservableObject, Identifiable, ChatLike`
**Source**: [`Shared/Model/ChatModel.swift`](../Shared/Model/ChatModel.swift#L1541)

Represents a single conversation in the chat list. Each `Chat` is an independent observable object.

### Properties

| Property | Type | Description | Line |
|----------|------|-------------|------|
| `chatInfo` | `ChatInfo` | Conversation type and metadata | [L1542](../Shared/Model/ChatModel.swift#L1542) |
| `chatItems` | `[ChatItem]` | Preview items (typically last message) | [L1543](../Shared/Model/ChatModel.swift#L1543) |
| `chatStats` | `ChatStats` | Unread counts and min unread item ID | [L1544](../Shared/Model/ChatModel.swift#L1544) |
| `created` | `Date` | Creation timestamp | [L1545](../Shared/Model/ChatModel.swift#L1545) |

### [ChatStats](../SimpleXChat/ChatTypes.swift#L2560-L2582)

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
| `id` | Chat ID from `chatInfo.id` | [L1576](../Shared/Model/ChatModel.swift#L1576) |
| `viewId` | Unique view identity including creation time | [L1578](../Shared/Model/ChatModel.swift#L1578) |
| `unreadTag` | Whether chat counts as "unread" based on notification settings | [L1568](../Shared/Model/ChatModel.swift#L1568) |
| `supportUnreadCount` | Unread count for group support scope | [L1580](../Shared/Model/ChatModel.swift#L1580) |

---

## 7. [ChatInfo](../SimpleXChat/ChatTypes.swift#L1992-L2535)

**Enum**: `public enum ChatInfo: Identifiable, Decodable, NamedChat, Hashable`
**Source**: [`SimpleXChat/ChatTypes.swift`](../SimpleXChat/ChatTypes.swift#L1992)

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
| `User` | `userChatRelay` | `Bool` | Whether user acts as a chat relay | [L52](../SimpleXChat/ChatTypes.swift#L52) |
| `GroupInfo` | `useRelays` | `Bool` | Whether group uses relay infrastructure (channel mode) | [L3041](../SimpleXChat/ChatTypes.swift#L3041) |
| `GroupInfo` | `relayOwnStatus` | `RelayStatus?` | Current user's relay status in this group | [L3042](../SimpleXChat/ChatTypes.swift#L3042) |
| `GroupProfile` | `publicGroup` | `PublicGroupProfile?` | Channel-specific profile data (type, link, ID) | [L3220](../SimpleXChat/ChatTypes.swift#L3220) |

#### New Types

| Type | Kind | Description | Line |
|------|------|-------------|------|
| `RelayStatus` | `enum` | Relay lifecycle: `.rsNew`, `.rsInvited`, `.rsAccepted`, `.rsActive`, `.rsInactive`, `.rsRejected` | [L3320](../SimpleXChat/ChatTypes.swift#L3320) |
| `RelayStatus.text` | `extension` | Localized display text: New/Invited/Accepted/Active/Inactive/Rejected | [L3394](../SimpleXChat/ChatTypes.swift#L3394) |
| `GroupRelay` | `struct` | Relay instance for a group (ID, member ID, relay status). Fetched at runtime via `apiGetGroupRelays` (owner only) | [L3384](../SimpleXChat/ChatTypes.swift#L3384) |
| `UserChatRelay` | `struct` | User's chat relay configuration (ID, SMP address, name, domains, preset/tested/enabled/deleted flags) | [L3337](../SimpleXChat/ChatTypes.swift#L3337) |

#### New Enum Cases

| Enum | Case | Description | Line |
|------|------|-------------|------|
| `GroupMemberRole` | `.relay` | Role for relay members (below `.observer`) | [L3652](../SimpleXChat/ChatTypes.swift#L3652) |
| `CIDirection` | `.channelRcv` | Message direction for channel-received messages (via relay) | [L4408](../SimpleXChat/ChatTypes.swift#L4408) |

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
| ActiveFilter | [`Shared/Views/ChatList/ChatListView.swift`](../Shared/Views/ChatList/ChatListView.swift#L53) |
| Preference defaults | [`Shared/Model/ChatModel.swift`](../Shared/Model/ChatModel.swift), [`SimpleXChat/FileUtils.swift`](../SimpleXChat/FileUtils.swift) |
