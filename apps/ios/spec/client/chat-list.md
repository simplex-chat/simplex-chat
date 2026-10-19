# SimpleX Chat iOS -- Chat List Module

> Technical specification for the conversation list, filtering, search, swipe actions, and user picker.
>
> Related specs: [Chat View](chat-view.md) | [Navigation](navigation.md) | [State Management](../state.md) | [README](../README.md)
> Related product: [Chat List View](../../product/views/chat-list.md)

**Source:** [`ChatListView.swift`](../../Shared/Views/ChatList/ChatListView.swift) | [`DirectorySearchView.swift`](../../Shared/Views/ChatList/DirectorySearchView.swift) | [`DirectorySearch.swift`](../../Shared/Model/DirectorySearch.swift)

---

## Table of Contents

1. [Overview](#1-overview)
2. [ChatListView](#2-chatlistview)
3. [ChatPreviewView](#3-chatpreviewview)
4. [ChatListNavLink](#4-chatlistnavlink)
5. [Filtering & Tags](#5-filtering--tags)
6. [Search](#6-search)
7. [Swipe Actions](#7-swipe-actions)
8. [UserPicker](#8-userpicker)
9. [Floating Action Button](#9-floating-action-button)
10. [Crowdfunding Banner](#10-crowdfunding-banner)
11. [Online Search](#11-online-search)

---

## 1. Overview

The chat list is the main screen of the app, displaying all conversations for the current user. It provides:

- Conversation previews with unread badges
- Filter tabs (All, Unread, Favorites, Groups, Contacts, Business, user-defined tags)
- Search across chat names and message content
- Swipe actions for quick operations
- User profile switcher
- Floating action button for new conversations

```
ChatListView
├── Navigation Bar
│   ├── User avatar (tap → UserPicker)
│   └── Filter tabs (TagListView)
├── Search bar (on pull-down or tap)
├── Chat List (List/LazyVStack)
│   └── ChatListNavLink (per conversation)
│       └── ChatPreviewView
│           ├── Avatar
│           ├── Chat name + last message preview
│           ├── Timestamp
│           └── Unread badge
├── FAB (New Chat button)
└── Pending connection cards
```

---

## 2. [`ChatListView`](../../Shared/Views/ChatList/ChatListView.swift#L154) {#2-chatlistview}

**File**: `Shared/Views/ChatList/ChatListView.swift`

The root list view. Key responsibilities:

### Data Source
- Reads `ChatModel.shared.chats` (all conversations)
- Applies active filter from `ChatTagsModel.shared.activeFilter`
- Applies search query filtering via [`filteredChats()`](../../Shared/Views/ChatList/ChatListView.swift#L761)
- Sorts by last activity (most recent first), with pinned chats at top

### Layout
- Uses SwiftUI `List` with `ForEach` over filtered chats
- Each row is a `ChatListNavLink` wrapping a `ChatPreviewView`, drawn with its list modifiers by [`chatListNavLink(_:)`](../../Shared/Views/ChatList/ChatListView.swift#L676), which also draws known chats among the Directory results (see [Online Search](#11-online-search))
- Pull-to-refresh triggers `updateChats()` API call
- Empty state: `ChatHelp` view with getting-started guidance

### Connection Cards
- Pending contact connections (`ChatInfo.contactConnection`) shown as cards
- Contact requests (`ChatInfo.contactRequest`) shown with accept/reject UI via `ContactRequestView`

### Key Functions

| Function | Line | Description |
|----------|------|-------------|
| [`body`](../../Shared/Views/ChatList/ChatListView.swift#L188) | 188 | Main view body |
| [`filteredChats()`](../../Shared/Views/ChatList/ChatListView.swift#L761) | 761 | Applies active filter and search to chat list |
| [`searchString()`](../../Shared/Views/ChatList/ChatListView.swift#L804) | 804 | Normalizes search text for comparison |
| [`unreadBadge()`](../../Shared/Views/ChatList/ChatListView.swift#L735) | 735 | Renders unread count circle badge |
| [`stopAudioPlayer()`](../../Shared/Views/ChatList/ChatListView.swift#L755) | 755 | Stops any playing voice message |

---

## 3. [`ChatPreviewView`](../../Shared/Views/ChatList/ChatPreviewView.swift#L13) {#3-chatpreviewview}

**File**: `Shared/Views/ChatList/ChatPreviewView.swift`

Renders a single row in the chat list. Shows:

| Element | Source | Description |
|---------|--------|-------------|
| Avatar | `chatInfo.image` | Profile image or default icon |
| Chat name | `chatInfo.displayName` | Contact name, group name, or connection label |
| Last message | `chat.chatItems.last` | Preview text of most recent message |
| Timestamp | `chat.chatItems.last?.timestampText` | Relative time of last message |
| Unread badge | `chat.chatStats.unreadCount` | Circular badge with unread count |
| Mute icon | `chatInfo.chatSettings?.enableNtfs` | Bell-slash icon if notifications muted |
| Pin icon | -- | Pin indicator for pinned chats |
| Incognito icon | Contact.contactConnIncognito | Incognito mode indicator |
| Delivery status | Last sent item's `meta.itemStatus` | Check marks for delivery confirmation |

### Preview Text Rendering
- Text messages: first line of message content
- Images: camera icon + caption (if any)
- Files: paperclip icon + filename
- Voice: microphone icon + duration
- Calls: phone icon + call status
- Group events: system event description
- Encrypted/deleted: placeholder text

---

## 4. [`ChatListNavLink`](../../Shared/Views/ChatList/ChatListNavLink.swift#L44) {#4-chatlistnavlink}

**File**: `Shared/Views/ChatList/ChatListNavLink.swift`

Wraps `ChatPreviewView` in a navigation link with tap and swipe behavior:

### Tap Behavior
- Direct chat: navigates to `ChatView` via `ItemsModel.loadOpenChat(chatId)` -- [`contactNavLink()`](../../Shared/Views/ChatList/ChatListNavLink.swift#L95) L93
- Group chat: navigates to `ChatView` -- [`groupNavLink()`](../../Shared/Views/ChatList/ChatListNavLink.swift#L217) L214
- Contact request: shows `ContactRequestView` with accept/reject -- [`contactRequestNavLink()`](../../Shared/Views/ChatList/ChatListNavLink.swift#L495) L486
- Contact connection: shows `ContactConnectionInfo` -- [`contactConnectionNavLink()`](../../Shared/Views/ChatList/ChatListNavLink.swift#L530) L520
- Notes folder: navigates to `ChatView` -- [`noteFolderNavLink()`](../../Shared/Views/ChatList/ChatListNavLink.swift#L302) L298

### Navigation
- Uses `NavigationLink` (iOS 15) or programmatic navigation (iOS 16+)
- Sets `ChatModel.chatId` to trigger navigation
- `ItemsModel.loadOpenChat()` loads messages with a 250ms navigation delay for smooth animation

### Channel Adaptations in ChatListNavLink

When `groupInfo.useRelays == true`:

| Change | Behavior |
|--------|----------|
| Swipe "Leave" | Hidden when `useRelays && isOwner` |
| Context menu "Leave" | Hidden under same condition |
| `deleteGroupAlert` label | "Delete channel?" |
| `leaveGroupAlert` title | "Leave channel?" |
| `leaveGroupAlert` message | "You will stop receiving messages from this channel. Chat history will be preserved." |

### ServerSettings

`ServerSettings` struct (defined in `ChatListView.swift`) includes `serverWarnings: [UserServersWarning]` field, initialized to `[]`. This field stores validation warnings from `validateServers` and is consumed by NetworkAndServers views.

---

## 5. Filtering & Tags

### Filter Tabs ([`TagListView`](../../Shared/Views/ChatList/TagListView.swift#L20))

**File**: `Shared/Views/ChatList/TagListView.swift`

Horizontal scrolling tab bar below the navigation bar. Tabs:

| Tab | Filter | Shows |
|-----|--------|-------|
| All | `nil` | All conversations |
| Unread | `.unread` | Conversations with unread messages |
| Favorites | `.presetTag(.favorites)` | Favorited conversations |
| Groups | `.presetTag(.groups)` | Group conversations |
| Contacts | `.presetTag(.contacts)` | Direct conversations |
| Business | `.presetTag(.business)` | Business conversations |
| Group Reports | `.presetTag(.groupReports)` | Groups with pending reports |
| User tags | `.userTag(ChatTag)` | User-defined custom tags |

Filter matching is handled by [`presetTagMatchesChat()`](../../Shared/Views/ChatList/ChatListView.swift#L1378) and the in-view [`TagsView`](../../Shared/Views/ChatList/ChatListView.swift#L1172) struct.

### ChatTagsModel State

Filtering state is managed by [`ChatTagsModel`](../../Shared/Model/ChatModel.swift#L189) (`ChatModel.swift` L189):

```swift
class ChatTagsModel: ObservableObject {
    @Published var userTags: [ChatTag] = []
    @Published var activeFilter: ActiveFilter? = nil
    @Published var presetTags: [PresetTag: Int] = [:]   // count per preset tag
    @Published var unreadTags: [Int64: Int] = [:]        // unread count per user tag
}
```

- `presetTags` counts are updated whenever `chats` changes via [`updateChatTags()`](../../Shared/Model/ChatModel.swift#L197) (L197)
- Tags with zero matching chats are auto-hidden
- Active filter is auto-cleared when its tag has no matching chats

### Supporting Types

| Type | File | Line | Description |
|------|------|------|-------------|
| [`PresetTag`](../../Shared/Views/ChatList/ChatListView.swift#L36) | ChatListView.swift | 36 | Enum of built-in filter categories |
| [`ActiveFilter`](../../Shared/Views/ChatList/ChatListView.swift#L53) | ChatListView.swift | 53 | Enum wrapping preset, user-tag, or unread filter |
| [`setActiveFilter()`](../../Shared/Views/ChatList/ChatListView.swift#L1357) | ChatListView.swift | 1357 | Applies a filter and persists selection |

### Tag Management Commands
- `apiCreateChatTag(tag: ChatTagData)` -- create tag
- `apiSetChatTags(type:, id:, tagIds:)` -- assign tags to a chat
- `apiDeleteChatTag(tagId:)` -- delete tag
- `apiUpdateChatTag(tagId:, tagData:)` -- rename tag
- `apiReorderChatTags(tagIds:)` -- reorder tags

---

## 6. Search

Search is available via pull-down gesture or search button in the navigation bar.

**Search bar UI:** [`ChatListSearchBar`](../../Shared/Views/ChatList/ChatListView.swift#L868)

### Filtering Logic
- Filters `ChatModel.chats` by matching search text against:
  - `chatInfo.displayName` (contact/group name)
  - `chatInfo.localAlias` (local alias)
  - `chatInfo.fullName` (full name)
- For deeper message content search, uses `apiGetChat(chatId:, search:)` parameter
- Core logic in [`filteredChats()`](../../Shared/Views/ChatList/ChatListView.swift#L761) and [`searchString()`](../../Shared/Views/ChatList/ChatListView.swift#L804)

### Search Results
- Matching chats are displayed in the same list format
- Results update as the user types (debounced)
- Clearing search restores the full filtered list
- Searching SimpleX Directory and resolving SimpleX names online happen only on request, below the local results: see [Online Search](#11-online-search)

---

## 7. Swipe Actions

`ChatListNavLink` provides swipe actions on each row:

### Leading Swipe (left-to-right)

| Action | Icon | Handler | Line | API | Condition |
|--------|------|---------|------|-----|-----------|
| Pin / Unpin | pin | [`toggleFavoriteButton()`](../../Shared/Views/ChatList/ChatListNavLink.swift#L353) | 353 | `apiSetChatSettings` (favorite) | Always |
| Read / Unread | envelope | [`markReadButton()`](../../Shared/Views/ChatList/ChatListNavLink.swift#L333) | 333 | `apiChatRead` / `apiChatUnread` | Always |

### Trailing Swipe (right-to-left)

| Action | Icon | Handler | Line | API | Condition |
|--------|------|---------|------|-----|-----------|
| Mute / Unmute | bell.slash | [`toggleNtfsButton()`](../../Shared/Views/ChatList/ChatListNavLink.swift#L372) | 372 | `apiSetChatSettings` (enableNtfs) | Always |
| Clear | trash | [`clearChatButton()`](../../Shared/Views/ChatList/ChatListNavLink.swift#L393) | 393 | `apiClearChat` | Has messages |
| Delete | trash.fill | -- | -- | `apiDeleteChat` | Not active chat |
| Tag | tag | -- | -- | `apiSetChatTags` | Always |

---

## 8. [`UserPicker`](../../Shared/Views/ChatList/UserPicker.swift#L10) {#8-userpicker}

**File**: `Shared/Views/ChatList/UserPicker.swift`

Triggered by tapping the user avatar in the navigation bar. Presented as a sheet with:

| Section | Contents |
|---------|----------|
| User list | All non-hidden users with unread counts |
| Active user | Highlighted with checkmark |
| Actions | Settings, Your SimpleX address, User profiles |

### User Switching
- Tapping a different user calls `apiSetActiveUser(userId:)`
- Triggers `apiGetChats` for the new user
- `ChatModel.currentUser` updates, causing full UI refresh
- Hidden users are not shown (require password entry via settings)

---

## 9. Floating Action Button

The FAB (floating action button) in the bottom-right corner opens the new chat flow:

- Tap: opens `NewChatView` sheet for creating a new contact connection or group
- Shows options: Create link, Scan QR code, Paste link, Create group

---

## 10. [`GetStakeBanner`](../../Shared/Views/ChatList/GetStakeBanner.swift#L13) {#10-crowdfunding-banner}

**File**: `Shared/Views/ChatList/GetStakeBanner.swift`

Gradient card inviting the user to invest on Wefunder. Shown only when [`isInUS`](../../Shared/Views/Onboarding/WhatsNewView.swift#L45) — the same condition that gates the Wefunder row in settings — and only while `DEFAULT_GET_STAKE_BANNER_DISMISSED` is false.

### Placement

| Where | Condition | Layout |
|-------|-----------|--------|
| Chat list | rendered with [`chatListContent`](../../Shared/Views/ChatList/ChatListView.swift#L551) while no online results are shown ([`showsCards`](../../Shared/Views/ChatList/ChatListView.swift#L554)), in the `List` after `OneHandUICard` and before the chats | `.padding(.vertical, 3)`, flipped for one-hand UI, `.zIndex(1)` |
| Onboarding | below [`ConnectOnboardingView`](../../Shared/Views/NewChat/OnboardingCards.swift#L135) when `shouldShowOnboarding` | `.padding(.horizontal, 20)` (the onboarding cards' margin), `.padding(.bottom, 8)` |

The list has a single banner slot, empty while online search results are shown (see [Rendering](#rendering)), and otherwise filled by an `if`/`else if` chain in priority order: the support-ended alert (`supportEnded`), the renewal-failure alert (`badgeIssueFailed`), the pitch, then the Wefunder banner. Each banner records itself in `ChatModel.chatListBanner` (`.badgeExpired`, `.badgeIssueFailed`, `.badgePitch`, `.getStake`) in its `onAppear`, and the pitch and Wefunder conditions start with `chatModel.bannerSlotFree(for:)` — true only while nothing else was shown this app session — so dismissing a banner never puts another in its place until restart. The alerts have no such check: an alert takes the slot whenever present, and once shown it holds it. The pitch also requires `noShownBadge`, false until `BadgeModel` holds the current user's state, so it cannot take the slot from a supporter whose badge loads a moment later. The onboarding placement applies the same `bannerSlotFree` check and records `.getStake`.

In the onboarding branch the `.scaleEffect` and `ThemedBackground` are applied to the enclosing `VStack` rather than to each child, so the banner stays below the pages in both toolbar modes.

### Dismissal

| Default | Set by | Effect |
|---------|--------|--------|
| `DEFAULT_GET_STAKE_BANNER_TAPPED` | [`openGetStake()`](../../Shared/Views/ChatList/ChatListView.swift#L546) | the dismiss X appears from then on, while there are chats |
| `DEFAULT_GET_STAKE_BANNER_DISMISSED` | the dismiss X | hides the banner in both placements |
| `DEFAULT_SUPPORTER_BANNER_TAPPED` | tapping the supporter pitch | the pitch's dismiss X appears from then on |
| `DEFAULT_SUPPORTER_BANNER_SHOWN` | the pitch's dismiss X, through its "You can support SimpleX later in Settings." alert, and a successful code redemption | hides the pitch |

The two badge alert banners always offer the X; only the pitch waits to be tapped once, so a user who has not looked at it cannot dismiss it unseen.

Both are in `hintDefaults`, so "Reset all hints" in the developer settings restores the banner. The X is never offered in the onboarding branch, so the banner cannot be dismissed before the user has a chat.

Tapping the card opens [`GetStakeView`](../../Shared/Views/Onboarding/WhatsNewView.swift#L884) as an `appSheet`.

### Shared card chrome

`BannerCard` (a `ViewModifier`: paddings, minimum height scaled by Dynamic Type, gradient background, rounded corners) and `BannerDismissButton` are declared separately from `GetStakeBanner` so other banners can adopt the same chrome. The gradient reuses `OnboardingCardView.gradientPoints`, `lightStops` and `darkStops`.

---

## 11. Online Search {#11-online-search}

**Files**: `Shared/Views/ChatList/DirectorySearchView.swift` (model and rows), `Shared/Model/DirectorySearch.swift` (response and result types), `Shared/Views/ChatList/ChatListView.swift` (triggers and rendering)

The search text can be sent online on request: to SimpleX Directory, to find public groups and channels, and, when the text is a SimpleX name, to the user's name-resolving server. Results are shown below the local chats in two parts, Names then Directory. They are never saved and live only as long as the search text and the active profile: [`ChatListSearchBar`](../../Shared/Views/ChatList/ChatListView.swift#L868) calls `reset()` on every change of `searchText` ([L957](../../Shared/Views/ChatList/ChatListView.swift#L957)) and of `currentUser?.userId` ([L960](../../Shared/Views/ChatList/ChatListView.swift#L960)). The New chat sheet uses the same model and rows (see [Navigation, New Chat Online Search](navigation.md#12-new-chat-online-search)).

### Triggers

| Trigger | Handler | Runs |
|---------|---------|------|
| Keyboard Search key (`.submitLabel(.search)`) | [`runOnlineSearch()`](../../Shared/Views/ChatList/ChatListView.swift#L1068) | `search(text, name: connectNameCandidate)`: the directory's first page, and name resolution when the text is a name candidate |
| "Search in Directory" row ([`SearchInDirectoryRow`](../../Shared/Views/ChatList/DirectorySearchView.swift#L211)) | [`runDirectorySearch()`](../../Shared/Views/ChatList/ChatListView.swift#L1062) | `searchDirectory(text)`: the directory only |

Both triggers do nothing for empty (trimmed) text or a pasted SimpleX link, and both go through [`confirmDirectorySearch()`](../../Shared/Views/ChatList/DirectorySearchView.swift#L351), which runs the search at once when `DEFAULT_DIRECTORY_SEARCH_ALERT_SHOWN` is set and otherwise shows the three-action alert ([`showDirectorySearchAlert`](../../Shared/Views/ChatList/DirectorySearchView.swift#L359)): Cancel / Search / Search and don't show again. Its message names both recipients: the directory, and, for a SimpleX name, the server that resolves names. The key is in `hintDefaults`, so "Reset all hints" brings the alert back.

The rows above or below the search field follow the toolbar mode:

| Search text | One-hand UI (slot above the field, [L890](../../Shared/Views/ChatList/ChatListView.swift#L890)) | Top bar (below the field, [L941](../../Shared/Views/ChatList/ChatListView.swift#L941)) |
|-------------|-----------------------------------|------------------------------|
| Empty | Tags | Tags above; nothing below |
| Plain text | "Search in Directory" | Tags above; "Search in Directory" below |
| Name candidate, no name results yet | "Connect to …" and "Search in Directory" | Tags above; "Connect to …" and "Search in Directory" below |
| Name candidate with name results | "Search in Directory" | Tags above; "Search in Directory" below |
| Pasted SimpleX link | Tags | Tags above; nothing below |

### [`OnlineSearchModel`](../../Shared/Views/ChatList/DirectorySearchView.swift#L18-L209)

`@MainActor` `ObservableObject`, owned by `ChatListView` as `@StateObject onlineSearch` and passed to `ChatListSearchBar`.

| State | Description |
|-------|-------------|
| `names: [NameSearchResult]` | Name results for the current text |
| `directory: [DirectoryResult]` | Directory entries shown so far, each with the local chat its link refers to |
| `loading` | A directory request is in flight |
| `resolvingNames` | Name resolution is in flight |
| `failed` | The last directory request failed |
| `searched` | A directory request for the current text completed, successfully or not (tells "not searched" from "found nothing") |
| `cursor` (private) | Opaque `JSONValue` from the last page, echoed back for the next one; nil when there are no more |
| `lastPageIds` (private) | The ids of the results the last successful request added (none when every entry of the page was already shown); cleared with the directory state, kept by a failed request |
| `generation` (private) | Bumped by `reset()` and `cancelRequest()`; a reply whose captured generation differs never writes model state |

| Computed | Definition |
|----------|------------|
| [`searching`](../../Shared/Views/ChatList/DirectorySearchView.swift#L34) | `loading \|\| resolvingNames` |
| [`showResults`](../../Shared/Views/ChatList/DirectorySearchView.swift#L35) | `searching \|\| searched \|\| !names.isEmpty` |
| [`footer`](../../Shared/Views/ChatList/DirectorySearchView.swift#L36) | `DirectoryFooter.retry` when `failed`, else `.showMore` when a cursor exists, else nil |

| Method | Description |
|--------|-------------|
| [`reset()`](../../Shared/Views/ChatList/DirectorySearchView.swift#L38) | Clears all results and flags, bumps `generation` (text change, profile switch) |
| [`cancelRequest()`](../../Shared/Views/ChatList/DirectorySearchView.swift#L52) | The spinner's `onCancel`: bumps `generation` and clears `loading` and `resolvingNames`; results already shown stay |
| [`search(_:name:)`](../../Shared/Views/ChatList/DirectorySearchView.swift#L58) | Search key: ignored for empty text or while `searching` for the same text; otherwise `reset()`, then the first directory page and, with a name, name resolution concurrently (`async let`, with the generation captured before the child task starts) |
| [`searchDirectory(_:)`](../../Shared/Views/ChatList/DirectorySearchView.swift#L75) | The row: ignored while `loading` for the same text; for the same text it clears only the directory state (name results stay), for new text it resets; then the first page |
| [`loadMore()`](../../Shared/Views/ChatList/DirectorySearchView.swift#L91) | "Show more": the next page, appended |
| [`retry()`](../../Shared/Views/ChatList/DirectorySearchView.swift#L97) | The retry row: resumes the page when a cursor exists, else repeats the first page; no reset |
| [`rows(shownChatIds:)`](../../Shared/Views/ChatList/DirectorySearchView.swift#L105) | The rows to render, given the ids of the local chats shown (see De-duplication) |
| [`refreshLocalChatIds()`](../../Shared/Views/ChatList/DirectorySearchView.swift#L126) | The `cleanup` of a connection started from a result row, run when its flow ends: looks the local chat up again, store-only, for every row that has none (a name row by its target, a directory row by its link), unless the generation changed. A chat opened from a result is created during that flow, so its row is then hidden like any shown chat, or drawn as a chat row when the list doesn't show it |

**Spinner.** `loading` and `resolvingNames` have a `didSet` that calls [`updateSpinner()`](../../Shared/Views/ChatList/DirectorySearchView.swift#L195), which starts the "Searching directory…" progress with owner `.directorySearch` when `searching` becomes true and stops it when it becomes false (`spinnerShown` keeps the last state, so repeated writes do nothing). The search field shows it as a spinner once it has run for a second. The spinner is shared with connections: [`ConnectProgressOwner`](../../Shared/Model/ChatModel.swift#L303) records whose it is, [`startConnectProgress`](../../Shared/Model/ChatModel.swift#L316) does not let a search take it from a running connection, and [`stopConnectProgress`](../../Shared/Model/ChatModel.swift#L327) ignores a stop from another owner. Tapping a result calls `planAndConnect`, whose `cancelConnectProgress()` ([L336](../../Shared/Model/ChatModel.swift#L336)) cancels the search through its `onCancel` before the connection takes the spinner.

### Directory requests

[`request(append:)`](../../Shared/Views/ChatList/DirectorySearchView.swift#L170) calls [`apiSearchDirectory`](../../Shared/Model/SimpleXAPI.swift#L1044), which sends `APISendServiceRequest` to [`DIRECTORY_SERVICE_LINK`](../../Shared/Model/DirectorySearch.swift#L14) with a [10-second timeout](../../Shared/Model/DirectorySearch.swift#L17) and the request built by [`directorySearchRequestJSON`](../../Shared/Model/DirectorySearch.swift#L84) (`searchText`, and `searchCursor` for later pages). [`parseDirectorySearchResponse`](../../Shared/Model/DirectorySearch.swift#L92) accepts only a `searchResults` response and decodes its entries one at a time, so an entry the app cannot decode is dropped and the rest of the page is kept.

[`DirectorySearchEntry`](../../Shared/Model/DirectorySearch.swift#L29-L64) has a custom [`init(from:)`](../../Shared/Model/DirectorySearch.swift#L46): its `link` is the wire `groupLink.connShortLink`, else `connFullLink`, and decoding throws unless [`simplexLinkType`](../../Shared/Model/DirectorySearch.swift#L66) classifies it as `.group` or `.channel`. `simplexLinkType` returns the link type only when [`strConnectTarget`](../../Shared/Views/NewChat/NewChatView.swift#L860) finds exactly one SimpleX link whose text is the whole string. `link` is also the entry's `id`. The directory sends only the short link when an entry has one.

For each new entry of a page (not already shown, not repeated in the page), the request looks up the local chat its link refers to with `apiConnectPlan(connLink: entry.link, resolveMode: .never, onFailure: .log)` and [`planLocalChatId`](../../Shared/Model/DirectorySearch.swift#L102), building a [`DirectoryResult`](../../Shared/Model/DirectorySearch.swift#L173). The link-type check makes this lookup store-only: the core plans contact-mode links under `.never` from the store, but would fetch an invitation link from its server. The page is published, appended or replacing, only if the generation did not change.

### Name resolution

[`resolveNames(_:generation:)`](../../Shared/Views/ChatList/DirectorySearchView.swift#L154) resolves `[name]` for a candidate starting with `@` or `#`, else `["@" + name, "#" + name]` (the candidate comes from `nameSearchCandidate`, as on the typing path). Each target is planned with `apiConnectPlan(connLink: target, resolveMode: .unknown, onFailure: .log)`; it stops before a target once the generation changed, so a cancelled or replaced search sends no further names. [`NameSearchResult.init?(target:result:)`](../../Shared/Model/DirectorySearch.swift#L138) maps the plan with one exhaustive switch: a contact address or group link gives a row (profile from the link data or the known chat), the user's own contact address, an invitation link and an error give none. `simplexName` is the plan's `planSimplexName.shortStr`, `localChatId` comes from `planLocalChatId`, and for a group link `resolvedLink` is the link the core resolved (`connLink.connShortLink`). Failures are logged at debug level and never shown as alerts; `inProgress: BoxedValue(false)` also suppresses the network retry alert.

### De-duplication

[`rows(shownChatIds:)`](../../Shared/Views/ChatList/DirectorySearchView.swift#L105) returns an [`OnlineSearchRows`](../../Shared/Model/DirectorySearch.swift#L180) at render time, so pages added by "Show more" are filtered too:

- a name result is hidden when its `localChatId` is among the shown local chats;
- a directory result is hidden when its `localChatId` is among the shown local chats or any name result's `localChatId`, or when its `entry.link` equals any name result's `resolvedLink` (all name results, shown or not);
- `pageInChats` is true when the last page's results (`lastPageIds`) are not empty and each one's `localChatId` is among the shown local chats or the name results' chat ids. A result hidden only because a name result resolved to its link is not a chat the user has, so it does not count, and the part keeps its usual look;
- `footer` passes through.

Only store-derived chat ids and the link the core resolved can hide a row, never a field the directory or a name's owner wrote. A mismatch in link encoding can only produce a duplicate row.

A directory result that is a chat the user has but that the screen doesn't list (the local filter doesn't match it) stays in `rows.directory` and is drawn as a chat row (see Rendering). When its chat id is not among the loaded chats (`getChat` returns nil), the Directory row is drawn as before, and tapping it goes through `planAndConnect`, which opens the known chat.

[GAP] A regular group the user joined through a different link, through the directory's full link, or by invitation, is not found by the store lookup and still appears as a Directory row with "Join group"; tapping it goes through `planAndConnect`.

### Rendering

| Where | Rows | Empty state |
|-------|------|-------------|
| [`chatListContent`](../../Shared/Views/ChatList/ChatListView.swift#L551) | `rows(shownChatIds:)` with the ids of the shown chats ([L553](../../Shared/Views/ChatList/ChatListView.swift#L553)); after the local chats, [`nameRows`](../../Shared/Views/ChatList/ChatListView.swift#L483) then [`directoryRows`](../../Shared/Views/ChatList/ChatListView.swift#L499). While `showResults` ([`showsCards`](../../Shared/Views/ChatList/ChatListView.swift#L554) false), the list leaves out `OneHandUICard`, the banner slot and `ConnectBannerCard`; they return when the search text is cleared, which resets the model | `noChatsView()` overlay only when no chat is shown, the model is not `searching` and `rows.isEmpty` ([L668](../../Shared/Views/ChatList/ChatListView.swift#L668)) |
| Onboarding branch of [`chatList`](../../Shared/Views/ChatList/ChatListView.swift#L434) | While `showResults`, a list of the same parts with `shownChatIds: []` ([L449](../../Shared/Views/ChatList/ChatListView.swift#L449)) in place of the onboarding pages | A centred "No chats found" row when not searching and `rows.isEmpty` |

Each part renders only when it has rows: the Names part when `rows.names` is not empty, the Directory part when `rows.directory` is not empty, `rows.pageInChats` or `rows.footer` is set. Each has a header row ([`searchPartHeader`](../../Shared/Views/ChatList/ChatListView.swift#L536): callout font, secondary colour, `.isHeader` trait, no separator) rather than a sticky `Section` header, since the one-hand list is flipped. [`searchPart`](../../Shared/Views/ChatList/ChatListView.swift#L526) places it before the part's rows with the top bar and after them with the one-hand UI, so it is drawn above them in both layouts, the one-hand list being flipped. Every row carries the per-row flip and a clear background.

| Row | View | Tap |
|-----|------|-----|
| Name | [`NameSearchRow`](../../Shared/Views/ChatList/DirectorySearchView.swift#L286): avatar, profile name in bold, the SimpleX name in parentheses (secondary), description (2 lines) | `planAndConnect(result.target, dismiss: false)`, `cleanup`: `refreshLocalChatIds()` |
| Directory entry | [`DirectorySearchRow`](../../Shared/Views/ChatList/DirectorySearchView.swift#L233): avatar, `#` + name, the directory's SimpleX name claim as plain text, description (2 lines), member or subscriber count, and the verb "Join channel" / "Join group" (one line, fixed size) | `planAndConnect(result.entry.link, dismiss: false)`, `cleanup`: `refreshLocalChatIds()` |
| Known chat | When `result.localChatId` names a loaded chat (`chatModel.getChat`), in place of the directory entry: the chat list's own row, [`chatListNavLink(_:)`](../../Shared/Views/ChatList/ChatListView.swift#L676) (`ChatListNavLink` with the local chats' modifiers) | as a local chat: opens it |
| Note | [`DirectoryPageInChatsRow`](../../Shared/Views/ChatList/DirectorySearchView.swift#L313) via [`directoryPageInChatsRow()`](../../Shared/Views/ChatList/ChatListView.swift#L694), after the directory rows when `rows.pageInChats`: "All results on this page are already in your chats", secondary colour | none |
| Retry | [`DirectoryRetryRow`](../../Shared/Views/ChatList/DirectorySearchView.swift#L337) via [`directoryRetryRow()`](../../Shared/Views/ChatList/ChatListView.swift#L706): "Search failed, tap to retry" | `retry()` |
| Show more | [`DirectoryShowMoreRow`](../../Shared/Views/ChatList/DirectorySearchView.swift#L322) via [`directoryShowMoreRow()`](../../Shared/Views/ChatList/ChatListView.swift#L700): "Show more", disabled and in the secondary colour while `loading` | `loadMore()` |

A name or directory row tap first sets `searchFocussed = false`. Text from the directory and from name owners is rendered with `Text(String)` / `Text(verbatim:)` (never parsed as markdown) with line limits.

[GAP] Resolving a SimpleX name with `.unknown` also loads the resolved address's link data from the server that hosts it; the alert names only the directory and the name-resolving server.

---

## Source Files

| File | Path | Key struct | Line |
|------|------|------------|------|
| Chat list view | [`ChatListView.swift`](../../Shared/Views/ChatList/ChatListView.swift) | `ChatListView` | [154](../../Shared/Views/ChatList/ChatListView.swift#L154) |
| Chat preview row | [`ChatPreviewView.swift`](../../Shared/Views/ChatList/ChatPreviewView.swift) | `ChatPreviewView` | [12](../../Shared/Views/ChatList/ChatPreviewView.swift#L13) |
| Navigation link wrapper | [`ChatListNavLink.swift`](../../Shared/Views/ChatList/ChatListNavLink.swift) | `ChatListNavLink` | [43](../../Shared/Views/ChatList/ChatListNavLink.swift#L44) |
| Tag filter tabs | [`TagListView.swift`](../../Shared/Views/ChatList/TagListView.swift) | `TagListView` | [19](../../Shared/Views/ChatList/TagListView.swift#L20) |
| User picker sheet | [`UserPicker.swift`](../../Shared/Views/ChatList/UserPicker.swift) | `UserPicker` | [9](../../Shared/Views/ChatList/UserPicker.swift#L10) |
| Getting started help | [`ChatHelp.swift`](../../Shared/Views/ChatList/ChatHelp.swift) | | |
| Contact request view | [`ContactRequestView.swift`](../../Shared/Views/ChatList/ContactRequestView.swift) | | |
| Contact connection info | [`ContactConnectionInfo.swift`](../../Shared/Views/ChatList/ContactConnectionInfo.swift) | | |
| Contact connection view | [`ContactConnectionView.swift`](../../Shared/Views/ChatList/ContactConnectionView.swift) | | |
| Server summary | [`ServersSummaryView.swift`](../../Shared/Views/ChatList/ServersSummaryView.swift) | | |
| One-hand UI card | [`OneHandUICard.swift`](../../Shared/Views/ChatList/OneHandUICard.swift) | | |
| Crowdfunding banner | [`GetStakeBanner.swift`](../../Shared/Views/ChatList/GetStakeBanner.swift) | `GetStakeBanner` | [13](../../Shared/Views/ChatList/GetStakeBanner.swift#L13) |
| Online search model and rows | [`DirectorySearchView.swift`](../../Shared/Views/ChatList/DirectorySearchView.swift) | `OnlineSearchModel` | [18](../../Shared/Views/ChatList/DirectorySearchView.swift#L18) |
| Directory response and online result types | [`DirectorySearch.swift`](../../Shared/Model/DirectorySearch.swift) | `DirectorySearchEntry` | [29](../../Shared/Model/DirectorySearch.swift#L29) |
