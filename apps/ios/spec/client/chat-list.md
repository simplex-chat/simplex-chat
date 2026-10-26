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
- Applies search query filtering via [`filteredChats()`](../../Shared/Views/ChatList/ChatListView.swift#L716)
- Sorts by last activity (most recent first), with pinned chats at top

### Layout
- Uses SwiftUI `List` with `ForEach` over filtered chats
- Each row is a `ChatListNavLink` wrapping a `ChatPreviewView`, drawn with its list modifiers by [`chatListNavLink(_:)`](../../Shared/Views/ChatList/ChatListView.swift#L637), which also draws your chats among the online search results (see [Online Search](#11-online-search))
- Pull-to-refresh triggers `updateChats()` API call
- Empty state: `ChatHelp` view with getting-started guidance

### Connection Cards
- Pending contact connections (`ChatInfo.contactConnection`) shown as cards
- Contact requests (`ChatInfo.contactRequest`) shown with accept/reject UI via `ContactRequestView`

### Key Functions

| Function | Line | Description |
|----------|------|-------------|
| [`body`](../../Shared/Views/ChatList/ChatListView.swift#L189) | 189 | Main view body |
| [`filteredChats()`](../../Shared/Views/ChatList/ChatListView.swift#L716) | 723 | Applies active filter and search to chat list |
| [`searchString()`](../../Shared/Views/ChatList/ChatListView.swift#L759) | 766 | Normalizes search text for comparison |
| [`unreadBadge()`](../../Shared/Views/ChatList/ChatListView.swift#L690) | 697 | Renders unread count circle badge |
| [`stopAudioPlayer()`](../../Shared/Views/ChatList/ChatListView.swift#L710) | 717 | Stops any playing voice message |

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

Filter matching is handled by [`presetTagMatchesChat()`](../../Shared/Views/ChatList/ChatListView.swift#L1328) and the in-view [`TagsView`](../../Shared/Views/ChatList/ChatListView.swift#L1122) struct.

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
| [`setActiveFilter()`](../../Shared/Views/ChatList/ChatListView.swift#L1307) | ChatListView.swift | 1314 | Applies a filter and persists selection |

### Tag Management Commands
- `apiCreateChatTag(tag: ChatTagData)` -- create tag
- `apiSetChatTags(type:, id:, tagIds:)` -- assign tags to a chat
- `apiDeleteChatTag(tagId:)` -- delete tag
- `apiUpdateChatTag(tagId:, tagData:)` -- rename tag
- `apiReorderChatTags(tagIds:)` -- reorder tags

---

## 6. Search

Search is available via pull-down gesture or search button in the navigation bar.

**Search bar UI:** [`ChatListSearchBar`](../../Shared/Views/ChatList/ChatListView.swift#L823)

### Filtering Logic
- Filters `ChatModel.chats` by matching search text against:
  - `chatInfo.displayName` (contact/group name)
  - `chatInfo.localAlias` (local alias)
  - `chatInfo.fullName` (full name)
- For deeper message content search, uses `apiGetChat(chatId:, search:)` parameter
- Core logic in [`filteredChats()`](../../Shared/Views/ChatList/ChatListView.swift#L716) and [`searchString()`](../../Shared/Views/ChatList/ChatListView.swift#L759)

### Search Results
- Matching chats are displayed in the same list format
- Results update as the user types (debounced)
- Clearing search restores the full filtered list
- Searching SimpleX Directory online happens only on request, below the local results: see [Online Search](#11-online-search)

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
| Chat list | rendered with [`chatListContent`](../../Shared/Views/ChatList/ChatListView.swift#L513) while no online results are shown ([`showsCards`](../../Shared/Views/ChatList/ChatListView.swift#L516)), in the `List` after `OneHandUICard` and before the chats | `.padding(.vertical, 3)`, flipped for one-hand UI, `.zIndex(1)` |
| Onboarding | below [`ConnectOnboardingView`](../../Shared/Views/NewChat/OnboardingCards.swift#L135) when `shouldShowOnboarding` | `.padding(.horizontal, 20)` (the onboarding cards' margin), `.padding(.bottom, 8)` |

The list has a single banner slot, empty while online search results are shown (see [Rendering](#rendering)), and otherwise filled by an `if`/`else if` chain in priority order: the support-ended alert (`supportEnded`), the renewal-failure alert (`badgeIssueFailed`), the pitch, then the Wefunder banner. Each banner records itself in `ChatModel.chatListBanner` (`.badgeExpired`, `.badgeIssueFailed`, `.badgePitch`, `.getStake`) in its `onAppear`, and the pitch and Wefunder conditions start with `chatModel.bannerSlotFree(for:)` — true only while nothing else was shown this app session — so dismissing a banner never puts another in its place until restart. The alerts have no such check: an alert takes the slot whenever present, and once shown it holds it. The pitch also requires `noShownBadge`, false until `BadgeModel` holds the current user's state, so it cannot take the slot from a supporter whose badge loads a moment later. The onboarding placement applies the same `bannerSlotFree` check and records `.getStake`.

In the onboarding branch the `.scaleEffect` and `ThemedBackground` are applied to the enclosing `VStack` rather than to each child, so the banner stays below the pages in both toolbar modes.

### Dismissal

| Default | Set by | Effect |
|---------|--------|--------|
| `DEFAULT_GET_STAKE_BANNER_TAPPED` | [`openGetStake()`](../../Shared/Views/ChatList/ChatListView.swift#L508) | the dismiss X appears from then on, while there are chats |
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

The search text can be sent online on request to SimpleX Directory, to find public groups and channels. Results are shown below the local chats as one list without headers: your chats first, then the other results. They are never saved and live only as long as the search text and the active profile: [`ChatListView`](../../Shared/Views/ChatList/ChatListView.swift#L154), which owns `searchText` and the model, calls `reset()` from `viewBody`, which is shown in every state, on every change of `searchText` ([L240](../../Shared/Views/ChatList/ChatListView.swift#L240)) and of `currentUser?.userId` ([L243](../../Shared/Views/ChatList/ChatListView.swift#L243)). The search bar is a different instance in the onboarding branch and in the list, so a profile switch that also switches between them would replace the bar before it could see the change. The New chat sheet uses the same model and rows (see [Navigation, New Chat Online Search](navigation.md#12-new-chat-online-search)).

### Triggers

| Trigger | Handler | Runs |
|---------|---------|------|
| Keyboard Search key (`.submitLabel(.search)`) | [`runDirectorySearch()`](../../Shared/Views/ChatList/ChatListView.swift#L1019) | `searchDirectory(text)` |
| "Search in Directory" row ([`SearchInDirectoryRow`](../../Shared/Views/ChatList/DirectorySearchView.swift#L163)) | [`runDirectorySearch()`](../../Shared/Views/ChatList/ChatListView.swift#L1019) | `searchDirectory(text)` |

Both triggers run the same directory search, which never resolves SimpleX names: a name is looked up only by "Connect to …" (`ConnectByNameRow`). The field neither autocorrects nor capitalizes, so the Search key searches what was typed. Both triggers do nothing for empty (trimmed) text or a pasted SimpleX link, and both go through [`confirmDirectorySearch()`](../../Shared/Views/ChatList/DirectorySearchView.swift#L321), which runs the search at once when the "Directory search" switch is on (`DEFAULT_PRIVACY_DIRECTORY_SEARCH`, default false, in Privacy settings › Chats after "Send link previews") and otherwise shows the alert [`showDirectorySearchAlert`](../../Shared/Views/ChatList/DirectorySearchView.swift#L329): "Enable Directory search?", with the message "Search text is sent to SimpleX Directory, each search separately and without your profile." and two actions, Cancel (sends nothing) and Enable (turns the switch on, then searches). So every search asks while the switch is off. The key is not in `hintDefaults`: "Reset all hints" leaves the switch as it is.

The rows above or below the search field follow the toolbar mode. "Search in Directory" hides while results for the current text show or load (`!onlineSearch.showResults`); "Connect to …" stays next to them:

| Search text | One-hand UI (slot above the field, [L852](../../Shared/Views/ChatList/ChatListView.swift#L845)) | Top bar (below the field, [L905](../../Shared/Views/ChatList/ChatListView.swift#L898)) |
|-------------|-----------------------------------|------------------------------|
| Empty | Tags | Tags above; nothing below |
| Plain text | "Search in Directory"; nothing while results show (the tags don't come back) | Tags above; "Search in Directory" below, nothing while results show |
| Name candidate | "Connect to …", and "Search in Directory" until results show | Tags above; "Connect to …" below, and "Search in Directory" until results show |
| Pasted SimpleX link | Tags | Tags above; nothing below |

### [`OnlineSearchModel`](../../Shared/Views/ChatList/DirectorySearchView.swift#L18-L161)

`@MainActor` `ObservableObject`, owned by `ChatListView` as `@StateObject onlineSearch` and passed to `ChatListSearchBar`.

| State | Description |
|-------|-------------|
| `directory: [DirectoryResult]` | Directory entries shown so far, each with its `DirectoryResultState` (see Directory requests) |
| `moreCount: Int?` | The count of matches left that the last successful page carried; nil when the directory sent none or an invalid one |
| `loading` | A directory request is in flight |
| `failed` | The last directory request failed |
| `searched` | A directory request for the current text completed, successfully or not (tells "not searched" from "found nothing") |
| `cursor` (private) | Opaque `JSONValue` from the last page, echoed back for the next one; nil when there are no more |
| `generation` (private) | Bumped by `reset()` and `cancelRequest()`; a reply whose captured generation differs never writes model state |

| Computed | Definition |
|----------|------------|
| [`searching`](../../Shared/Views/ChatList/DirectorySearchView.swift#L32) | `loading` |
| [`showResults`](../../Shared/Views/ChatList/DirectorySearchView.swift#L33) | `loading \|\| searched` |
| [`footer`](../../Shared/Views/ChatList/DirectorySearchView.swift#L34) | `DirectoryFooter.retry` when `failed`, else `.loadMore` when a cursor exists, else nil |

| Method | Description |
|--------|-------------|
| [`reset()`](../../Shared/Views/ChatList/DirectorySearchView.swift#L36) | Clears all results, `moreCount` and flags, bumps `generation` (text change, profile switch) |
| [`cancelRequest()`](../../Shared/Views/ChatList/DirectorySearchView.swift#L48) | The spinner's `onCancel`: bumps `generation` and clears `loading`; results already shown stay |
| [`searchDirectory(_:)`](../../Shared/Views/ChatList/DirectorySearchView.swift#L53) | Both triggers: ignored for empty text or while `loading` for the same text; for the same text it clears the results, `moreCount`, cursor and flags, for new text it resets; then the first page |
| [`loadMore()`](../../Shared/Views/ChatList/DirectorySearchView.swift#L69) | The load row: the next page, appended |
| [`retry()`](../../Shared/Views/ChatList/DirectorySearchView.swift#L75) | The retry row: resumes the page when a cursor exists, else repeats the first page of the text searched, and does nothing after a reset (its consent alert can outlast one); no reset |
| [`rows(shownChatIds:)`](../../Shared/Views/ChatList/DirectorySearchView.swift#L83) | The rows to render, given the ids of the local chats shown (see De-duplication) |
| [`refreshStates()`](../../Shared/Views/ChatList/DirectorySearchView.swift#L102) | The `cleanup` of a connection started from a result row, run when its flow ends: plans every result that is not `.yourChat` again, store-only, unless the generation changed. A chat opened from a result is created during that flow, so its result then becomes `.yourChat`: hidden like any shown chat, or drawn as its chat row when the list doesn't show it |

**Spinner.** `loading` has a `didSet` that calls [`updateSpinner()`](../../Shared/Views/ChatList/DirectorySearchView.swift#L147), which starts the "Searching directory…" progress with owner `.directorySearch` when `searching` becomes true and stops it when it becomes false (`spinnerShown` keeps the last state, so repeated writes do nothing). The search field shows it as a spinner once it has run for a second. The spinner is shared with connections: [`ConnectProgressOwner`](../../Shared/Model/ChatModel.swift#L303) records whose it is, [`startConnectProgress`](../../Shared/Model/ChatModel.swift#L316) does not let a search take it from a running connection, and [`stopConnectProgress`](../../Shared/Model/ChatModel.swift#L327) ignores a stop from another owner. Tapping a result calls `planAndConnect`, whose `cancelConnectProgress()` ([L336](../../Shared/Model/ChatModel.swift#L336)) cancels the search through its `onCancel` before the connection takes the spinner.

### Directory requests

[`request(append:)`](../../Shared/Views/ChatList/DirectorySearchView.swift#L122) calls [`apiSearchDirectory`](../../Shared/Model/SimpleXAPI.swift#L1044), which sends `APISendServiceRequest` to [`DIRECTORY_SERVICE_LINK`](../../Shared/Model/DirectorySearch.swift#L14) with a [10-second timeout](../../Shared/Model/DirectorySearch.swift#L17) and the request built by [`directorySearchRequestJSON`](../../Shared/Model/DirectorySearch.swift#L89) (`searchText`, cut to the directory's limit of [100 characters](../../Shared/Model/DirectorySearch.swift#L20), and `searchCursor` for later pages). [`parseDirectorySearchResponse`](../../Shared/Model/DirectorySearch.swift#L98) accepts only a `searchResults` response and decodes its entries one at a time, so an entry the app cannot decode is dropped and the rest of the page is kept.

The directory's response (`DRSearchResults`, apps/simplex-directory-service/src/Directory/Rpc.hs) is `{"type": "searchResults", "entries": […], "searchCursor": …, "moreCount": N, "pageSize": N}`. `searchCursor` is absent when there are no more results. `moreCount` is the number of matches after the cursor, 0 without one. `pageSize` is the directory's page size, which the app does not read: a page is cut to fit the service envelope, so it often carries fewer entries than that. The app keeps `moreCount` only when it is a JSON number that is an integer in 0…`Int32.max` ([L106](../../Shared/Model/DirectorySearch.swift#L106)); anything else becomes nil without failing the response. It is only shown as text in the load row. An older directory sends no count, and the row then reads "Load more".

[GAP] `moreCount` counts the matching listings, including ones without a public link that the directory never sends, so the load row can promise more results than it brings.

[`DirectorySearchEntry`](../../Shared/Model/DirectorySearch.swift#L32-L67) has a custom [`init(from:)`](../../Shared/Model/DirectorySearch.swift#L49): its `link` is the wire `groupLink.connShortLink`, else `connFullLink`, and decoding throws unless [`simplexLinkType`](../../Shared/Model/DirectorySearch.swift#L69) classifies it as `.group` or `.channel`. `simplexLinkType` returns the link type only when [`strConnectTarget`](../../Shared/Views/NewChat/NewChatView.swift#L860) finds exactly one SimpleX link whose text is the whole string. `link` is also the entry's `id`. The directory sends only the short link when an entry has one.

For each new entry of a page (not already shown, not repeated in the page), the request plans its link with `apiConnectPlan(connLink: entry.link, resolveMode: .never, onFailure: .log)` ([`resultState`](../../Shared/Views/ChatList/DirectorySearchView.swift#L117)) and maps the plan with [`directoryResultState`](../../Shared/Model/DirectorySearch.swift#L140) to a [`DirectoryResultState`](../../Shared/Model/DirectorySearch.swift#L133), building a [`DirectoryResult`](../../Shared/Model/DirectorySearch.swift#L148) `{entry, state}`:

| State | Plan | Drawn as |
|-------|------|----------|
| `.yourChat(chatId)` | [`planLocalChatId`](../../Shared/Model/DirectorySearch.swift#L110) names a chat (a known or prepared group, your own group, or a pending join with its chat) | the chat's own row |
| `.joining` | `.groupLink(.connectingConfirmReconnect)` or `.groupLink(.connectingProhibit(nil))`: a pending join without a chat | a result with "Already joining" |
| `.open` | anything else: `.groupLink(.ok)`, or no plan (`notResolvedLocally` for a link the store doesn't know, which `apiConnectPlan` returns as nil without an alert, or a logged failure) | a result with Open |

The link-type check makes this lookup store-only: the core plans contact-mode links under `.never` from the store, but would fetch an invitation link from its server. The directory sends the short link whenever an entry has one, and a short link reaches a connecting plan from the store only through a pending `/c` join, whose connection stores the full link under the short link, so `.joining` is reachable for a short-link entry with a pending `/c` join and for entries the directory sends with a full link. The page is published with its `moreCount`, appended or replacing, only if the generation did not change.

### De-duplication

[`rows(shownChatIds:)`](../../Shared/Views/ChatList/DirectorySearchView.swift#L83) returns an [`OnlineSearchRows`](../../Shared/Model/DirectorySearch.swift#L155) `{yourChats, results, footer}` at render time, so pages added by "Load more" are handled too:

- `yourChats`: the chat ids of `.yourChat` results whose chat is loaded (`ChatModel.shared.getChat`) and not among the shown local chats, in directory order, each id once (two entries with different links can name one group);
- `results`: the other results, in directory order; a `.yourChat` result whose chat is not loaded becomes `.open`, and Open's online plan then finds the known chat;
- `footer` passes through.

So nothing is shown twice: a result that is a chat the list shows is hidden, a chat the list doesn't show (the local filter doesn't match it) is drawn once as its own row, and a link repeated across pages was already dropped by the request. A page whose results are all chats the list shows adds no row and no note. Only store-derived chat ids decide this, never a field the directory wrote. A mismatch in link encoding can only produce a duplicate row.

[GAP] The store-only plan finds a group only when it was prepared from that short link (every short-link join in current apps, Open included), joined with `/c` and that short link (the join connection stores the full link under it), or joined with the full link the directory sends. A group joined otherwise shows as a result with Open beside its own chat row. For a group joined with a full link (an older app) when the directory sends the short link, Open's online plan finds it known and shows the "Open group" alert. For a member added by invitation, or one who joined through a link the owner has since replaced, the online plan is `.ok`, so Open prepares and opens a second copy of the group without asking. The core tests "short link without resolving: prepared and /c-joined groups known, invited member not found" and "short link without resolving: pending /c join is connecting" (tests/ChatTests/Groups.hs) pin this.

### Rendering

| Where | Rows | Empty state |
|-------|------|-------------|
| [`chatListContent`](../../Shared/Views/ChatList/ChatListView.swift#L513) | `rows(shownChatIds:)` with the ids of the shown chats ([L516](../../Shared/Views/ChatList/ChatListView.swift#L515)); after the local chats, [`directoryRows`](../../Shared/Views/ChatList/ChatListView.swift#L483). While `showResults` ([`showsCards`](../../Shared/Views/ChatList/ChatListView.swift#L516) false), the list leaves out `OneHandUICard`, the banner slot and `ConnectBannerCard`; they return when the search text is cleared, which resets the model | `noChatsView()` overlay only when no chat is shown, the model is not `searching` and `rows.isEmpty` ([L630](../../Shared/Views/ChatList/ChatListView.swift#L629)) |
| Onboarding branch of [`chatList`](../../Shared/Views/ChatList/ChatListView.swift#L435) | While `showResults`, a list of the same rows with `shownChatIds: []` ([L451](../../Shared/Views/ChatList/ChatListView.swift#L451)) in place of the onboarding pages | A centred "No chats found" row when not searching and `rows.isEmpty` |

`directoryRows` draws one list without headers, in this order: your chats (`rows.yourChats`), the results, then the footer row. So the load or retry row is the last row of the results: at the bottom with the top bar, and at the top with the one-hand UI, the list being flipped, which puts it farthest from the field. Every row carries the per-row flip and a clear background.

| Row | View | Tap |
|-----|------|-----|
| Your chat | Each loaded chat of `rows.yourChats`, drawn with the chat list's own row, [`chatListNavLink(_:)`](../../Shared/Views/ChatList/ChatListView.swift#L637) (`ChatListNavLink` with the local chats' modifiers) | as a local chat: opens it |
| Result | [`DirectorySearchRow`](../../Shared/Views/ChatList/DirectorySearchView.swift#L185) with `imageSize: dynamicSize(userFont).profileImageSize`, the chat rows' avatar size (63 pt at the default text size): avatar, `#` + name, the directory's SimpleX name claim as plain text, description (2 lines), then the caption: the member or subscriber count, or "Already joining" for `.joining` (secondary colour); trailing "Open" (primary colour, one line, fixed size), except for `.joining` | [`openResult(_:)`](../../Shared/Views/ChatList/ChatListView.swift#L503): `openDirectoryResult(result, dismiss: false)`, `cleanup`: `refreshStates()` (see Opening a result) |
| Load more | [`DirectoryLoadMoreRow`](../../Shared/Views/ChatList/DirectorySearchView.swift#L247) via [`directoryLoadMoreRow()`](../../Shared/Views/ChatList/ChatListView.swift#L655): `plus.magnifyingglass` and "Load more · %d left" when `moreCount` is above 0, else "Load more"; disabled and in the secondary colour while `loading` | `loadMore()`, through `confirmDirectorySearch` |
| Retry | [`DirectoryRetryRow`](../../Shared/Views/ChatList/DirectorySearchView.swift#L273) via [`directoryRetryRow()`](../../Shared/Views/ChatList/ChatListView.swift#L661): `arrow.clockwise` and "Search failed, tap to retry" | `retry()`, through `confirmDirectorySearch` |

On iOS 16 and later a long press on a result shows a context menu, [`DirectoryResultMenu`](../../Shared/Views/ChatList/DirectorySearchView.swift#L290), with one action, Open (`message` icon), which does what a tap does. Its preview is the row without its Open, padded, the screen width less 32 pt, on the theme background. The modifier is applied before the row's flip, so in the one-hand UI the preview is drawn upright (checked in the iOS simulator tests). iOS 15 has no menu; a tap still opens.

A result tap first sets `searchFocussed = false`. Text from the directory is rendered with `Text(String)` (never parsed as markdown) with line limits.

### Opening a result

[`openDirectoryResult(_:theme:dismiss:cleanup:)`](../../Shared/Views/ChatList/DirectorySearchView.swift#L316) calls [`planAndConnect`](../../Shared/Views/NewChat/NewChatView.swift#L1342) with the result's link and `prepareGroup: .open`, or `.confirm` (the default) for `.joining`. `planAndConnect` makes the online plan, with the spinner in the field. [`PrepareGroupMode`](../../Shared/Views/NewChat/NewChatView.swift#L1337) is read only in the `.groupLink(.ok)` branch with link data ([L1551](../../Shared/Views/NewChat/NewChatView.swift#L1551)): `.confirm` shows the "Open channel?" / "Open group?" alert ([`showPrepareGroupAlert`](../../Shared/Views/NewChat/NewChatView.swift#L1167)), as for a pasted link, and `.open` calls [`prepareAndOpenGroup`](../../Shared/Views/NewChat/NewChatView.swift#L1217) at once, the function the alert's confirm also calls: `apiPrepareGroup`, the channel's relay hostnames, `addChat`, then `openKnownChat`. `.open` applies only when the plan has no `ownerVerification` (verified or failed) and no other name (`connectOtherLink`); with either, the branch shows the alert as `.confirm` does, because only the alert shows the signature line and the other name's button. Open plans a link, not a name, without a signature, so its plan has neither, and Open goes straight to the prepared chat with its Join button. Every other plan shows what a pasted link shows: the "Open group" alert for a known group, the repeat-join sheet or the connecting alert for a pending join, and the no-relays and update-required alerts. A `.joining` result keeps the default mode, so whatever its online plan turns out to be, it shows what a pasted link would.

The chat banner of a prepared channel (`nextConnectPrepared && useRelays`) shows its subscriber count, `subscriberCountStr(groupSummary.publicMemberCount)` from the link data, after the full name ([ChatView.swift L1030](../../Shared/Views/Chat/ChatView.swift#L1030)), in `.subheadline` and the secondary colour.

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
| Directory response and online result types | [`DirectorySearch.swift`](../../Shared/Model/DirectorySearch.swift) | `DirectorySearchEntry` | [32](../../Shared/Model/DirectorySearch.swift#L32) |
