# Chat List Specification

Source: `common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt`, `common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt`, `common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt`

---

## Table of Contents

1. [Overview](#1-overview)
2. [ChatListView Composable](#2-chatlistview-composable)
3. [Data Sources](#3-data-sources)
4. [Filter System](#4-filter-system)
5. [Chat Preview](#5-chat-preview)
6. [ChatListNavLinkView](#6-chatlistnavlinkview)
7. [Tag System](#7-tag-system)
8. [UserPicker](#8-userpicker)
9. [Source Files](#9-source-files)
10. [Crowdfunding Banner](#10-crowdfunding-banner)
11. [Online Search](#11-online-search)

---

## Executive Summary

The Chat List is the landing screen of SimpleX Chat, rendering all conversations for the active user. Built around `ChatListView` (line 218 in `ChatListView.kt`), it provides a searchable, filterable `LazyColumn` of chat previews with a toolbar, tag-based filtering, and a user-switching side panel. The view adapts between one-hand UI mode (toolbar at bottom, reversed list) and standard mode (toolbar at top). Search also accepts SimpleX links for direct connection.

---

## 1. Overview

```
ChatListView
|-- ChatListToolbar               (top or bottom app bar)
|   |-- UserProfileButton         (opens UserPicker)
|   |-- Title ("Your chats")
|   |-- SubscriptionStatusIndicator
|   +-- NewChatButton / StoppedIndicator
|-- ChatListWithLoadingScreen
|   |-- ChatList (LazyColumnWithScrollBar)
|   |   |-- Spacer (top/bottom padding)
|   |   |-- stickyHeader
|   |   |   |-- ChatListSearchBar (search input + filter toggle)
|   |   |   +-- TagsView          (preset + custom tag chips)
|   |   |-- ChatListNavLinkView[] (per-chat row items)
|   |   +-- ChatListFeatureCards  (one-hand UI card, address card)
|   +-- EmptyState text
|-- NewChatSheetFloatingButton    (FAB, standard mode only)
|-- UserPicker                    (slide-in panel, Android)
+-- ActiveCallInteractiveArea     (desktop, in-call banner)
```

---

<a id="ChatListView"></a>

## 2. ChatListView Composable

**Location:** [`ChatListView.kt#L218`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L218)

```kotlin
fun ChatListView(
  chatModel: ChatModel,
  userPickerState: MutableStateFlow<AnimatedViewState>,
  setPerformLA: (Boolean) -> Unit,
  stopped: Boolean
)
```

### Initialization

- Shows "What's New" modal on first launch after update (line ~224), with a 1-second delay.
- On desktop, closing a chat resets audio/video players (line ~232).

### Layout Modes

The `oneHandUI` preference (`appPrefs.oneHandUI.state`) controls the layout:

| Mode | Toolbar Position | List Direction | FAB | Search/Tags Position |
|---|---|---|---|---|
| **Standard** (`oneHandUI = false`) | Top | Top-to-bottom | Bottom-right FAB | Below toolbar |
| **One-hand** (`oneHandUI = true`) | Bottom | Bottom-to-top (reversed) | Integrated in toolbar | Above toolbar |

### State

| State | Type | Purpose |
|---|---|---|
| `searchText` | `MutableState<TextFieldValue>` | Search query (saved across recomposition) |
| `listState` | `LazyListState` | Scroll position (persisted in `lazyListState` var) |
| `oneHandUI` | `State<Boolean>` | One-hand UI mode toggle |

### Android-specific

- `SetNotificationsModeAdditions`: Notification permission setup (line ~286).
- `UserPicker`: Overlay side panel for user switching (line ~290).

---

## 3. Data Sources

| Source | Location | Description |
|---|---|---|
| `chatModel.chats` | `ChatModel.chatsContext.chats` | Full list of `Chat` objects for the active user |
| `chatModel.activeChatTagFilter` | `ChatModel.activeChatTagFilter` | Currently active filter (`PresetTag`, `UserTag`, or `Unread`) |
| `chatModel.userTags` | `ChatModel.userTags` | User-created custom tags |
| `chatModel.presetTags` | `ChatModel.presetTags` | Map of `PresetTagKind` to count |
| `chatModel.unreadTags` | `ChatModel.unreadTags` | Map of tag ID to unread count |
| `chatModel.chatId` | `ChatModel.chatId` | Currently selected chat ID (highlights row) |
| `chatModel.currentUser` | `ChatModel.currentUser` | Active user profile |
| `chatModel.users` | `ChatModel.users` | All user profiles (for UserPicker) |
| `chatModel.showChatPreviews` | `ChatModel.showChatPreviews` | Privacy toggle for message previews |

---

## 4. Filter System

### Active Filter Types

Defined as sealed class `ActiveFilter` (line ~63):

```kotlin
sealed class ActiveFilter {
  data class PresetTag(val tag: PresetTagKind) : ActiveFilter()
  data class UserTag(val tag: ChatTag) : ActiveFilter()
  data object Unread : ActiveFilter()
}
```

### PresetTagKind Enum

| Value | Description |
|---|---|
| `GROUP_REPORTS` | Groups with active reports (moderator-visible) |
| `FAVORITES` | Chats marked as favorite |
| `CONTACTS` | Direct (1:1) chats |
| `GROUPS` | Group chats |
| `BUSINESS` | Business-type chats |
| `NOTES` | Local note folders |

### Search Filtering

The `filteredChats` function (line ~1689) applies filters in this order:

1. **SimpleX link match:** If a pasted link resolved to a known contact/group, show only that chat.
2. **Text search:** Case-insensitive match against `chat.chatInfo.chatViewName`, `chat.chatInfo.fullName`, and `chat.chatInfo.localAlias`.
3. **Active filter:**
   - `PresetTag`: Matches chat type and characteristics (e.g., `CONTACTS` filters `ChatInfo.Direct`, `GROUPS` filters `ChatInfo.Group`).
   - `UserTag`: Matches chats whose `chatTags` contain the tag ID.
   - `Unread`: Matches chats with `unreadCount > 0` or `unreadChat == true`.

### Search Bar

`ChatListSearchBar` (line ~866) provides:
- Text input with search icon.
- SimpleX link detection: When a pasted string contains a single SimpleX link, it triggers `planAndConnect` for connection, suppressing normal search.
- Unread filter toggle button (right side, when search is empty).
- Searching SimpleX Directory and resolving SimpleX names online happen only on request, below the local results: see [Online Search](#11-online-search).

---

<a id="ChatPreviewView"></a>

## 5. Chat Preview

**Location:** [`ChatPreviewView.kt#L41`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatPreviewView.kt#L41)

```kotlin
fun ChatPreviewView(
  chat: Chat,
  showChatPreviews: Boolean,
  chatModelDraft: ComposeState?,
  chatModelDraftChatId: ChatId?,
  currentUserProfileDisplayName: String?,
  disabled: Boolean,
  linkMode: SimplexLinkMode,
  inProgress: Boolean,
  progressByTimeout: Boolean,
  defaultClickAction: () -> Unit
)
```

### Layout

Each chat preview row contains:

| Element | Position | Content |
|---|---|---|
| Profile image | Left | `ChatInfoImage` with overlay icons for inactive contacts/groups |
| Title row | Top-right of image | Chat name (bold), verified shield (direct), timestamp |
| Preview row | Below title | Last message preview or draft indicator, unread badge |
| Unread badge | Right | Circular badge with count, or dot for muted chats |

### Draft Display

When `chatModelDraftChatId` matches the chat ID, the preview shows a draft indicator (pencil icon) with the draft message text instead of the last chat item.

### Inactive Indicators

- Inactive contacts: cancel icon overlay on profile image.
- Left/removed/deleted groups: cancel icon overlay.

---

<a id="ChatListNavLinkView"></a>

## 6. ChatListNavLinkView

**Location:** [`ChatListNavLinkView.kt#L37`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListNavLinkView.kt#L37)

Routes each chat to the appropriate click action and context menu based on `chat.chatInfo`:

| ChatInfo Type | Click Action | Context Menu |
|---|---|---|
| `ChatInfo.Direct` | `directChatAction` (opens chat) | `ContactMenuItems`: mark read/unread, mute, favorite, tag, clear, delete |
| `ChatInfo.Group` | `groupChatAction` (opens chat or joins) | `GroupMenuItems`: mark read/unread, mute, favorite, tag, clear, leave, delete |
| `ChatInfo.Local` | `noteFolderChatAction` (opens notes) | `NoteFolderMenuItems`: mark read, clear, delete |
| `ChatInfo.ContactRequest` | `contactRequestAlertDialog` (accept/reject) | `ContactRequestMenuItems`: reject |
| `ChatInfo.ContactConnection` | Sets `chatModel.chatId` (opens connection info) | `ContactConnectionMenuItems`: delete |
| `ChatInfo.InvalidJSON` | Sets `chatModel.chatId` | No menu |

### Selection Highlight

On desktop, the currently selected chat (`chatModel.chatId.value == chat.id`) receives a highlight background. `nextChatSelected` state is used to suppress the bottom divider when the next chat in the list is selected.

---

## 7. Tag System

### TagsView

**Location:** [`ChatListView.kt#L1451`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L1459)

Renders a horizontally scrollable row of tag chips (via `TagsRow`, which is a platform-specific `expect` composable).

Layout logic:
- If there are more than 1 collapsible preset tags and the total tag count exceeds 3, preset tags collapse into a `CollapsedTagsFilterView` dropdown.
- Otherwise, each preset tag renders as an `ExpandedTagFilterView` chip.
- User tags render as individual chips with emoji or label icon, bold when active.
- A "+" button at the end opens `TagListEditor` for creating new tags.

### Tag Interactions

- **Single tap:** Toggles the tag filter on `chatModel.activeChatTagFilter`.
- **Long press / right-click (user tags):** Opens dropdown menu with edit/delete/reorder options.
- **Unread dot:** Shown on tags that have chats with unread messages.

<a id="TagListView"></a>

### TagListView

**Location:** [`TagListView.kt#L47`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/TagListView.kt#L47)

Full-screen tag management view opened from the "+" button or long-press menu.

```kotlin
fun TagListView(rhId: Long?, chat: Chat? = null, close: () -> Unit, reorderMode: Boolean)
```

- Displays all user tags in a `LazyColumnWithScrollBar`.
- Supports drag-and-drop reordering via `rememberDragDropState` (calls `apiReorderChatTags`).
- Each tag row shows emoji/icon, name, chat count, and a checkbox if opened for a specific chat (to assign/unassign tags).
- "Create list" button opens `TagListEditor` modal.

---

<a id="UserPicker"></a>

## 8. UserPicker

**Location:** [`UserPicker.kt#L46`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/UserPicker.kt#L46)

```kotlin
fun UserPicker(
  chatModel: ChatModel,
  userPickerState: MutableStateFlow<AnimatedViewState>,
  setPerformLA: (Boolean) -> Unit
)
```

### Behavior

- **Android:** Renders as a slide-up overlay panel on the chat list, triggered by tapping the user profile button in the toolbar.
- **Desktop:** Rendered inline in the left column of `DesktopScreen`, always accessible.
- Closes automatically when any `ModalManager.start` modal opens.
- On Desktop, when there is no active user and no mobile is in use (`desktopNoUserNoRemote`), the SimpleX address and Chat preferences rows and the divider below them are hidden.
- The SimpleX address row opens `UserAddressView` in `ModalManager.start`. With no active user (`currentUser == null`) the modal draws nothing and closes itself when it is the top modal (`ModalManager.start.isLastModal`); if another screen covers it, it closes when the user goes back to it.
- The Chat preferences row closes its modal the same way when there is no active user.

### Content

| Section | Content |
|---|---|
| **Active user** | Profile image, display name, "active" indicator |
| **Other users** | List of non-hidden user profiles sorted by `activeOrder`; tapping switches user |
| **Remote hosts** | Connected remote devices (desktop linking) |
| **Settings** | Opens `SettingsView` modal |
| **Color mode** | `ColorModeSwitcher` for theme toggle |
| **Add profile** | Opens `CreateProfile` flow |
| **Lock** | Locks app (calls `AppLock.setPerformLA`) |

### State Machine

Uses `AnimatedViewState` (`GONE`, `VISIBLE`, `HIDING`) with a `MutableStateFlow` to coordinate animation between the parent screen and the picker overlay.

---

## 9. Source Files

| File | Description |
|---|---|
| `ChatListView.kt` | Main chat list view, toolbar, search, tags, filtering |
| `ChatListNavLinkView.kt` | Per-chat row routing and context menus |
| `ChatPreviewView.kt` | Chat preview row layout (image, title, last message) |
| `ChatHelpView.kt` | Empty-state help content |
| `ContactConnectionView.kt` | Pending connection preview row |
| `ContactRequestView.kt` | Contact request preview row |
| `ServersSummaryView.kt` | Server connection status summary |
| `ShareListNavLinkView.kt` | Share target list row (forwarding) |
| `ShareListView.kt` | Share target list (forwarding flow) |
| `TagListView.kt` | Tag management and assignment view |
| `UserPicker.kt` | User switching side panel |
| `GetStakeBanner.kt` | Crowdfunding banner and the shared banner card chrome |
| `DirectorySearchView.kt` | Online search model, Names and Directory rows, first-search alert |
| `model/DirectorySearch.kt` | Directory response parsing and online result types |

---

<a id="GetStakeBanner"></a>

## 10. Crowdfunding Banner

**Location:** [`GetStakeBanner.kt#L35`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/GetStakeBanner.kt#L35)

```kotlin
fun GetStakeBanner(showDismiss: Boolean, onTap: () -> Unit, onDismiss: () -> Unit)
```

Gradient card inviting the user to invest on Wefunder. Shown only when `crowdfundingAvailable()` — always outside Play Store builds, and in Play builds only while the store country is the US or not yet known — and only while `getStakeBannerDismissed` is false.

### Placement

| Where | Condition | Layout |
|-------|-----------|--------|
| Chat list | in `ChatList`'s `LazyColumn`, after `ToggleChatListCard` and before the chats | `Box(Modifier.zIndex(1f).padding(16.dp))` |
| Onboarding | inside `ConnectOnboardingView` (`views/newchat/OnboardingCards.kt`), below the pager, so it shares the pages' width limit on desktop and their dimming while a start modal is open; opens the page in `ModalManager.center` on desktop, `ModalManager.start` on Android | `padding(start/end = DEFAULT_PADDING, bottom = 8.dp)`, in a `Column` where the pager takes `weight(1f)` |

The list has a single banner slot, filled by an `if`/`else if` chain in priority order: the support-ended alert (`supportEnded()`), the renewal-failure alert (`badgeIssueFailed()`), the pitch, then the Wefunder banner. Each banner's `item` records itself in `ChatModel.chatListBanner` (`BadgeExpired`, `BadgeIssueFailed`, `BadgePitch`, `GetStake`) in a `SideEffect`, and the pitch and Wefunder conditions start with `chatModel.bannerSlotFree(banner)` — true only while nothing else was shown this app session — so dismissing a banner never puts another in its place until restart. The alerts have no such check: an alert takes the slot whenever present, and once shown it holds it. The pitch also requires `noShownBadge()` (`views/newchat/OnboardingCards.kt`), false until `BadgeModel.isCurrent` for the current user, so it cannot take the slot from a supporter whose badge loads a moment later. `ConnectOnboardingView` applies the same `bannerSlotFree` check and records `GetStake`. While online search results are shown (`showsCards` is `!onlineSearch.showResults`, false after "Search in Directory" or the Search key) the slot stays empty, and `ToggleChatListCard` and `ChatListFeatureCards` are left out too; typing alone, which only filters the chats, keeps them, and they return when the search is cleared or cancelled before showing anything.

`crowdfundingAvailable()` launches an effect to load the store country, so both call sites read it in the composable body rather than inside the `LazyColumn` builder.

### Dismissal

| Preference | Set by | Effect |
|------------|--------|--------|
| `getStakeBannerTapped` | `openGetStake()` | the dismiss X appears from then on, while there are chats |
| `getStakeBannerDismissed` | the dismiss X | hides the banner in both placements |
| `supporterBannerTapped` | tapping the supporter pitch | the pitch's dismiss X appears from then on |
| `supporterBannerShown` | the pitch's dismiss X, through its "You can support SimpleX later in Settings." alert, and a successful code redemption | hides the pitch |

The two badge alert banners always offer the X; only the pitch waits to be tapped once, so a user who has not looked at it cannot dismiss it unseen.

Both are in `AppPreferences.hintPreferences`, so "Reset all hints" restores the banner. The X is never offered below the onboarding cards, so the banner cannot be dismissed before the user has a chat.

Tapping the card runs `openGetStake()`, which opens `GetStakeView(showFirstImage = true)` as a card modal — `showFirstImage` decides whether the page repeats the first slide's image, which only What's New shows above its own link.

### Shared card chrome

`Modifier.bannerCard(onTap)` (size state, gradient brush, `heightIn`, `clip`, `background`, `clickable`) and `BannerDismissButton` are declared separately from `GetStakeBanner` so other banners can adopt the same chrome; the paddings stay with the caller. The gradient reuses `gradientPoints`, `lightStops` and `darkStops` from `views/newchat/OnboardingCards.kt`.

---

<a id="OnlineSearch"></a>

## 11. Online Search

**Files:** `views/chatlist/DirectorySearchView.kt` (model and rows), `model/DirectorySearch.kt` (response and result types), `views/chatlist/ChatListView.kt` (triggers and rendering), `views/helpers/SearchTextField.kt` (Search key)

The search text can be sent online on request: to SimpleX Directory, to find public groups and channels, and, when the text is a SimpleX name, to the user's name-resolving server. Results are shown below the local chats in two parts, Names then Directory. They are never saved and live only as long as the search text and the active profile: [`ChatListSearchBar`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L895-L999) calls `reset()` from a `KeyChangeEffect` on `searchText.value.text` and another on `chatModel.currentUser.value?.userId` ([source](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L940-L945)). The model is a plain `remember` in [`ChatListView`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L246), so an Android rotation (which recreates the Activity) drops the results while the `rememberSaveable` search text stays. The New chat sheet uses the same model and rows (see [Navigation, New Chat Online Search](navigation.md#10-new-chat-online-search)).

### Triggers

| Trigger | Handler | Runs |
|---------|---------|------|
| Search key: the keyboard's Search action on Android, Enter (key up) on desktop, through [`SearchTextField`](../../common/src/commonMain/kotlin/chat/simplex/common/views/helpers/SearchTextField.kt#L33-L43)`(onSearch)` | [`runOnlineSearch()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L1024-L1029) | `search(text, name = connectNameCandidate.value)`: the directory's first page, and name resolution when the text is a name candidate |
| "Search in Directory" row ([`SearchInDirectoryRow`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L233-L248)) | [`runDirectorySearch()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L1017-L1022) | `searchDirectory(text)`: the directory only |

`SearchTextField` handles the key per platform: on Android through `KeyboardActions(onSearch)` ([source](../../common/src/commonMain/kotlin/chat/simplex/common/views/helpers/SearchTextField.kt#L116-L121)), on desktop through `onPreviewKeyEvent` for `Key.Enter` / `Key.NumPadEnter` on `KeyUp` ([source](../../common/src/commonMain/kotlin/chat/simplex/common/views/helpers/SearchTextField.kt#L95-L102)), so one key press runs the search once. Every caller keeps `ImeAction.Search`; a caller without `onSearch` gets no handler.

Both triggers do nothing for empty (trimmed) text or a pasted SimpleX link ([`searchTrimmed`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L1014-L1015), `searchShowingSimplexLink`), and both go through [`confirmDirectorySearch()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L369-L375), which runs the search at once when `appPrefs.directorySearchAlertShown` is set and otherwise shows the three-button alert ([`showDirectorySearchAlert`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L377-L405), `showAlertDialogButtonsColumn` with `parseHtml = false` so the paragraph break shows): Search / Search and don't show again / Cancel. Its message names both recipients: the directory, and, for a SimpleX name, the server that resolves names. The preference is in `hintPreferences`, so "Reset all hints" brings the alert back. Each trigger launches the model method with `withApi`.

The rows next to the search field share one slot with the tags, laid out by [`TagsOrConnectByName`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L1328-L1382) and [`ChatListSearchBarWithTags`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L1249-L1274):

| Search text | Android, one-hand UI (slot above the field) | Android, top bar (slot below the field) | Desktop (either mode) |
|-------------|-----------------------------------|------------------------------|------|
| Empty | Tags | Tags | Tags |
| Plain text | "Search in Directory" | Tags, then "Search in Directory" | Tags and "Search in Directory" |
| Name candidate, no name results yet | "Connect to …" and "Search in Directory" | "Connect to …" (in the tags' place), then "Search in Directory" | Tags, "Connect to …" and "Search in Directory" |
| Name candidate with name results | "Search in Directory" | Tags, then "Search in Directory" | Tags and "Search in Directory" |
| Pasted SimpleX link | Tags | Tags | Tags |

On desktop the rows are passed to the caller's `desktopView`, which places them between the tags and the field: above the field with the bottom toolbar, below it with the top toolbar.

<a id="OnlineSearchModel"></a>

### OnlineSearchModel

**Location:** [`OnlineSearchModel`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L32-L230)

A plain class holding Compose state, owned by `ChatListView` as `remember { OnlineSearchModel() }` and passed through `ChatListWithLoadingScreen` to `ChatList`, `ChatListSearchBarWithTags`, `ChatListSearchBar` and `TagsOrConnectByName`. Every method runs on the main thread: the triggers and rows launch them with `withApi` (`Dispatchers.Main`), and the spinner's `onCancel` hops back with `withApi { cancelRequest() }`; the API calls move the FFI work to `Dispatchers.IO` themselves. This keeps every read and write of `generation` on one thread.

| State | Description |
|-------|-------------|
| `names: List<NameSearchResult>` | Name results for the current text |
| `directory: List<DirectoryResult>` | Directory entries shown so far, each with the local chat its link refers to |
| `loading` | A directory request is in flight |
| `resolvingNames` | Name resolution is in flight |
| `failed` | The last directory request failed |
| `searched` | A directory request for the current text completed, successfully or not (tells "not searched" from "found nothing") |
| `cursor` (private) | Opaque `JsonObject?` from the last page, echoed back for the next one; null when there are no more. Snapshot state, because `footer` reads it |
| `lastPageIds` (private) | The ids of the results the last successful request added (none when every entry of the page was already shown); cleared with the directory state, kept by a failed request. Snapshot state, because `rows` reads it |
| `generation` (private) | Bumped by `reset()` and `cancelRequest()`; a reply whose captured generation differs never writes model state |

The public state has private setters; `searchedText`, `generation` and `spinnerShown` are plain fields.

| Computed | Definition |
|----------|------------|
| [`searching`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L58) | `loading \|\| resolvingNames` |
| [`showResults`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L59) | `searching \|\| searched \|\| names.isNotEmpty()` |
| [`footer`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L60) | `DirectoryFooter.Retry` when `failed`, else `ShowMore` when a cursor exists, else null |

| Method | Description |
|--------|-------------|
| [`reset()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L62-L73) | Clears all results and flags, bumps `generation` (text change, profile switch) |
| [`cancelRequest()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L76-L80) | The spinner's `onCancel`: bumps `generation` and clears `loading` and `resolvingNames`; results already shown stay |
| [`search(text, name)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L82-L99) | Search key: ignored for empty text or while `searching` for the same text; otherwise `reset()`, then the first directory page and, with a name, name resolution concurrently (`coroutineScope { async { resolveNames(name, gen) } }`, with the generation captured before the child coroutine starts) |
| [`searchDirectory(text)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L101-L115) | The row: ignored while `loading` for the same text; for the same text it clears only the directory state (name results stay), for new text it resets; then the first page |
| [`loadMore()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L117-L120) | "Show more": the next page, appended |
| [`retry()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L123-L129) | The retry row: resumes the page when a cursor exists, else repeats the first page; no reset |
| [`rows(shownChatIds)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L131-L148) | The rows to render, given the ids of the local chats shown (see De-duplication) |
| [`refreshLocalChatIds()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L150-L165) | The `cleanup` of a connection started from a result row, run when its flow ends: looks the local chat up again, store-only, for every row that has none (a name row by its target, a directory row by its link), unless the generation changed. A chat opened from a result is created during that flow, so its row is then hidden like any shown chat, or drawn as a chat row when the list doesn't show it. Name rows are copied with `withLocalChatId`, as `NameSearchResult` keeps its constructor private |

**Spinner.** The private setters of `loading` and `resolvingNames` call [`updateSpinner()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L216-L229) on every write, which starts the "Searching directory…" progress with owner `ConnectProgressOwner.DirectorySearch` when `searching` becomes true and stops it when it becomes false (`spinnerShown` keeps the last state, so repeated writes do nothing). The chat list shows it in the toolbar ([`ChatListToolbar`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L668-L674)) once it has run for a second, not in the search field. The spinner is shared with connections: [`ConnectProgressOwner`](../../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L50) records whose it is, [`startConnectProgress`](../../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L60-L69) does not let a search take it from a running connection, and [`stopConnectProgress`](../../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L72-L78) ignores a stop from another owner. Tapping a result calls [`planAndConnect`](../../common/src/commonMain/kotlin/chat/simplex/common/views/newchat/ConnectPlan.kt#L24-L55), whose `cancelConnectProgress()` ([source](../../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L81-L88)) cancels the search through its `onCancel` before the connection takes the spinner. Clearing the search text does the same through the typing path.

### Directory requests

[`request(append)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L190-L214) reads `chatModel.remoteHostId()` and calls [`apiSearchDirectory`](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1645-L1652), which sends `APISendServiceRequest` to [`DIRECTORY_SERVICE_LINK`](../../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L16) with a [10-second timeout](../../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L19) and the request built by [`directorySearchRequest`](../../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L71-L75) (`searchText`, and `searchCursor` for later pages). [`parseDirectorySearchResponse`](../../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L79-L88) accepts only a `searchResults` response and decodes its entries one at a time inside `runCatching`, so an entry the app cannot decode is dropped and the rest of the page is kept.

[`DirectorySearchEntry`](../../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L33-L58) keeps the wire field `groupLink` private; its [`link`](../../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L47-L57) is a `@Transient` property whose initializer, run by the generated decoder, takes `groupLink.connShortLink ?: groupLink.connFullLink` and throws `SerializationException` unless [`simplexLinkType`](../../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L60-L63) classifies it as `SimplexLinkType.group` or `SimplexLinkType.channel`. Being `@Transient`, `link` cannot be set by a `link` key in the JSON. `simplexLinkType` returns the link type only when [`strConnectTarget`](../../common/src/commonMain/kotlin/chat/simplex/common/views/newchat/NewChatView.kt#L834) finds exactly one SimpleX link whose text is the whole string. `link` is also the entry's `id`. The directory sends only the short link when an entry has one.

For each new entry of a page (not already shown, not repeated in the page), the request looks up the local chat its link refers to with `apiConnectPlan(rh, entry.link, resolveMode = PRMNever, inProgress = mutableStateOf(false), onFailure = ConnectPlanFailure.Log)` and [`planLocalChatId`](../../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L90-L110), building a [`DirectoryResult`](../../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L176-L181). The link-type check makes this lookup store-only: the core plans contact-mode links under `PRMNever` from the store, but would fetch an invitation link from its server. The page is published, appended or replacing, only if the generation did not change.

### Name resolution

[`resolveNames(name, gen)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L172-L188) resolves `[name]` for a candidate starting with `@` or `#`, else `["@$name", "#$name"]` (the candidate comes from [`nameSearchCandidate`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L1291-L1302), as on the typing path). Each target is planned with `apiConnectPlan(rh, target, resolveMode = PRMUnknown, inProgress = mutableStateOf(false), onFailure = ConnectPlanFailure.Log)`; it stops before a target once the generation changed, so a cancelled or replaced search sends no further names. [`NameSearchResult(target, result)`](../../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L112-L174) (a private constructor behind a companion `operator fun invoke` that returns null for no row) maps the plan with one exhaustive `when`: a contact address or group link gives a row (profile from the link data or the known chat), the user's own contact address, an invitation link and an error give none. `simplexName` is the plan's `planSimplexName?.shortStr`, `localChatId` comes from `planLocalChatId`, `iconName` is an `ImageResource`, and for a group link `resolvedLink` is the link the core resolved (`connLink.connShortLink`). Failures are logged at debug level and never shown as alerts; the false `inProgress` also keeps `sendCmdWithRetry` from showing its network retry alert.

### De-duplication

[`rows(shownChatIds)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L131-L148) returns an [`OnlineSearchRows`](../../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L183-L190) at composition time, so pages added by "Show more" are filtered too:

- a name result is hidden when its `localChatId` is among the shown local chats;
- a directory result is hidden when its `localChatId` is among the shown local chats or any name result's `localChatId`, or when its `entry.link` equals any name result's `resolvedLink` (all name results, shown or not);
- `pageInChats` is true when the last page's results (`lastPageIds`) are not empty and each one's `localChatId` is among the shown local chats or the name results' chat ids. A result hidden only because a name result resolved to its link is not a chat the user has, so it does not count, and the part keeps its usual look;
- `footer` passes through.

Only store-derived chat ids and the link the core resolved can hide a row, never a field the directory or a name's owner wrote. A mismatch in link encoding can only produce a duplicate row.

A directory result that is a chat the user has but that the screen doesn't list (the local filter doesn't match it) stays in `rows.directory` and is drawn as a chat row (see Rendering). When its chat id is not among the loaded chats (`chatModel.getChat` returns null), the Directory row is drawn as before, and tapping it goes through `planAndConnect`, which opens the known chat.

[GAP] A regular group the user joined through a different link, through the directory's full link, or by invitation, is not found by the store lookup and still appears as a Directory row with "Join group"; tapping it goes through `planAndConnect`. See [GAP-09](../../product/gaps.md#gap-09-online-search-groups-joined-through-another-link-are-not-recognised).

### Rendering

| Where | Rows | Empty state |
|-------|------|-------------|
| [`ChatList`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L1074-L1247) | `rows(shownChatIds)` with the ids of the shown chats ([source](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L1117)); after the local chats and before `ChatListFeatureCards`, [`nameRows`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L517-L532) then [`directoryRows`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L534-L562) | `NoChatsView` overlay only when no chat is shown, the model is not `searching` and `rows.isEmpty` ([source](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L1225-L1230)); the `you_have_no_chats` overlay of `ChatListWithLoadingScreen` (no chats at all) is hidden while `showResults` |
| Onboarding branch of [`ChatListWithLoadingScreen`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L459-L506) | A `Column` with `ChatListSearchBarWithTags` first (top bar) or last (one-hand UI), and between them a `Box(Modifier.weight(1f))` holding, while `showResults`, a `LazyColumnWithScrollBarNoAppBar` of the same parts with `shownChatIds = emptySet()` (reversed in one-hand mode, `imePadding()` in top-bar mode), otherwise `ConnectOnboardingView()` on Android | A centred "No chats found" item when not searching and `rows.isEmpty` |

The onboarding `Column` takes the toolbar and navigation-bar paddings and consumes the navigation-bar insets, so the results list's `imePadding()` does not count them twice. On desktop the onboarding pages are drawn in the centre pane, so the chat list pane shows only the search bar and the results. When a joined result adds a chat and the onboarding state ends, `ChatList` enters composition and its `LaunchedEffect(activeFilter.value)` ([source](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L1244-L1246)) clears the search text, which resets the model; the iOS app keeps the search there.

Each part renders only when it has rows: the Names part when `rows.names` is not empty, the Directory part when `rows.directory` is not empty, `rows.pageInChats` or `rows.footer` is set. `nameRows` and `directoryRows` are `LazyListScope` builders; each part has a header `item` ([`SearchPartHeader`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L575-L583): `body2`, secondary colour, `semantics { heading() }`) rather than a `stickyHeader`, and its rows are `items` keyed by `id`. [`searchPart`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L564-L573) emits the header before the part's rows with the top bar and after them in the one-hand UI, so it is drawn above them in both layouts, the one-hand list being reversed. Its header defaults to `SearchPartHeader`; the New chat sheet passes its own ([Navigation, New Chat Online Search](navigation.md#10-new-chat-online-search)). A lazy list keeps its first visible item in place by key, so in the reversed onboarding list, whose first item is then a keyed row, names arriving after the directory page can end up out of view: that list scrolls to its first item when name results arrive in one-hand mode ([source](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L474-L480)). The `ChatList` branch starts with a keyless spacer, which stays the anchor.

| Row | View | Tap |
|-----|------|-----|
| Name | [`NameSearchRow`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L297-L325): avatar, profile name in bold, the SimpleX name in parentheses (secondary, one `AnnotatedString`, one line), description (2 lines) | `planAndConnect(rhId, result.target, close = null, cleanup = { withApi { refreshLocalChatIds() } })` |
| Directory entry | [`DirectorySearchRow`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L250-L295): avatar, `#` + name, the directory's SimpleX name claim as plain text (truncates first), description (2 lines), member or subscriber count (`body2`, secondary colour), and the verb "Join channel" / "Join group" (one line, never wrapped) | `planAndConnect(rhId, result.entry.link, close = null, cleanup = { withApi { refreshLocalChatIds() } })` |
| Known chat | When `result.localChatId` names a loaded chat (`chatModel.getChat`), in place of the directory entry: the chat list's own row, [`ChatListNavLinkView(chat, remember { mutableStateOf(false) })`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L540) | as a local chat: opens it |
| Note | [`DirectoryPageInChatsRow`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L327-L336), an `item` after the directory rows when `rows.pageInChats` ([source](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L552-L554)): "All results on this page are already in your chats", secondary colour, padded as "Show more" | none |
| Retry | [`DirectoryRetryRow`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L355-L367): "Search failed, tap to retry" | `withApi { retry() }` |
| Show more | [`DirectoryShowMoreRow`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L338-L353): "Show more", disabled (secondary colour) while `loading` | `withApi { loadMore() }` |

A name or directory row tap first hides the keyboard (`hideKeyboard(view)`), then runs `planAndConnect` with `withBGApi`. Text from the directory and from name owners is rendered with `Text(String)` (never parsed as markdown) with line limits.

[GAP] Resolving a SimpleX name with `PRMUnknown` also loads the resolved address's link data from the server that hosts it; the alert names only the directory and the name-resolving server. See [GAP-10](../../product/gaps.md#gap-10-online-search-first-search-alert-does-not-mention-the-server-hosting-a-resolved-address).
