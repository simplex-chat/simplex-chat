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
- Searching SimpleX Directory online happens only on request, below the local results: see [Online Search](#11-online-search).

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

**Location:** [`ChatListView.kt#L1424`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L1424)

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
| `DirectorySearchView.kt` | Online search model, result, load and retry rows, result menu, Open, "Enable Directory search?" alert |
| `model/DirectorySearch.kt` | Directory response parsing and result states |

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

The search text can be sent online on request to SimpleX Directory, to find public groups and channels. Results are shown below the local chats as one list without headers: your chats first, then the other results. They are never saved and live only as long as the search text and the active profile: [`ChatListSearchBar`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L863-L966) calls `reset()` from a `KeyChangeEffect` on `searchText.value.text` and another on `chatModel.currentUser.value?.userId` ([source](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L907-L912)). The model is a plain `remember` in [`ChatListView`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L246), so an Android rotation (which recreates the Activity) drops the results while the `rememberSaveable` search text stays. The New chat sheet uses the same model and rows (see [Navigation, New Chat Online Search](navigation.md#10-new-chat-online-search)).

### Triggers

| Trigger | Handler | Runs |
|---------|---------|------|
| Search key: the keyboard's Search action on Android, Enter (key up) on desktop, through [`SearchTextField`](../../common/src/commonMain/kotlin/chat/simplex/common/views/helpers/SearchTextField.kt#L33-L43)`(onSearch)` | [`runDirectorySearch()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L985-L989) | `searchDirectory(text)` |
| "Search in Directory" row ([`SearchInDirectoryRow`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L186-L201)) | [`runDirectorySearch()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L985-L989) | `searchDirectory(text)` |

`SearchTextField` handles the key per platform: on Android through `KeyboardActions(onSearch)` ([source](../../common/src/commonMain/kotlin/chat/simplex/common/views/helpers/SearchTextField.kt#L116-L121)), on desktop through `onPreviewKeyEvent` for `Key.Enter` / `Key.NumPadEnter` on `KeyUp` ([source](../../common/src/commonMain/kotlin/chat/simplex/common/views/helpers/SearchTextField.kt#L95-L102)), so one key press runs the search once. Every caller keeps `ImeAction.Search`; a caller without `onSearch` gets no handler. The field neither autocorrects nor capitalizes (`KeyboardOptions(capitalization = None, autoCorrect = false)`), so the Search key searches what was typed; being shared, this reaches every `SearchTextField`, unlike iOS, which changed only the chat list and New chat fields.

Both triggers run the same directory search, which never resolves SimpleX names: a name is looked up only by "Connect to …" (`ConnectByNameRow`). Both do nothing for empty (trimmed) text or a pasted SimpleX link ([`searchTrimmed`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L981-L982), `searchShowingSimplexLink`), and both go through [`confirmDirectorySearch()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L326-L332), which runs the search at once when the "Directory search" switch is on (`appPrefs.privacyDirectorySearch`, key "PrivacyDirectorySearch", default false, in Privacy settings › Chats after "Send link previews") and otherwise shows the alert [`showDirectorySearchAlert`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L334-L356) (`showAlertDialogButtonsColumn` with `parseHtml = false`): "Enable Directory search?", with the message "Search text is sent to SimpleX Directory, each search separately and without your profile, so it can't build your search history." and two buttons, Enable (turns the switch on, then searches) and Cancel (sends nothing), in that order, as this alert's column already placed Cancel last. So every search asks while the switch is off. The preference is not in `hintPreferences`: "Reset all hints" leaves the switch as it is. Each trigger launches the model method with `withApi`.

The rows next to the search field share one slot with the tags, laid out by [`TagsOrConnectByName`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L1291-L1347) and [`ChatListSearchBarWithTags`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L1208-L1237). "Search in Directory" hides while results for the current text show or load (`!onlineSearch.showResults`); "Connect to …" stays next to them:

| Search text | Android, one-hand UI (slot above the field) | Android, top bar (slot below the field) | Desktop (either mode) |
|-------------|-----------------------------------|------------------------------|------|
| Empty | Tags | Tags | Tags |
| Plain text | "Search in Directory"; nothing while results show (the tags don't come back) | Tags, then "Search in Directory" until results show | Tags and "Search in Directory"; tags only while results show |
| Name candidate | "Connect to …", and "Search in Directory" until results show | "Connect to …" (in the tags' place), then "Search in Directory" until results show | Tags, "Connect to …", and "Search in Directory" until results show |
| Pasted SimpleX link | Tags | Tags | Tags |

On desktop the rows are passed to the caller's `desktopView`, which places them between the tags and the field: above the field with the bottom toolbar, below it with the top toolbar.

<a id="OnlineSearchModel"></a>

### OnlineSearchModel

**Location:** [`OnlineSearchModel`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L34-L183)

A plain class holding Compose state, owned by `ChatListView` as `remember { OnlineSearchModel() }` and passed through `ChatListWithLoadingScreen` to `ChatList`, `ChatListSearchBarWithTags`, `ChatListSearchBar` and `TagsOrConnectByName`. Every method runs on the main thread: the triggers and rows launch them with `withApi` (`Dispatchers.Main`), and the spinner's `onCancel` hops back with `withApi { cancelRequest() }`; the API calls move the FFI work to `Dispatchers.IO` themselves. This keeps every read and write of `generation` on one thread.

| State | Description |
|-------|-------------|
| `directory: List<DirectoryResult>` | Directory entries shown so far, each with its `DirectoryResultState` (see Directory requests) |
| `moreCount: Int?` | The count of matches left that the last successful page carried; null when the directory sent none or an invalid one |
| `loading` | A directory request is in flight |
| `failed` | The last directory request failed |
| `searched` | A directory request for the current text completed, successfully or not (tells "not searched" from "found nothing") |
| `cursor` (private) | Opaque `JsonObject?` from the last page, echoed back for the next one; null when there are no more. Snapshot state, because `footer` reads it |
| `generation` (private) | Bumped by `reset()` and `cancelRequest()`; a reply whose captured generation differs never writes model state |

The public state has private setters; `searchedText`, `generation` and `spinnerShown` are plain fields.

| Computed | Definition |
|----------|------------|
| [`searching`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L55) | `loading` |
| [`showResults`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L56) | `loading \|\| searched` |
| [`footer`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L57) | `DirectoryFooter.Retry` when `failed`, else `LoadMore` when a cursor exists, else null |

| Method | Description |
|--------|-------------|
| [`reset()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L59-L68) | Clears all results, `moreCount` and flags, bumps `generation` (text change, profile switch) |
| [`cancelRequest()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L71-L74) | The spinner's `onCancel`: bumps `generation` and clears `loading`; results already shown stay |
| [`searchDirectory(text)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L76-L90) | Both triggers: ignored for empty text or while `loading` for the same text; for the same text it clears the results, `moreCount`, cursor and flags, for new text it resets; then the first page |
| [`loadMore()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L92-L95) | The load row: the next page, appended |
| [`retry()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L98-L104) | The retry row: resumes the page when a cursor exists, else repeats the first page; no reset |
| [`rows(shownChatIds)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L106-L123) | The rows to render, given the ids of the local chats shown (see De-duplication) |
| [`refreshStates()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L126-L136) | The `cleanup` of a connection started from a result row, run when its flow ends: plans every result that is not `YourChat` again, store-only, unless the generation changed. A chat opened from a result is created during that flow, so its result then becomes `YourChat`: hidden like any shown chat, or drawn as its chat row when the list doesn't show it |

**Spinner.** The private setter of `loading` calls [`updateSpinner()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L169-L182) on every write, which starts the "Searching directory…" progress with owner `ConnectProgressOwner.DirectorySearch` when `searching` becomes true and stops it when it becomes false (`spinnerShown` keeps the last state, so repeated writes do nothing). The chat list shows it in the toolbar ([`ChatListToolbar`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L607-L613)) once it has run for a second, not in the search field. The spinner is shared with connections: [`ConnectProgressOwner`](../../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L50) records whose it is, [`startConnectProgress`](../../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L60-L69) does not let a search take it from a running connection, and [`stopConnectProgress`](../../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L72-L78) ignores a stop from another owner. Tapping a result calls [`planAndConnect`](../../common/src/commonMain/kotlin/chat/simplex/common/views/newchat/ConnectPlan.kt#L28-L60), whose `cancelConnectProgress()` ([source](../../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L81-L88)) cancels the search through its `onCancel` before the connection takes the spinner. Clearing the search text does the same through the typing path.

### Directory requests

[`request(append)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L143-L167) reads `chatModel.remoteHostId()` and calls [`apiSearchDirectory`](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1677-L1684), which sends `APISendServiceRequest` to [`DIRECTORY_SERVICE_LINK`](../../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L14) with a [10-second timeout](../../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L17) and the request built by [`directorySearchRequest`](../../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L70-L74) (`searchText`, and `searchCursor` for later pages). [`parseDirectorySearchResponse`](../../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L78-L89) accepts only a `searchResults` response and decodes its entries one at a time inside `runCatching`, so an entry the app cannot decode is dropped and the rest of the page is kept.

The directory's response (`DRSearchResults`, apps/simplex-directory-service/src/Directory/Rpc.hs) is `{"type": "searchResults", "entries": […], "searchCursor": …, "moreCount": N, "pageSize": N}`. `searchCursor` is absent when there are no more results. `moreCount` is the number of matches after the cursor, 0 without one. `pageSize` is the directory's page size, which the app does not read: a page is cut to fit the service envelope, so it often carries fewer entries than that. The app keeps `moreCount` only when it is a non-string JSON primitive that reads as a non-negative `Int` ([L85](../../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L85), `intOrNull`, which rejects `5.0` where iOS accepts it); anything else becomes null without failing the response. It is only shown as text in the load row. An older directory sends no count, and the row then reads "Load more".

[GAP] `moreCount` counts the matching listings, including ones without a public link that the directory never sends, so the load row can promise more results than it brings. See [GAP-11](../../product/gaps.md#gap-11-online-search-the-count-of-results-left-can-include-listings-the-directory-never-sends).

[`DirectorySearchEntry`](../../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L31-L56) keeps the wire field `groupLink` private; its [`link`](../../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L45-L55) is a `@Transient` property whose initializer, run by the generated decoder, takes `groupLink.connShortLink ?: groupLink.connFullLink` and throws `SerializationException` unless [`simplexLinkType`](../../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L58-L61) classifies it as `SimplexLinkType.group` or `SimplexLinkType.channel`. Being `@Transient`, `link` cannot be set by a `link` key in the JSON. `simplexLinkType` returns the link type only when [`strConnectTarget`](../../common/src/commonMain/kotlin/chat/simplex/common/views/newchat/NewChatView.kt#L834) finds exactly one SimpleX link whose text is the whole string. `link` is also the entry's `id`. The directory sends only the short link when an entry has one.

For each new entry of a page (not already shown, not repeated in the page), the request plans its link with `apiConnectPlan(rh, entry.link, resolveMode = PRMNever, inProgress = mutableStateOf(false), onFailure = ConnectPlanFailure.Log)` ([`resultState`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L138-L141)) and maps the plan with [`directoryResultState`](../../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L120-L129) to a [`DirectoryResultState`](../../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L113-L117), building a [`DirectoryResult`](../../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L131-L136)`(entry, state)`. The `when` branches in the same order as iOS:

| State | Plan | Drawn as |
|-------|------|----------|
| `YourChat(chatId)` | [`planLocalChatId`](../../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L91-L111) names a chat (a known or prepared group, your own group, or a pending join with its chat) | the chat's own row |
| `Joining` | `GroupLinkPlan.ConnectingConfirmReconnect`, or `ConnectingProhibit` with no `groupInfo_`: a pending join without a chat | a result with "Already joining" |
| `Open` | anything else: `GroupLinkPlan.Ok`, or no plan (`NotResolvedLocally` for a link the store doesn't know, which `apiConnectPlan` returns as null without an alert, or a logged failure) | a result with Open |

The link-type check makes this lookup store-only: the core plans contact-mode links under `PRMNever` from the store, but would fetch an invitation link from its server. The directory sends the short link whenever an entry has one, and a short link reaches a connecting plan from the store only through a pending `/c` join, whose connection stores the full link under the short link, so `Joining` is reachable for a short-link entry with a pending `/c` join and for entries the directory sends with a full link. The page is published with its `moreCount`, appended or replacing, only if the generation did not change.

### De-duplication

[`rows(shownChatIds)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L106-L123) returns an [`OnlineSearchRows`](../../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L138-L144)`(yourChats, results, footer)` at composition time, so pages added by "Load more" are handled too:

- `yourChats`: the chat ids of `YourChat` results whose chat is loaded (`chatModel.getChat`) and not among the shown local chats, in directory order, each id once (two entries with different links can name one group);
- `results`: the other results, in directory order; a `YourChat` result whose chat is not loaded is copied as `Open`, and Open's online plan then finds the known chat;
- `footer` passes through.

So nothing is shown twice: a result that is a chat the list shows is hidden, a chat the list doesn't show (the local filter doesn't match it) is drawn once as its own row, and a link repeated across pages was already dropped by the request. A page whose results are all chats the list shows adds no row and no note. Only store-derived chat ids decide this, never a field the directory wrote. A mismatch in link encoding can only produce a duplicate row.

[GAP] The store-only plan finds a group only when it was prepared from that short link (every short-link join in current apps, Open included), joined with `/c` and that short link (the join connection stores the full link under it), or joined with the full link the directory sends. A group joined otherwise shows as a result with Open beside its own chat row. For a group joined with a full link (an older app) when the directory sends the short link, Open's online plan finds it known and shows the "Open group" alert. For a member added by invitation, or one who joined through a link the owner has since replaced, the online plan is `Ok`, so Open prepares and opens a second copy of the group without asking. The core tests "short link without resolving: prepared and /c-joined groups known, invited member not found" and "short link without resolving: pending /c join is connecting" (tests/ChatTests/Groups.hs) pin this. See [GAP-10](../../product/gaps.md#gap-10-online-search-groups-joined-without-the-listed-short-link-still-appear-as-results-with-open).

### Rendering

| Where | Rows | Empty state |
|-------|------|-------------|
| [`ChatList`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L1035-L1206) | `rows(shownChatIds)` with the ids of the shown chats ([source](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L1077)); after the local chats and before `ChatListFeatureCards`, [`directoryRows`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L510-L523) | `NoChatsView` overlay only when no chat is shown, the model is not `searching` and `rows.isEmpty` ([source](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L1184-L1189)); the `you_have_no_chats` overlay of `ChatListWithLoadingScreen` (no chats at all) is hidden while `showResults` |
| Onboarding branch of [`ChatListWithLoadingScreen`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L459-L499) | A `Column` with `ChatListSearchBarWithTags` first (top bar) or last (one-hand UI), and between them a `Box(Modifier.weight(1f))` holding, while `showResults`, a `LazyColumnWithScrollBarNoAppBar` of the same rows with `shownChatIds = emptySet()` (reversed in one-hand mode, `imePadding()` in top-bar mode), otherwise `ConnectOnboardingView()` on Android | A centred "No chats found" item when not searching and `rows.isEmpty` |

The onboarding `Column` takes the toolbar and navigation-bar paddings and consumes the navigation-bar insets, so the results list's `imePadding()` does not count them twice. In one-hand mode `ChatListSearchBarWithTags` ends with a keyboard spacer (`windowInsetsBottomHeight(WindowInsets.ime)`) only while its search field has the focus (`searchFocused`, which `ChatListSearchBar` sets from `onFocusChanged`): the bar sits above its own keyboard, and a keyboard opened by a view over the list (New chat › Create group) leaves the onboarding cards' size alone. iOS has no such spacer: SwiftUI's keyboard safe area lifts its field, and Create group opens in a sheet. On desktop the onboarding pages are drawn in the centre pane, so the chat list pane shows only the search bar and the results. When an opened result adds a chat and the onboarding state ends, `ChatList` enters composition and its `LaunchedEffect(activeFilter.value)` ([source](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L1203-L1205)) clears the search text, which resets the model; the iOS app keeps the search there.

`directoryRows` is a `LazyListScope` builder that emits one list without headers, in this order: your chats (`rows.yourChats`, `items` keyed `remoteHostId to id`), the results (`items` keyed by `id`, the link), then the footer item. So the load or retry row is the last row of the results: at the bottom with the top bar, and at the top in the one-hand UI, the list being reversed, which puts it farthest from the field.

| Row | View | Tap |
|-----|------|-----|
| Your chat | Each loaded chat of `rows.yourChats`, drawn with the chat list's own row, `ChatListNavLinkView(chat, remember { mutableStateOf(false) })` | as a local chat: opens it |
| Result | [`DirectoryResultNavLink`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L297-L318)`(result, imageSize = 72.dp * fontSizeSqrtMultiplier, close = null, cleanup = { withApi { refreshStates() } })`: `ChatListNavLinkLayout` with [`DirectorySearchRow`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L204-L254) as its preview, so the row gets the chat rows' padding and divider and the chat rows' avatar size. The row: avatar, `#` + name, the directory's SimpleX name claim as plain text (truncates first), description (2 lines), then the caption (`body2`, secondary colour): the member or subscriber count, or "Already joining" for `Joining`; trailing "Open" (`open_verb`, primary colour, one line, never wrapped), except for `Joining` | `hideKeyboard(view)`, then [`openDirectoryResult`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L321-L324) (see Opening a result) |
| Load more | [`DirectoryLoadMoreRow`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L258-L276): `ic_zoom_in` (the magnifier with a plus, as iOS's `plus.magnifyingglass`) and "Load more · %1$d left" (`directory_search_load_more_n_left`) when `moreCount` is above 0, else "Load more"; not clickable and in the secondary colour while `loading` | `withApi { loadMore() }` |
| Retry | [`DirectoryRetryRow`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L280-L292): `ic_refresh` and "Search failed, tap to retry" | `withApi { retry() }` |

A result's menu is `ChatListNavLinkLayout`'s dropdown, opened by a long press, or by a right click on desktop, with one item, `ItemAction(open_verb, ic_chat_bubble)`, which does what a tap does. It has no lifted preview, unlike the iOS context menu, so `DirectorySearchRow` has no `showsOpen` parameter. Text from the directory is rendered with `Text(String)` (never parsed as markdown) with line limits.

### Opening a result

[`openDirectoryResult(result, close, cleanup)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/DirectorySearchView.kt#L321-L324) runs `withBGApi { planAndConnect(rhId, result.entry.link, close, cleanup, prepareGroup = …) }` with `PrepareGroupMode.Open`, or `Confirm` (the default) for `Joining`. `planAndConnect` makes the online plan with the "Loading profile…" progress. [`PrepareGroupMode`](../../common/src/commonMain/kotlin/chat/simplex/common/views/newchat/ConnectPlan.kt#L24-L26) is passed on to `planAndConnectTask` and read only in the `GroupLinkPlan.Ok` branch with link data ([source](../../common/src/commonMain/kotlin/chat/simplex/common/views/newchat/ConnectPlan.kt#L258-L282)): `Confirm` shows the "Open channel?" / "Open group?" alert ([`showPrepareGroupAlert`](../../common/src/commonMain/kotlin/chat/simplex/common/views/newchat/ConnectPlan.kt#L769)), as for a pasted link, and `Open` calls [`prepareAndOpenGroup`](../../common/src/commonMain/kotlin/chat/simplex/common/views/newchat/ConnectPlan.kt#L809-L836) at once, the function the alert's confirm also calls: `apiPrepareGroup`, the channel's relay hostnames, `addChat`, `openChat_`, then `cleanup`. `Open` applies only when the plan has no `ownerVerification` (verified or failed) and no other name (`connectOtherLink`); with either, the branch shows the alert as `Confirm` does, because only the alert shows the signature line and the other name's button. Open plans a link, not a name, without a signature, so its plan has neither, and Open goes straight to the prepared chat with its Join button. Every other plan shows what a pasted link shows: the "Open group" alert for a known group, the reconnect and connecting alerts for a pending join, and the no-relays and update-required alerts. A `Joining` result keeps the default mode, so whatever its online plan turns out to be, it shows what a pasted link would.

The chat banner of a prepared channel (`nextConnectPrepared && useRelays`) shows its subscriber count, `subscriberCountStr(groupSummary.publicMemberCount)` from the link data, after the full name ([ChatView.kt](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatView.kt#L2355-L2364)), in `body2`, the secondary colour, centred.
