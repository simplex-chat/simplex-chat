# Chat List Specification

Source: `common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt`

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

---

## Executive Summary

The Chat List is the landing screen of SimpleX Chat, rendering all conversations for the active user. Built around `ChatListView` (line 126 in `ChatListView.kt`), it provides a searchable, filterable `LazyColumn` of chat previews with a toolbar, tag-based filtering, and a user-switching side panel. The view adapts between one-hand UI mode (toolbar at bottom, reversed list) and standard mode (toolbar at top). Search also accepts SimpleX links for direct connection.

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

**Location:** [`ChatListView.kt#L127`](ChatListView.kt#L127)

```kotlin
fun ChatListView(
  chatModel: ChatModel,
  userPickerState: MutableStateFlow<AnimatedViewState>,
  setPerformLA: (Boolean) -> Unit,
  stopped: Boolean
)
```

### Initialization

- Shows "What's New" modal on first launch after update (line ~130), with a 1-second delay.
- On desktop, closing a chat resets audio/video players (line ~138).

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

- `SetNotificationsModeAdditions`: Notification permission setup (line ~184).
- `UserPicker`: Overlay side panel for user switching (line ~192).

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

Defined as sealed class `ActiveFilter` (line ~51):

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

The `filteredChats` function (line ~1188) applies filters in this order:

1. **SimpleX link match:** If a pasted link resolved to a known contact/group, show only that chat.
2. **Text search:** Case-insensitive match against `chat.chatInfo.chatViewName`, `chat.chatInfo.fullName`, and `chat.chatInfo.localAlias`.
3. **Active filter:**
   - `PresetTag`: Matches chat type and characteristics (e.g., `CONTACTS` filters `ChatInfo.Direct`, `GROUPS` filters `ChatInfo.Group`).
   - `UserTag`: Matches chats whose `chatTags` contain the tag ID.
   - `Unread`: Matches chats with `unreadCount > 0` or `unreadChat == true`.

### Search Bar

`ChatListSearchBar` (line ~611) provides:
- Text input with search icon.
- SimpleX link detection: When a pasted string contains a single SimpleX link, it triggers `planAndConnect` for connection, suppressing normal search.
- Unread filter toggle button (right side, when search is empty).

---

<a id="ChatPreviewView"></a>

## 5. Chat Preview

**Location:** [`ChatPreviewView.kt#L40`](ChatPreviewView.kt#L40)

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

**Location:** [`ChatListNavLinkView.kt#L37`](ChatListNavLinkView.kt#L37)

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

**Location:** [`ChatListView.kt#L929`](ChatListView.kt#L929)

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

**Location:** [`TagListView.kt#L48`](TagListView.kt#L48)

Full-screen tag management view opened from the "+" button or long-press menu.

```kotlin
fun TagListView(rhId: Long?, chat: Chat?, close: () -> Unit, reorderMode: Boolean)
```

- Displays all user tags in a `LazyColumnWithScrollBar`.
- Supports drag-and-drop reordering via `rememberDragDropState` (calls `apiReorderChatTags`).
- Each tag row shows emoji/icon, name, chat count, and a checkbox if opened for a specific chat (to assign/unassign tags).
- "Create list" button opens `TagListEditor` modal.

---

<a id="UserPicker"></a>

## 8. UserPicker

**Location:** [`UserPicker.kt#L46`](UserPicker.kt#L46)

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
