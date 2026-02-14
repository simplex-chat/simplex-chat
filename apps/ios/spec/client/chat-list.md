# SimpleX Chat iOS -- Chat List Module

> Technical specification for the conversation list, filtering, search, swipe actions, and user picker.
>
> Related specs: [Chat View](chat-view.md) | [Navigation](navigation.md) | [State Management](../state.md) | [README](../README.md)
> Related product: [Chat List View](../../product/views/chat-list.md)

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

## 2. ChatListView

**File**: `Shared/Views/ChatList/ChatListView.swift`

The root list view. Key responsibilities:

### Data Source
- Reads `ChatModel.shared.chats` (all conversations)
- Applies active filter from `ChatTagsModel.shared.activeFilter`
- Applies search query filtering
- Sorts by last activity (most recent first), with pinned chats at top

### Layout
- Uses SwiftUI `List` with `ForEach` over filtered chats
- Each row is a `ChatListNavLink` wrapping a `ChatPreviewView`
- Pull-to-refresh triggers `updateChats()` API call
- Empty state: `ChatHelp` view with getting-started guidance

### Connection Cards
- Pending contact connections (`ChatInfo.contactConnection`) shown as cards
- Contact requests (`ChatInfo.contactRequest`) shown with accept/reject UI via `ContactRequestView`

---

## 3. ChatPreviewView

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

## 4. ChatListNavLink

**File**: `Shared/Views/ChatList/ChatListNavLink.swift`

Wraps `ChatPreviewView` in a navigation link with tap and swipe behavior:

### Tap Behavior
- Direct chat: navigates to `ChatView` via `ItemsModel.loadOpenChat(chatId)`
- Group chat: navigates to `ChatView`
- Contact request: shows `ContactRequestView` with accept/reject
- Contact connection: shows `ContactConnectionInfo`
- Notes folder: navigates to `ChatView`

### Navigation
- Uses `NavigationLink` (iOS 15) or programmatic navigation (iOS 16+)
- Sets `ChatModel.chatId` to trigger navigation
- `ItemsModel.loadOpenChat()` loads messages with a 250ms navigation delay for smooth animation

---

## 5. Filtering & Tags

### Filter Tabs (TagListView)

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

### ChatTagsModel State

Filtering state is managed by `ChatTagsModel.shared`:

```swift
class ChatTagsModel: ObservableObject {
    @Published var userTags: [ChatTag] = []
    @Published var activeFilter: ActiveFilter? = nil
    @Published var presetTags: [PresetTag: Int] = [:]   // count per preset tag
    @Published var unreadTags: [Int64: Int] = [:]        // unread count per user tag
}
```

- `presetTags` counts are updated whenever `chats` changes via `updateChatTags()`
- Tags with zero matching chats are auto-hidden
- Active filter is auto-cleared when its tag has no matching chats

### Tag Management Commands
- `apiCreateChatTag(tag: ChatTagData)` -- create tag
- `apiSetChatTags(type:, id:, tagIds:)` -- assign tags to a chat
- `apiDeleteChatTag(tagId:)` -- delete tag
- `apiUpdateChatTag(tagId:, tagData:)` -- rename tag
- `apiReorderChatTags(tagIds:)` -- reorder tags

---

## 6. Search

Search is available via pull-down gesture or search button in the navigation bar.

### Filtering Logic
- Filters `ChatModel.chats` by matching search text against:
  - `chatInfo.displayName` (contact/group name)
  - `chatInfo.localAlias` (local alias)
  - `chatInfo.fullName` (full name)
- For deeper message content search, uses `apiGetChat(chatId:, search:)` parameter

### Search Results
- Matching chats are displayed in the same list format
- Results update as the user types (debounced)
- Clearing search restores the full filtered list

---

## 7. Swipe Actions

`ChatListNavLink` provides swipe actions on each row:

### Leading Swipe (left-to-right)

| Action | Icon | API | Condition |
|--------|------|-----|-----------|
| Pin / Unpin | pin | `apiSetChatSettings` (favorite) | Always |
| Read / Unread | envelope | `apiChatRead` / `apiChatUnread` | Always |

### Trailing Swipe (right-to-left)

| Action | Icon | API | Condition |
|--------|------|-----|-----------|
| Mute / Unmute | bell.slash | `apiSetChatSettings` (enableNtfs) | Always |
| Clear | trash | `apiClearChat` | Has messages |
| Delete | trash.fill | `apiDeleteChat` | Not active chat |
| Tag | tag | `apiSetChatTags` | Always |

---

## 8. UserPicker

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

## Source Files

| File | Path |
|------|------|
| Chat list view | `Shared/Views/ChatList/ChatListView.swift` |
| Chat preview row | `Shared/Views/ChatList/ChatPreviewView.swift` |
| Navigation link wrapper | `Shared/Views/ChatList/ChatListNavLink.swift` |
| Tag filter tabs | `Shared/Views/ChatList/TagListView.swift` |
| User picker sheet | `Shared/Views/ChatList/UserPicker.swift` |
| Getting started help | `Shared/Views/ChatList/ChatHelp.swift` |
| Contact request view | `Shared/Views/ChatList/ContactRequestView.swift` |
| Contact connection info | `Shared/Views/ChatList/ContactConnectionInfo.swift` |
| Contact connection view | `Shared/Views/ChatList/ContactConnectionView.swift` |
| Server summary | `Shared/Views/ChatList/ServersSummaryView.swift` |
| One-hand UI card | `Shared/Views/ChatList/OneHandUICard.swift` |
