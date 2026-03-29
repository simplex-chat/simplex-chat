# Chat List (Home Screen)

> **Related spec:** [spec/client/chat-list.md](../../spec/client/chat-list.md)

## Purpose

Main screen of the SimpleX Chat app. Displays all conversations sorted by last activity, serves as the navigation root, and provides access to user profiles, settings, and new chat creation.

## Route / Navigation

- **Entry point**: App launch (root view), or back-navigation from any chat
- **Presented by**: `ContentView` as the default view when `chatModel.chatId == nil`
- **Navigation stack**: `NavStackCompat` wrapping `chatListView` with destination `chatView`
- **UserPicker sheet**: Triggered by tapping the user avatar in the toolbar; presents `UserPicker` as a custom sheet, which links to `UserPickerSheetView` sub-sheets (address, preferences, profiles, current profile, use from desktop, settings)

## Page Sections

### Toolbar

| Element | Location | Behavior |
|---|---|---|
| User avatar button | Leading | Opens `UserPicker` sheet (profile switcher, address, settings, preferences, connect to desktop) |
| Connection status indicator | Center (`SubsStatusIndicator`) | Shows server subscription status; taps navigate to `ServersSummaryView` |
| New chat button (pencil icon) | Trailing | Opens `NewChatSheet` modal |

The toolbar supports two layout modes:
- **Standard (top)**: Navigation bar with `.topBarLeading`, `.principal`, `.topBarTrailing` placements
- **One-hand UI (bottom)**: Toolbar items placed in `.bottomBar` with the list vertically flipped via `scaleEffect(y: -1)`

### Search Bar

- Text field with magnifying glass icon
- When active, `searchMode = true` hides the navigation bar and shows inline search
- Filters chat list in real-time by contact/group name and message content
- Detects pasted SimpleX links (`searchShowingSimplexLink`) and offers to connect

### Chat Filter Tabs (Tags)

Managed by `ChatTagsModel` and `TagListView`:

| Filter | PresetTag | Description |
|---|---|---|
| All | (none) | No filter, shows all chats |
| Unread | `.unread` | Chats with unread messages |
| Favorites | `.favorites` | User-favorited chats |
| Groups | `.groups` | Group conversations only |
| Contacts | `.contacts` | Direct contacts only |
| Business | `.business` | Business chat conversations |
| Notes | `.notes` | Notes to self |
| Group Reports | `.groupReports` | Moderation reports (non-collapsible) |
| Custom tags | `.userTag(ChatTag)` | User-created tags with custom names |

### Chat Preview Rows

Each row rendered by `ChatPreviewView` inside `ChatListNavLink`:

| Element | Description |
|---|---|
| Avatar | Profile image or colored initials circle; online status indicator for contacts |
| Chat name | Display name (contact, group, or note-to-self) |
| Last message preview | Truncated text of most recent message; supports markdown rendering |
| Timestamp | Relative time of last activity (e.g., "2m", "1h", "Yesterday") |
| Unread badge | Numeric count badge for unread messages; distinct styling for mentions |
| Muted indicator | Bell-slash icon when notifications are muted |
| Pinned indicator | Pin icon for pinned chats |
| Incognito indicator | Shows when connected via incognito profile |
| Connection status | Shows connecting/pending state for incomplete connections |

### Swipe Actions

- **Trailing swipe**: Mute/unmute, pin/unpin, tag management
- **Leading swipe**: Mark as read/unread
- **Context menu** (long press): Full set of actions including delete, clear chat, toggle favorite

### Floating Elements

- **One-hand UI card** (`OneHandUICard`): Dismissible card shown to introduce bottom toolbar mode
- **Address creation card** (`AddressCreationCard`): Prompts user to create a SimpleX address

### Pull-to-Refresh

Triggers `reconnectAllServers()` after user confirmation alert ("Reconnect servers?"). Uses additional traffic to force message delivery.

## Loading / Error States

| State | Behavior |
|---|---|
| Chat database not started | Settings row shows exclamation icon; chat running == false disables interactions |
| No chats | `ChatHelp` view displayed with onboarding guidance |
| Connection in progress | `ConnectProgressManager` overlay with connecting text |
| Search with no results | Empty list with no special empty-state view |

## Related Specs

- `spec/client/chat-list.md` -- Chat list feature specification
- `spec/state.md` -- Application state management
- [User Profiles](user-profiles.md) -- Profile switching from UserPicker
- [Settings](settings.md) -- Settings accessed via UserPicker
- [New Chat](new-chat.md) -- New chat sheet triggered from toolbar
- [Chat](chat.md) -- Navigated to when tapping a chat row

## Source Files

- `Shared/Views/ChatList/ChatListView.swift` -- Main view, toolbar, search, filter logic
- `Shared/Views/ChatList/ChatPreviewView.swift` -- Individual chat row rendering
- `Shared/Views/ChatList/ChatListNavLink.swift` -- Navigation link wrapper with swipe actions
- `Shared/Views/ChatList/TagListView.swift` -- Filter tab bar (preset + custom tags)
- `Shared/Views/ChatList/UserPicker.swift` -- User profile picker sheet
- `Shared/Views/ChatList/ChatHelp.swift` -- Empty-state help view
- `Shared/Views/ChatList/ContactRequestView.swift` -- Contact request row rendering
- `Shared/Views/ChatList/ContactConnectionView.swift` -- Pending connection row rendering
- `Shared/Views/ChatList/OneHandUICard.swift` -- One-hand UI introduction card
- `Shared/Views/ChatList/ServersSummaryView.swift` -- Server subscription summary
