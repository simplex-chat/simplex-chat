# Chat List (Home Screen)

> **Related spec:** [spec/client/chat-list.md](../../spec/client/chat-list.md)

## Purpose

Main screen of the SimpleX Chat Android and Desktop apps. Displays all conversations sorted by last activity, serves as the navigation root, and provides access to user profiles, settings, and new chat creation.

## Route / Navigation

- **Entry point**: App launch (root view), or back-navigation from any chat
- **Presented by**: `ChatListView` composable as the default view when `chatModel.chatId == null`
- **Navigation**: `ChatListNavLinkView` handles click routing to `ChatView` for each chat type
- **UserPicker**: Triggered by tapping the user avatar in the toolbar; presents `UserPicker` as a custom sheet (Android: bottom sheet overlay; Desktop: sidebar panel)

## Platform Layout

| Platform | Layout |
|---|---|
| Android | Single-column list; toolbar at top or bottom (one-hand UI); FAB for new chat |
| Desktop | 3-column layout: chat list (left), chat view (center), info/detail panel (right via `ModalManager.end`) |

## Page Sections

### Toolbar (`ChatListToolbar`)

| Element | Location | Behavior |
|---|---|---|
| User avatar button | Leading | Opens `UserPicker` sheet (profile switcher, address, settings, preferences, connect to desktop/mobile) |
| "Your chats" title | Center | Tappable to scroll list to top |
| Connection status indicator (`SubscriptionStatusIndicator`) | Adjacent to title | Shows SMP server subscription status; taps open `ServersSummaryView` |
| New chat button (pencil icon) | Trailing (one-hand UI) or FAB (standard) | Opens `NewChatSheet` modal via `showNewChatSheet()` |
| Active call indicator | Trailing (Desktop, one-hand UI) | `ActiveCallInteractiveArea` shown when a call is active |
| Updating progress | Trailing | Shows progress circle/indicator during database updates |
| Stopped indicator | Trailing | Red warning icon when chat engine is stopped |

The toolbar supports two layout modes controlled by `appPrefs.oneHandUI`:
- **Standard (top)**: `DefaultAppBar` at top with `NavigationButtonMenu` leading, title center, buttons trailing. FAB at bottom-right for new chat.
- **One-hand UI (bottom)**: Toolbar at bottom of screen with `Column(Modifier.align(Alignment.BottomCenter))`; list rendered with `reverseLayout = true`; no FAB (new chat button is inline in toolbar).

### Search Bar (`ChatListSearchBar`)

| Element | Description |
|---|---|
| Search icon | Magnifying glass icon at leading edge |
| Text field | `SearchTextField` with placeholder "Search or paste SimpleX link" |
| Filter button | `ToggleFilterEnabledButton` (filter icon) toggles unread-only filter; shown when search text is empty |
| Clear button | Appears when text is entered; `BackHandler` clears search on back |

Behavior:
- Filters chat list in real-time by contact/group name via `filteredChats()`
- Detects pasted SimpleX links (`strHasSingleSimplexLink`) and triggers `planAndConnect()` connection dialogue
- In one-hand UI mode, search bar appears below tag filters with IME spacer; in standard mode, above tag filters

### Chat Filter Tags (`TagsView`)

Managed by `chatModel.userTags`, `chatModel.presetTags`, and `chatModel.activeChatTagFilter`:

| Filter | `PresetTagKind` | Icon | Description |
|---|---|---|---|
| Group Reports | `GROUP_REPORTS` | Flag | Chats with moderation reports (non-collapsible) |
| Favorites | `FAVORITES` | Star | User-favorited chats |
| Contacts | `CONTACTS` | Person | Direct contacts and contact requests |
| Groups | `GROUPS` | Group | Group conversations (non-business) |
| Business | `BUSINESS` | Work | Business chat conversations |
| Notes | `NOTES` | Folder | Notes to self |
| Custom tags | `UserTag(ChatTag)` | Label/emoji | User-created tags with custom emoji and name |
| Unread | `ActiveFilter.Unread` | Filter list icon | Chats with unread messages (toggle via filter button) |

Display logic:
- When collapsible preset tags exceed 3 total (with user tags), they collapse into a `CollapsedTagsFilterView` dropdown menu
- Non-collapsible tags (`GROUP_REPORTS`) always show expanded
- User tags show with emoji or label icon; long-press opens `TagsDropdownMenu` (edit, delete, change order)
- "+" button at end opens `TagListEditor` for creating new tags

### Chat Preview Rows (`ChatPreviewView`)

Each row rendered by `ChatPreviewView` inside `ChatListNavLinkView`:

| Element | Description |
|---|---|
| Avatar | `ProfileImage` with overlay icons (inactive contact, left/removed group member) |
| Chat name | Display name with verified icon for verified contacts; colored for pending/connecting states |
| Last message preview | Truncated text of most recent message; draft indicator with edit icon; attachment icons |
| Timestamp | Relative time of last activity |
| Unread badge | Numeric count badge; distinct styling for mentions |
| Muted indicator | Bell-off icon when notifications are muted |
| Favorite indicator | Star icon for favorited chats |
| Incognito indicator | Shows when connected via incognito profile |
| Connection status | Shows connecting/pending state for incomplete connections |

Chat types handled by `ChatListNavLinkView`:
- `ChatInfo.Direct` -- direct contact chat
- `ChatInfo.Group` -- group chat (with in-progress indicator for joining)
- `ChatInfo.Local` -- note-to-self folder
- `ChatInfo.ContactRequest` -- incoming contact request (tap shows accept/reject alert)
- `ChatInfo.ContactConnection` -- pending connection (tap opens `ContactConnectionInfoView`)

### Context Menu (Long Press / Right Click)

Each chat type provides specific dropdown menu items:

| Chat Type | Menu Items |
|---|---|
| Direct contact | Mark read/unread, toggle favorite, toggle notify, tag list, clear chat, delete contact |
| Group | Mark read/unread, toggle favorite, toggle notify, tag list, clear chat, leave group, delete group |
| Note folder | Mark read/unread, clear notes, delete notes |
| Contact request | Accept, reject |
| Contact connection | Set name/alias, delete |

### Floating Elements

| Element | Condition | Description |
|---|---|---|
| One-hand UI card (`ToggleChatListCard`) | `oneHandUICardShown == false` | Dismissible card introducing bottom toolbar mode with toggle switch |
| Address creation card (`AddressCreationCard`) | `addressCreationCardShown == false` | Prompts user to create a SimpleX address; tappable card opens `UserAddressLearnMore` |
| FAB (new chat button) | Standard mode, search empty, chat running | `FloatingActionButton` at bottom-right, pencil icon, opens `NewChatSheet` |

### Empty States

| State | Display |
|---|---|
| Loading | "Loading chats..." centered text |
| No chats | "You have no chats" centered text |
| No filtered chats | "No chats in list [tag name]" or "No unread chats" with clickable filter reset |
| No search results | "No chats found" centered text |

## Source Files

| File | Path |
|---|---|
| `ChatListView.kt` | `views/chatlist/ChatListView.kt` |
| `ChatListNavLinkView.kt` | `views/chatlist/ChatListNavLinkView.kt` |
| `ChatPreviewView.kt` | `views/chatlist/ChatPreviewView.kt` |
| `UserPicker.kt` | `views/chatlist/UserPicker.kt` |
| `TagListView.kt` | `views/chatlist/TagListView.kt` |
