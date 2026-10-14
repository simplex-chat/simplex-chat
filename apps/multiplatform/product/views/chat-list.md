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
| Desktop, flat layout (experimental) | Rail at the far left, then the same 3 columns; chat list rows are compact |

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
| Channels | `CHANNELS` | Updates | Channels |
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
- `ChatInfo.ContactConnection` -- pending connection (tap opens `ContactConnectionView`)

### Context Menu (Long Press / Right Click)

Each chat type provides specific dropdown menu items:

| Chat Type | Menu Items |
|---|---|
| Direct contact | Mark read/unread, toggle favorite, toggle notify, tag list, clear chat, delete contact |
| Group | Mark read/unread, toggle favorite, toggle notify, tag list, clear chat, archive all reports (moderator, when reports exist), leave group, delete group |
| Note folder | Mark read/unread, clear notes |
| Contact request | Accept, reject |
| Contact connection | Set name/alias, delete |

### Floating Elements

| Element | Condition | Description |
|---|---|---|
| One-hand UI card (`ToggleChatListCard`) | `oneHandUICardShown == false` | Dismissible card introducing bottom toolbar mode with toggle switch |
| Address creation card (`AddressCreationCard`) | `addressCreationCardShown == false` | Prompts user to create a SimpleX address; tappable card opens `UserAddressLearnMore` |
| FAB (new chat button) | Standard mode, search empty, chat running | `FloatingActionButton` at bottom-right, pencil icon, opens `NewChatSheet` |
| Crowdfunding banner (`GetStakeBanner`) | `crowdfundingAvailable()` and not dismissed | Gradient card above the chats, and below the onboarding cards when there are no conversations; opens `GetStakeView`. The dismiss X appears once the banner has been tapped and there is at least one chat; dismissing hides it until hints are reset |

### Desktop Flat Layout (experimental)

Enabled by "Flat layout (experimental)" in Developer options (desktop only, while developer tools are on).

Rail (`DesktopRail`), top to bottom:

| Element | Behavior |
|---|---|
| User tiles | Active and non-hidden users, most recently active first. Each tile shows the profile image, or the first character of the name on a light primary tint. Tile corners follow the profile image corner setting and become squarer on hover and for the active user. A marker on the left edge grows on hover and is tallest for the active user. Other users show an unread badge. Click another user to switch to it (closes `UserPicker` if open); click the active user to open `UserPicker` (address, preferences, remote hosts, other actions) |
| All | Clears the list filter; highlighted when no filter is active |
| Preset lists | Reports, favorites, contacts, groups, channels, business, notes, each shown when it has chats; click toggles the filter |
| User lists | Emoji or label icon with name and unread dot; click toggles the filter; long-press or right-click opens the edit/delete/reorder menu |
| Add list | Opens `TagListEditor` |
| Settings | Below a divider at the bottom; opens `SettingsView` |

Items have rounded click areas. The active list item is neutral: a light grey background with full-contrast icon and label. The scrollable area fades out at the bottom. Clicking a list, All, Add list or Settings closes any open start-panel screen first.

Profile switching from the rail is blocked while forwarding messages, sharing a chat link or the user address, and while a switch is in progress. Switching to another user is also blocked while chat is stopped, unless connected to a remote host; the active user's tile still opens `UserPicker`. With no users, the rail shows only Settings.

Surfaces: the rail and the chat list have their own shades, with no dividers between rail, list and chat. In light themes both are slightly darker than the chat; in dark themes the rail is darker and the list slightly lighter than the chat, so the chat keeps the theme background and wallpaper. Known limitations: start-panel screens (settings and others), the share list and the delivery receipts prompt use the plain theme background over the shaded list, and so does the blur under the app bar.

Chat list changes:
- The tag filter row (`TagsView`) is hidden and the rail replaces it. The toolbar user avatar button is hidden unless there is no active user and no remote host. With no users while a remote host is connected, the toolbar Settings menu button is hidden too (the rail has Settings).
- Chat rows are a single line: 28dp avatar (with the inactive icon for inactive contacts and groups), chat name, unread or mention badge (or muted/favorite icon), and status icon. Last message, draft and timestamp are not shown.
- The chat name is SemiBold when the chat has unread messages and secondary-colored when the chat is muted with no unread mentions.
- Contact requests and pending connections use the same 28dp image.
- Rows have smaller vertical padding and no dividers, are slightly inset with rounded corners, and the selected chat has a light grey background.

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
| `GetStakeBanner.kt` | `views/chatlist/GetStakeBanner.kt` |
| `DesktopRail.kt` | `views/chatlist/DesktopRail.kt` |
