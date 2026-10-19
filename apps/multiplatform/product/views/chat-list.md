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
- For non-empty text that is not a link, offers "Search in Directory"; the keyboard Search key (Enter on desktop) searches online (see below)

### Online Search (Names and Directory)

Searching beyond the user's own chats happens only when the user asks, and the results are never saved: they disappear when the search text changes or the user switches profile. On Android, rotating the screen also drops them while the search text stays.

| Element | Behavior |
|---|---|
| "Search in Directory" row | Shown for non-empty text that is not a SimpleX link. On Android with the one-hand UI it takes the place of the tags above the search field; with the top bar the tags stay and the row follows them below the field. On desktop the tags always stay and the row is shown next to them, on the search field's side. Searches SimpleX Directory for public groups and channels |
| Keyboard Search key | On Android the keyboard's Search action, on desktop Enter. Searches the directory and, when the text is a SimpleX name (e.g. `alice` or `#news.testing`), resolves the name online. For a bare name both the contact (`@`) and the channel (`#`) form are tried |
| "Connect to …" row | Shown for a SimpleX name, as before (on Android with the top bar it takes the tags' place); hidden once the online search found anything for that name |
| First-search alert | Before the first online search: "Search in Directory?" — the text goes to SimpleX Directory, and a SimpleX name also to the user's server that resolves names; no contact is created and the profile is not sent. Search / Search and don't show again / Cancel ("Reset all hints" brings it back) |
| Names part | Below the local chats, headed "Names": avatar, profile name, the SimpleX name in parentheses, description. Tapping connects (or opens the chat if it is already known) |
| Directory part | Below the Names part, headed "Directory": avatar, `#` name, the directory's SimpleX name, description, member or subscriber count, and "Join channel" / "Join group". Tapping connects; "Show more" loads the next page; a failed request shows "Search failed, tap to retry". An entry for a group or channel the user already has, that the list above doesn't show (its name doesn't match the typed text), is shown as the regular chat row and opens the chat, never "Join …". When every result of the last page loaded is a chat already in the list or found by name, the part shows "All results on this page are already in your chats" (secondary colour), followed by "Show more" when there are more pages; nothing loads more pages automatically |

A result is not shown twice: a name or directory entry for a chat already shown in the list is hidden, and a directory entry is hidden when the name search found the same channel (matched by its link). A group the user joined through a link other than the directory's, or by invitation, is not recognised: it still appears in the Directory part with "Join group". Directory entries that are not group or channel links are ignored. Looking up a name never shows an error; a name that is not found simply gives no row, and "Connect to …" stays.

With the one-hand UI the whole list is reversed: going up from the search field at the bottom come the local chats, then Names, then Directory, each part's header above its rows. This order is intended, so the user's own chats stay nearest the field.

While an online search runs or has results, the list shows no banner, no one-hand UI card and no connect cards. Typing alone, which only filters the chats, keeps them; they come back when the results go (the text changes or is cleared) or when a search is cancelled before showing anything.

With no conversations yet (onboarding), the search bar is shown above the onboarding pages (below them with the one-hand UI) on Android, and the online results replace the pages while a search runs or has results. On desktop the onboarding pages are in the centre pane, so the chat list pane shows the search bar and the results only. Once a result is joined and the onboarding state ends, the search is cleared.

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
| One-hand UI card (`ToggleChatListCard`) | `oneHandUICardShown == false`, and no online search results shown | Dismissible card introducing bottom toolbar mode with toggle switch |
| Connect cards (`ChatListFeatureCards`, `ConnectBannerCard`) | `addressCreationCardShown == false`, the user has conversations, and no online search results shown | Two cards, "New 1-time link" and "Paste link" ("Paste link / Scan" on Android), each opening `NewChatView`; the X sets `addressCreationCardShown` |
| FAB (new chat button) | Standard mode, search empty, chat running | `FloatingActionButton` at bottom-right, pencil icon, opens `NewChatSheet` |
| Crowdfunding banner (`GetStakeBanner`) | `crowdfundingAvailable()`, not dismissed, and, in the chat list, no online search results shown | Gradient card above the chats, and below the onboarding cards when there are no conversations; opens `GetStakeView`. The dismiss X appears once the banner has been tapped and there is at least one chat; dismissing hides it until hints are reset |

### Empty States

| State | Display |
|---|---|
| Loading | "Loading chats..." centered text |
| No chats | "You have no chats" centered text, unless an online search runs or shows results |
| No filtered chats | "No chats in list [tag name]" or "No unread chats" with clickable filter reset |
| No search results | "No chats found" centered text, unless an online search is running or shows results |
| Online search running | Progress indicator in the toolbar (after about a second); tapping a result or clearing the search cancels it |
| Directory search failed | "Search failed, tap to retry" row at the end of the Directory part |
| Online search found nothing | No Names or Directory part; "No chats found" when no local chat matches either (in onboarding, a "No chats found" row) |

## Source Files

| File | Path |
|---|---|
| `ChatListView.kt` | `views/chatlist/ChatListView.kt` |
| `ChatListNavLinkView.kt` | `views/chatlist/ChatListNavLinkView.kt` |
| `ChatPreviewView.kt` | `views/chatlist/ChatPreviewView.kt` |
| `UserPicker.kt` | `views/chatlist/UserPicker.kt` |
| `TagListView.kt` | `views/chatlist/TagListView.kt` |
| `GetStakeBanner.kt` | `views/chatlist/GetStakeBanner.kt` |
| `DirectorySearchView.kt` | `views/chatlist/DirectorySearchView.kt` |
| `DirectorySearch.kt` | `model/DirectorySearch.kt` |
