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
- For non-empty text that is not a link, offers "Search in Directory"; the keyboard Search key does the same search (see below)
- The field doesn't autocorrect or capitalize, so the Search key searches exactly what was typed

### Online Search (SimpleX Directory)

Searching beyond the user's own chats happens only when the user asks, and the results are never saved: they disappear when the search text changes or the user switches profile.

| Element | Behavior |
|---|---|
| "Search in Directory" row | Shown for non-empty text that is not a SimpleX link, until results for that text show or load. With the one-hand UI it takes the place of the tags above the search field (the tags don't come back while the text is there); with the top bar it is below the field. Searches SimpleX Directory for public groups and channels |
| Keyboard Search key | The same directory search. It never looks up SimpleX names |
| "Connect to …" row | Shown for a SimpleX name (e.g. `alice` or `#news.testing`), and stays next to the results; it is the only way to look up a name |
| "Directory search" switch | In Settings › Privacy › Chats, after "Send link previews"; off by default |
| "Enable Directory search?" alert | Every search asks while the switch is off: search text is sent to SimpleX Directory, each search separately and without the user's profile, so it can't build a search history. Cancel sends nothing; Enable turns the switch on and searches. Once it is on, searches don't ask |
| Results | One list without headers below the local chats. First your chats among the results that the list above doesn't show (for example because the name doesn't match the typed text), as their regular chat rows; then the other results, with the same avatar size as chat rows: avatar, `#` name, the directory's SimpleX name, description, then the member or subscriber count and "Open" on the right. A join still in progress without a chat shows "Already joining" in place of the count and no "Open" |
| Open | Tapping a result (or its "Open") loads the group's or channel's profile, with the spinner in the search field, and opens the chat straight away, with its Join button and no "Open channel?" question; a channel's banner shows its subscriber count. The user joins from there. A result that is already joining shows the same sheet or alert as pasting its link would |
| Long press | On iOS 16 and later, a long press on a result shows a menu with Open, previewing the row; iOS 15 has no menu |
| Load row | The last row of the results: "Load more · N left" when the directory says how many matches are left, "Load more" otherwise; greyed while a page loads. A failed request turns it into "Search failed, tap to retry". Nothing loads more pages automatically |
| Page note | When every result of the last page loaded is a chat already in the list, "All results on this page are already in your chats" (secondary colour), followed by the load row when there are more pages |

A result is not shown twice: an entry for a chat already shown in the list is hidden, and one the list doesn't show is drawn once, as the chat. A group joined without preparing it first (with an older app, by `/c`, or added by invitation) is not recognised: it appears as a result with "Open" beside its own chat. Directory entries that are not group or channel links are ignored.

With the one-hand UI the whole list is mirrored: going up from the search field at the bottom come the local chats, then your chats from the results, then the other results, with the load row at the top, farthest from the field. This order is intended, so the user's own chats stay nearest the field.

With no conversations yet (onboarding), the online results replace the onboarding pages while a search runs or has results. With conversations, while an online search runs or after it has run, even one that found nothing, the list hides the banner (Wefunder or badges), the one-hand UI card and the "New 1-time link" / "Paste link / Scan" cards; filtering the chats by typing keeps them, and they come back when the search text is cleared (× or Cancel).

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

### Channel Adaptations

When a group has `groupInfo.useRelays == true` (channel):

| Element | Channel behavior |
|---|---|
| Chat icon | Antenna icon (`antenna.radiowaves.left.and.right.circle.fill`) instead of group icon |
| Swipe "Leave" | Hidden for channel owners (`useRelays && isOwner`) |
| Context menu "Leave" | Hidden for channel owners |
| Delete alert | "Delete channel?" (not "Delete group?") |
| Leave alert title | "Leave channel?" (not "Leave group?") |
| Leave alert message | "You will stop receiving messages from this channel. Chat history will be preserved." |

### Relay URL Handling

When a relay address link (`/r` path) is opened via URL deep link, `ContentView.connectViaUrl_()` intercepts it and shows an alert: "Relay address" / "This is a chat relay address, it cannot be used to connect." The link is not processed further.

### Swipe Actions

- **Trailing swipe**: Mute/unmute, pin/unpin, tag management
- **Leading swipe**: Mark as read/unread
- **Context menu** (long press): Full set of actions including delete, clear chat, toggle favorite

### Floating Elements

- **One-hand UI card** (`OneHandUICard`): Dismissible card shown to introduce bottom toolbar mode
- **Address creation card** (`AddressCreationCard`): Prompts user to create a SimpleX address
- **Crowdfunding banner** (`GetStakeBanner`): Shown to users in the US only, above the chats and below the onboarding pages. Tapping it opens `GetStakeView` (invest on Wefunder). The dismiss X appears once the banner has been tapped and there is at least one chat; dismissing hides it until hints are reset

### Pull-to-Refresh

Triggers `reconnectAllServers()` after user confirmation alert ("Reconnect servers?"). Uses additional traffic to force message delivery.

## Loading / Error States

| State | Behavior |
|---|---|
| Chat database not started | Settings row shows exclamation icon; chat running == false disables interactions |
| No conversations yet | `ConnectOnboardingView` pages replace the list, with the crowdfunding banner below them where it applies |
| Connection in progress | `ConnectProgressManager` overlay with connecting text |
| Search with no results | "No chats found" in the middle of the list, unless an online search is running or shows results |
| Online search running | Progress indicator in the search field (after about a second); tapping a result or clearing the search cancels it |
| Loading more results | The "Load more" row is greyed and can't be tapped until the page arrives |
| Directory search failed | "Search failed, tap to retry" row at the end of the results, in place of the load row |
| Online search found nothing | No result rows; "No chats found" when no local chat matches either (in onboarding, a "No chats found" row) |

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
- `Shared/Views/ChatList/GetStakeBanner.swift` -- Wefunder crowdfunding banner and shared banner card chrome
- `Shared/Views/ChatList/ServersSummaryView.swift` -- Server subscription summary
- `Shared/Views/ChatList/DirectorySearchView.swift` -- Online search model, result, load and retry rows, long-press menu, Open, "Enable Directory search?" alert
- `Shared/Model/DirectorySearch.swift` -- Directory response parsing and result states
