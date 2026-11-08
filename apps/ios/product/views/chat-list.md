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

Contacts and groups the user is a member of; contact requests, contact cards, invitations, pending connections and note folders keep their own actions.

- **Leading swipe**: Read / Unread, List (green, opens the list sheet), Mute / Unmute
- **Trailing swipe**, from the screen edge; a full swipe performs the first:

  | Chat | Actions |
  |---|---|
  | Contact | Delete, Ban, More (Clear, Archive) |
  | Business chat, owner on the business side | Delete, Ban, More (Clear, Archive reports, Leave) |
  | Group, channel, other business chats | Delete, Leave, More (Clear, Archive reports) |

- "More" is shown when it holds two actions; a single action is shown in its place, and "More" is an action sheet without a title
- Delete: for contacts, and for groups the user can delete; Leave: while the membership is current or pending, except for channel owners; Clear: when the chat has messages; Archive reports: for moderators and above when the group has reports
- Ban (orange): opens "Delete and ban?" for a contact connected via the user's address whose app supports bans; for the owner of a business chat the customer is checked on tap, and "Can't ban" is shown when the customer left or uses an older app version
- Archive: moves the contact to Archived contacts with the conversation and shows "Contact archived!"
- List sheet: a Favorites row with a star, filled for a favorite, at the top; tapping it changes the favorite and closes the sheet. The sheet opens also when no lists exist.
- Archived contacts: Delete, and Ban under the same conditions as in the chat list

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
- `Shared/Views/ChatList/GetStakeBanner.swift` -- Wefunder crowdfunding banner and shared banner card chrome
- `Shared/Views/ChatList/ServersSummaryView.swift` -- Server subscription summary
