# New Chat / Connection

> **Related spec:** [spec/client/navigation.md](../../spec/client/navigation.md) | [spec/client/chat-list.md](../../spec/client/chat-list.md) (online search)

## Purpose

Create new contacts, groups, or connect with others via one-time invitation links or by scanning/pasting SimpleX links. This is the primary entry point for establishing new E2E encrypted connections.

## Route / Navigation

- **Entry point**: Tap the new chat button (pencil icon) in `ChatListView` toolbar or FAB
- **Presented by**: `NewChatSheet` modal from `ChatListView` via `showNewChatSheet()`; wraps `NewChatView` and group creation in `ModalManager.start`
- **Internal navigation**: `NewChatSheet` provides 3 action buttons:
  - "Create 1-time link" -- opens `NewChatView` with `INVITE` tab (generate and share a one-time invitation link)
  - "Scan / paste link" -- opens `NewChatView` with `CONNECT` tab (scan QR code or paste a received link)
  - "Create group" -- opens `AddGroupView`
- **Tabs within NewChatView**: `HorizontalPager` with `TabRow` toggles between `NewChatOption.INVITE` (1-time link) and `NewChatOption.CONNECT` (connect via link)
- **Swipe gesture**: Left/right swipe switches between tabs (Android only; `userScrollEnabled = appPlatform.isAndroid`)
- **Dismiss behavior**: On dispose, a `DisposableEffect` shows an alert dialog (via `AlertManager.shared.showAlertDialog`) asking whether to keep an unused invitation link or delete it via `controller.deleteChat()`

## Page Sections

### Tab Selector

| Tab | Icon | Label | Description |
|---|---|---|---|
| 1-time link | `ic_repeat_one` | "1-time link" | Generate and share a one-time invitation link |
| Connect via link | `ic_qr_code` | "Connect via link" | Scan QR code or paste a received link |

### Invite Tab (1-time Link) -- `PrepareAndInviteView`

Displayed when `selection == INVITE`:

| Element | Description |
|---|---|
| QR code display | Generated QR code for the invitation link (`SimpleXLinkQRCode`) |
| Short/full link toggle | Switch between short and full link display |
| Share button | System share for the invitation link |
| Copy button | Copy link to clipboard |
| Incognito toggle | Option to connect with a random profile |
| Loading state | `CreatingLinkProgressView` with "Creating link" text while `creatingConnReq` is true |
| Retry button | `RetryButton` shown if link creation fails; calls `createInvitation()` |

Link creation calls `apiAddContact` which returns a `CreatedConnLink` with both `connFullLink` and optional `connShortLink`. The invitation is tracked via `chatModel.showingInvitation`.

### Connect Tab -- `ConnectView`

Displayed when `selection == CONNECT`:

| Element | Description |
|---|---|
| QR code scanner | Camera-based QR code scanner (`showQRCodeScanner` state) |
| Paste link field | Text field for pasting a SimpleX link (`pastedLink`) |
| Connect button | Initiates connection via `planAndConnect()` |

When a valid SimpleX link is detected:
1. `planAndConnect()` is called with the link URI
2. If the link matches a known contact, filters to that chat
3. If the link matches a known group, filters to that group
4. Otherwise, creates a new connection

### Create Group (`AddGroupView`)

| Element | Description |
|---|---|
| Group name field | Required display name input with `FocusRequester` |
| Profile image picker | `GetImageBottomSheet` for selecting/cropping a group avatar |
| Incognito toggle | Option to create group with random profile (`incognitoPref`) |
| Create button | Calls `apiNewGroup()`, then opens `AddGroupMembersView` (normal) or `GroupLinkView` (incognito) |

Group creation flow:
1. User enters group name and optionally selects an image
2. `apiNewGroup()` creates the group and returns `GroupInfo`
3. `openGroupChat()` navigates to the new group chat
4. `setGroupMembers()` preloads member data
5. `AddGroupMembersView` opens for inviting contacts (or `GroupLinkView` for incognito groups)

### QR Code Components (`QRCode.kt`)

| Component | Description |
|---|---|
| `SimpleXLinkQRCode` | Renders a QR code for a SimpleX connection link |
| QR scanner | Platform camera scanner for reading QR codes |
| Short link display | Compact link text with copy/share actions |

### Search (New Chat Sheet)

The search field of the New chat sheet filters "Your contacts" and offers the same online search as the chat list (see [Chat List, Online Search](chat-list.md#online-search-simplex-directory)):

| Element | Behavior |
|---|---|
| "Connect to …" row | Below the field for a SimpleX name; stays next to the results |
| "Search in Directory" row | Below the field (after "Connect to …") for non-empty text that is not a SimpleX link, until results for that text show or load; searches SimpleX Directory |
| Keyboard Search key | On Android the keyboard's Search action, on desktop Enter. The same directory search; it never looks up names |
| Results card | One group of rows without a header, continuing the contacts that match with no divider between them. First your chats among the results that the contacts list doesn't show, as contact-style chat rows (avatar, name, favourite star, incognito mask) that open the chat and close the sheet; then the other results with the contact rows' avatar size, each with "Open" (or "Already joining") and the same long-press / right-click menu as on the chat list. Open opens the prepared chat and closes the sheet, as other New chat actions do. The card ends with the load row ("Load more · N left" or "Load more") or the retry row, which is the row farthest from the field in both layouts: at the bottom with the top bar, at the top of the reversed list with the one-hand UI |
| Contacts empty state | "No filtered contacts" is not shown while the online search runs or shows results |
| Online search running | Progress indicator in the search bar; closing the sheet cancels the search |
| Directory search failed | "Search failed, tap to retry" row at the end of the results card |

While the "Directory search" switch is off, every online search asks with the same "Enable Directory search?" alert as on the chat list. Results disappear when the text changes or the user switches profile (which also closes the sheet), and when a sheet opened from New chat covers it. Archived contacts keep their own search field without online results; their Search key does nothing.

## Source Files

| File | Path |
|---|---|
| `NewChatView.kt` | `views/newchat/NewChatView.kt` |
| `AddGroupView.kt` | `views/newchat/AddGroupView.kt` |
| `QRCode.kt` | `views/newchat/QRCode.kt` |
| `NewChatSheet.kt` | `views/newchat/NewChatSheet.kt` |
| `ConnectPlan.kt` | `views/newchat/ConnectPlan.kt` |
| `QRCodeScanner.kt` | `views/newchat/QRCodeScanner.kt` (expect/actual) |
| `ContactConnectionInfoView.kt` | `views/newchat/ContactConnectionInfoView.kt` |
