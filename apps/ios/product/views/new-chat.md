# New Chat / Connection

> **Related spec:** [spec/client/navigation.md](../../spec/client/navigation.md)

## Purpose

Create new contacts, groups, or connect with others via one-time invitation links or by scanning/pasting SimpleX links. This is the primary onramp for establishing new E2E encrypted connections.

## Route / Navigation

- **Entry point**: Tap the new chat button (pencil icon) in `ChatListView` toolbar
- **Presented by**: `NewChatSheet` modal from `ChatListView`
- **Internal navigation**: `NewChatMenuButton` provides a dropdown with options:
  - "New chat" -- opens `NewChatView`
  - "Create group" -- opens `AddGroupView`
- **Tabs within NewChatView**: Segmented picker toggles between `.invite` (1-time link) and `.connect` (connect via link)
- **Swipe gesture**: Left/right swipe switches between invite and connect tabs
- **Dismiss behavior**: On dismiss, `showKeepInvitationAlert()` asks whether to keep an unused invitation link or delete it

## Page Sections

### Segmented Picker

| Tab | Icon | Description |
|---|---|---|
| 1-time link | `link` | Generate and share a one-time invitation link |
| Connect via link | `qrcode` | Scan QR code or paste a received link |

### Invite Tab (1-time Link)

Displayed when `selection == .invite`:

| Element | Description |
|---|---|
| QR code display | Generated QR code for the invitation link (`SimpleXLinkQRCode`) |
| Short/full link toggle | Switch between short and full link display |
| Share button | System share sheet for the invitation link |
| Copy button | Copy link to clipboard |
| Incognito toggle | Option to connect with a random profile |
| Loading state | `creatingLinkProgressView` spinner while `creatingConnReq` is true |
| Retry button | Shown if link creation fails |

Link creation calls `apiAddContact` which returns a `CreatedConnLink` with both `connFullLink` and optional `connShortLink`.

### Connect Tab (Connect via Link)

Displayed when `selection == .connect`:

| Element | Description |
|---|---|
| QR code scanner | Camera-based `CodeScanner` view for scanning SimpleX QR codes |
| Paste link field | Text input for pasting a SimpleX link manually |
| Connect button | Initiates connection via the pasted/scanned link |

Handled by `ConnectView` sub-view with `showQRCodeScanner` state.

### Info Sheet

Toolbar trailing button opens `AddContactLearnMore` info sheet explaining how SimpleX connections work.

### Add Group

Accessed via `NewChatMenuButton` dropdown:

| Element | Description |
|---|---|
| Group name | Required text field |
| Group image | Optional profile image picker |
| Incognito option | Create group with random profile |
| Create button | Creates group via API and navigates to group chat |

## Loading / Error States

| State | Behavior |
|---|---|
| Creating invitation | `ProgressView` spinner shown; buttons disabled |
| Link creation failure | Retry button displayed |
| Invalid link pasted | Alert shown via `NewChatViewAlert.newChatSomeAlert` |
| Connection in progress | Chat list shows pending connection entry |
| Unused invitation on dismiss | Alert: "Keep unused invitation?" with Keep/Delete options |

## Create Channel (`AddChannelView`)

Accessed via `NewChatMenuButton` dropdown: "Create channel (BETA)" with antenna icon (`antenna.radiowaves.left.and.right.circle.fill`).

### Three-Step Channel Creation Wizard

| Step | View | Description |
|---|---|---|
| 1. Profile | `profileStepView()` | Channel name input with validation, optional profile image. "Configure relays" link navigates to `NetworkAndServers`. Warning footer if no relays enabled. |
| 2. Progress | `progressStepView(_:)` | Relay connection progress: circular indicator (active/total), expandable relay list with status indicators (green=active, orange=invited/accepted, red=new). Cancel button deletes channel. |
| 3. Link | `linkStepView(_:)` | Wraps `GroupLinkView(isChannel: true)` showing the channel link for sharing. |

### Channel Creation Defaults

- History preference auto-enabled (`GroupPreference(enable: .on)`)
- Group link member role hardcoded to `.observer`
- Up to 3 random enabled relays selected from user's configured relays

### Channel Creation API

Calls `apiNewPublicGroup(incognito:relayIds:groupProfile:)` which returns `publicGroupCreated` response with group info, link, and relay list. On cancel, `apiDeleteChat` deletes the channel.

### Relay Validation

- `checkHasRelays()`: validates at least one enabled, non-deleted relay exists
- Warning footer: "Enable at least one chat relay in Network & Servers."
- `getEnabledRelays()`: filters enabled/non-deleted relays from user's server config

## Channel-Specific Connection Behavior

### Relay Link Blocking

When `planAndConnect` encounters a `.simplexLink(_, .relay, _, _)`, it shows a "Relay address" alert: "This is a chat relay address, it cannot be used to connect." Connection is blocked.

### Channel Prepare/Join Alerts

| Context | Channel behavior | Group behavior |
|---|---|---|
| Prepare alert icon | `antenna.radiowaves.left.and.right.circle.fill` | `person.2.circle.fill` |
| Prepare alert title | "Open new channel" | "Open new group" |
| Error text | "Error opening channel" | "Error opening group" |
| Own-link confirm | "This is your link for channel" with only "Open channel" + "Cancel" (no incognito/profile options) | Full incognito/profile selection |
| Known group alert | "Open channel" / "Open new channel" | "Open group" / "Open new group" |

### Pre-Join Relay Info

When preparing a channel link, `groupShortLinkInfo.groupRelays` (hostnames) are stored in `ChatModel.shared.channelRelayHostnames` for display in the subscriber relay bar before joining.

## Related Specs

- `spec/api.md` -- API commands: `APIAddContact`, `APIConnect`, `APICreateUserAddress`
- `spec/client/navigation.md` -- Navigation architecture for channel creation flow
- [Chat List](chat-list.md) -- Parent view that presents this sheet
- [Chat](chat.md) -- Navigated to after successful connection

## Source Files

- `Shared/Views/NewChat/NewChatView.swift` -- Main view with invite/connect tabs, link generation
- `Shared/Views/NewChat/NewChatMenuButton.swift` -- Dropdown menu (new chat, create group, create channel)
- `Shared/Views/NewChat/QRCode.swift` -- QR code generation and display
- `Shared/Views/NewChat/AddGroupView.swift` -- Group creation form
- `Shared/Views/NewChat/AddChannelView.swift` -- Channel creation wizard (3 steps)
- `Shared/Views/NewChat/AddContactLearnMore.swift` -- Info sheet explaining connection process
