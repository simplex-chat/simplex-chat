# Connection Flow

> **Related spec:** [spec/api.md](../../spec/api.md) | [spec/architecture.md](../../spec/architecture.md)

## Overview

Establishing contact between two SimpleX Chat users. SimpleX uses no user identifiers; connections are formed through one-time invitation links or permanent SimpleX addresses. Each connection creates unique unidirectional SMP queues, ensuring no server can correlate sender and receiver. Supports incognito mode for per-contact random profile generation.

## Prerequisites

- User profile created and chat engine running
- Network connectivity to SMP relay servers
- For QR code scanning: camera permission granted

## Step-by-Step Processes

### 1. Create Invitation Link

1. User taps "+" button in `ChatListView` -> `NewChatMenuButton` -> "Add contact".
2. `NewChatView` is presented.
3. Calls `apiAddContact(incognito:)`:
   ```swift
   func apiAddContact(incognito: Bool) async
       -> ((CreatedConnLink, PendingContactConnection)?, Alert?)
   ```
4. Internally sends `ChatCommand.apiAddContact(userId:incognito:)` to core.
5. Core creates SMP queues and returns `ChatResponse1.invitation(user, connLinkInv, connection)`.
6. Returns `(CreatedConnLink, PendingContactConnection)`.
7. `CreatedConnLink` contains the invitation URI (both full and short link forms).
8. UI displays:
   - QR code rendered by `QRCode` view (scannable by peer)
   - Share button to send link via system share sheet
   - Copy button for clipboard
9. A `PendingContactConnection` appears in the chat list while awaiting peer.

### 2. Connect via Link

1. User receives a SimpleX link (pasted, scanned, or opened via URL scheme).
2. If opened via deep link: `SimpleXApp.onOpenURL` sets `chatModel.appOpenUrl`.
3. For manual entry: User pastes link in `NewChatView`.
4. First, `apiConnectPlan(connLink:inProgress:)` is called to validate:
   ```swift
   func apiConnectPlan(connLink: String, inProgress: BoxedValue<Bool>) async
       -> ((CreatedConnLink, ConnectionPlan)?, Alert?)
   ```
5. Returns `ConnectionPlan` indicating whether it is an invitation, contact address, or group link, and whether connection is already established.
6. If valid, calls `apiConnect(incognito:connLink:)`:
   ```swift
   func apiConnect(incognito: Bool, connLink: CreatedConnLink) async
       -> (ConnReqType, PendingContactConnection)?
   ```
7. Core creates the connection and returns one of:
   - `ChatResponse1.sentConfirmation(user, connection)` -- for invitation links (type: `.invitation`)
   - `ChatResponse1.sentInvitation(user, connection)` -- for contact address links (type: `.contact`)
   - `ChatResponse1.contactAlreadyExists(user, contact)` -- duplicate
8. `PendingContactConnection` appears in chat list while awaiting peer confirmation.

### 3. Prepared Contact/Group Flow (Short Links)

1. For short links with embedded profile data, the app uses a two-phase flow.
2. `apiPrepareContact(connLink:contactShortLinkData:)` or `apiPrepareGroup(connLink:groupShortLinkData:)` creates a local prepared chat.
3. Returns `ChatData` with the prepared contact/group shown in UI before connecting.
4. User can switch profiles or set incognito before committing.
5. `apiConnectPreparedContact(contactId:incognito:msg:)` finalizes the connection.
6. Returns `ChatResponse1.startedConnectionToContact(user, contact)`.

### 4. Accept Contact Request

1. When a peer connects via the user's SimpleX address, core generates a `ChatEvent.receivedContactRequest`.
2. `processReceivedMsg` handles the event, adding a `UserContactRequest` to `ChatModel`.
3. Contact request appears in `ChatListView` as a special `ContactRequestView` row.
4. User taps "Accept":
   ```swift
   func apiAcceptContactRequest(incognito: Bool, contactReqId: Int64) async -> Contact?
   ```
5. Sends `ChatCommand.apiAcceptContact(incognito:contactReqId:)`.
6. Core returns `ChatResponse1.acceptingContactRequest(user, contact)`.
7. Connection handshake proceeds asynchronously.
8. User can also reject: `apiRejectContactRequest(contactReqId:)` -> `ChatResponse1.contactRequestRejected`.

### 5. Connection Established

1. Both sides complete the SMP handshake asynchronously.
2. Core sends `ChatEvent.contactConnected(user, contact, userCustomProfile)`.
3. `processReceivedMsg` updates `ChatModel`:
   - Contact status transitions from pending to active.
   - Chat becomes available for messaging.
4. `NtfManager` may post a notification: "Contact connected".
5. The `PendingContactConnection` in the chat list is replaced by the full contact chat.

### 6. Create SimpleX Address

1. User navigates to Settings or taps "Create SimpleX address" during onboarding.
2. Calls `apiCreateUserAddress()`:
   ```swift
   func apiCreateUserAddress() async throws -> CreatedConnLink?
   ```
3. Core creates a permanent address (unlike one-time invitations).
4. Address is stored in `ChatModel.shared.userAddress`.
5. Can be shared publicly; multiple contacts can connect via the same address.
6. User must accept each incoming contact request individually.
7. To delete: `apiDeleteUserAddress()` removes the address and associated SMP queues.

### 7. Incognito Connection

1. Before connecting, user toggles "Incognito" in the connection UI.
2. `incognito: true` is passed to `apiAddContact`, `apiConnect`, or `apiAcceptContactRequest`.
3. Core generates a random display name for this connection only.
4. The random profile is stored per-connection; the user's real profile is never shared.
5. Incognito status is shown with a mask icon in the chat.
6. Can also be toggled for pending connections via `apiSetConnectionIncognito(connId:incognito:)`.

## Data Structures

| Type | Location | Description |
|------|----------|-------------|
| `CreatedConnLink` | `SimpleXChat/APITypes.swift` | Contains `connFullLink` (URI) and optional `connShortLink` |
| `PendingContactConnection` | `SimpleXChat/ChatTypes.swift` | Represents an in-progress connection before contact is established |
| `ConnectionPlan` | `Shared/Model/AppAPITypes.swift` | Enum describing what a link will do: connect contact, join group, already connected, etc. |
| `ConnReqType` | `Shared/Views/NewChat/NewChatView.swift` | `.invitation`, `.contact`, or `.groupLink` -- type of connection request |
| `Contact` | `SimpleXChat/ChatTypes.swift` | Full contact model with profile, connection status, preferences |
| `UserContactRequest` | `SimpleXChat/ChatTypes.swift` | Incoming contact request awaiting acceptance |
| `ChatType` | `SimpleXChat/ChatTypes.swift` | `.direct`, `.group`, `.local`, `.contactRequest`, `.contactConnection` |

## Error Cases

| Error | Cause | Handling |
|-------|-------|----------|
| `ChatError.invalidConnReq` | Malformed or expired link | Alert: "Invalid connection link" |
| `ChatError.unsupportedConnReq` | Link requires newer app version | Alert: "Unsupported connection link" |
| `ChatError.errorAgent(.SMP(_, .AUTH))` | Link already used or deleted | Alert: "Connection error (AUTH)" |
| `ChatError.errorAgent(.SMP(_, .BLOCKED(info)))` | Server operator blocked connection | Alert: "Connection blocked" with reason |
| `ChatError.errorAgent(.SMP(_, .QUOTA))` | Too many undelivered messages | Alert: "Undelivered messages" |
| `ChatError.errorAgent(.INTERNAL("SEUniqueID"))` | Duplicate connection attempt | Alert: "Already connected?" |
| `ChatError.errorAgent(.BROKER(_, .TIMEOUT))` | Server timeout | Retryable via `chatApiSendCmdWithRetry` |
| `ChatError.errorAgent(.BROKER(_, .NETWORK))` | Network failure | Retryable via `chatApiSendCmdWithRetry` |
| `contactAlreadyExists` | Connecting to existing contact | Alert: "Contact already exists" with contact name |
| `errorAgent(.SMP(_, .AUTH))` on accept | Sender deleted request | Alert: "Sender may have deleted the connection request" |

## Key Files

| File | Purpose |
|------|---------|
| `Shared/Views/NewChat/NewChatView.swift` | Main connection UI: create link, paste link, QR scan |
| `Shared/Views/NewChat/NewChatMenuButton.swift` | "+" button menu in chat list |
| `Shared/Views/NewChat/QRCode.swift` | QR code rendering for invitation links |
| `Shared/Views/NewChat/AddContactLearnMore.swift` | Help text explaining connection process |
| `Shared/Views/ChatList/ContactRequestView.swift` | Incoming contact request display |
| `Shared/Views/ChatList/ContactConnectionView.swift` | Pending connection display |
| `Shared/Views/ChatList/ContactConnectionInfo.swift` | Connection details sheet |
| `Shared/Model/SimpleXAPI.swift` | API functions: `apiAddContact`, `apiConnect`, `apiConnectPlan`, `apiAcceptContactRequest`, `apiCreateUserAddress` |
| `Shared/Model/AppAPITypes.swift` | `ConnectionPlan` enum, `GroupLink` struct |
| `SimpleXChat/APITypes.swift` | `CreatedConnLink`, `ComposedMessage`, command/response types |
| `SimpleXChat/ChatTypes.swift` | `Contact`, `PendingContactConnection`, `UserContactRequest` |

## Related Specifications

- `apps/ios/product/README.md` -- Product overview: Contacts capability map
- `apps/ios/product/flows/messaging.md` -- Messaging after connection is established
