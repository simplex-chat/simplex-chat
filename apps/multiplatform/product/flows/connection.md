# Connection Flow

> **Related spec:** [spec/client/navigation.md](../../spec/client/navigation.md) | [spec/api.md](../../spec/api.md)

## Overview

Establishing a contact connection in SimpleX Chat follows an invitation-link model. One party creates a connection link (one-time invitation or long-term address), shares it out-of-band, and the other party connects via that link. The process uses SMP queues for the handshake, with no central server involved in identity management.

Connections support incognito mode, where a random profile is used per-connection instead of the user's real profile.

## Prerequisites

- Chat is initialized and running.
- An active user profile exists.
- For connecting: a valid SimpleX connection link (invitation or address).

---

## 1. Creating a Connection Link (Inviter Side)

### 1.1 One-Time Invitation Link

1. User navigates to "New Chat" and selects "Add Contact" (or uses the "+" action).
2. `ChatController.apiAddContact(rh, incognito)` is called:

```kotlin
suspend fun apiAddContact(rh: Long?, incognito: Boolean): Pair<Pair<CreatedConnLink, PendingContactConnection>?, (() -> Unit)?>
```

3. Internally, `CC.APIAddContact(userId, incognito)` is sent to the core.
4. The core creates a new SMP queue pair and returns:
   - `CR.Invitation` with `connLinkInvitation: CreatedConnLink` and `connection: PendingContactConnection`.
5. The `CreatedConnLink` contains the invitation URI (long form and short link).
6. The link is displayed as a QR code in `NewChatView` and can be copied or shared.
7. A `PendingContactConnection` appears in the chat list while waiting.

### 1.2 Long-Term Contact Address

1. User goes to Settings and creates a SimpleX address.
2. This creates a persistent address link that multiple people can use.
3. Incoming connection requests from the address require explicit acceptance (see section 4).

---

## 2. Connecting via Link (Connector Side)

### 2.1 Preview the Connection Plan

Before connecting, the link is analyzed:

```kotlin
suspend fun apiConnectPlan(rh: Long?, connLink: String, inProgress: MutableState<Boolean>): Pair<CreatedConnLink, ConnectionPlan>?
```

1. User pastes or scans a link.
2. `apiConnectPlan` sends `CC.APIConnectPlan(userId, connLink)` to the core.
3. The core resolves short links, validates the link, and returns a `ConnectionPlan`:

```kotlin
sealed class ConnectionPlan {
  class InvitationLink(val invitationLinkPlan: InvitationLinkPlan): ConnectionPlan()
  class ContactAddress(val contactAddressPlan: ContactAddressPlan): ConnectionPlan()
  class GroupLink(val groupLinkPlan: GroupLinkPlan): ConnectionPlan()
  class Error(val chatError: ChatError): ConnectionPlan()
}
```

4. For `InvitationLinkPlan`:
   - `Ok`: Fresh invitation, safe to connect.
   - `OwnLink`: User's own link, alert shown.
   - `Connecting(contact_)`: Already connecting to this contact.
   - `Known(contact)`: Already connected, existing contact shown.

5. For `ContactAddressPlan`:
   - `Ok`: Fresh address, safe to connect.
   - `OwnLink`: User's own address.
   - `ConnectingConfirmReconnect`: Was connecting, offer to retry.
   - `ConnectingProhibit(contact)`: Connection in progress, cannot duplicate.
   - `Known(contact)`: Already a contact.

6. For `GroupLinkPlan`:
   - `Ok`: Fresh group link, safe to join.
   - `OwnLink(groupInfo)`: User's own group.
   - `ConnectingConfirmReconnect`: Was connecting, offer to retry.
   - `ConnectingProhibit(groupInfo_)`: Connection in progress.
   - `Known(groupInfo)`: Already a member.

### 2.2 High-Level Connect Flow: planAndConnect

The `planAndConnect` function in `ConnectPlan.kt` orchestrates the full connect experience:

```kotlin
suspend fun planAndConnect(
  rhId: Long?,
  shortOrFullLink: String,
  close: (() -> Unit)?,
  cleanup: (() -> Unit)? = null,
  filterKnownContact: ((Contact) -> Unit)? = null,
  filterKnownGroup: ((GroupInfo) -> Unit)? = null,
): CompletableDeferred<Boolean>
```

1. A progress indicator is shown.
2. `apiConnectPlan` is called to analyze the link.
3. Based on the plan type, the appropriate UI is shown:
   - For `Ok` plans: proceed to `apiConnect`.
   - For `Known`: navigate to the existing contact/group.
   - For `OwnLink`: show alert.
   - For `Connecting`: show reconnect confirmation or prohibit.
4. Returns a `CompletableDeferred<Boolean>` indicating success.

### 2.3 Execute Connection

```kotlin
suspend fun apiConnect(rh: Long?, incognito: Boolean, connLink: CreatedConnLink): PendingContactConnection?
```

1. `CC.APIConnect(userId, incognito, connLink)` is sent to the core.
2. The core initiates the SMP handshake:
   - For invitation links: `CR.SentConfirmation` is returned.
   - For contact addresses: `CR.SentInvitation` is returned.
3. A `PendingContactConnection` is returned and appears in the chat list.
4. The connect progress indicator is shown via `ConnectProgressManager`.

---

## 3. Connection Handshake Completion

### 3.1 For Invitation Links

1. After the connector sends confirmation, the inviter's core receives it.
2. Both sides complete the SMP handshake automatically.
3. A `CR.ContactConnected` event is received on both sides.
4. The `PendingContactConnection` in the chat list is replaced by a full `Contact`.
5. Both parties can now exchange messages.

### 3.2 For Contact Addresses

1. The connector's confirmation arrives as a `ContactRequest` on the address owner's side.
2. The address owner must explicitly accept or reject (see section 4).
3. Once accepted, the handshake completes and `CR.ContactConnected` fires.

---

## 4. Contact Request Acceptance

### 4.1 Accept a Contact Request

```kotlin
suspend fun apiAcceptContactRequest(rh: Long?, incognito: Boolean, contactReqId: Long): Contact?
```

1. The address owner sees a contact request notification in the chat list.
2. User taps to open and selects "Accept".
3. `CC.ApiAcceptContact(incognito, contactReqId)` is sent to the core.
4. The core responds with `CR.AcceptingContactRequest` and a `Contact` object.
5. The SMP handshake continues; once complete, `CR.ContactConnected` fires.
6. The `incognito` flag determines whether the real profile or a random profile is shared.

### 4.2 Reject a Contact Request

```kotlin
suspend fun apiRejectContactRequest(rh: Long?, contactReqId: Long): Contact?
```

1. User selects "Reject" on the contact request.
2. `CC.ApiRejectContact(contactReqId)` is sent to the core.
3. The core responds with `CR.ContactRequestRejected`.
4. The contact request is removed from the chat list.
5. The connector's side eventually times out or receives an error.

---

## 5. Incognito Mode

### 5.1 Per-Connection Incognito

1. The `incognito` parameter is available on both `apiAddContact` and `apiConnect`.
2. When `incognito = true`:
   - A random display name is generated for this connection.
   - The real user profile is not shared with the contact.
   - The incognito profile is stored per-connection in the database.
3. The global incognito toggle is in `AppPreferences.incognito`.
4. Incognito status is visible in the chat info view.

### 5.2 Accept with Incognito

1. When accepting a contact request with `incognito = true`, a random profile is used.
2. The accepted contact only sees the random profile.
3. The user can have some contacts with real profile and others with incognito profiles.

---

## 6. Connection Progress and UI

### 6.1 ConnectProgressManager

```kotlin
object ConnectProgressManager {
  fun startConnectProgress(text: String, onCancel: (() -> Unit)? = null)
  fun stopConnectProgress()
  fun cancelConnectProgress()
}
```

1. When a connection is initiated, `startConnectProgress` is called.
2. After a 1-second delay, a progress indicator appears if the operation is still in progress.
3. On completion (success or failure), `stopConnectProgress` is called.
4. The user can cancel via `cancelConnectProgress`.

### 6.2 Pending Connection States

While connecting, the chat list shows a `PendingContactConnection` with status:
- Waiting for the other party to scan/use the link.
- Connecting (handshake in progress).
- Connected (transitions to a full Contact chat).

---

## Key Types Reference

| Type | Location | Purpose |
|------|----------|---------|
| `CreatedConnLink` | `model/SimpleXAPI.kt` | Connection link with full URI and short link |
| `PendingContactConnection` | `model/ChatModel.kt` | In-progress connection shown in chat list |
| `ConnectionPlan` | `model/SimpleXAPI.kt` | Sealed class: InvitationLink, ContactAddress, GroupLink, Error |
| `InvitationLinkPlan` | `model/SimpleXAPI.kt` | Ok, OwnLink, Connecting, Known |
| `ContactAddressPlan` | `model/SimpleXAPI.kt` | Ok, OwnLink, ConnectingConfirmReconnect, ConnectingProhibit, Known |
| `GroupLinkPlan` | `model/SimpleXAPI.kt` | Ok, OwnLink, ConnectingConfirmReconnect, ConnectingProhibit, Known |
| `ConnectProgressManager` | `model/ChatModel.kt` | Manages connect progress indicator with timeout |
| `Contact` | `model/ChatModel.kt` | Established contact with profile, connection status |
| `ContactRequest` | `model/ChatModel.kt` | Pending inbound contact request |
