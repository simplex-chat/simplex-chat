# Contact Info

## Purpose

View contact details, manage per-contact preferences, verify security codes for E2E encryption, manage connection settings, and perform destructive actions like blocking or deleting a contact.

## Route / Navigation

- **Entry point**: Tap the info button in `ChatView` navigation bar (when viewing a direct contact chat)
- **Presented by**: `NavigationView` sheet from `ChatView` via `showChatInfoSheet`
- **Sub-navigation**:
  - Contact preferences -> `ContactPreferencesView`
  - Security code verification -> `VerifyCodeView`
  - Chat wallpaper -> `ChatWallpaperEditorSheet`

## Page Sections

### Contact Info Header

| Element | Description |
|---|---|
| Profile image | Large circular avatar; tappable |
| Display name | Contact's display name |
| Full name | Optional full name below display name |
| Connection status | Shows if contact is ready, connecting, or has issues |

### Local Alias

Editable text field (`aliasTextFieldFocused`) for setting a local-only name visible only on this device. Not shared with the contact.

### Action Buttons

Horizontal row of quick-action buttons (width divided by 4):

| Button | Description |
|---|---|
| Search | Triggers `onSearch` to search messages in chat |
| Audio call | Initiate audio call (`AudioCallButton`) |
| Video call | Initiate video call (`VideoButton`) |
| Mute/Unmute | Toggle notification mode (`nextNtfMode`) |

Call buttons check `connectionStats` and show alerts if connection state prevents calling.

### Incognito Section

Shown only when `customUserProfile` is set (connected via incognito):

| Element | Description |
|---|---|
| "Your random profile" label | Shows the incognito display name used for this contact |

### Connection Settings Section

| Element | Condition | Description |
|---|---|---|
| Verify security code | `connectionCode` available | Navigate to `VerifyCodeView` for QR-based code verification |
| Contact preferences | Always | Navigate to `ContactPreferencesView` |
| Send receipts | Always | Toggle: yes / no / default(yes) / default(no) |
| Synchronize connection | `ratchetSyncAllowed` | Fix encryption ratchet desynchronization |
| Chat theme | Always | Navigate to `ChatWallpaperEditorSheet` |

All items disabled when `!contact.ready || !contact.active`.

### Chat TTL Section

| Element | Description |
|---|---|
| Chat TTL option | `ChatTTLOption` -- auto-delete timer for messages on this device |

Footer: "Delete chat messages from your device."

### Encryption Info Section

Shown when `contact.activeConn` exists:

| Element | Description |
|---|---|
| E2E encryption | "Quantum resistant" (PQ enabled) or "Standard" |

### Contact Address Section

Shown when `contact.contactLink` exists:

| Element | Description |
|---|---|
| QR code | `SimpleXLinkQRCode` displaying the contact's address |
| Share address | Share button for the contact's SimpleX address link |

Footer: "You can share this address with your contacts to let them connect with **[name]**."

### Servers Section

Shown when `contact.ready && contact.active`:

| Element | Description |
|---|---|
| Subscription status | `SubStatusRow` showing connection health; tappable for details |
| Change receiving address | Button to switch SMP receiving queue (disabled during switch) |
| Abort changing address | Button to cancel in-progress address switch |
| Receiving via | SMP server hostnames for receiving queues |
| Sending via | SMP server hostnames for sending queues |

### Danger Zone Section

| Action | Description |
|---|---|
| Clear chat | Delete all messages locally (confirmation alert) |
| Delete contact | Remove contact entirely (confirmation alert) |

### Developer Section

Shown when `developerTools` is enabled:

| Element | Description |
|---|---|
| Local name | Internal local display name |
| Database ID | API entity ID |
| Debug delivery | Button to fetch queue info via `apiContactQueueInfo` |

## Loading / Error States

| State | Behavior |
|---|---|
| Loading connection info | `apiContactInfo` and `apiGetContactCode` called on appear; stats and code populated asynchronously |
| Progress indicator | `ProgressView` overlay during TTL changes |
| Contact not ready | Settings section disabled with reduced opacity |
| Contact inactive | Settings section disabled |
| Errors | Alert with localized error title and message |

## Alerts

| Alert | Trigger |
|---|---|
| `clearChatAlert` | Tap clear chat |
| `subStatusAlert` | Tap subscription status row |
| `switchAddressAlert` | Tap change receiving address |
| `abortSwitchAddressAlert` | Tap abort address change |
| `syncConnectionForceAlert` | Force ratchet sync |
| `queueInfo` | Debug delivery results |
| `someAlert` | Various sub-component alerts |

## Related Specs

- `spec/api.md` -- Contact API commands (info, code verification, preferences, delete)
- [Chat](chat.md) -- Parent chat view
- [Group Info](group-info.md) -- Similar pattern for group info

## Source Files

- `Shared/Views/Chat/ChatInfoView.swift` -- Main contact info view with all sections
- `Shared/Views/Chat/ContactPreferencesView.swift` -- Per-contact feature preferences (timed messages, reactions, voice, calls, file transfer, full delete)
- `Shared/Views/Chat/VerifyCodeView.swift` -- Security code verification via QR scan or visual comparison
