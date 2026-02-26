# Contact Info

> **Related spec:** [spec/client/chat-view.md](../../spec/client/chat-view.md)

## Purpose

View contact details, manage per-contact preferences, verify security codes for E2E encryption, manage connection settings (switch address, sync ratchet), and perform destructive actions like clearing or deleting a contact.

## Route / Navigation

- **Entry point**: Tap the info button in `ChatView` navigation bar (when viewing a direct contact chat)
- **Presented by**: `ChatInfoView` composable shown via `ModalManager.end` from `ChatView`
- **Sub-navigation**:
  - Contact preferences -> `ContactPreferencesView` (via `ModalManager.end`)
  - Security code verification -> `VerifyCodeView` (via `ModalManager.end`)
  - Chat wallpaper -> wallpaper editor
  - Group profile view (for group-direct contacts)

## Page Sections

### Contact Info Header

| Element | Description |
|---|---|
| Profile image | Large circular avatar (tappable) |
| Display name | Contact's display name |
| Full name | Optional full name below display name |
| Connection status | Shows if contact is ready, connecting, or has issues |

### Local Alias

Editable text field for setting a local-only name visible only on this device. Not shared with the contact. Changes saved via `setContactAlias()`.

### Action Buttons

Horizontal row of quick-action buttons:

| Button | Description |
|---|---|
| Search | Triggers `onSearchClicked` to search messages in chat |
| Audio call | Initiate audio call |
| Video call | Initiate video call |
| Mute/Unmute | Toggle notification mode |

### Incognito Section

Shown only when `customUserProfile` is set (connected via incognito profile):

| Element | Description |
|---|---|
| Incognito icon | Indicates incognito connection |
| Profile name | The random profile name used for this connection |

### Chat Preferences

| Setting | Description |
|---|---|
| Send receipts | Per-contact delivery receipt setting (`SendReceipts` tristate: default/on/off) |
| Chat item TTL | Per-contact message retention setting (`ChatItemTTL` with alert confirmation) |
| Contact preferences | Opens `ContactPreferencesView` for feature toggles (timed messages, full delete, reactions, voice, calls) |

### Connection Details

Shown when `connectionStats` is available:

| Element | Description |
|---|---|
| Connection stats | Server information, agent connection ID |
| Switch address | Initiates SMP server address switch (`apiSwitchContact`) with confirmation alert |
| Abort switch | Cancels an in-progress address switch (`apiAbortSwitchContact`) |
| Sync connection | Fixes encryption ratchet synchronization (`apiSyncContactRatchet`) |
| Force sync | Force ratchet re-synchronization with confirmation alert |

### Security Code Verification

| Element | Description |
|---|---|
| Verify button | Opens `VerifyCodeView` showing the connection security code |
| Verified badge | Shows checkmark when contact is verified |
| Code comparison | Side-by-side code display for out-of-band verification via `apiVerifyContact` |

### Developer Tools Section

Shown when `developerTools` preference is enabled:

| Element | Description |
|---|---|
| Database ID | Contact's internal database identifier |
| Agent connection ID | Underlying SMP agent connection ID |

### Destructive Actions

| Action | Description |
|---|---|
| Clear chat | Deletes all messages in chat (with confirmation via `clearChatDialog`) |
| Delete contact | Removes the contact and all associated data (with confirmation via `deleteContactDialog`) |

## Source Files

| File | Path |
|---|---|
| `ChatInfoView.kt` | `views/chat/ChatInfoView.kt` |
| `ContactPreferences.kt` | `views/chat/ContactPreferences.kt` |
| `VerifyCodeView.kt` | `views/chat/VerifyCodeView.kt` |
