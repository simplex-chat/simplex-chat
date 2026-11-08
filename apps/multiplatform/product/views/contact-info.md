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
| Delete and ban | Shown when `Contact.canDeleteAndBan`; see Delete and Ban below |
| Delete contact | Removes the contact (with confirmation via `deleteContactDialog`); see Contact Deletion below |

### Contact Deletion

A contact that is ready and active, archived or not, is deleted in two dialogs:
1. "Delete contact?" (`confirmActiveContactDeletion`): the name, "Contact will be deleted - this cannot be undone!" and "You won't be able to reconnect unless <name> sends you a new link." With the privacy setting "Contact deletion" Notify it has "Delete"; with Ask it has "Delete and notify" and "Delete silently".
2. "Delete contact?" (`deleteActiveContactDialog`): "<name> will be notified." or "<name> won't be notified.", and "<name> can connect to you again via your address." when the user has an address; "Keep conversation" and "Delete" (`showKeepConversationDialog`). Keep conversation on deletes the contact and keeps the conversation (`ChatDeleteMode.Entity`); an archived contact then stays in Archived contacts. After the core confirms the deletion, "Contact deleted!" follows with "You can still view conversation with <name> in the list of chats.", or "...in Archived contacts." for an archived contact (`showDeleteContactNotice`); "Delete and ban" with Keep conversation shows the same notice. Off deletes the contact with the conversation (`ChatDeleteMode.Full`).

Any other contact gets "Confirm contact deletion?" with Confirm (`deleteNotReadyContact`).

"Archive" in the chat list menu (`archiveContact`) moves the contact to Archived contacts without clearing the conversation (`ChatDeleteMode.Archive`), with no confirmation; after the core confirms it, "Contact archived!" is shown with "You can send messages to <name> from Archived contacts.", or "You can view the conversation with <name> in Archived contacts." for a deleted contact; "Don't show again" sets `showDeleteConversationNotice`.

### Delete and Ban

Offered in contact info, in the chat list menu and in the Archived contacts menu when `Contact.canDeleteAndBan`: the contact is ready and active, connected via the user's address (`Connection.viaUserContactLink`), and supports `LINK_NOTICE_VERSION`. The contact is deleted without notification, and the contact's app does not connect via the user's address for the chosen duration (`deleteAndBanContactDialog`):
1. "Delete and ban?" (`deleteAndBanDialog`): the name, "Contact will be deleted - this cannot be undone!" and "<name> won't be notified and won't be able to connect to you while banned."; "Ban for" 1 day (default), 1 week or Forever; "Delete and ban".
2. For 1 day or 1 week, "Delete and ban?": "<name> won't be notified." and "<name> won't be able to connect to you for <duration>."; "Keep conversation" and "Delete and ban".
3. For Forever, "Delete and ban forever?": the text on the block that can't be lifted and is stored only in the contact's app, then "Type yes to confirm."; "Keep conversation", the field and "Delete and ban", enabled when the field is "yes".

The deletion is `apiDeleteContact` with `ChatDeleteMode.Entity` (Keep conversation) or `ChatDeleteMode.Full`, `notify` off, and the `LinkNotice` of the duration. A one-time link is not affected by the ban.

## Source Files

| File | Path |
|---|---|
| `ChatInfoView.kt` | `views/chat/ChatInfoView.kt` |
| `ContactPreferences.kt` | `views/chat/ContactPreferences.kt` |
| `VerifyCodeView.kt` | `views/chat/VerifyCodeView.kt` |
