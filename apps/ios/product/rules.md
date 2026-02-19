# SimpleX Chat iOS -- Business Rules

> Business invariants enforced by the SimpleX Chat iOS app and Haskell core. Each rule states the invariant, where it is enforced, and links to the relevant spec.
>
> **Related spec:** [spec/api.md](../spec/api.md) | [spec/architecture.md](../spec/architecture.md) | [spec/state.md](../spec/state.md)

---

## Security & Privacy

### RULE-01: No user identifiers
**Rule:** The system MUST NOT assign, generate, or expose any persistent user identifier (phone number, email, username, UUID) that could be used to correlate a user across conversations.
**Enforced by:** SMP protocol design in simplexmq library; each connection uses independent unidirectional queues with no shared identifier.
**Spec:** [spec/architecture.md](../spec/architecture.md)

### RULE-02: End-to-end encryption on all messages
**Rule:** All message content MUST be encrypted end-to-end using double-ratchet (with optional post-quantum KEM). The SMP server MUST NOT have access to plaintext.
**Enforced by:** simplexmq library (`Simplex.Messaging.Crypto.Ratchet`); encryption happens before `chat_send_cmd_retry` FFI call.
**Spec:** [spec/architecture.md](../spec/architecture.md)

### RULE-03: Database encryption at rest
**Rule:** Both SQLite databases (chat and agent) MUST be encrypted with SQLCipher when the user sets a database passphrase.
**Enforced by:** `chat_migrate_init_key` in Haskell core via SQLCipher; `DatabaseEncryptionView.swift` in UI.
**Spec:** [spec/database.md](../spec/database.md)

### RULE-04: Local authentication before content access
**Rule:** When app lock is enabled, the app MUST authenticate the user (Face ID, Touch ID, or passcode) before displaying any chat content.
**Enforced by:** `LocalAuthView.swift`, `ContentView.swift` (`contentViewAccessAuthenticated` guard on `ChatModel`).
**Spec:** [spec/architecture.md](../spec/architecture.md)

### RULE-05: Incognito profiles are per-connection
**Rule:** When incognito mode is used for a connection, the generated random profile MUST be unique to that connection and MUST NOT be reused across connections.
**Enforced by:** `ProfileGenerator.hs` generates fresh profile per connection; stored on the connection entity.
**Spec:** [spec/api.md](../spec/api.md)

---

## Message Integrity

### RULE-06: Message order preservation
**Rule:** Messages within a single connection MUST be displayed in the order determined by the SMP agent's sequence numbers, not by local timestamps.
**Enforced by:** `Store/Messages.hs` (`createNewChatItem` uses agent-assigned ordering); `ItemsModel` in `ChatModel.swift` preserves this order.
**Spec:** [spec/state.md](../spec/state.md)

### RULE-07: Edited messages retain history
**Rule:** When a message is edited, the previous version MUST be preserved in `chat_item_versions` and accessible via the item info view.
**Enforced by:** `Controller.hs` (`APIUpdateChatItem`); `Store/Messages.hs` (`updateChatItem` creates version record); `ChatItemInfoView.swift` displays history.
**Spec:** [spec/api.md](../spec/api.md)

### RULE-08: Deleted messages respect deletion mode
**Rule:** `CIDeleteMode.cidmBroadcast` sends deletion to recipient; `cidmInternal` only deletes locally. Moderation deletion (`cidmInternalMark`) marks the item but retains a placeholder.
**Enforced by:** `Controller.hs` (`APIDeleteChatItem` checks `CIDeleteMode`); `MarkedDeletedItemView.swift` renders moderation placeholders.
**Spec:** [spec/api.md](../spec/api.md)

### RULE-09: Timed messages auto-delete after TTL
**Rule:** Messages with a TTL MUST be automatically deleted from local storage after the configured time-to-live expires.
**Enforced by:** `Controller.hs` (background task scheduling); `Store/Messages.hs` (TTL-based cleanup).
**Spec:** [spec/api.md](../spec/api.md)

---

## Group Integrity

### RULE-10: Role hierarchy enforcement
**Rule:** A member can only modify members with strictly lower roles. Owner > Admin > Moderator > Member > Observer.
**Enforced by:** `Controller.hs` (`APIMembersRole` validates role hierarchy); `GroupMemberInfoView.swift` restricts available actions in UI.
**Spec:** [spec/api.md](../spec/api.md)

### RULE-11: Group creator is always owner
**Rule:** The user who creates a group MUST be assigned the `GROwner` role and cannot be demoted.
**Enforced by:** `Controller.hs` (`APINewGroup`); `Store/Groups.hs` (`createNewGroup` assigns owner role).
**Spec:** [spec/api.md](../spec/api.md)

### RULE-12: Group link role assignment
**Rule:** Members joining via group link MUST receive the role configured on the link (default: `GRMember`). Only admins and owners can create group links.
**Enforced by:** `Controller.hs` (`APICreateGroupLink` takes `memberRole` parameter); `GroupLinkView.swift` UI restricts to admin+.
**Spec:** [spec/api.md](../spec/api.md)

---

## File Transfer

### RULE-13: File size limits
**Rule:** Files up to 1GB are transferred via XFTP. The system MUST reject files exceeding the configured maximum.
**Enforced by:** Haskell core (`Files.hs` checks file size); XFTP protocol enforces chunk limits.
**Spec:** [spec/services/files.md](../spec/services/files.md)

### RULE-14: File encryption at rest
**Rule:** When `privacyEncryptLocalFiles` is enabled, downloaded files MUST be encrypted locally using AES with per-file random key/nonce stored in `CryptoFile`.
**Enforced by:** `CryptoFile.swift` (`encryptCryptoFile`, `decryptCryptoFile`); `Library/Commands.hs` uses `CryptoFileArgs` for file encryption.
**Spec:** [spec/services/files.md](../spec/services/files.md)

---

## Notification Delivery

### RULE-15: Notification preview respects privacy setting
**Rule:** Notification content MUST respect `NotificationPreviewMode`: `.message` shows full content, `.contact` shows sender only, `.hidden` shows generic alert.
**Enforced by:** `Notifications.swift` (notification content creation checks `ntfPreviewModeGroupDefault`); `NotificationService.swift` (NSE content generation).
**Spec:** [spec/services/notifications.md](../spec/services/notifications.md)

### RULE-16: NSE database coordination
**Rule:** The NSE and main app MUST NOT write to the database simultaneously. File locks coordinate access.
**Enforced by:** `chat_close_store` / `chat_reopen_store` FFI calls; NSE uses short-lived database sessions.
**Spec:** [spec/architecture.md](../spec/architecture.md)

---

## Call Integrity

### RULE-17: Call encryption key exchange
**Rule:** WebRTC call encryption keys MUST be negotiated over the existing E2E encrypted SMP channel, not through any external signaling server.
**Enforced by:** `ActiveCallView.swift` sends call signaling via `apiSendCallInvitation`/`apiSendCallAnswer` which use SMP; `Call.hs` defines call protocol.
**Spec:** [spec/services/calls.md](../spec/services/calls.md)

### RULE-18: CallKit region restriction
**Rule:** CallKit MUST be disabled in regions where it is restricted (China). The app uses in-app call UI as fallback.
**Enforced by:** `CallController.swift` checks `useCallKit()` based on region; `ActiveCallView.swift` provides fallback UI.
**Spec:** [spec/services/calls.md](../spec/services/calls.md)
