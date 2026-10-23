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

## Channel Integrity

### RULE-19: Channel owner cannot leave own channel
**Rule:** A channel owner (`groupInfo.useRelays && groupInfo.isOwner`) who is the sole owner MUST NOT be able to leave the channel. The leave button is hidden in both swipe actions and context menu.
**Enforced by:** `ChatListNavLink.swift` (swipe/context menu guards), `GroupChatInfoView.swift` (leave button conditional).
**Spec:** [spec/client/chat-view.md](../spec/client/chat-view.md) | [spec/client/chat-list.md](../spec/client/chat-list.md)

### RULE-20: Relay members cannot be removed
**Rule:** Members with role `.relay` MUST NOT be removable through the member info UI. The remove button is hidden for relay members.
**Enforced by:** `GroupMemberInfoView.swift` (`mem.memberRole != .relay` guard on remove button).
**Spec:** [spec/client/chat-view.md](../spec/client/chat-view.md)

### RULE-21: Relay links cannot be used to connect
**Rule:** SimpleX links with path `/r` (relay addresses) MUST be rejected when users attempt to connect. An explanatory alert is shown instead.
**Enforced by:** `ContentView.swift` (`connectViaUrl_` early return for `/r` path), `NewChatView.swift` (`planAndConnect` guard for `.simplexLink(_, .relay, _, _)`).
**Spec:** [spec/client/navigation.md](../spec/client/navigation.md)

### RULE-22: Channel subscribers default to observer role
**Rule:** Members joining a channel via its link MUST receive the `.observer` role. The initial role picker is hidden for channels.
**Enforced by:** `AddChannelView.swift` (`groupLinkMemberRole: .observer` hardcoded), `GroupLinkView.swift` (role picker hidden when `isChannel`).
**Spec:** [spec/api.md](../spec/api.md)

### RULE-23: Channels default to history enabled
**Rule:** Newly created channels MUST have message history enabled by default (`GroupPreference(enable: .on)`).
**Enforced by:** `AddChannelView.swift` (`createChannel()` sets history preference).
**Spec:** [spec/api.md](../spec/api.md)

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

---

## Online Search

### RULE-24: Online results are never saved
**Rule:** Directory results MUST NOT be stored, and MUST live only as long as the search text and the active profile: any text change or profile switch clears them, and a reply that arrives after that (or after the search was cancelled) MUST NOT be shown.
**Enforced by:** `DirectorySearchView.swift` (`OnlineSearchModel.reset()` bumps `generation`; every publish checks it); `ChatListView.swift` (`ChatListView`, which owns the search text and the model, resets on `searchText` and `currentUser?.userId` changes); `NewChatMenuButton.swift` (`NewChatSheet` resets on the same changes).
**Spec:** [spec/client/chat-list.md](../spec/client/chat-list.md#11-online-search)

### RULE-25: Only group and channel links are accepted from the directory
**Rule:** A directory entry MUST be shown only when its link is exactly one SimpleX link of type group or channel; any other entry (no link, an invitation or contact link, text around the link) MUST be dropped while the rest of the page is kept. Such a link is never passed to the core, which would fetch an invitation link from its server even in a store-only plan.
**Enforced by:** `DirectorySearch.swift` (`DirectorySearchEntry.init(from:)` with `simplexLinkType`; `parseDirectorySearchResponse` decodes entries one at a time).
**Spec:** [spec/client/chat-list.md](../spec/client/chat-list.md#11-online-search)

### RULE-26: Online lookups never show alerts
**Rule:** Looking up directory entries locally MUST NOT show any alert, including the network retry alert; a failed lookup shows the entry with Open, whose online plan shows the real error.
**Enforced by:** `SimpleXAPI.swift` (`apiConnectPlan(onFailure: .log)`); `DirectorySearchView.swift` (`inProgress: BoxedValue(false)` for every lookup); `DirectorySearch.swift` (`directoryResultState` maps no plan to `.open`).
**Spec:** [spec/api.md](../spec/api.md#8-directory-search-and-connection-plans)

### RULE-27: Nothing is shown twice
**Rule:** A directory result MUST be hidden when its link refers, in the local store, to a chat shown in the local list. A result whose link refers to a chat the screen doesn't list MUST be drawn once, as that screen's chat row, which opens the chat, never with Open; two entries that refer to one chat MUST give one row, and an entry repeated across pages MUST be shown once. Only store-derived chat ids may decide this, never a field the directory wrote. When every result of the last page loaded is a chat the local list shows, the page adds no row and no note. Limits: the store-only lookup finds a group only when it was prepared from that short link, joined with `/c` and that short link, or joined with the full link the directory sends. A group joined otherwise (a full link when the directory sends the short link, an invitation, a link the owner has since replaced) appears as a result with Open beside its own chat row; for an invited member, or one who joined through a replaced link, Open prepares and opens a second copy of the group without asking.
**Enforced by:** `DirectorySearchView.swift` (`OnlineSearchModel.rows(shownChatIds:)`; `request` drops entries already shown); `DirectorySearch.swift` (`planLocalChatId`, `directoryResultState`); `ChatListView.swift` (`directoryRows`, `chatListNavLink`); `NewChatMenuButton.swift` (`onlineSearchSection`); `ContactListNavLink.swift` (`groupNavLink`).
**Spec:** [spec/client/chat-list.md](../spec/client/chat-list.md#11-online-search)

### RULE-28: Consent before the first online search
**Rule:** The search text MUST NOT be sent online while the "Directory search" switch in Privacy settings is off, unless the user confirms the "Enable Directory search?" alert, which says the text goes to SimpleX Directory and turns the switch on. Cancel MUST send nothing. The search never resolves SimpleX names, so the directory is its only recipient.
**Enforced by:** `DirectorySearchView.swift` (`confirmDirectorySearch`, `DEFAULT_PRIVACY_DIRECTORY_SEARCH`); `PrivacySettings.swift` (the switch); both triggers in `ChatListView.swift` and `NewChatMenuButton.swift` go through it, and so do the load and retry rows (`DirectoryLoadMoreRow`, `DirectoryRetryRow`).
**Spec:** [spec/client/chat-list.md](../spec/client/chat-list.md#11-online-search)
