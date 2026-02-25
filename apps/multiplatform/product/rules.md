# Business Rules -- SimpleX Chat (Android & Desktop, Kotlin Multiplatform)

This document specifies invariants enforced by the Android and Desktop (Kotlin/Compose Multiplatform) clients.

---

## Table of Contents

1. [Security (RULE-01 through RULE-05)](#1-security)
2. [Message Integrity (RULE-06 through RULE-09)](#2-message-integrity)
3. [Group Integrity (RULE-10 through RULE-13)](#3-group-integrity)
4. [File Transfer (RULE-14 through RULE-15)](#4-file-transfer)
5. [Notification Delivery (RULE-16 through RULE-17)](#5-notification-delivery)
6. [Call Integrity (RULE-18)](#6-call-integrity)

---

## 1. Security

### RULE-01: End-to-End Encryption is Mandatory

**Invariant:** Every message, file chunk, and call signaling payload MUST be encrypted end-to-end before transmission. The app MUST NOT transmit plaintext content to any relay server.

**Enforcement:** The Haskell core library handles all encryption. The Kotlin layer never constructs raw SMP messages. All communication flows through `ChatController.sendCmd()` which delegates to the FFI, ensuring the encryption layer cannot be bypassed.

**Location:** `common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt` -- `ChatController.sendCmd()`, `chatSendCmd()` FFI call

---

### RULE-02: Database Encryption at Rest

**Invariant:** The local SQLite database MUST be encrypted. A passphrase (either user-chosen or randomly generated) MUST be set before the database is operational.

**Enforcement:** On first launch, a random passphrase is generated and stored encrypted via the platform keystore (`CryptorInterface.encryptText`). The `initialRandomDBPassphrase` preference tracks whether the user has set a custom passphrase. Database encryption state is tracked in `ChatModel.chatDbEncrypted`. Encryption/re-encryption is performed via `CC.ApiStorageEncryption(config: DBEncryptionConfig)`.

**Caveat:** The user is not forced to set a custom passphrase -- the random passphrase is stored in app-accessible encrypted preferences. See GAP: "Database passphrase not enforced."

**Location:**
- `common/src/commonMain/kotlin/chat/simplex/common/views/helpers/DatabaseUtils.kt`
- `common/src/commonMain/kotlin/chat/simplex/common/platform/Cryptor.kt` -- `CryptorInterface`
- Android: `common/src/androidMain/kotlin/chat/simplex/common/platform/Cryptor.android.kt` -- Android Keystore
- Desktop: `common/src/desktopMain/kotlin/chat/simplex/common/platform/Cryptor.desktop.kt` -- **placeholder, not implemented**

---

### RULE-03: Local Authentication Gating

**Invariant:** When local authentication is enabled (`AppPreferences.performLA == true`), the app MUST require biometric/PIN authentication before displaying any chat content. The lock engages after `laLockDelay` seconds of inactivity.

**Enforcement:** `AppLock.setPerformLA` controls the lock state. The lock delay is configurable via `AppPreferences.laLockDelay` (default 30 seconds). Authentication mode is set via `AppPreferences.laMode` (system biometric or passcode).

**Location:**
- `common/src/commonMain/kotlin/chat/simplex/common/AppLock.kt`
- `SimpleXAPI.kt` -- `AppPreferences.performLA`, `AppPreferences.laMode`, `AppPreferences.laLockDelay`

---

### RULE-04: Self-Destruct Profile

**Invariant:** When self-destruct is enabled (`AppPreferences.selfDestruct == true`), entering the self-destruct passphrase instead of the real passphrase MUST wipe the database and present a clean profile with `selfDestructDisplayName`.

**Enforcement:** The self-destruct passphrase is stored separately (`encryptedSelfDestructPassphrase` / `initializationVectorSelfDestructPassphrase`). On Android, `SimplexService` checks for self-destruct on initialization. The comparison happens during the local authentication flow.

**Location:**
- `SimpleXAPI.kt` -- `AppPreferences.selfDestruct`, `AppPreferences.selfDestructDisplayName`
- `android/src/main/java/chat/simplex/app/SimplexService.kt` -- initialization check

---

### RULE-05: Screen Protection

**Invariant:** When `AppPreferences.privacyProtectScreen == true` (default), the app MUST prevent screenshots and screen recording. On Android this uses `FLAG_SECURE`; on Desktop this is advisory only.

**Enforcement:** The preference defaults to `true`. The Android activity applies `FLAG_SECURE` to its window based on this preference. The Desktop app cannot enforce this at the OS level.

**Location:** `SimpleXAPI.kt` -- `AppPreferences.privacyProtectScreen`

---

## 2. Message Integrity

### RULE-06: Message Ordering Verification

**Invariant:** The app MUST detect and surface message integrity violations (gaps, duplicates, out-of-order delivery) to the user.

**Enforcement:** The Haskell core tracks message sequence numbers per connection. When a gap or integrity error is detected, a `CIContent.RcvIntegrityError(msgError: MsgErrorType)` chat item is inserted into the conversation. The UI renders these as system messages indicating the integrity issue.

**Location:** `ChatModel.kt:3560` -- `CIContent.RcvIntegrityError`

---

### RULE-07: Decryption Error Surfacing

**Invariant:** When a message cannot be decrypted, the app MUST display a `RcvDecryptionError` item showing the error type and count of affected messages. The app MUST NOT silently drop undecryptable messages.

**Enforcement:** The Haskell core emits `CIContent.RcvDecryptionError(msgDecryptError, msgCount)` which the UI renders with an explanation and count. Ratchet re-synchronization can be triggered via `APISyncContactRatchet` / `APISyncGroupMemberRatchet`.

**Location:** `ChatModel.kt:3561` -- `CIContent.RcvDecryptionError`

---

### RULE-08: Delivery Receipt Consistency

**Invariant:** Delivery receipt settings MUST be consistent: when a user enables/disables receipts globally, the change MUST propagate to all contacts/groups (optionally clearing per-chat overrides via `clearOverrides`).

**Enforcement:** Global receipt toggle triggers `CC.SetAllContactReceipts(enable)`. Per-type settings use `CC.ApiSetUserContactReceipts` / `CC.ApiSetUserGroupReceipts` with `UserMsgReceiptSettings(enable, clearOverrides)`. The `privacyDeliveryReceiptsSet` preference gates the initial setup prompt shown during onboarding.

**Location:**
- `SimpleXAPI.kt` -- `CC.SetAllContactReceipts`, `CC.ApiSetUserContactReceipts`, `CC.ApiSetUserGroupReceipts`
- `SimpleXAPI.kt` -- `ChatController.startChat()` -- triggers `setDeliveryReceipts` prompt

---

### RULE-09: Chat Item TTL Enforcement

**Invariant:** When a chat item TTL (time-to-live) is set globally or per-chat, expired messages MUST be deleted by the core. The app MUST NOT display expired items.

**Enforcement:** Global TTL set via `CC.APISetChatItemTTL(userId, seconds)`. Per-chat TTL set via `CC.APISetChatTTL(userId, chatType, id, seconds)`. The Haskell core performs periodic cleanup. The current global TTL is stored in `ChatModel.chatItemTTL`.

**Location:** `SimpleXAPI.kt` -- `CC.APISetChatItemTTL`, `CC.APISetChatTTL`

---

## 3. Group Integrity

### RULE-10: Role-Based Access Control

**Invariant:** Group operations MUST respect the member's role. Only members with sufficient role level can perform privileged operations:
- **Owner:** can delete group, change any member's role, transfer ownership
- **Admin:** can add/remove members, change roles (up to Admin), create/delete group links
- **Moderator:** can delete other members' messages, block members
- **Member / Author / Observer:** cannot perform administrative actions

**Enforcement:** The Haskell core validates role permissions server-side. The Kotlin UI layer uses `GroupMemberRole` comparisons (the enum is ordered: Observer < Author < Member < Moderator < Admin < Owner) to show/hide action buttons.

**Location:** `ChatModel.kt:2364` -- `enum class GroupMemberRole`; various group management views

---

### RULE-11: Group Member Removal Atomicity

**Invariant:** When removing members from a group, the removal command MUST specify all member IDs atomically. Partial removal MUST NOT leave the group in an inconsistent state.

**Enforcement:** `CC.ApiRemoveMembers(groupId, memberIds: List<Long>, withMessages: Boolean)` sends all member IDs in a single command. The `withMessages` flag controls whether the removed members' messages are also deleted.

**Location:** `SimpleXAPI.kt` -- `CC.ApiRemoveMembers`

---

### RULE-12: Group Link Role Default

**Invariant:** When creating a group link, the default member role for joiners MUST be explicitly specified. The role can be updated after creation without regenerating the link.

**Enforcement:** `CC.APICreateGroupLink(groupId, memberRole)` requires a role. `CC.APIGroupLinkMemberRole(groupId, memberRole)` updates it. The link itself remains stable.

**Location:** `SimpleXAPI.kt` -- `CC.APICreateGroupLink`, `CC.APIGroupLinkMemberRole`

---

### RULE-13: Member Blocking Scope

**Invariant:** Blocking a member (`ApiBlockMembersForAll`) MUST apply the block for all group members (not just the requester). The `blocked` flag is visible to all members. Only roles >= Moderator can block.

**Enforcement:** `CC.ApiBlockMembersForAll(groupId, memberIds, blocked)` sends the block/unblock to the core, which propagates it to all group members.

**Location:** `SimpleXAPI.kt` -- `CC.ApiBlockMembersForAll`; `ChatModel.kt` -- `GroupMember.blockedByAdmin`

---

## 4. File Transfer

### RULE-14: File Encryption in Transit and at Rest

**Invariant:** Files sent via XFTP MUST be encrypted before upload. Files received MUST be decrypted only after download. When `privacyEncryptLocalFiles` is enabled (default `true`), files stored locally MUST be encrypted with per-file keys (`CryptoFile.cryptoArgs`).

**Enforcement:** The Haskell core handles XFTP encryption. Local file encryption is toggled via `CC.ApiSetEncryptLocalFiles(enable)`. The `CryptoFile` type carries optional `CryptoFileArgs` (key + nonce) for local decryption. Files are decrypted on-demand for display via `decryptCryptoFile()`.

**Location:**
- `SimpleXAPI.kt` -- `CC.ApiSetEncryptLocalFiles`, `AppPreferences.privacyEncryptLocalFiles`
- `ChatModel.kt` -- `CryptoFile`, `CryptoFileArgs`
- `RecAndPlay.desktop.kt` -- `decryptCryptoFile()` usage in audio playback

---

### RULE-15: Relay Approval for File Transfer

**Invariant:** When `privacyAskToApproveRelays` is enabled (default `true`), the app MUST prompt the user before using XFTP relay servers suggested by contacts (as opposed to the user's own configured servers). The `userApprovedRelays` flag on `CC.ReceiveFile` records the user's consent.

**Enforcement:** `CC.ReceiveFile(fileId, userApprovedRelays, encrypt, inline)` passes the approval flag. The UI prompts the user when the file is from an unapproved relay.

**Location:** `SimpleXAPI.kt` -- `CC.ReceiveFile`, `AppPreferences.privacyAskToApproveRelays`

---

## 5. Notification Delivery

### RULE-16: Background Message Delivery (Android)

**Invariant:** On Android, when `NotificationsMode.SERVICE` is selected (default), the app MUST maintain a foreground service (`SimplexService`) to ensure continuous message delivery. The service MUST survive app backgrounding and device sleep. When `NotificationsMode.PERIODIC` is selected, `MessagesFetcherWorker` MUST periodically wake and fetch messages. When `NotificationsMode.OFF`, no background delivery occurs.

**Enforcement:**
- `SimplexService` runs as a foreground service with `START_STICKY` and a `WakeLock`. It displays a persistent notification on the `SIMPLEX_SERVICE_NOTIFICATION` channel.
- `MessagesFetcherWorker` is a `PeriodicWorkRequest` scheduled via `WorkManager`.
- The mode is stored in `AppPreferences.notificationsMode` and checked at app startup.

**Location:**
- `android/src/main/java/chat/simplex/app/SimplexService.kt`
- `android/src/main/java/chat/simplex/app/MessagesFetcherWorker.kt`
- `SimpleXAPI.kt:7725` -- `enum class NotificationsMode`

---

### RULE-17: Notification Preview Privacy

**Invariant:** Notification content MUST respect `notificationPreviewMode`:
- `HIDDEN` -- notification shows no sender or message content
- `CONTACT` -- notification shows sender name only
- `MESSAGE` -- notification shows sender name and message preview

**Enforcement:** `NtfManager` (Android) reads the preview mode from `AppPreferences.notificationPreviewMode` and constructs notifications accordingly. The `CallService` also respects this mode for call notifications (showing or hiding caller identity).

**Location:**
- `android/src/main/java/chat/simplex/app/model/NtfManager.android.kt` -- `displayNotification()`, `notifyCallInvitation()`
- `android/src/main/java/chat/simplex/app/CallService.kt` -- `updateNotification()`
- `SimpleXAPI.kt` -- `AppPreferences.notificationPreviewMode`

---

## 6. Call Integrity

### RULE-18: Call Lifecycle Management

**Invariant:** An active call MUST be properly managed across the full lifecycle:
1. **Incoming calls** MUST be reported via `CallManager.reportNewIncomingCall()` which triggers a notification (and on Android, a full-screen intent for lock-screen display).
2. **Only one call** can be active at a time. Accepting a new call MUST end any existing call first (`CallManager.acceptIncomingCall` checks `activeCall` and calls `endCall` if needed, guarded by `switchingCall` flag).
3. **Call state** MUST progress through defined states: `WaitCapabilities` -> `InvitationSent`/`InvitationAccepted` -> `OfferSent`/`OfferReceived` -> `Negotiated` -> `Connected` -> `Ended`.
4. **Call end** MUST clean up all resources: send `WCallCommand.End`, call `apiEndCall`, clear `activeCall`, cancel call notifications, and release platform resources.

**Android enforcement:**
- `CallService` (foreground service) keeps the call alive in background with a `WakeLock` and ongoing notification on `CALL_SERVICE_NOTIFICATION` channel.
- `CallActivity` hosts the WebRTC WebView.
- Lock-screen behavior controlled by `AppPreferences.callOnLockScreen` (DISABLE / SHOW / ACCEPT).

**Desktop enforcement:**
- Calls run in the system browser via the NanoWSD WebSocket server on `localhost:50395`.
- The `WebRTCController` composable manages the WebSocket lifecycle.
- On dispose, `WCallCommand.End` is sent and the server is stopped.

**Location:**
- `common/src/commonMain/kotlin/chat/simplex/common/views/call/CallManager.kt`
- `common/src/commonMain/kotlin/chat/simplex/common/views/call/WebRTC.kt`
- Android: `android/src/main/java/chat/simplex/app/CallService.kt`, `android/src/main/java/chat/simplex/app/views/call/CallActivity.kt`
- Desktop: `common/src/desktopMain/kotlin/chat/simplex/common/views/call/CallView.desktop.kt`
