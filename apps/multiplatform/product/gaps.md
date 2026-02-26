# Known Gaps & Recommendations -- SimpleX Chat (Android & Desktop, Kotlin Multiplatform)

This document catalogs known gaps in the multiplatform codebase (Android and Desktop) with severity, impact, and recommendations.

---

## Table of Contents

1. [UI: Error Feedback](#gap-01-ui-error-feedback)
2. [UI: Loading States](#gap-02-ui-loading-states)
3. [Security: Database Passphrase Not Enforced](#gap-03-security-database-passphrase-not-enforced)
4. [Security: No Forward Secrecy Indicator](#gap-04-security-no-forward-secrecy-indicator)
5. [Documentation: Haskell Store Layer Not Fully Specified](#gap-05-documentation-haskell-store-layer-not-fully-specified)
6. [Desktop: Recording Not Implemented](#gap-06-desktop-recording-not-implemented)
7. [Desktop: Cryptor Not Implemented](#gap-07-desktop-cryptor-not-implemented)

---

## GAP-01: UI Error Feedback

**Severity:** Medium
**Category:** UI / UX
**Platforms:** Android, Desktop

### Description

Many API calls through `ChatController.sendCmd()` return `API.Error` responses that are logged but not surfaced to the user. The general pattern is:

```kotlin
val r = sendCmd(rh, cmd)
if (r is API.Result && r.res is CR.ExpectedResponse) return r.res.value
Log.e(TAG, "someFunction bad response: ${r.responseType} ${r.details}")
return null
```

When the call fails, the caller receives `null` and either silently does nothing or shows a generic error. The specific `ChatError` details (which may contain actionable information like quota exceeded, server unreachable, or store errors) are lost to the user.

### Affected Locations

- `SimpleXAPI.kt` -- `getAgentSubsTotal()`, `getAgentServersSummary()`, and dozens of similar `api*` functions
- Throughout the codebase wherever `sendCmd` results are pattern-matched

### Impact

Users experience silent failures with no indication of what went wrong. This is particularly problematic for:
- Connection attempts that fail due to network issues
- File transfer failures
- Group operations that fail due to role permissions
- Server configuration errors

### Recommendation

1. Introduce a structured error-handling utility that maps `ChatError` subtypes to user-visible messages, similar to how `retryableNetworkErrorAlert` already handles a subset of `AgentErrorType.BROKER` errors.
2. At minimum, surface a dismissible snackbar/toast with a summary when an API call fails unexpectedly.
3. For critical operations (send message, join group, create connection), show a dialog with retry/cancel options (the `sendCmdWithRetry` pattern already exists for some cases -- extend it).

---

## GAP-02: UI Loading States

**Severity:** Low-Medium
**Category:** UI / UX
**Platforms:** Android, Desktop

### Description

Several long-running operations lack loading indicators, leaving the user uncertain whether the action is in progress. The `ComposeState.inProgress` flag and `progressByTimeout` mechanism exist for the compose area, and `ConnectProgressManager` handles connection progress, but many other flows have no visual feedback.

### Affected Locations

- Group member list loading (`ChatModel.membersLoaded` exists but is not always checked before displaying stale data)
- Server configuration validation (`ApiValidateServers` can take several seconds with no indicator)
- Database export/import (`ApiExportArchive`, `ApiImportArchive`)
- Profile switching (`changeActiveUser_` acquires `changingActiveUserMutex` but the UI may appear frozen)

### Impact

Users may tap actions multiple times, causing duplicate requests, or assume the app is frozen and force-quit during a long operation like database export.

### Recommendation

1. Introduce a centralized `ProgressOverlay` composable that can be shown/hidden via a `ChatModel` flag.
2. Wrap all operations that acquire `changingActiveUserMutex` or take > 1 second with a visible loading state.
3. Use `ChatModel.switchingUsersAndHosts` (which already exists) more consistently as a gate for showing a blocking progress indicator.

---

## GAP-03: Security: Database Passphrase Not Enforced

**Severity:** High
**Category:** Security
**Platforms:** Android, Desktop

### Description

When the app is first installed, a random database passphrase is generated and stored in encrypted preferences. The user is never required to set a custom passphrase. The `initialRandomDBPassphrase` flag tracks this state, and a setup prompt exists in onboarding (`SetupDatabasePassphrase`), but the user can skip it.

On Android, the encrypted passphrase is stored via the Android Keystore, which provides hardware-backed security. On Desktop, the `Cryptor` is a **placeholder** (see GAP-07), meaning the passphrase is stored in plaintext.

### Affected Locations

- `SimpleXAPI.kt` -- `AppPreferences.storeDBPassphrase`, `AppPreferences.initialRandomDBPassphrase`, `AppPreferences.encryptedDBPassphrase`
- `common/src/commonMain/kotlin/chat/simplex/common/views/helpers/DatabaseUtils.kt`
- `common/src/commonMain/kotlin/chat/simplex/common/views/onboarding/SetupDatabasePassphrase.kt`

### Impact

- Users who skip passphrase setup rely entirely on device security. If the device is compromised, the database can be decrypted using the stored passphrase.
- On Desktop, the passphrase is effectively stored in plaintext (see GAP-07), meaning anyone with filesystem access can read the database.

### Recommendation

1. Consider making passphrase setup mandatory during onboarding (or at least prominently warn users who skip it).
2. On Desktop, implement proper key storage (GAP-07) before any passphrase enforcement is meaningful.
3. Add a periodic reminder for users who still have `initialRandomDBPassphrase == true`.

---

## GAP-04: Security: No Forward Secrecy Indicator

**Severity:** Medium
**Category:** Security / UI
**Platforms:** Android, Desktop

### Description

The double-ratchet algorithm provides forward secrecy per message, and PQ key exchange provides resistance to quantum attacks. The `Connection` type tracks `pqSupport`, `pqEncryption`, `pqSndEnabled`, and `pqRcvEnabled`. However, the UI does not prominently display the current forward secrecy state or PQ encryption status for a given conversation.

### Affected Locations

- `ChatModel.kt` -- `Connection.pqSupport`, `Connection.pqEncryption`, `Connection.pqSndEnabled`, `Connection.pqRcvEnabled`
- Contact info views, group member info views

### Impact

Users cannot easily verify whether their conversations are using PQ-enhanced encryption. Security-conscious users have no visual indicator of the ratchet state or whether PQ key exchange was successful.

### Recommendation

1. Add a security badge/icon in the chat header or contact info screen showing:
   - Whether PQ key exchange is active (both peers support it)
   - Whether the connection has been verified (security code comparison)
   - The ratchet state (in-sync vs. needs re-sync)
2. The `connectionCode` field on `Connection` can be used to show verification status.
3. The `Call.encryptionStatus` pattern (used in call views) could be adapted for the chat view.

---

## GAP-05: Documentation: Haskell Store Layer Not Fully Specified

**Severity:** Medium
**Category:** Documentation / Architecture
**Platforms:** Android, Desktop

### Description

The Kotlin client communicates with the Haskell core via a text-based command protocol (`CC.cmdString` -> FFI -> Haskell). The Haskell store layer (SQLite operations, migration logic, and the exact semantics of `StoreError` variants) is not documented from the Kotlin side. The `ChatErrorStore` error type wraps a `StoreError` whose variants are defined in Haskell and deserialized by the Kotlin client, but the conditions under which each error occurs are not specified.

### Affected Locations

- `SimpleXAPI.kt:6986` -- `ChatErrorStore(storeError: StoreError)`
- `SimpleXAPI.kt` -- `StoreError` sealed class (deserialized from Haskell responses)
- `SimpleXAPI.kt` -- `ChatErrorDatabase(databaseError: DatabaseError)` for migration errors

### Impact

- Developers cannot predict which `StoreError` will occur for a given operation without reading the Haskell source.
- Error handling in the Kotlin layer is necessarily generic since the error semantics are not specified.
- Migration failures (`ChatErrorDatabase`) are particularly opaque.

### Recommendation

1. Create a specification document mapping each `CC` command to its possible `StoreError` / `DatabaseError` responses.
2. Document the database migration versioning scheme and the conditions under which `confirmDBUpgrades` is triggered.
3. Add inline documentation to the `StoreError` sealed class variants explaining their trigger conditions.

---

## GAP-06: Desktop: Recording Not Implemented

**Severity:** High
**Category:** Feature / Platform
**Platform:** Desktop only

### Description

The `RecorderNative` class on Desktop is a placeholder. Both `start()` and `stop()` are stubbed with `/*LALAL*/` comments and return dummy values (empty string and 0, respectively). Users cannot record voice messages on Desktop.

```kotlin
// common/src/desktopMain/kotlin/chat/simplex/common/platform/RecAndPlay.desktop.kt
actual class RecorderNative: RecorderInterface {
  override fun start(onProgressUpdate: (position: Int?, finished: Boolean) -> Unit): String {
    /*LALAL*/
    return ""
  }

  override fun stop(): Int {
    /*LALAL*/
    return 0
  }
}
```

Audio playback IS implemented on Desktop (via VLC/`vlcj` library), so received voice messages can be played. Only recording is missing.

### Affected Locations

- `common/src/desktopMain/kotlin/chat/simplex/common/platform/RecAndPlay.desktop.kt:15-25`
- `common/src/commonMain/kotlin/chat/simplex/common/platform/RecAndPlay.kt` -- `RecorderInterface`

### Impact

Desktop users cannot send voice messages. The record button either does nothing or produces a zero-length file.

### Recommendation

1. Implement `RecorderNative` using a JVM audio capture library (e.g., `javax.sound.sampled`, or integrate with the existing `vlcj` dependency for capture).
2. The output format should match the mobile app's voice message format (likely Opus in an OGG container) for cross-platform compatibility.
3. Until implemented, the record button should be hidden or disabled on Desktop with a tooltip explaining the limitation.

### Additional Desktop LALAL Placeholders

Several other Desktop features are also marked with `LALAL` placeholders:
- **QR Code Scanner** (`QRCodeScanner.desktop.kt:12`) -- scanning QR codes is not implemented on Desktop
- **Animated Drawables** (`Utils.desktop.kt:179`) -- animated image support (e.g., GIF in-line rendering) is not implemented
- **Animated Chat Images** (`CIImageView.desktop.kt:19`) -- animated image rendering in chat items
- **isImage detection** (`Images.desktop.kt:168`) -- image type detection (implemented but marked as incomplete)

---

## GAP-07: Desktop: Cryptor Not Implemented

**Severity:** Critical
**Category:** Security / Platform
**Platform:** Desktop only

### Description

The `CryptorInterface` implementation on Desktop is a non-functional placeholder. All three methods are stubbed:

```kotlin
// common/src/desktopMain/kotlin/chat/simplex/common/platform/Cryptor.desktop.kt
actual val cryptor: CryptorInterface = object : CryptorInterface {
  override fun decryptData(data: ByteArray, iv: ByteArray, alias: String): String? {
    return String(data) // LALAL
  }

  override fun encryptText(text: String, alias: String): Pair<ByteArray, ByteArray> {
    return text.toByteArray() to text.toByteArray() // LALAL
  }

  override fun deleteKey(alias: String) {
    // LALAL
  }
}
```

- `decryptData` returns the data as-is (no decryption)
- `encryptText` returns the plaintext as both "encrypted data" and "IV"
- `deleteKey` is a no-op

### Affected Locations

- `common/src/desktopMain/kotlin/chat/simplex/common/platform/Cryptor.desktop.kt`
- `common/src/commonMain/kotlin/chat/simplex/common/platform/Cryptor.kt` -- `CryptorInterface`
- `common/src/commonMain/kotlin/chat/simplex/common/views/helpers/DatabaseUtils.kt` -- uses `cryptor` for passphrase encryption

### Impact

**This is a critical security gap.** On Desktop:
- The database passphrase is stored **in plaintext** in the preferences file. Anyone with read access to the user's home directory can extract the passphrase and decrypt the database.
- The self-destruct passphrase is similarly stored in plaintext.
- The app passphrase (for local authentication) provides no real protection.
- Key deletion is a no-op, so "deleting" a key has no effect.

This directly undermines RULE-02 (Database Encryption at Rest) and RULE-04 (Self-Destruct Profile) on the Desktop platform.

### Recommendation

1. **Priority: Critical.** Implement proper key storage on Desktop using one of:
   - **OS Keychain integration:** macOS Keychain, Windows Credential Manager, Linux Secret Service (via `libsecret`/GNOME Keyring/KWallet)
   - **Java Cryptography Architecture (JCA)** with a PKCS#12 keystore file protected by a master password
   - **Bouncy Castle** library for platform-independent key management
2. Until a real implementation exists, display a prominent warning to Desktop users that their database passphrase is not securely stored.
3. Consider requiring the user to enter their passphrase on each app launch (do not store it) as an interim measure.

### Related

- GAP-03 (Database Passphrase Not Enforced) is compounded by this gap on Desktop.
- The `testCrypto()` function referenced in `AppCommon.desktop.kt:39` is commented out with a `// LALAL` marker, suggesting crypto testing was planned but never completed.
