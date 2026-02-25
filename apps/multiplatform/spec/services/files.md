# File Transfer Service

## Table of Contents

1. [Overview](#1-overview)
2. [File Size Constants](#2-file-size-constants)
3. [CryptoFile](#3-cryptofile)
4. [File Storage Paths](#4-file-storage-paths)
5. [API Commands](#5-api-commands)
6. [Auto-Receive Logic](#6-auto-receive-logic)
7. [Source Files](#7-source-files)

## Executive Summary

SimpleX Chat uses two file transfer mechanisms: inline SMP transfers for small files (embedded in message bodies) and XFTP (eXtended File Transfer Protocol) for larger files up to 1 GB. Files are optionally encrypted at rest using `CryptoFile` functions backed by the chat core's native crypto library. File storage paths are platform-specific: Android uses `Context.dataDir`-based directories while Desktop uses platform-appropriate data directories (XDG on Linux, AppData on Windows, Application Support on macOS). Auto-receive logic automatically accepts images, voice messages, and videos below configurable size thresholds.

---

## 1. Overview

File transfer decision logic:

- **Inline (SMP)**: Files small enough to be base64-encoded and embedded directly in an SMP message body. The practical limit is defined by `MAX_IMAGE_SIZE` (255 KB) for compressed images. The maximum SMP inline size is `MAX_FILE_SIZE_SMP` (~7.6 MB).
- **XFTP**: For files exceeding the inline threshold, up to `MAX_FILE_SIZE_XFTP` (1 GB). XFTP uses dedicated file relay servers with chunked, encrypted transfers.

The `receiveFile` / `receiveFiles` API commands handle both protocols transparently -- the chat core selects the appropriate transfer mechanism based on file metadata received from the sender.

---

## 2. File Size Constants

Defined in [`Utils.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/views/helpers/Utils.kt#L118):

| Constant | Value | Human-Readable | Line | Purpose |
|---|---|---|---|---|
| `MAX_IMAGE_SIZE` | 261,120 | 255 KB | [L118](../../common/src/commonMain/kotlin/chat/simplex/common/views/helpers/Utils.kt#L118) | Inline image compression target |
| `MAX_IMAGE_SIZE_AUTO_RCV` | 522,240 | 510 KB | [L119](../../common/src/commonMain/kotlin/chat/simplex/common/views/helpers/Utils.kt#L119) | Auto-receive threshold for images (`2 * MAX_IMAGE_SIZE`) |
| `MAX_VOICE_SIZE_AUTO_RCV` | 522,240 | 510 KB | [L120](../../common/src/commonMain/kotlin/chat/simplex/common/views/helpers/Utils.kt#L120) | Auto-receive threshold for voice messages (`2 * MAX_IMAGE_SIZE`) |
| `MAX_VIDEO_SIZE_AUTO_RCV` | 1,047,552 | 1023 KB | [L121](../../common/src/commonMain/kotlin/chat/simplex/common/views/helpers/Utils.kt#L121) | Auto-receive threshold for video |
| `MAX_VOICE_MILLIS_FOR_SENDING` | 300,000 | 5 min | [L123](../../common/src/commonMain/kotlin/chat/simplex/common/views/helpers/Utils.kt#L123) | Maximum voice message duration |
| `MAX_FILE_SIZE_SMP` | 8,000,000 | ~7.6 MB | [L125](../../common/src/commonMain/kotlin/chat/simplex/common/views/helpers/Utils.kt#L125) | Maximum SMP inline file size |
| `MAX_FILE_SIZE_XFTP` | 1,073,741,824 | 1 GB | [L127](../../common/src/commonMain/kotlin/chat/simplex/common/views/helpers/Utils.kt#L127) | Maximum XFTP transfer size |
| `MAX_FILE_SIZE_LOCAL` | `Long.MAX_VALUE` | Unlimited | [L129](../../common/src/commonMain/kotlin/chat/simplex/common/views/helpers/Utils.kt#L129) | Local file protocol (no size limit) |

The `getMaxFileSize()` function ([`Utils.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/views/helpers/Utils.kt#L444)) selects the limit based on `FileProtocol`:

```kotlin
FileProtocol.XFTP -> MAX_FILE_SIZE_XFTP
FileProtocol.SMP  -> MAX_FILE_SIZE_SMP
FileProtocol.LOCAL -> MAX_FILE_SIZE_LOCAL
```

---

## 3. CryptoFile

[`CryptoFile.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/model/CryptoFile.kt) (62 lines)

Provides encrypted file I/O backed by the chat core's native cryptography (via JNI/JNA calls to `chatWriteFile`, `chatReadFile`, `chatEncryptFile`, `chatDecryptFile`).

### Data types

```kotlin
@Serializable
sealed class WriteFileResult {
  @SerialName("result") data class Result(val cryptoArgs: CryptoFileArgs): WriteFileResult()
  @SerialName("error") data class Error(val writeError: String): WriteFileResult()
}
```

`CryptoFileArgs` contains `fileKey` and `fileNonce` -- the symmetric encryption key and nonce for AES-GCM encryption.

### Functions

| Function | Line | Signature | Description |
|---|---|---|---|
| `writeCryptoFile` | [L24](../../common/src/commonMain/kotlin/chat/simplex/common/model/CryptoFile.kt#L24) | `(path: String, data: ByteArray): CryptoFileArgs` | Writes data to an encrypted file via a direct `ByteBuffer`. Returns the generated key and nonce. Requires initialized `ChatController`. |
| `readCryptoFile` | [L36](../../common/src/commonMain/kotlin/chat/simplex/common/model/CryptoFile.kt#L36) | `(path: String, cryptoArgs: CryptoFileArgs): ByteArray` | Reads and decrypts a file given its key and nonce. Returns the plaintext bytes. Throws on error (status != 0). |
| `encryptCryptoFile` | [L47](../../common/src/commonMain/kotlin/chat/simplex/common/model/CryptoFile.kt#L47) | `(fromPath: String, toPath: String): CryptoFileArgs` | Encrypts an existing plaintext file to a new encrypted file. Returns the generated key and nonce. |
| `decryptCryptoFile` | [L57](../../common/src/commonMain/kotlin/chat/simplex/common/model/CryptoFile.kt#L57) | `(fromPath: String, cryptoArgs: CryptoFileArgs, toPath: String)` | Decrypts an encrypted file to a plaintext output file. Throws on non-empty error string. |

All functions delegate to native C library functions through the chat core JNI bridge.

---

## 4. File Storage Paths

### Common expect declarations

[`Files.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt) (191 lines, commonMain)

| Property | Line | Description |
|---|---|---|
| `dataDir` | [L18](../../common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt#L18) | Root application data directory |
| `tmpDir` | [L19](../../common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt#L19) | Temporary files directory |
| `filesDir` | [L20](../../common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt#L20) | Base files directory |
| `appFilesDir` | [L21](../../common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt#L21) | Application files (chat attachments) |
| `wallpapersDir` | [L22](../../common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt#L22) | Theme wallpaper images |
| `coreTmpDir` | [L23](../../common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt#L23) | Temporary files for the chat core |
| `dbAbsolutePrefixPath` | [L24](../../common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt#L24) | Database file path prefix |
| `preferencesDir` | [L25](../../common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt#L25) | Preferences/config directory |
| `databaseExportDir` | [L35](../../common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt#L35) | Temporary DB archive storage for export |
| `remoteHostsDir` | [L37](../../common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt#L37) | Remote host connection data |

### Android implementation

[`Files.android.kt`](../../common/src/androidMain/kotlin/chat/simplex/common/platform/Files.android.kt) (80 lines)

| Property | Value |
|---|---|
| `dataDir` | `androidAppContext.dataDir` |
| `tmpDir` | `androidAppContext.getDir("temp", MODE_PRIVATE)` |
| `filesDir` | `dataDir/files` |
| `appFilesDir` | `dataDir/files/app_files` |
| `wallpapersDir` | `dataDir/files/assets/wallpapers` |
| `coreTmpDir` | `dataDir/files/temp_files` |
| `dbAbsolutePrefixPath` | `dataDir/files` |
| `preferencesDir` | `dataDir/shared_prefs` |
| `databaseExportDir` | `androidAppContext.cacheDir` |
| `remoteHostsDir` | `tmpDir/remote_hosts` |

### Desktop implementation

[`Files.desktop.kt`](../../common/src/desktopMain/kotlin/chat/simplex/common/platform/Files.desktop.kt) (117 lines)

| Property | Value |
|---|---|
| `dataDir` | `desktopPlatform.dataPath` (XDG_DATA_HOME on Linux, AppData on Windows, Application Support on macOS) |
| `tmpDir` | `java.io.tmpdir/simplex` (deleted on exit) |
| `filesDir` | `dataDir/simplex_v1_files` |
| `appFilesDir` | Same as `filesDir` |
| `wallpapersDir` | `dataDir/simplex_v1_assets/wallpapers` |
| `coreTmpDir` | `dataDir/tmp` |
| `dbAbsolutePrefixPath` | `dataDir/simplex_v1` |
| `preferencesDir` | `desktopPlatform.configPath` |
| `databaseExportDir` | Same as `tmpDir` |
| `remoteHostsDir` | `dataDir/remote_hosts` |

### Helper functions (common)

| Function | Line | Description |
|---|---|---|
| `getAppFilePath` | [L81](../../common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt#L81) | Resolves file path considering remote hosts |
| `getWallpaperFilePath` | [L91](../../common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt#L91) | Resolves wallpaper image path, creates parent directories |
| `getLoadedFilePath` | [L105](../../common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt#L105) | Returns path if file exists and is fully loaded |
| `getLoadedFileSource` | [L115](../../common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt#L115) | Returns `CryptoFile` source if file is loaded |
| `readThemeOverrides` | [L125](../../common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt#L125) | Reads theme overrides from `themes.yaml` |
| `writeThemeOverrides` | [L151](../../common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt#L151) | Atomically writes theme overrides to `themes.yaml` |
| `copyFileToFile` | [L47](../../common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt#L47) | Copies a `File` to a `URI` destination with toast feedback |
| `copyBytesToFile` | [L63](../../common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt#L63) | Copies a `ByteArrayInputStream` to a `URI` destination |

---

## 5. API Commands

Defined in [`SimpleXAPI.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt):

| Function | Line | Signature | Description |
|---|---|---|---|
| `receiveFiles` | [L1946](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1946) | `(rhId, user, fileIds, userApprovedRelays, auto)` | Receive multiple files. Sends `CC.ReceiveFile` for each ID. Handles relay approval workflow: collects unapproved files, shows alert, re-calls with `userApprovedRelays=true`. Respects `privacyEncryptLocalFiles` preference. |
| `receiveFile` | [L2062](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2062) | `(rhId, user, fileId, userApprovedRelays, auto)` | Delegates to `receiveFiles` with a single-element list. |
| `cancelFile` | [L2072](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2072) | `(rh, user, fileId)` | Cancels an in-progress file transfer (send or receive). Cleans up the local file. |
| `apiCancelFile` | [L2080](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2080) | `(rh, fileId, ctrl?)` | Low-level cancel. Returns `AChatItem?` on success (`SndFileCancelled` or `RcvFileCancelled`). |
| `uploadStandaloneFile` | [L1916](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1916) | `(user, file, ctrl?)` | Upload a standalone file (for database migration). Returns `FileTransferMeta?` with XFTP link. |
| `downloadStandaloneFile` | [L1926](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1926) | `(user, url, file, ctrl?)` | Download a standalone file from an XFTP URL. Returns `RcvFileTransfer?`. |

---

## 6. Auto-Receive Logic

Located in [`SimpleXAPI.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2696) within the `CR.NewChatItems` handler:

```kotlin
if (file != null &&
    appPrefs.privacyAcceptImages.get() &&
    ((mc is MsgContent.MCImage && file.fileSize <= MAX_IMAGE_SIZE_AUTO_RCV)
        || (mc is MsgContent.MCVideo && file.fileSize <= MAX_VIDEO_SIZE_AUTO_RCV)
        || (mc is MsgContent.MCVoice && file.fileSize <= MAX_VOICE_SIZE_AUTO_RCV
            && file.fileStatus !is CIFileStatus.RcvAccepted))
) {
    receiveFile(rhId, r.user, file.fileId, auto = true)
}
```

**Conditions for auto-receive:**

1. The `privacyAcceptImages` preference is enabled (user opt-in).
2. The content type and size match one of:
   - **Images** (`MCImage`): file size <= 510 KB (`MAX_IMAGE_SIZE_AUTO_RCV`)
   - **Video** (`MCVideo`): file size <= 1023 KB (`MAX_VIDEO_SIZE_AUTO_RCV`)
   - **Voice** (`MCVoice`): file size <= 510 KB (`MAX_VOICE_SIZE_AUTO_RCV`) AND file is not already accepted
3. The file has a non-null `file` attachment.

When `auto = true`, relay approval alerts are suppressed (the file is silently received).

---

## 7. Source Files

| File | Path | Lines | Description |
|---|---|---|---|
| `CryptoFile.kt` | [`common/src/commonMain/.../model/CryptoFile.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/model/CryptoFile.kt) | 62 | Encrypted file read/write via native crypto |
| `Files.kt` | [`common/src/commonMain/.../platform/Files.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt) | 191 | Common file path declarations, theme I/O, file helpers |
| `Files.android.kt` | [`common/src/androidMain/.../platform/Files.android.kt`](../../common/src/androidMain/kotlin/chat/simplex/common/platform/Files.android.kt) | 80 | Android file path implementations |
| `Files.desktop.kt` | [`common/src/desktopMain/.../platform/Files.desktop.kt`](../../common/src/desktopMain/kotlin/chat/simplex/common/platform/Files.desktop.kt) | 117 | Desktop file path implementations |
| `Utils.kt` | [`common/src/commonMain/.../views/helpers/Utils.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/views/helpers/Utils.kt) | -- | File size constants (L117--L128), `getMaxFileSize()` (L443) |
| `SimpleXAPI.kt` | [`common/src/commonMain/.../model/SimpleXAPI.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt) | -- | File transfer API commands (L1911--L2085), auto-receive (L2690) |
