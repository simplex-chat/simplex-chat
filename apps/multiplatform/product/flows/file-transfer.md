# File Transfer Flow

> **Related spec:** [spec/services/files.md](../../spec/services/files.md)

## Overview

SimpleX Chat transfers files using two protocols based on file size: inline delivery through SMP messages for small files, and XFTP (SimpleX File Transfer Protocol) for larger files. All locally stored files can be AES-encrypted via CryptoFile. The system supports automatic receiving of small media, manual download for larger files, and cancellation at any stage.

## Prerequisites

- An active chat connection (direct contact or group).
- Sufficient storage space on the device.
- For XFTP: network connectivity to XFTP relay servers.

---

## 1. File Size Thresholds and Constants

| Constant | Value | Purpose |
|----------|-------|---------|
| `MAX_IMAGE_SIZE` | 261,120 bytes (255 KB) | Maximum inline image thumbnail size (base64 in message body) |
| `MAX_IMAGE_SIZE_AUTO_RCV` | 522,240 bytes (510 KB) | Auto-receive threshold for images |
| `MAX_VOICE_SIZE_AUTO_RCV` | 522,240 bytes (510 KB) | Auto-receive threshold for voice messages |
| `MAX_VIDEO_SIZE_AUTO_RCV` | 1,047,552 bytes (1023 KB) | Auto-receive threshold for video thumbnails |
| `MAX_FILE_SIZE_SMP` | 8,000,000 bytes (~7.6 MB) | Maximum file size for SMP inline transfer |
| `MAX_FILE_SIZE_XFTP` | 1,073,741,824 bytes (1 GB) | Maximum file size for XFTP transfer |
| `MAX_FILE_SIZE_LOCAL` | `Long.MAX_VALUE` | No limit for local files |

These constants are defined in `views/helpers/Utils.kt`.

The core decides the transfer protocol:
- Files within the SMP inline threshold are embedded directly in SMP messages.
- Files exceeding the inline threshold (up to 1 GB) use XFTP with chunked, encrypted upload/download through relay servers.

---

## 2. CryptoFile Encryption

### 2.1 Overview

When `privacyEncryptLocalFiles` is enabled (default: `true`), files stored on device are AES-GCM encrypted. The encryption/decryption is performed via JNI calls to the Haskell core.

### 2.2 Key Types

```kotlin
// model/ChatModel.kt
@Serializable
data class CryptoFileArgs(
  val fileKey: String,   // AES-256 key (base64)
  val fileNonce: String  // GCM nonce (base64)
)

@Serializable
data class CryptoFile {
  val filePath: String
  val cryptoArgs: CryptoFileArgs?  // null for unencrypted files
}
```

### 2.3 Write (Encrypt)

```kotlin
fun writeCryptoFile(path: String, data: ByteArray): CryptoFileArgs
```

1. `ChatController.getChatCtrl()` obtains the active controller handle.
2. Data is placed in a `DirectByteBuffer`.
3. `chatWriteFile(ctrl, path, buffer)` is called via JNI.
4. The core generates a random AES key and nonce, encrypts the data, writes to `path`.
5. Returns `CryptoFileArgs(fileKey, fileNonce)` needed for decryption.
6. On error, throws an exception with the error message.

### 2.4 Read (Decrypt)

```kotlin
fun readCryptoFile(path: String, cryptoArgs: CryptoFileArgs): ByteArray
```

1. `chatReadFile(path, cryptoArgs.fileKey, cryptoArgs.fileNonce)` is called via JNI.
2. Returns a two-element array: `[status: Int, data: ByteArray]`.
3. If `status == 0`, the decrypted data is returned.
4. Otherwise, an exception is thrown with the error message.

### 2.5 File-to-File Encryption

```kotlin
fun encryptCryptoFile(fromPath: String, toPath: String): CryptoFileArgs
```

Encrypts a plaintext file at `fromPath` to an encrypted file at `toPath`. Used when saving user-selected files to the app's encrypted storage.

### 2.6 File-to-File Decryption

```kotlin
fun decryptCryptoFile(fromPath: String, cryptoArgs: CryptoFileArgs, toPath: String)
```

Decrypts an encrypted file at `fromPath` to plaintext at `toPath`. Used when exporting/sharing files.

---

## 3. Sending Files

### 3.1 Attach and Send via ComposeView

1. User attaches a file via the file picker.
2. File size is validated: `fileSize <= MAX_FILE_SIZE_XFTP` (1 GB).
3. If valid, `ComposeState.preview` is set to `ComposePreview.FilePreview(fileName, uri)`.
4. If too large, an alert is shown with the maximum supported size.
5. On send, the file is copied to the app files directory.
6. If `privacyEncryptLocalFiles` is enabled, the file is encrypted via `encryptCryptoFile`, producing a `CryptoFile` with `cryptoArgs`.
7. A `ComposedMessage` is created with:
   - `fileSource`: the `CryptoFile` (path + optional cryptoArgs).
   - `msgContent`: `MsgContent.MCFile(text)` for generic files, `MsgContent.MCImage(text, thumbnail)` for images, `MsgContent.MCVideo(text, thumbnail, duration)` for videos, or `MsgContent.MCVoice(text, duration)` for voice.
8. `ChatController.apiSendMessages(...)` dispatches the message.
9. The core determines the transfer protocol and begins the upload.

### 3.2 Standalone File Upload (XFTP)

For uploading files outside of a chat message context:

```kotlin
suspend fun uploadStandaloneFile(user: UserLike, file: CryptoFile, ctrl: ChatCtrl? = null): Pair<FileTransferMeta?, String?>
```

1. `CC.ApiUploadStandaloneFile(userId, file)` is sent to the core.
2. On success, `CR.SndStandaloneFileCreated` returns a `FileTransferMeta`.
3. The meta contains a file description URI that can be shared for download.

### 3.3 Upload Progress

1. The core emits `SndFileProgressXFTP` events periodically during upload.
2. `CIFileStatus` on the chat item transitions through:
   - `SndStored` (queued)
   - `SndTransfer(sndProgress, sndTotal)` (uploading)
   - `SndComplete` (upload finished, link sent)
3. The UI updates the progress indicator on the file attachment.

---

## 4. Receiving Files

### 4.1 Auto-Receive

When `privacyAcceptImages` is enabled (default: `true`), small media files are auto-received:

1. On receiving a message with a file attachment, the auto-receive logic checks:
   - `MCImage` files with `fileSize <= MAX_IMAGE_SIZE_AUTO_RCV` (510 KB)
   - `MCVideo` files with `fileSize <= MAX_VIDEO_SIZE_AUTO_RCV` (1023 KB)
   - `MCVoice` files with `fileSize <= MAX_VOICE_SIZE_AUTO_RCV` (510 KB) and not already accepted
2. If criteria are met, `receiveFile` is called automatically.

### 4.2 Manual Receive

For files that are not auto-received:

1. The chat item shows a download button with file size info.
2. File size is validated: `fileSizeValid(file)` checks `file.fileSize <= getMaxFileSize(file.fileProtocol)`.
3. User taps the download button.
4. `ChatController.receiveFile(rhId, user, fileId, userApprovedRelays, auto)` is called:

```kotlin
suspend fun receiveFile(rhId: Long?, user: UserLike, fileId: Long, userApprovedRelays: Boolean = false, auto: Boolean = false)
```

5. This delegates to `receiveFiles` which handles relay approval:

```kotlin
suspend fun receiveFiles(rhId: Long?, user: UserLike, fileIds: List<Long>, userApprovedRelays: Boolean = false, auto: Boolean = false)
```

6. For each file, `CC.ReceiveFile(fileId, userApprovedRelays, encrypted, inline)` is sent to the core.
7. If the file requires unapproved XFTP relays, the user is prompted to approve them.
8. Relay approval errors (`FileError.Auth` with `SMP AUTH` and `PROXY BROKER`) trigger relay approval alerts.
9. Other errors are collected and shown after all files are processed.

### 4.3 Batch Receive

Multiple files can be received at once:

```kotlin
suspend fun receiveFiles(rhId: Long?, user: UserLike, fileIds: List<Long>, ...)
```

1. Iterates through all `fileIds`.
2. Files needing relay approval are batched and prompted once.
3. After approval, those files are retried with `userApprovedRelays = true`.
4. Errors for individual files are aggregated.

### 4.4 Download Progress

1. The core emits `RcvFileProgressXFTP` events during download.
2. `CIFileStatus` transitions through:
   - `RcvAccepted` (download initiated)
   - `RcvTransfer(rcvProgress, rcvTotal)` (downloading)
   - `RcvComplete` (download finished)
3. On completion, if the file is encrypted, it remains encrypted on disk with `cryptoArgs` stored in the database.
4. When the user opens/views the file, `readCryptoFile` or `decryptCryptoFile` is called on demand.

---

## 5. Cancelling a File Transfer

### 5.1 Cancel via API

```kotlin
suspend fun cancelFile(rh: Long?, user: User, fileId: Long)
```

1. `apiCancelFile(rh, fileId)` sends `CC.CancelFile(fileId)` to the core.
2. The core cancels any in-progress upload or download.
3. On success, the chat item is updated via `chatItemSimpleUpdate`.
4. `cleanupFile(chatItem)` removes any partial local files.

### 5.2 Cancel via UI

1. User long-presses a file message and selects "Cancel".
2. `cancelFileAlertDialog(fileId, cancelFile, cancelAction)` shows a confirmation dialog.
3. `CancelAction` provides the appropriate alert text based on direction (sending/receiving).
4. On confirmation, `cancelFile` is called.

### 5.3 Compose Cancel

Before sending, user can cancel the file attachment:

1. User taps the "X" on the file preview in the compose area.
2. `ComposeState.preview` is reset to `ComposePreview.NoPreview`.
3. No API call is needed since the file was not yet sent.

---

## 6. File Cleanup

1. Files pending deletion are tracked in `ChatModel.filesToDelete`.
2. When a chat item with a file is deleted, the file path is added to `filesToDelete`.
3. The actual file deletion happens asynchronously.
4. Encrypted files require no special cleanup beyond deleting the encrypted file; the key exists only in the database record.

---

## Key Types Reference

| Type | Location | Purpose |
|------|----------|---------|
| `CryptoFile` | `model/ChatModel.kt` | File reference with path and optional encryption args |
| `CryptoFileArgs` | `model/ChatModel.kt` | AES key + nonce for encrypted files |
| `WriteFileResult` | `model/CryptoFile.kt` | Result of `writeCryptoFile`: success with args or error |
| `CIFile` | `model/ChatModel.kt` | Chat item file metadata: fileId, fileName, fileSize, fileStatus, fileProtocol |
| `CIFileStatus` | `model/ChatModel.kt` | File transfer status: SndStored, SndTransfer, SndComplete, RcvInvitation, RcvAccepted, RcvTransfer, RcvComplete, etc. |
| `FileProtocol` | `model/ChatModel.kt` | Transfer protocol: XFTP, SMP, LOCAL |
| `FileTransferMeta` | `model/ChatModel.kt` | Metadata for standalone XFTP uploads |
| `ComposePreview.FilePreview` | `views/chat/ComposeView.kt` | Compose state for file attachment |
