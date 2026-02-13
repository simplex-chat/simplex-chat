# SimpleX Chat iOS -- File Transfer Service

> Technical specification for file transfer: inline/XFTP protocols, auto-receive thresholds, CryptoFile encryption, and file constants.
>
> Related specs: [Compose Module](../client/compose.md) | [Chat View](../client/chat-view.md) | [API Reference](../api.md) | [Database](../database.md) | [README](../README.md)
> Related product: [Product Overview](../../product/README.md)

---

## Table of Contents

1. [Overview](#1-overview)
2. [Transfer Methods](#2-transfer-methods)
3. [Auto-Receive Thresholds](#3-auto-receive-thresholds)
4. [File Size Constants](#4-file-size-constants)
5. [Image Handling](#5-image-handling)
6. [Voice Messages](#6-voice-messages)
7. [CryptoFile -- At-Rest Encryption](#7-cryptofile)
8. [File Storage Paths](#8-file-storage-paths)
9. [File Lifecycle](#9-file-lifecycle)
10. [API Commands](#10-api-commands)

---

## 1. Overview

SimpleX Chat supports two file transfer methods depending on file size:

```
File ≤ 255KB (inline)
├── Base64 encoded directly in SMP message
├── Single message delivery
└── No extra server infrastructure needed

File > 255KB up to 1GB (XFTP)
├── Encrypted and chunked
├── Uploaded to XFTP relay servers
├── Recipient downloads chunks from relays
└── Files auto-deleted from relays after download or expiry
```

All files are end-to-end encrypted. The XFTP protocol adds a second encryption layer on top of the SMP channel encryption.

---

## 2. Transfer Methods

### Inline Transfer

- Files up to `MAX_IMAGE_SIZE` (255KB) are base64-encoded and embedded directly in the SMP message body
- No additional protocol or server needed
- Delivered with the same reliability guarantees as regular messages
- Used primarily for compressed images

### XFTP Transfer

For files exceeding the inline threshold (up to `MAX_FILE_SIZE_XFTP` = 1GB):

1. **Sender side**:
   - File is AES-encrypted with a random key
   - Encrypted file is split into chunks
   - Chunks are uploaded to one or more XFTP relay servers
   - File metadata (key, chunk locations) sent to recipient via SMP message

2. **Recipient side**:
   - Receives file metadata via SMP
   - Downloads chunks from XFTP relays
   - Reassembles and decrypts the file

3. **Cleanup**:
   - XFTP relays delete chunks after download or after expiry period
   - No persistent storage on relays

### SMP Transfer (legacy)

`MAX_FILE_SIZE_SMP` (8MB) exists as a constant for larger inline transfers through SMP, used in specific scenarios.

---

## 3. Auto-Receive Thresholds

Files below certain size thresholds are automatically accepted and downloaded without user confirmation:

| Media Type | Auto-Receive Threshold | Constant |
|------------|----------------------|----------|
| Images | 510 KB | `MAX_IMAGE_SIZE_AUTO_RCV` |
| Voice messages | 510 KB | `MAX_VOICE_SIZE_AUTO_RCV` |
| Video | 1023 KB | `MAX_VIDEO_SIZE_AUTO_RCV` |
| Other files | Not auto-received | Requires manual acceptance |

### Behavior

- When a message with a file attachment arrives:
  1. Check if file size is below the auto-receive threshold for its type
  2. If below: automatically call `setFileToReceive(fileId:, userApprovedRelays:, encrypted:)` followed by download
  3. If above: show download button in chat item, wait for user action
  4. User manually triggers download via `receiveFile(fileId:, userApprovedRelays:, encrypted:, inline:)`

### Relay Approval

`userApprovedRelays` parameter: when the file is hosted on relays not in the user's configured server list, the user is asked for confirmation before connecting to unknown relays.

---

## 4. File Size Constants

Defined in `SimpleXChat/FileUtils.swift`:

```swift
// Image compression target for inline transfer
public let MAX_IMAGE_SIZE: Int64 = 261_120           // 255 KB

// Auto-receive thresholds
public let MAX_IMAGE_SIZE_AUTO_RCV: Int64 = 522_240  // 510 KB (2 * MAX_IMAGE_SIZE)
public let MAX_VOICE_SIZE_AUTO_RCV: Int64 = 522_240  // 510 KB (2 * MAX_IMAGE_SIZE)
public let MAX_VIDEO_SIZE_AUTO_RCV: Int64 = 1_047_552 // 1023 KB

// Transfer method limits
public let MAX_FILE_SIZE_XFTP: Int64 = 1_073_741_824 // 1 GB
public let MAX_FILE_SIZE_SMP: Int64 = 8_000_000       // ~7.6 MB
public let MAX_FILE_SIZE_LOCAL: Int64 = Int64.max      // No limit (local notes)

// Voice message constraints
public let MAX_VOICE_MESSAGE_LENGTH = TimeInterval(300) // 5 minutes (300 seconds)
```

---

## 5. Image Handling

### Compression Pipeline

1. User selects image (camera or photo library)
2. Image is compressed to fit within `MAX_IMAGE_SIZE` (255KB):
   - Progressive JPEG compression with decreasing quality
   - Resize if dimensions are too large
3. Compressed image is base64-encoded into the message content
4. For larger images that cannot compress to 255KB: sent via XFTP

### Display

- `CIImageView` renders images in chat bubbles with aspect-fit sizing
- Tapping opens `FullScreenMediaView` with zoom/pan/share capabilities
- Thumbnail is displayed immediately; full-size loaded on demand for XFTP images

### Animated Images

- GIFs are handled by `AnimatedImageView`
- Displayed inline with animation support

---

## 6. Voice Messages

### Recording

1. `ComposeVoiceView` manages the recording UI
2. `AudioRecPlay` handles `AVAudioRecorder` lifecycle
3. Recorded in compressed audio format
4. Maximum duration: 300 seconds (5 minutes)
5. Waveform data extracted for visualization

### Transfer

- Voice files up to `MAX_VOICE_SIZE_AUTO_RCV` (510KB) are auto-received
- Larger voice files follow standard file transfer flow
- Voice messages include waveform metadata for UI rendering

### Playback

- `CIVoiceView` / `FramedCIVoiceView` render voice messages
- Shows waveform visualization and play/pause control
- `ChatModel.stopPreviousRecPlay` ensures only one audio source plays at a time
- Playback position and progress tracked

---

## 7. CryptoFile -- At-Rest Encryption

When `apiSetEncryptLocalFiles(enable: true)` is configured, files stored on the device are AES-encrypted.

### CryptoFile Type

```swift
struct CryptoFile {
    var filePath: String
    var cryptoArgs: CryptoFileArgs?   // nil = unencrypted
}

struct CryptoFileArgs {
    var fileKey: String       // AES encryption key
    var fileNonce: String     // AES nonce/IV
}
```

### Encryption Operations (C FFI)

| Function | Purpose |
|----------|---------|
| `chat_write_file(ctrl, path, data, len)` | Write encrypted file, returns `WriteFileResult` JSON |
| `chat_read_file(path, key, nonce)` | Read and decrypt file, returns buffer |
| `chat_encrypt_file(ctrl, fromPath, toPath)` | Encrypt existing file to new path |
| `chat_decrypt_file(fromPath, key, nonce, toPath)` | Decrypt file to new path |

### Storage

- Encrypted files stored alongside unencrypted files in `Documents/files/`
- The `CryptoFileArgs` (key + nonce) are stored in the Haskell database, not on the filesystem
- Toggle via privacy settings: `apiSetEncryptLocalFiles(enable:)`

---

## 8. File Storage Paths

### Directory Structure

```swift
func getAppFilesDirectory() -> URL    // Documents/files/
func getTempFilesDirectory() -> URL   // Documents/temp_files/
func getWallpaperDirectory() -> URL   // Documents/wallpapers/
```

### Path Management

- Downloaded files: `Documents/files/{filename}`
- Temporary files during transfer: `Documents/temp_files/`
- Wallpaper images: `Documents/wallpapers/`
- File paths are set via `apiSetAppFilePaths(filesFolder:, tempFolder:, assetsFolder:)` at startup

---

## 9. File Lifecycle

### Sending

```
1. User selects file/image/video in compose
2. ComposeView creates ComposedMessage with file reference
3. apiSendMessages() → Haskell core processes:
   a. File ≤ inline threshold: base64 encode into message
   b. File > inline threshold: start XFTP upload
4. Upload events:
   - ChatEvent.sndFileStart
   - ChatEvent.sndFileProgressXFTP (periodic progress)
   - ChatEvent.sndFileCompleteXFTP (upload done)
   - ChatEvent.sndFileError (on failure)
```

### Receiving

```
1. Message with file attachment arrives
2. Auto-receive check:
   a. Below threshold: automatic download starts
   b. Above threshold: user sees download button
3. User triggers download (or auto-triggered):
   - receiveFile(fileId:, userApprovedRelays:, encrypted:, inline:)
4. Download events:
   - ChatEvent.rcvFileStart
   - ChatEvent.rcvFileProgressXFTP (periodic progress)
   - ChatEvent.rcvFileComplete (download done)
   - ChatEvent.rcvFileError (on failure)
   - ChatEvent.rcvFileSndCancelled (sender cancelled)
```

### Cancellation

```swift
ChatCommand.cancelFile(fileId: Int64)
```

Cancels an in-progress upload or download. For XFTP transfers, also requests chunk deletion from relays.

### Cleanup

- When a `ChatItem` is deleted, its associated file is deleted from disk
- When a timed message expires, its file is deleted
- `ChatModel.filesToDelete` queues files for deferred deletion
- `deleteAppFiles()` removes all files (preserving databases)
- `deleteAppDatabaseAndFiles()` removes everything

---

## 10. API Commands

| Command | Parameters | Description |
|---------|-----------|-------------|
| `receiveFile` | `fileId, userApprovedRelays, encrypted, inline` | Accept and start downloading a file |
| `setFileToReceive` | `fileId, userApprovedRelays, encrypted` | Mark file for auto-receive (no immediate download) |
| `cancelFile` | `fileId` | Cancel in-progress transfer |
| `apiUploadStandaloneFile` | `userId, file: CryptoFile` | Upload file to XFTP without a chat context |
| `apiDownloadStandaloneFile` | `userId, url, file: CryptoFile` | Download from XFTP URL |
| `apiStandaloneFileInfo` | `url` | Get metadata for an XFTP URL |

### File Transfer Events

| Event | Description |
|-------|-------------|
| `rcvFileAccepted` | Download request accepted |
| `rcvFileStart` | Download started |
| `rcvFileProgressXFTP` | Download progress (receivedSize, totalSize) |
| `rcvFileComplete` | Download complete |
| `rcvFileSndCancelled` | Sender cancelled the transfer |
| `rcvFileError` | Download failed |
| `rcvFileWarning` | Download warning (non-fatal) |
| `sndFileStart` | Upload started |
| `sndFileComplete` | Inline upload complete |
| `sndFileProgressXFTP` | XFTP upload progress (sentSize, totalSize) |
| `sndFileCompleteXFTP` | XFTP upload complete |
| `sndFileRcvCancelled` | Receiver cancelled |
| `sndFileError` | Upload failed |
| `sndFileWarning` | Upload warning (non-fatal) |

---

## Source Files

| File | Path |
|------|------|
| File utilities & constants | `SimpleXChat/FileUtils.swift` |
| File view (chat item) | `Shared/Views/Chat/ChatItem/CIFileView.swift` |
| Image view (chat item) | `Shared/Views/Chat/ChatItem/CIImageView.swift` |
| Video view (chat item) | `Shared/Views/Chat/ChatItem/CIVideoView.swift` |
| Voice view (chat item) | `Shared/Views/Chat/ChatItem/CIVoiceView.swift` |
| Compose file preview | `Shared/Views/Chat/ComposeMessage/ComposeFileView.swift` |
| Compose image preview | `Shared/Views/Chat/ComposeMessage/ComposeImageView.swift` |
| Compose voice preview | `Shared/Views/Chat/ComposeMessage/ComposeVoiceView.swift` |
| C FFI (file encryption) | `SimpleXChat/SimpleX.h` |
| Haskell file logic | `../../src/Simplex/Chat/Files.hs` |
| Haskell file store | `../../src/Simplex/Chat/Store/Files.hs` |
