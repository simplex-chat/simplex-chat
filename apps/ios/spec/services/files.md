# SimpleX Chat iOS -- File Transfer Service

> Technical specification for file transfer: inline/XFTP protocols, auto-receive thresholds, CryptoFile encryption, and file constants.
>
> Related specs: [Compose Module](../client/compose.md) | [Chat View](../client/chat-view.md) | [API Reference](../api.md) | [Database](../database.md) | [README](../README.md)
> Related product: [Product Overview](../../product/README.md)

**Source:** [`FileUtils.swift`](../../SimpleXChat/FileUtils.swift) | [`CryptoFile.swift`](../../SimpleXChat/CryptoFile.swift) | [`ChatTypes.swift`](../../SimpleXChat/ChatTypes.swift) | [`AppAPITypes.swift`](../../Shared/Model/AppAPITypes.swift) | [`SimpleXAPI.swift`](../../Shared/Model/SimpleXAPI.swift)

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

- Files up to [`MAX_IMAGE_SIZE`](../../SimpleXChat/FileUtils.swift#L18) (255KB) are base64-encoded and embedded directly in the SMP message body
- No additional protocol or server needed
- Delivered with the same reliability guarantees as regular messages
- Used primarily for compressed images

### XFTP Transfer

For files exceeding the inline threshold (up to [`MAX_FILE_SIZE_XFTP`](../../SimpleXChat/FileUtils.swift#L30) = 1GB):

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

[`MAX_FILE_SIZE_SMP`](../../SimpleXChat/FileUtils.swift#L41) (8MB) exists as a constant for larger inline transfers through SMP, used in specific scenarios.

---

## 3. Auto-Receive Thresholds

Files below certain size thresholds are automatically accepted and downloaded without user confirmation:

| Media Type | Auto-Receive Threshold | Constant | Line |
|------------|----------------------|----------|------|
| Images | 510 KB | [`MAX_IMAGE_SIZE_AUTO_RCV`](../../SimpleXChat/FileUtils.swift#L21) | [L21](../../SimpleXChat/FileUtils.swift#L21) |
| Voice messages | 510 KB | [`MAX_VOICE_SIZE_AUTO_RCV`](../../SimpleXChat/FileUtils.swift#L24) | [L24](../../SimpleXChat/FileUtils.swift#L24) |
| Video | 1023 KB | [`MAX_VIDEO_SIZE_AUTO_RCV`](../../SimpleXChat/FileUtils.swift#L27) | [L27](../../SimpleXChat/FileUtils.swift#L27) |
| Other files | Not auto-received | Requires manual acceptance | -- |

### Behavior

- When a message with a file attachment arrives:
  1. Check if file size is below the auto-receive threshold for its type
  2. If below: automatically call [`setFileToReceive(fileId:, userApprovedRelays:, encrypted:)`](../../Shared/Model/AppAPITypes.swift#L181) followed by download
  3. If above: show download button in chat item, wait for user action
  4. User manually triggers download via [`receiveFile(fileId:, userApprovedRelays:, encrypted:, inline:)`](../../Shared/Model/AppAPITypes.swift#L180)

### Relay Approval

`userApprovedRelays` parameter: when the file is hosted on relays not in the user's configured server list, the user is asked for confirmation before connecting to unknown relays.

---

## [4. File Size Constants](../../SimpleXChat/FileUtils.swift#L18)

Defined in [`SimpleXChat/FileUtils.swift`](../../SimpleXChat/FileUtils.swift):

| Constant | Value | Line |
|----------|-------|------|
| `MAX_IMAGE_SIZE` | 261,120 (255 KB) | [L18](../../SimpleXChat/FileUtils.swift#L18) |
| `MAX_IMAGE_SIZE_AUTO_RCV` | 522,240 (510 KB) | [L21](../../SimpleXChat/FileUtils.swift#L21) |
| `MAX_VOICE_SIZE_AUTO_RCV` | 522,240 (510 KB) | [L24](../../SimpleXChat/FileUtils.swift#L24) |
| `MAX_VIDEO_SIZE_AUTO_RCV` | 1,047,552 (1023 KB) | [L27](../../SimpleXChat/FileUtils.swift#L27) |
| `MAX_FILE_SIZE_XFTP` | 1,073,741,824 (1 GB) | [L30](../../SimpleXChat/FileUtils.swift#L30) |
| `MAX_FILE_SIZE_LOCAL` | Int64.max (no limit) | [L39](../../SimpleXChat/FileUtils.swift#L39) |
| `MAX_FILE_SIZE_SMP` | 8,000,000 (~7.6 MB) | [L41](../../SimpleXChat/FileUtils.swift#L41) |
| `MAX_VOICE_MESSAGE_LENGTH` | 300 s (5 min) | [L43](../../SimpleXChat/FileUtils.swift#L43) |

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
2. Image is compressed to fit within [`MAX_IMAGE_SIZE`](../../SimpleXChat/FileUtils.swift#L18) (255KB):
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
4. Maximum duration: [`MAX_VOICE_MESSAGE_LENGTH`](../../SimpleXChat/FileUtils.swift#L43) = 300 seconds (5 minutes)
5. Waveform data extracted for visualization

### Transfer

- Voice files up to [`MAX_VOICE_SIZE_AUTO_RCV`](../../SimpleXChat/FileUtils.swift#L24) (510KB) are auto-received
- Larger voice files follow standard file transfer flow
- Voice messages include waveform metadata for UI rendering

### Playback

- `CIVoiceView` / `FramedCIVoiceView` render voice messages
- Shows waveform visualization and play/pause control
- `ChatModel.stopPreviousRecPlay` ensures only one audio source plays at a time
- Playback position and progress tracked

---

## [7. CryptoFile -- At-Rest Encryption](../../SimpleXChat/ChatTypes.swift#L5086)

When [`apiSetEncryptLocalFiles(enable: true)`](../../Shared/Model/SimpleXAPI.swift#L392) is configured, files stored on the device are AES-encrypted.

### [`CryptoFile`](../../SimpleXChat/ChatTypes.swift#L5086) Type

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

> Defined in [`ChatTypes.swift` L5086](../../SimpleXChat/ChatTypes.swift#L5086) (`CryptoFile`) and [L5134](../../SimpleXChat/ChatTypes.swift#L5134) (`CryptoFileArgs`).

### Encryption Operations (C FFI)

Implemented in [`CryptoFile.swift`](../../SimpleXChat/CryptoFile.swift):

| Function | Purpose | Line |
|----------|---------|------|
| [`writeCryptoFile`](../../SimpleXChat/CryptoFile.swift#L18) | Write encrypted file, returns `CryptoFileArgs` | [L18](../../SimpleXChat/CryptoFile.swift#L18) |
| [`readCryptoFile`](../../SimpleXChat/CryptoFile.swift#L31) | Read and decrypt file, returns `Data` | [L31](../../SimpleXChat/CryptoFile.swift#L31) |
| [`encryptCryptoFile`](../../SimpleXChat/CryptoFile.swift#L54) | Encrypt existing file to new path | [L54](../../SimpleXChat/CryptoFile.swift#L54) |
| [`decryptCryptoFile`](../../SimpleXChat/CryptoFile.swift#L66) | Decrypt file to new path | [L66](../../SimpleXChat/CryptoFile.swift#L66) |

### Storage

- Encrypted files stored alongside unencrypted files in `Documents/files/`
- The `CryptoFileArgs` (key + nonce) are stored in the Haskell database, not on the filesystem
- Toggle via privacy settings: [`apiSetEncryptLocalFiles(enable:)`](../../Shared/Model/SimpleXAPI.swift#L392)

---

## [8. File Storage Paths](../../SimpleXChat/FileUtils.swift#L243)

### Directory Structure

| Function | Path | Line |
|----------|------|------|
| [`getAppFilesDirectory()`](../../SimpleXChat/FileUtils.swift#L252) | `Documents/files/` | [L252](../../SimpleXChat/FileUtils.swift#L252) |
| [`getTempFilesDirectory()`](../../SimpleXChat/FileUtils.swift#L243) | `Documents/temp_files/` | [L243](../../SimpleXChat/FileUtils.swift#L243) |
| [`getWallpaperDirectory()`](../../SimpleXChat/FileUtils.swift#L261) | `Documents/wallpapers/` | [L261](../../SimpleXChat/FileUtils.swift#L261) |
| [`getAppFilePath(_:)`](../../SimpleXChat/FileUtils.swift#L256) | `Documents/files/{filename}` | [L256](../../SimpleXChat/FileUtils.swift#L256) |
| [`getWallpaperFilePath(_:)`](../../SimpleXChat/FileUtils.swift#L265) | `Documents/wallpapers/{filename}` | [L265](../../SimpleXChat/FileUtils.swift#L265) |

```swift
func getAppFilesDirectory() -> URL    // Documents/files/
func getTempFilesDirectory() -> URL   // Documents/temp_files/
func getWallpaperDirectory() -> URL   // Documents/wallpapers/
```

### Path Management

- Downloaded files: `Documents/files/{filename}`
- Temporary files during transfer: `Documents/temp_files/`
- Wallpaper images: `Documents/wallpapers/`
- File paths are set via [`apiSetAppFilePaths(filesFolder:, tempFolder:, assetsFolder:)`](../../Shared/Model/SimpleXAPI.swift#L385) at startup

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

| Function | Purpose | Line |
|----------|---------|------|
| [`cleanupFile(_:)`](../../SimpleXChat/FileUtils.swift#L311) | Remove file associated with a chat item | [L311](../../SimpleXChat/FileUtils.swift#L311) |
| [`cleanupDirectFile(_:)`](../../SimpleXChat/FileUtils.swift#L304) | Remove file only for direct chats | [L304](../../SimpleXChat/FileUtils.swift#L304) |
| [`removeFile(_:)`](../../SimpleXChat/FileUtils.swift#L287) | Delete file at URL | [L287](../../SimpleXChat/FileUtils.swift#L287) |
| [`removeFile(_:)`](../../SimpleXChat/FileUtils.swift#L295) | Delete file by name | [L295](../../SimpleXChat/FileUtils.swift#L295) |
| [`deleteAppFiles()`](../../SimpleXChat/FileUtils.swift#L133) | Remove all app files (preserving databases) | [L133](../../SimpleXChat/FileUtils.swift#L133) |
| [`deleteAppDatabaseAndFiles()`](../../SimpleXChat/FileUtils.swift#L109) | Remove everything | [L109](../../SimpleXChat/FileUtils.swift#L109) |

- When a `ChatItem` is deleted, its associated file is deleted from disk
- When a timed message expires, its file is deleted
- `ChatModel.filesToDelete` queues files for deferred deletion
- [`deleteAppFiles()`](../../SimpleXChat/FileUtils.swift#L133) removes all files (preserving databases)
- [`deleteAppDatabaseAndFiles()`](../../SimpleXChat/FileUtils.swift#L109) removes everything

---

## [10. API Commands](../../Shared/Model/AppAPITypes.swift#L180)

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| [`receiveFile`](../../Shared/Model/AppAPITypes.swift#L180) | `fileId, userApprovedRelays, encrypted, inline` | Accept and start downloading a file | [L180](../../Shared/Model/AppAPITypes.swift#L180) |
| [`setFileToReceive`](../../Shared/Model/AppAPITypes.swift#L181) | `fileId, userApprovedRelays, encrypted` | Mark file for auto-receive (no immediate download) | [L181](../../Shared/Model/AppAPITypes.swift#L181) |
| [`cancelFile`](../../Shared/Model/AppAPITypes.swift#L182) | `fileId` | Cancel in-progress transfer | [L182](../../Shared/Model/AppAPITypes.swift#L182) |
| [`apiUploadStandaloneFile`](../../Shared/Model/AppAPITypes.swift#L192) | `userId, file: CryptoFile` | Upload file to XFTP without a chat context | [L192](../../Shared/Model/AppAPITypes.swift#L192) |
| [`apiDownloadStandaloneFile`](../../Shared/Model/AppAPITypes.swift#L193) | `userId, url, file: CryptoFile` | Download from XFTP URL | [L193](../../Shared/Model/AppAPITypes.swift#L193) |
| [`apiStandaloneFileInfo`](../../Shared/Model/AppAPITypes.swift#L194) | `url` | Get metadata for an XFTP URL | [L194](../../Shared/Model/AppAPITypes.swift#L194) |

### File Transfer Events

| Event | Description | Line |
|-------|-------------|------|
| [`rcvFileAccepted`](../../Shared/Model/AppAPITypes.swift#L1257) | Download request accepted | [L1257](../../Shared/Model/AppAPITypes.swift#L1257) |
| [`rcvFileStart`](../../Shared/Model/AppAPITypes.swift#L1259) | Download started | [L1259](../../Shared/Model/AppAPITypes.swift#L1259) |
| [`rcvFileProgressXFTP`](../../Shared/Model/AppAPITypes.swift#L1260) | Download progress (receivedSize, totalSize) | [L1260](../../Shared/Model/AppAPITypes.swift#L1260) |
| [`rcvFileComplete`](../../Shared/Model/AppAPITypes.swift#L1261) | Download complete | [L1261](../../Shared/Model/AppAPITypes.swift#L1261) |
| [`rcvFileSndCancelled`](../../Shared/Model/AppAPITypes.swift#L1263) | Sender cancelled the transfer | [L1263](../../Shared/Model/AppAPITypes.swift#L1263) |
| [`rcvFileError`](../../Shared/Model/AppAPITypes.swift#L1264) | Download failed | [L1264](../../Shared/Model/AppAPITypes.swift#L1264) |
| [`rcvFileWarning`](../../Shared/Model/AppAPITypes.swift#L1265) | Download warning (non-fatal) | [L1265](../../Shared/Model/AppAPITypes.swift#L1265) |
| [`sndFileStart`](../../Shared/Model/AppAPITypes.swift#L1267) | Upload started | [L1267](../../Shared/Model/AppAPITypes.swift#L1267) |
| [`sndFileComplete`](../../Shared/Model/AppAPITypes.swift#L1268) | Inline upload complete | [L1268](../../Shared/Model/AppAPITypes.swift#L1268) |
| [`sndFileProgressXFTP`](../../Shared/Model/AppAPITypes.swift#L1270) | XFTP upload progress (sentSize, totalSize) | [L1270](../../Shared/Model/AppAPITypes.swift#L1270) |
| [`sndFileCompleteXFTP`](../../Shared/Model/AppAPITypes.swift#L1272) | XFTP upload complete | [L1272](../../Shared/Model/AppAPITypes.swift#L1272) |
| [`sndFileRcvCancelled`](../../Shared/Model/AppAPITypes.swift#L1269) | Receiver cancelled | [L1269](../../Shared/Model/AppAPITypes.swift#L1269) |
| [`sndFileError`](../../Shared/Model/AppAPITypes.swift#L1274) | Upload failed | [L1274](../../Shared/Model/AppAPITypes.swift#L1274) |
| [`sndFileWarning`](../../Shared/Model/AppAPITypes.swift#L1275) | Upload warning (non-fatal) | [L1275](../../Shared/Model/AppAPITypes.swift#L1275) |

---

## Source Files

| File | Path | Key Definitions |
|------|------|-----------------|
| File utilities & constants | [`SimpleXChat/FileUtils.swift`](../../SimpleXChat/FileUtils.swift) | `MAX_IMAGE_SIZE`, `saveFile`, `removeFile`, `getMaxFileSize` |
| CryptoFile FFI operations | [`SimpleXChat/CryptoFile.swift`](../../SimpleXChat/CryptoFile.swift) | `writeCryptoFile`, `readCryptoFile`, `encryptCryptoFile`, `decryptCryptoFile` |
| CryptoFile / CryptoFileArgs types | [`SimpleXChat/ChatTypes.swift`](../../SimpleXChat/ChatTypes.swift) | `CryptoFile` (L4241), `CryptoFileArgs` (L4289) |
| API command definitions | [`Shared/Model/AppAPITypes.swift`](../../Shared/Model/AppAPITypes.swift) | `receiveFile`, `cancelFile`, `ChatEvent` file events |
| API implementations | [`Shared/Model/SimpleXAPI.swift`](../../Shared/Model/SimpleXAPI.swift) | `receiveFile` (L1624), `cancelFile` (L1743) |
| File view (chat item) | [`Shared/Views/Chat/ChatItem/CIFileView.swift`](../../Shared/Views/Chat/ChatItem/CIFileView.swift) | |
| Image view (chat item) | [`Shared/Views/Chat/ChatItem/CIImageView.swift`](../../Shared/Views/Chat/ChatItem/CIImageView.swift) | |
| Video view (chat item) | [`Shared/Views/Chat/ChatItem/CIVideoView.swift`](../../Shared/Views/Chat/ChatItem/CIVideoView.swift) | |
| Voice view (chat item) | [`Shared/Views/Chat/ChatItem/CIVoiceView.swift`](../../Shared/Views/Chat/ChatItem/CIVoiceView.swift) | |
| Compose file preview | [`Shared/Views/Chat/ComposeMessage/ComposeFileView.swift`](../../Shared/Views/Chat/ComposeMessage/ComposeFileView.swift) | |
| Compose image preview | [`Shared/Views/Chat/ComposeMessage/ComposeImageView.swift`](../../Shared/Views/Chat/ComposeMessage/ComposeImageView.swift) | |
| Compose voice preview | [`Shared/Views/Chat/ComposeMessage/ComposeVoiceView.swift`](../../Shared/Views/Chat/ComposeMessage/ComposeVoiceView.swift) | |
| C FFI (file encryption) | [`SimpleXChat/SimpleX.h`](../../SimpleXChat/SimpleX.h) | `chat_write_file`, `chat_read_file`, `chat_encrypt_file`, `chat_decrypt_file` |
| Haskell file logic | `../../src/Simplex/Chat/Files.hs` | -- |
| Haskell file store | `../../src/Simplex/Chat/Store/Files.hs` | -- |
