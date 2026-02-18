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

- Files up to [`MAX_IMAGE_SIZE`](../../SimpleXChat/FileUtils.swift#L17) (255KB) are base64-encoded and embedded directly in the SMP message body
- No additional protocol or server needed
- Delivered with the same reliability guarantees as regular messages
- Used primarily for compressed images

### XFTP Transfer

For files exceeding the inline threshold (up to [`MAX_FILE_SIZE_XFTP`](../../SimpleXChat/FileUtils.swift#L25) = 1GB):

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

[`MAX_FILE_SIZE_SMP`](../../SimpleXChat/FileUtils.swift#L29) (8MB) exists as a constant for larger inline transfers through SMP, used in specific scenarios.

---

## 3. Auto-Receive Thresholds

Files below certain size thresholds are automatically accepted and downloaded without user confirmation:

| Media Type | Auto-Receive Threshold | Constant | Line |
|------------|----------------------|----------|------|
| Images | 510 KB | [`MAX_IMAGE_SIZE_AUTO_RCV`](../../SimpleXChat/FileUtils.swift#L19) | [L19](../../SimpleXChat/FileUtils.swift#L19) |
| Voice messages | 510 KB | [`MAX_VOICE_SIZE_AUTO_RCV`](../../SimpleXChat/FileUtils.swift#L21) | [L21](../../SimpleXChat/FileUtils.swift#L21) |
| Video | 1023 KB | [`MAX_VIDEO_SIZE_AUTO_RCV`](../../SimpleXChat/FileUtils.swift#L23) | [L23](../../SimpleXChat/FileUtils.swift#L23) |
| Other files | Not auto-received | Requires manual acceptance | -- |

### Behavior

- When a message with a file attachment arrives:
  1. Check if file size is below the auto-receive threshold for its type
  2. If below: automatically call [`setFileToReceive(fileId:, userApprovedRelays:, encrypted:)`](../../Shared/Model/AppAPITypes.swift#L167) followed by download
  3. If above: show download button in chat item, wait for user action
  4. User manually triggers download via [`receiveFile(fileId:, userApprovedRelays:, encrypted:, inline:)`](../../Shared/Model/AppAPITypes.swift#L166)

### Relay Approval

`userApprovedRelays` parameter: when the file is hosted on relays not in the user's configured server list, the user is asked for confirmation before connecting to unknown relays.

---

## [4. File Size Constants](../../SimpleXChat/FileUtils.swift#L17)

Defined in [`SimpleXChat/FileUtils.swift`](../../SimpleXChat/FileUtils.swift):

| Constant | Value | Line |
|----------|-------|------|
| `MAX_IMAGE_SIZE` | 261,120 (255 KB) | [L17](../../SimpleXChat/FileUtils.swift#L17) |
| `MAX_IMAGE_SIZE_AUTO_RCV` | 522,240 (510 KB) | [L19](../../SimpleXChat/FileUtils.swift#L19) |
| `MAX_VOICE_SIZE_AUTO_RCV` | 522,240 (510 KB) | [L21](../../SimpleXChat/FileUtils.swift#L21) |
| `MAX_VIDEO_SIZE_AUTO_RCV` | 1,047,552 (1023 KB) | [L23](../../SimpleXChat/FileUtils.swift#L23) |
| `MAX_FILE_SIZE_XFTP` | 1,073,741,824 (1 GB) | [L25](../../SimpleXChat/FileUtils.swift#L25) |
| `MAX_FILE_SIZE_LOCAL` | Int64.max (no limit) | [L27](../../SimpleXChat/FileUtils.swift#L27) |
| `MAX_FILE_SIZE_SMP` | 8,000,000 (~7.6 MB) | [L29](../../SimpleXChat/FileUtils.swift#L29) |
| `MAX_VOICE_MESSAGE_LENGTH` | 300 s (5 min) | [L31](../../SimpleXChat/FileUtils.swift#L31) |

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
2. Image is compressed to fit within [`MAX_IMAGE_SIZE`](../../SimpleXChat/FileUtils.swift#L17) (255KB):
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
4. Maximum duration: [`MAX_VOICE_MESSAGE_LENGTH`](../../SimpleXChat/FileUtils.swift#L31) = 300 seconds (5 minutes)
5. Waveform data extracted for visualization

### Transfer

- Voice files up to [`MAX_VOICE_SIZE_AUTO_RCV`](../../SimpleXChat/FileUtils.swift#L21) (510KB) are auto-received
- Larger voice files follow standard file transfer flow
- Voice messages include waveform metadata for UI rendering

### Playback

- `CIVoiceView` / `FramedCIVoiceView` render voice messages
- Shows waveform visualization and play/pause control
- `ChatModel.stopPreviousRecPlay` ensures only one audio source plays at a time
- Playback position and progress tracked

---

## [7. CryptoFile -- At-Rest Encryption](../../SimpleXChat/ChatTypes.swift#L4238)

When [`apiSetEncryptLocalFiles(enable: true)`](../../Shared/Model/SimpleXAPI.swift#L373) is configured, files stored on the device are AES-encrypted.

### [`CryptoFile`](../../SimpleXChat/ChatTypes.swift#L4238) Type

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

> Defined in [`ChatTypes.swift` L4238](../../SimpleXChat/ChatTypes.swift#L4238) (`CryptoFile`) and [L4285](../../SimpleXChat/ChatTypes.swift#L4285) (`CryptoFileArgs`).

### Encryption Operations (C FFI)

Implemented in [`CryptoFile.swift`](../../SimpleXChat/CryptoFile.swift):

| Function | Purpose | Line |
|----------|---------|------|
| [`writeCryptoFile`](../../SimpleXChat/CryptoFile.swift#L17) | Write encrypted file, returns `CryptoFileArgs` | [L17](../../SimpleXChat/CryptoFile.swift#L17) |
| [`readCryptoFile`](../../SimpleXChat/CryptoFile.swift#L29) | Read and decrypt file, returns `Data` | [L29](../../SimpleXChat/CryptoFile.swift#L29) |
| [`encryptCryptoFile`](../../SimpleXChat/CryptoFile.swift#L51) | Encrypt existing file to new path | [L51](../../SimpleXChat/CryptoFile.swift#L51) |
| [`decryptCryptoFile`](../../SimpleXChat/CryptoFile.swift#L62) | Decrypt file to new path | [L62](../../SimpleXChat/CryptoFile.swift#L62) |

### Storage

- Encrypted files stored alongside unencrypted files in `Documents/files/`
- The `CryptoFileArgs` (key + nonce) are stored in the Haskell database, not on the filesystem
- Toggle via privacy settings: [`apiSetEncryptLocalFiles(enable:)`](../../Shared/Model/SimpleXAPI.swift#L373)

---

## [8. File Storage Paths](../../SimpleXChat/FileUtils.swift#L187)

### Directory Structure

| Function | Path | Line |
|----------|------|------|
| [`getAppFilesDirectory()`](../../SimpleXChat/FileUtils.swift#L195) | `Documents/files/` | [L195](../../SimpleXChat/FileUtils.swift#L195) |
| [`getTempFilesDirectory()`](../../SimpleXChat/FileUtils.swift#L187) | `Documents/temp_files/` | [L187](../../SimpleXChat/FileUtils.swift#L187) |
| [`getWallpaperDirectory()`](../../SimpleXChat/FileUtils.swift#L203) | `Documents/wallpapers/` | [L203](../../SimpleXChat/FileUtils.swift#L203) |
| [`getAppFilePath(_:)`](../../SimpleXChat/FileUtils.swift#L199) | `Documents/files/{filename}` | [L199](../../SimpleXChat/FileUtils.swift#L199) |
| [`getWallpaperFilePath(_:)`](../../SimpleXChat/FileUtils.swift#L207) | `Documents/wallpapers/{filename}` | [L207](../../SimpleXChat/FileUtils.swift#L207) |

```swift
func getAppFilesDirectory() -> URL    // Documents/files/
func getTempFilesDirectory() -> URL   // Documents/temp_files/
func getWallpaperDirectory() -> URL   // Documents/wallpapers/
```

### Path Management

- Downloaded files: `Documents/files/{filename}`
- Temporary files during transfer: `Documents/temp_files/`
- Wallpaper images: `Documents/wallpapers/`
- File paths are set via [`apiSetAppFilePaths(filesFolder:, tempFolder:, assetsFolder:)`](../../Shared/Model/SimpleXAPI.swift#L367) at startup

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
| [`cleanupFile(_:)`](../../SimpleXChat/FileUtils.swift#L249) | Remove file associated with a chat item | [L249](../../SimpleXChat/FileUtils.swift#L249) |
| [`cleanupDirectFile(_:)`](../../SimpleXChat/FileUtils.swift#L243) | Remove file only for direct chats | [L243](../../SimpleXChat/FileUtils.swift#L243) |
| [`removeFile(_:)`](../../SimpleXChat/FileUtils.swift#L227) | Delete file at URL | [L227](../../SimpleXChat/FileUtils.swift#L227) |
| [`removeFile(_:)`](../../SimpleXChat/FileUtils.swift#L235) | Delete file by name | [L235](../../SimpleXChat/FileUtils.swift#L235) |
| [`deleteAppFiles()`](../../SimpleXChat/FileUtils.swift#L97) | Remove all app files (preserving databases) | [L97](../../SimpleXChat/FileUtils.swift#L97) |
| [`deleteAppDatabaseAndFiles()`](../../SimpleXChat/FileUtils.swift#L76) | Remove everything | [L76](../../SimpleXChat/FileUtils.swift#L76) |

- When a `ChatItem` is deleted, its associated file is deleted from disk
- When a timed message expires, its file is deleted
- `ChatModel.filesToDelete` queues files for deferred deletion
- [`deleteAppFiles()`](../../SimpleXChat/FileUtils.swift#L97) removes all files (preserving databases)
- [`deleteAppDatabaseAndFiles()`](../../SimpleXChat/FileUtils.swift#L76) removes everything

---

## [10. API Commands](../../Shared/Model/AppAPITypes.swift#L166)

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| [`receiveFile`](../../Shared/Model/AppAPITypes.swift#L166) | `fileId, userApprovedRelays, encrypted, inline` | Accept and start downloading a file | [L166](../../Shared/Model/AppAPITypes.swift#L166) |
| [`setFileToReceive`](../../Shared/Model/AppAPITypes.swift#L167) | `fileId, userApprovedRelays, encrypted` | Mark file for auto-receive (no immediate download) | [L167](../../Shared/Model/AppAPITypes.swift#L167) |
| [`cancelFile`](../../Shared/Model/AppAPITypes.swift#L168) | `fileId` | Cancel in-progress transfer | [L168](../../Shared/Model/AppAPITypes.swift#L168) |
| [`apiUploadStandaloneFile`](../../Shared/Model/AppAPITypes.swift#L178) | `userId, file: CryptoFile` | Upload file to XFTP without a chat context | [L178](../../Shared/Model/AppAPITypes.swift#L178) |
| [`apiDownloadStandaloneFile`](../../Shared/Model/AppAPITypes.swift#L179) | `userId, url, file: CryptoFile` | Download from XFTP URL | [L179](../../Shared/Model/AppAPITypes.swift#L179) |
| [`apiStandaloneFileInfo`](../../Shared/Model/AppAPITypes.swift#L180) | `url` | Get metadata for an XFTP URL | [L180](../../Shared/Model/AppAPITypes.swift#L180) |

### File Transfer Events

| Event | Description | Line |
|-------|-------------|------|
| [`rcvFileAccepted`](../../Shared/Model/AppAPITypes.swift#L1090) | Download request accepted | [L1090](../../Shared/Model/AppAPITypes.swift#L1090) |
| [`rcvFileStart`](../../Shared/Model/AppAPITypes.swift#L1092) | Download started | [L1092](../../Shared/Model/AppAPITypes.swift#L1092) |
| [`rcvFileProgressXFTP`](../../Shared/Model/AppAPITypes.swift#L1093) | Download progress (receivedSize, totalSize) | [L1093](../../Shared/Model/AppAPITypes.swift#L1093) |
| [`rcvFileComplete`](../../Shared/Model/AppAPITypes.swift#L1094) | Download complete | [L1094](../../Shared/Model/AppAPITypes.swift#L1094) |
| [`rcvFileSndCancelled`](../../Shared/Model/AppAPITypes.swift#L1096) | Sender cancelled the transfer | [L1096](../../Shared/Model/AppAPITypes.swift#L1096) |
| [`rcvFileError`](../../Shared/Model/AppAPITypes.swift#L1097) | Download failed | [L1097](../../Shared/Model/AppAPITypes.swift#L1097) |
| [`rcvFileWarning`](../../Shared/Model/AppAPITypes.swift#L1098) | Download warning (non-fatal) | [L1098](../../Shared/Model/AppAPITypes.swift#L1098) |
| [`sndFileStart`](../../Shared/Model/AppAPITypes.swift#L1100) | Upload started | [L1100](../../Shared/Model/AppAPITypes.swift#L1100) |
| [`sndFileComplete`](../../Shared/Model/AppAPITypes.swift#L1101) | Inline upload complete | [L1101](../../Shared/Model/AppAPITypes.swift#L1101) |
| [`sndFileProgressXFTP`](../../Shared/Model/AppAPITypes.swift#L1103) | XFTP upload progress (sentSize, totalSize) | [L1103](../../Shared/Model/AppAPITypes.swift#L1103) |
| [`sndFileCompleteXFTP`](../../Shared/Model/AppAPITypes.swift#L1105) | XFTP upload complete | [L1105](../../Shared/Model/AppAPITypes.swift#L1105) |
| [`sndFileRcvCancelled`](../../Shared/Model/AppAPITypes.swift#L1102) | Receiver cancelled | [L1102](../../Shared/Model/AppAPITypes.swift#L1102) |
| [`sndFileError`](../../Shared/Model/AppAPITypes.swift#L1107) | Upload failed | [L1107](../../Shared/Model/AppAPITypes.swift#L1107) |
| [`sndFileWarning`](../../Shared/Model/AppAPITypes.swift#L1108) | Upload warning (non-fatal) | [L1108](../../Shared/Model/AppAPITypes.swift#L1108) |

---

## Source Files

| File | Path | Key Definitions |
|------|------|-----------------|
| File utilities & constants | [`SimpleXChat/FileUtils.swift`](../../SimpleXChat/FileUtils.swift) | `MAX_IMAGE_SIZE`, `saveFile`, `removeFile`, `getMaxFileSize` |
| CryptoFile FFI operations | [`SimpleXChat/CryptoFile.swift`](../../SimpleXChat/CryptoFile.swift) | `writeCryptoFile`, `readCryptoFile`, `encryptCryptoFile`, `decryptCryptoFile` |
| CryptoFile / CryptoFileArgs types | [`SimpleXChat/ChatTypes.swift`](../../SimpleXChat/ChatTypes.swift) | `CryptoFile` (L4238), `CryptoFileArgs` (L4285) |
| API command definitions | [`Shared/Model/AppAPITypes.swift`](../../Shared/Model/AppAPITypes.swift) | `receiveFile`, `cancelFile`, `ChatEvent` file events |
| API implementations | [`Shared/Model/SimpleXAPI.swift`](../../Shared/Model/SimpleXAPI.swift) | `receiveFile` (L1459), `cancelFile` (L1577) |
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
