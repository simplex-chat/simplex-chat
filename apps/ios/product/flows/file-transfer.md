# File Transfer Flow

## Overview

File and media sharing in SimpleX Chat iOS. Small files are sent inline within SMP messages; large files use the XFTP (eXtended File Transfer Protocol) for chunked, encrypted uploads up to 1GB. All files are encrypted end-to-end. Optional local encryption protects downloaded files at rest using AES via `CryptoFile`.

## Prerequisites

- Established contact or group conversation
- For sending: photo library or file picker access permission
- For receiving: sufficient device storage
- XFTP relay servers configured (default servers or custom)

## Size Limits

| Category | Limit | Constant |
|----------|-------|----------|
| Inline image (compressed) | 255 KB | `MAX_IMAGE_SIZE` = 261,120 bytes |
| Auto-receive image | 510 KB | `MAX_IMAGE_SIZE_AUTO_RCV` = MAX_IMAGE_SIZE * 2 |
| Auto-receive voice | 510 KB | `MAX_VOICE_SIZE_AUTO_RCV` = MAX_IMAGE_SIZE * 2 |
| Auto-receive video | 1,023 KB | `MAX_VIDEO_SIZE_AUTO_RCV` = 1,047,552 bytes |
| Max file via XFTP | 1 GB | `MAX_FILE_SIZE_XFTP` = 1,073,741,824 bytes |
| Max file via SMP | ~8 MB | `MAX_FILE_SIZE_SMP` = 8,000,000 bytes |
| Max voice message length | 5 min | `MAX_VOICE_MESSAGE_LENGTH` = 300s |

## Step-by-Step Processes

### 1. Send Image

1. User taps the attachment button in `ComposeView` and selects an image.
2. `ComposeImageView` displays the selected image preview.
3. Image is compressed to fit within `MAX_IMAGE_SIZE` (255KB).
4. `ComposedMessage` is built:
   ```swift
   ComposedMessage(
       fileSource: CryptoFile(filePath: compressedImagePath),
       msgContent: .image(text: captionText, image: base64Thumbnail)
   )
   ```
5. `apiSendMessages(type:id:scope:composedMessages:)` is called.
6. For images <=255KB: sent inline within the SMP message.
7. For larger images: XFTP upload is used (see XFTP transfer below).
8. Recipient auto-receives images up to 510KB (`MAX_IMAGE_SIZE_AUTO_RCV`).

### 2. Send Video

1. User picks a video from the library.
2. Thumbnail is generated from the first frame.
3. Video duration is calculated.
4. `ComposedMessage` is built:
   ```swift
   ComposedMessage(
       fileSource: CryptoFile(filePath: videoFilePath),
       msgContent: .video(text: captionText, image: base64Thumbnail, duration: durationSeconds)
   )
   ```
5. `apiSendMessages(...)` is called.
6. Video files are typically >255KB, so XFTP upload is used.
7. Recipient auto-receives videos up to 1,023KB (`MAX_VIDEO_SIZE_AUTO_RCV`).
8. `CIVideoView` displays thumbnail with play button; video downloads on tap if not auto-received.

### 3. Send File

1. User taps the attachment button and selects a document via the system file picker.
2. `ComposeFileView` shows the file name and size.
3. `ComposedMessage` is built:
   ```swift
   ComposedMessage(
       fileSource: CryptoFile(filePath: filePath),
       msgContent: .file(fileName)
   )
   ```
4. `apiSendMessages(...)` is called.
5. If file <=255KB: sent inline via SMP.
6. If file >255KB and <=1GB: uploaded via XFTP.
7. Files >1GB: rejected (prevented in UI).
8. `CIFileView` displays file icon, name, and size for the recipient.

### 4. Send Voice Message

1. User taps and holds the microphone button in `ComposeView`.
2. `AudioRecPlay` records audio to a temporary file.
3. `ComposeVoiceView` shows recording waveform and duration.
4. On release (or tapping stop), recording ends.
5. Duration is checked against `MAX_VOICE_MESSAGE_LENGTH` (5 minutes / 300 seconds).
6. `ComposedMessage` is built:
   ```swift
   ComposedMessage(
       fileSource: CryptoFile(filePath: voiceFilePath),
       msgContent: .voice(text: "", duration: durationSeconds)
   )
   ```
7. `apiSendMessages(...)` is called.
8. Voice messages <=510KB are sent inline.
9. Recipient auto-receives voice up to 510KB (`MAX_VOICE_SIZE_AUTO_RCV`).
10. `CIVoiceView` renders waveform with playback controls.

### 5. Receive File

1. Core receives a message with a file reference via SMP.
2. `ChatEvent.newChatItems` delivers the chat item with file metadata.
3. Auto-receive logic checks:
   - File type and size against auto-receive thresholds.
   - User's auto-receive preferences.
4. If auto-received or user taps "Download":
   ```swift
   func receiveFile(user: any UserLike, fileId: Int64, userApprovedRelays: Bool = false, auto: Bool = false) async
   ```
5. Internally calls `receiveFiles(user:fileIds:userApprovedRelays:auto:)`.
6. Sends `ChatCommand.receiveFile(fileId:userApprovedRelays:encrypted:inline:)`.
7. `encrypted` is determined by `privacyEncryptLocalFilesGroupDefault`.
8. `userApprovedRelays` controls whether unknown XFTP relay servers are trusted.
9. On success: `ChatResponse2.rcvFileAccepted(user, chatItem)` -- file download begins.
10. On sender cancelled: `ChatResponse2.rcvFileAcceptedSndCancelled(user, rcvFileTransfer)`.
11. Download progress is tracked and shown in the UI.
12. Completed files are stored in the app's `Documents/files/` directory.

### 6. XFTP Transfer (Large Files)

**Upload (sender side):**
1. File is encrypted locally with a random symmetric key.
2. Encrypted file is split into chunks.
3. Chunks are uploaded to one or more XFTP relay servers.
4. A file description (URI with encryption key and chunk locations) is created.
5. The file description is sent to the recipient via the SMP message.

**Download (recipient side):**
1. Recipient receives the file description via SMP.
2. Chunks are downloaded from XFTP relay servers.
3. Chunks are reassembled and decrypted locally.
4. File is available at the local path.

**Standalone file operations** (used for database migration):
- `uploadStandaloneFile(user:file:ctrl:)` -- upload without a chat message
- `downloadStandaloneFile(user:url:file:ctrl:)` -- download from a standalone URL
- `standaloneFileInfo(url:ctrl:)` -- get metadata for a standalone file URL

### 7. Local File Encryption

1. If `privacyEncryptLocalFilesGroupDefault` is enabled in privacy settings:
   - Downloaded files are encrypted at rest using AES via `CryptoFile`.
   - `CryptoFile` wraps a file path with encryption metadata.
2. Encryption key is derived and stored securely.
3. Files are decrypted on-the-fly when accessed for viewing/playback.
4. This protects files even if the device storage is accessed externally.

### 8. Unknown Relay Server Approval

1. When receiving a file, XFTP relay servers are checked against known/approved servers.
2. If unknown servers are detected: `ChatError.error(.fileNotApproved(fileId, unknownServers))`.
3. If not auto-receiving, user is shown an alert:
   - "Unknown servers! Without Tor or VPN, your IP address will be visible to these XFTP relays: [server list]."
   - Option to "Download" (approve) or cancel.
4. On approval: `receiveFiles(user:fileIds:userApprovedRelays: true)` retries with approval.
5. If `privacyAskToApproveRelaysGroupDefault` is disabled, relays are auto-approved.

## Data Structures

| Type | Location | Description |
|------|----------|-------------|
| `CryptoFile` | `SimpleXChat/CryptoFile.swift` | File path with optional encryption key and nonce for local AES encryption |
| `MsgContent.image` | `SimpleXChat/ChatTypes.swift` | `.image(text: String, image: String)` -- text caption + base64 thumbnail |
| `MsgContent.video` | `SimpleXChat/ChatTypes.swift` | `.video(text: String, image: String, duration: Int)` -- caption + thumbnail + duration |
| `MsgContent.voice` | `SimpleXChat/ChatTypes.swift` | `.voice(text: String, duration: Int)` -- empty text + duration in seconds |
| `MsgContent.file` | `SimpleXChat/ChatTypes.swift` | `.file(String)` -- file name |
| `ComposedMessage` | `SimpleXChat/APITypes.swift` | Outgoing message with fileSource, quotedItemId, msgContent, mentions |
| `FileTransferMeta` | `SimpleXChat/ChatTypes.swift` | Metadata for an ongoing file transfer |
| `RcvFileTransfer` | `SimpleXChat/ChatTypes.swift` | State of a file being received |
| `MigrationFileLinkData` | Used for standalone file transfers during database migration |

## Error Cases

| Error | Cause | Handling |
|-------|-------|----------|
| `fileNotApproved(fileId, unknownServers)` | Unknown XFTP relay servers | Alert with option to approve and retry |
| `fileCancelled` | File transfer was cancelled | Silently ignored in `receiveFiles` |
| `fileAlreadyReceiving` | Duplicate receive request | Silently ignored |
| `rcvFileAcceptedSndCancelled` | Sender cancelled after acceptance | Alert: "Sender cancelled file transfer" |
| File too large | Exceeds 1GB XFTP limit | Prevented in UI picker |
| Network errors | XFTP server unreachable | Standard retry mechanism |
| Storage full | Insufficient device storage | System-level error |

## Key Files

| File | Purpose |
|------|---------|
| `SimpleXChat/FileUtils.swift` | File size constants, path utilities, database file management |
| `SimpleXChat/CryptoFile.swift` | Local file encryption/decryption with AES |
| `SimpleXChat/ImageUtils.swift` | Image compression and thumbnail generation |
| `Shared/Views/Chat/ComposeMessage/ComposeView.swift` | File/media attachment selection and composition |
| `Shared/Views/Chat/ComposeMessage/ComposeImageView.swift` | Image preview in compose area |
| `Shared/Views/Chat/ComposeMessage/ComposeFileView.swift` | File preview in compose area |
| `Shared/Views/Chat/ComposeMessage/ComposeVoiceView.swift` | Voice recording UI with waveform |
| `Shared/Views/Chat/ChatItem/CIFileView.swift` | File message display: icon, name, size, download action |
| `Shared/Views/Chat/ChatItem/CIImageView.swift` | Image message display: thumbnail, full-screen tap |
| `Shared/Views/Chat/ChatItem/CIVideoView.swift` | Video message display: thumbnail, play button, inline playback |
| `Shared/Views/Chat/ChatItem/CIVoiceView.swift` | Voice message display: waveform, playback controls |
| `Shared/Views/Chat/ChatItem/FramedCIVoiceView.swift` | Voice message inside a framed (quoted/forwarded) context |
| `Shared/Views/Chat/ChatItem/FullScreenMediaView.swift` | Full-screen image/video viewer |
| `Shared/Model/SimpleXAPI.swift` | `apiSendMessages`, `receiveFile`, `receiveFiles`, `uploadStandaloneFile`, `downloadStandaloneFile` |
| `Shared/Model/AudioRecPlay.swift` | Audio recording and playback engine for voice messages |

## Related Specifications

- `apps/ios/product/README.md` -- Product overview: Messaging capability (file sharing)
- `apps/ios/product/flows/messaging.md` -- File transfer is part of the message send flow
- `apps/ios/product/views/chat.md` -- Chat view file/media display
