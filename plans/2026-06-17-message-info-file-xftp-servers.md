# Plan: Show XFTP servers used for a file in Message Info

## Goal
In the message-info screen, when the message contains a file, show the list of XFTP servers that hosted the file's chunks ("servers used to upload the file").

## Why this is needed
For transparency: a recipient downloads a file's chunks from XFTP relays without any visible indication of which servers those are. Surfacing them in message info lets a user see exactly what servers they are downloading from (and, for sent files, uploading to) whenever they want to know — useful for trust, debugging, and deciding whether to download.

## Decisions (confirmed)
- **Direction:** Both sent and received files.
- **Visibility:** Always shown when the item has a file (not gated behind Developer tools).
- **Platform:** Android, desktop, and iOS.

## Why this needs a core change (not Kotlin-only)
The server list is **already known to the app** — the file description (which lists each chunk's XFTP server replicas) is what the agent uses to upload/download. But that data lives in the **core (Haskell) layer**: it's stored in the `files` table (`private_snd_file_descr` for sent, the rcv file descr row for received) and parsed inside the core. It is **never surfaced across the core→client API boundary**: the client-facing `CIFile` and `ChatItemInfo` types carry no server info. So this is a **plumbing task** (expose existing data through the API response), not an algorithmic one. The core already has the extraction helper.

## Existing building blocks (no new logic needed)
- `Internal.hs:764-766` — `fileServers` extracts the unique `[XFTPServer]` from a parsed description's chunk replicas. Currently a local `where` helper; will be lifted to a reusable top-level function.
- `Internal.hs:747` — `parseFileDescription :: Text -> CM (ValidFileDescription 'FRecipient)`.
- `Store/Files.hs:42` — `getRcvFileDescrByRcvFileId` returns `RcvFileDescr {fileDescrText, fileDescrComplete}` (received files).
- `Store/Files.hs:197` — `setSndFTPrivateSndDescr` stores `private_snd_file_descr` (sent files). A matching getter must be added (none exists today).
- `XFTPServer` already JSON-encodes to a string on the wire (see `XFTPServerSummary.xftpServer :: XFTPServer` → Kotlin `xftpServer: String`), so the Kotlin field can be `List<String>`.

---

## Implementation

### 1. Core — extraction helper (`src/Simplex/Chat/Library/Internal.hs`)
- Lift `fileServers` out of `receiveViaCompleteFD`'s `where` block to a top-level function, generalized to work on any party's description chunks:
  ```haskell
  fileDescrServers :: FD.FileDescription p -> [XFTPServer]
  fileDescrServers FD.FileDescription {chunks} =
    S.toList $ S.fromList $ concatMap (\FD.FileChunk {replicas} -> map (\FD.FileChunkReplica {server} -> server) replicas) chunks
  ```
  (Keep `receiveViaCompleteFD` working by calling the lifted helper.)
- Add a convenience that resolves a file's servers by direction, returning `[]` when no XFTP description exists (inline/legacy files):
  ```haskell
  getChatItemFileServers :: User -> ChatItem c d -> CM [XFTPServer]
  ```
  - For **received** (`SMDRcv`) with an XFTP file: `getRcvFileDescrByRcvFileId` → `parseFileDescription` → `fileDescrServers`.
  - For **sent** (`SMDSnd`) with an XFTP file: read `private_snd_file_descr` (new getter) → parse the sender description → `fileDescrServers`.
  - Guard on `fileProtocol == FPXFTP`; wrap parse in tolerant error handling so a missing/partial descr yields `[]` (no crash, no section).

### 2. Core — store getter (`src/Simplex/Chat/Store/Files.hs`)
- Add `getSndFTPrivateSndDescr :: DB.Connection -> User -> FileTransferId -> IO (Maybe Text)` reading `private_snd_file_descr` from `files` (mirror of `setSndFTPrivateSndDescr`; column already selected at `Files.hs:799`). Export it.

### 3. Core — extend `ChatItemInfo` (`src/Simplex/Chat/Messages.hs`)
- Add field:
  ```haskell
  data ChatItemInfo = ChatItemInfo
    { itemVersions :: [ChatItemVersion],
      memberDeliveryStatuses :: Maybe (NonEmpty MemberDeliveryStatus),
      forwardedFromChatItem :: Maybe AChatItem,
      fileXftpServers :: [XFTPServer]          -- NEW (empty when no file / not XFTP)
    }
  ```
- `$(JQ.deriveJSON defaultJSON ''ChatItemInfo)` at `Messages.hs:1531` regenerates the instance automatically. Import `XFTPServer` if not already in scope.
- Note: using `[XFTPServer]` (not `Maybe`) keeps the JSON additive and the Kotlin side simple; empty list = nothing to show.

### 4. Core — populate in handler (`src/Simplex/Chat/Library/Commands.hs:619`)
- In `APIGetChatItemInfo`, after computing `forwardedFromChatItem`:
  ```haskell
  fileXftpServers <- getChatItemFileServers user ci
  pure $ CRChatItemInfo user aci ChatItemInfo {itemVersions, memberDeliveryStatuses, forwardedFromChatItem, fileXftpServers}
  ```

### 5. Kotlin model (`apps/multiplatform/.../model/ChatModel.kt:5207`)
```kotlin
class ChatItemInfo(
  val itemVersions: List<ChatItemVersion>,
  val memberDeliveryStatuses: List<MemberDeliveryStatus>?,
  val forwardedFromChatItem: AChatItem?,
  val fileXftpServers: List<String> = emptyList()   // NEW; server hosts as strings
)
```
(`XFTPServer` serializes to a string, matching the existing `XFTPServerSummary.xftpServer: String`.)

### 6. Desktop/shared UI (`apps/multiplatform/.../views/chat/ChatItemInfoView.kt`)
- In `Details()` (`:250-283`), after the existing file-status block, add (always-visible, when present):
  ```kotlin
  if (ci.file != null && ciInfo.fileXftpServers.isNotEmpty()) {
    // section header + one InfoRow/host per server
  }
  ```
  - Render as its own `SectionView` with a title (e.g. "File servers") and one row per server host, styled like the existing info rows. For long host strings, single-line with ellipsis (consistent with the long-name handling already on this branch).
- Add the servers to the shareable text in `itemInfoShareText` (`:534-580`), near the existing file-status share line (`:564-565`), so "Share" / copy includes them.

### 7. Translations (additive only — never rename/remove keys)
- Add one new key to `apps/multiplatform/common/src/commonMain/resources/MR/base/strings.xml`, e.g.:
  ```xml
  <string name="info_row_file_servers">File servers</string>
  ```
  Base locale only; Weblate fans out to other locales. Do not touch existing keys.

### 8. iOS model (`apps/ios/SimpleXChat/ChatTypes.swift:5773`)
Same core change (shared `libsimplex`), so iOS only needs the Swift model + view. Add the optional field (optional so an absent key never breaks decoding):
```swift
public struct ChatItemInfo: Decodable, Hashable {
    public var itemVersions: [ChatItemVersion]
    public var memberDeliveryStatuses: [MemberDeliveryStatus]?
    public var forwardedFromChatItem: AChatItem?
    public var fileXftpServers: [String]?   // NEW
}
```

### 9. iOS UI (`apps/ios/Shared/Views/Chat/ChatItemInfoView.swift`)
- In `details()`, after the `developerTools` block (always-visible when present):
  ```swift
  if ci.file != nil, let servers = chatItemInfo?.fileXftpServers, !servers.isEmpty {
      infoRow("File servers", servers.map(serverHostname).joined(separator: "\n"))
  }
  ```
  `serverHostname(_:)` is public in `SimpleXChat` (`ErrorAlert.swift:142`).
- Add the same to `itemInfoShareText()` (joined with ", "). Strings are inline `NSLocalizedString` (no separate resource file to edit).

---

## Edge cases / behavior
- **Inline / legacy files** (no XFTP description): `fileXftpServers == []` → section hidden. No crash.
- **Sent file before upload completes** (`private_snd_file_descr` not yet set): `[]` → hidden until available.
- **Received file not yet accepted/downloaded:** the rcv description exists as soon as the offer is received (it's what enables download), so servers are available even before download. ✓
- **Multiple chunks across different servers:** deduped via `S.fromList`; all distinct servers listed.
- **Large files (any size up to the limits):** servers are accurate regardless of size. Chat-message files **never** use XFTP redirect — the description is split across multiple `XMsgFileDescr` messages (`splitFileDescr`) and reassembled in full by the recipient (`appendRcvFD`), so a received chat file always carries the real data-chunk servers. (`maxFileSize` = 1 GB soft, `maxFileSizeHard` = 5 GB; XFTP chunk sizes 64 KB / 256 KB / 1 MB / 4 MB.)
- **Redirect descriptions:** XFTP redirect (a small `redirect = Just` description whose chunks point to the relay hosting the real description) is used **only** for *standalone file links* (`SFDONE` `Nothing` branch → `xftpSndFileRedirect`), which have no chat item and never appear in message info. The chat-message branch never redirects, so no redirect description can reach `getChatItemFileServers` — no guard is needed.

## Build / verification
- **Heaviest step:** rebuild the native core lib (`libsimplex`) consumed by the multiplatform app, since core types changed. Without the rebuild, the new JSON field is simply ignored by the old lib (the Kotlin default `emptyList()` keeps it backward-compatible — section just won't appear).
- Tests: a core unit/integration check that `APIGetChatItemInfo` returns a non-empty `fileXftpServers` for an XFTP file (sent and received), empty for a text message and for an inline file.
- Manual: send a file → open message info → confirm the server list; repeat on the receiving device for the received item.

## Touch list
- `src/Simplex/Chat/Library/Internal.hs` — lift `fileDescrServers`, add `getChatItemFileServers`.
- `src/Simplex/Chat/Store/Files.hs` — add `getSndFTPrivateSndDescr` (+ export).
- `src/Simplex/Chat/Messages.hs` — add `fileXftpServers` to `ChatItemInfo`.
- `src/Simplex/Chat/Library/Commands.hs` — populate it in `APIGetChatItemInfo`.
- `apps/multiplatform/.../model/ChatModel.kt` — add Kotlin field.
- `apps/multiplatform/.../views/chat/ChatItemInfoView.kt` — UI section + share text.
- `apps/multiplatform/.../resources/MR/base/strings.xml` — new label key.
- `apps/ios/SimpleXChat/ChatTypes.swift` — add `fileXftpServers` to the Swift `ChatItemInfo`.
- `apps/ios/Shared/Views/Chat/ChatItemInfoView.swift` — UI row in `details()` + `itemInfoShareText()`.
