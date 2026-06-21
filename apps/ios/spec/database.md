# SimpleX Chat iOS -- Database & Storage

**Source:** [`FileUtils.swift`](../SimpleXChat/FileUtils.swift)

> Technical specification for the database architecture, encryption, file storage, and export/import functionality.
>
> Related specs: [Architecture](architecture.md) | [State Management](state.md) | [README](README.md)
> Related product: [Product Overview](../product/README.md)

---

## Table of Contents

1. [Database Overview](#1-database-overview)
2. [Database Files & Paths](#2-database-files--paths)
3. [Haskell Store Modules](#3-haskell-store-modules)
4. [Migrations](#4-migrations)
5. [Database Encryption](#5-database-encryption)
6. [File Storage](#6-file-storage)
7. [Export & Import](#7-export--import)
8. [App Group Sharing](#8-app-group-sharing)

---

## 1. Database Overview

SimpleX Chat uses two SQLite databases managed entirely by the Haskell core. The iOS Swift layer never reads or writes directly to the databases -- all data access goes through the FFI command/response API.

| Database | Suffix | Contents |
|----------|--------|----------|
| Chat DB | `_chat.db` | Messages, contacts, groups, user profiles, files, tags, preferences, call history |
| Agent DB | `_agent.db` | SMP agent connections, cryptographic keys, message queues, server state, XFTP chunks |

Both databases are initialized and migrated via the C FFI function `chat_migrate_init_key()`, which applies pending migrations and returns a `chat_ctrl` pointer.

---

## 2. Database Files & Paths

### [Path Resolution](../SimpleXChat/FileUtils.swift#L63-L73) (FileUtils.swift)

```swift
let DB_FILE_PREFIX = "simplex_v1"

// Database path depends on container preference
func getAppDatabasePath() -> URL {
    dbContainerGroupDefault.get() == .group
    ? getGroupContainerDirectory().appendingPathComponent(DB_FILE_PREFIX)
    : getLegacyDatabasePath()
}

// Full database file paths:
// Chat:  {container}/simplex_v1_chat.db
// Agent: {container}/simplex_v1_agent.db
```

### [File Constants](../SimpleXChat/FileUtils.swift#L38-L44)

```swift
let CHAT_DB: String = "_chat.db"
let AGENT_DB: String = "_agent.db"
private let CHAT_DB_BAK: String = "_chat.db.bak"
private let AGENT_DB_BAK: String = "_agent.db.bak"
```

### Container Locations

See [`getDocumentsDirectory()`](../SimpleXChat/FileUtils.swift#L47) and [`getGroupContainerDirectory()`](../SimpleXChat/FileUtils.swift#L52).

| Container | Path | Used When |
|-----------|------|-----------|
| App Group | `FileManager.containerURL(forSecurityApplicationGroupIdentifier: APP_GROUP_NAME)` | Default (shared with NSE) |
| Documents | `FileManager.urls(for: .documentDirectory)` | Legacy installations |

The container choice is stored in `dbContainerGroupDefault` (`GroupDefaults`).

---

## 3. Haskell Store Modules

All database operations are implemented in Haskell. Key store modules (paths relative to repo root):

| Module | Path | Size | Description |
|--------|------|------|-------------|
| Messages | `src/Simplex/Chat/Store/Messages.hs` | ~178KB | Message CRUD, pagination, search, reactions, delivery receipts |
| Groups | `src/Simplex/Chat/Store/Groups.hs` | ~126KB | Group CRUD, member management, roles, links, invitations |
| Direct | `src/Simplex/Chat/Store/Direct.hs` | ~52KB | Direct contact connections, contact requests. See `createDirectChat` in `Store/Direct.hs` |
| Files | `src/Simplex/Chat/Store/Files.hs` | ~43KB | File transfer state, XFTP chunks, inline files |
| Profiles | `src/Simplex/Chat/Store/Profiles.hs` | ~42KB | User profiles, contact profiles, incognito profiles |
| Connections | `src/Simplex/Chat/Store/Connections.hs` | ~17KB | Connection lifecycle, queue management |

### Data Model (key tables)

```
users              -- User profiles (userId, displayName, fullName, image, ...)
contacts           -- Contact records (contactId, userId, localDisplayName, ...)
groups             -- Group records (groupId, userId, groupProfile, ...)
group_members      -- Group membership (groupMemberId, groupId, memberId, role, ...)
messages           -- Message records (messageId, chatItemId, msgBody, ...)
chat_items         -- Chat items (chatItemId, chatType, chatId, content, ...)
files              -- File transfer records (fileId, chatItemId, fileName, fileSize, ...)
connections        -- SMP connections (connId, agentConnId, ...)
chat_tags          -- User-defined chat tags
chat_tags_chats    -- Tag-to-chat assignments
```

---

## 4. Migrations

Database migrations are managed by the Haskell core. Migration files are located in:

```
src/Simplex/Chat/Store/SQLite/Migrations/
```

Migrations are numbered sequentially starting from `M20220101` through `M20260122` (200+ migrations). Each migration is a Haskell module containing SQL statements for schema changes.

The migration process:
1. `chat_migrate_init_key()` is called with the database path
2. Haskell reads the current schema version from the database
3. Pending migrations are applied in order
4. If migration fails, the function returns an error string (not a `chat_ctrl`)
5. On success, a `chat_ctrl` pointer is returned

Migration results are decoded in Swift as `DBMigrationResult`:
- `.ok` -- migrations applied successfully
- `.invalidConfirmation` -- migration requires user confirmation
- `.errorNotADatabase(dbFile:)` -- file is not a valid SQLite database
- `.errorMigration(dbFile:, migrationError:)` -- migration failed
- `.errorSQL(dbFile:, migrationSQLError:)` -- SQL error during migration
- `.errorKeychain` -- keychain access failed
- `.unknown(json:)` -- unrecognized response

---

## 5. Database Encryption

### Encryption Configuration

Database encryption uses SQLCipher (AES-256) and is managed through the API:

```swift
// Set or change encryption
ChatCommand.apiStorageEncryption(config: DBEncryptionConfig)

// Test if a key is correct
ChatCommand.testStorageEncryption(key: String)
```

`DBEncryptionConfig` contains:
- `currentKey: String` -- current encryption key (empty if unencrypted)
- `newKey: String` -- new encryption key (empty to decrypt)

### Key Storage

The encryption key is stored in the iOS Keychain via `kcDatabasePassword`:
- On first launch with encryption, the key is generated and stored
- The `storeDBPassphraseGroupDefault` flag controls whether the key is auto-stored
- If the user opts out of auto-storage, they must enter the key on each launch

### UI

- [`DatabaseEncryptionView.swift`](../Shared/Views/Database/DatabaseEncryptionView.swift) -- Encryption settings UI
- [`DatabaseView.swift`](../Shared/Views/Database/DatabaseView.swift) -- Database management UI (size, export, import, encryption)

---

## 6. File Storage

### Directory Structure

```
{App Container}/
├── Documents/
│   ├── app_files/          -- Downloaded and sent files
│   ├── temp_files/         -- Temporary files during transfer
│   └── assets/wallpapers/  -- Custom wallpaper images
├── {App Group Container}/
│   ├── simplex_v1_chat.db  -- Chat database
│   ├── simplex_v1_agent.db -- Agent database
│   └── ...
```

### [File Size Constants](../SimpleXChat/FileUtils.swift#L18-L36) (FileUtils.swift)

```swift
public let MAX_IMAGE_SIZE: Int64 = 261_120        // 255 KB -- inline image compression target
public let MAX_IMAGE_SIZE_AUTO_RCV: Int64 = 522_240  // 510 KB -- auto-receive images
public let MAX_VOICE_SIZE_AUTO_RCV: Int64 = 522_240  // 510 KB -- auto-receive voice
public let MAX_VIDEO_SIZE_AUTO_RCV: Int64 = 1_047_552 // 1023 KB -- auto-receive video
public let MAX_FILE_SIZE_XFTP: Int64 = 1_073_741_824 // 1 GB -- max XFTP transfer
public let MAX_FILE_SIZE_SMP: Int64 = 8_000_000      // ~7.6 MB -- max SMP inline
public let MAX_FILE_SIZE_LOCAL: Int64 = Int64.max     // No limit for local files
public let MAX_VOICE_MESSAGE_LENGTH = TimeInterval(300) // 5 minutes
```

### CryptoFile (Encrypted File Storage)

When `apiSetEncryptLocalFiles(enable: true)` is set, files stored on device are AES-encrypted:

- Encryption/decryption uses `chat_encrypt_file` / `chat_decrypt_file` C FFI functions
- Each file gets a unique key and nonce stored alongside the file reference
- The `CryptoFile` type wraps `(filePath: String, cryptoArgs: CryptoFileArgs?)` where `CryptoFileArgs` contains `(fileKey: String, fileNonce: String)`

### [File Path Helpers](../SimpleXChat/FileUtils.swift#L219-L221)

```swift
public func getDocumentsDirectory() -> URL      // Standard documents dir
public func getGroupContainerDirectory() -> URL // App group container
func getAppFilesDirectory() -> URL              // {appDir}/app_files/
func getTempFilesDirectory() -> URL             // {appDir}/temp_files/
func getWallpaperDirectory() -> URL             // {appDir}/assets/wallpapers/
```

See also [`saveFile()`](../SimpleXChat/FileUtils.swift#L226), [`removeFile()`](../SimpleXChat/FileUtils.swift#L243), and [`getMaxFileSize()`](../SimpleXChat/FileUtils.swift#L276).

### [Cleanup](../SimpleXChat/FileUtils.swift#L86-L116)

- Files are deleted when their associated `ChatItem` is deleted. See [`cleanupFile()`](../SimpleXChat/FileUtils.swift#L267) and [`cleanupDirectFile()`](../SimpleXChat/FileUtils.swift#L260).
- Timed message expiry triggers file deletion
- [`deleteAppDatabaseAndFiles()`](../SimpleXChat/FileUtils.swift#L86) removes all databases, files, temp files, and wallpapers
- [`deleteAppFiles()`](../SimpleXChat/FileUtils.swift#L108) removes only the files directory (preserving databases)

---

## 7. Export & Import

### Export

```swift
ChatCommand.apiExportArchive(config: ArchiveConfig)
// Response: ChatResponse2.archiveExported(archiveErrors: [ArchiveError])
```

`ArchiveConfig` specifies:
- `archivePath: String` -- destination path for the archive
- `disableCompression: Bool?` -- optional flag to skip compression

The archive contains both databases and optionally files. The Haskell core handles the actual export, creating a ZIP archive.

### Import

```swift
ChatCommand.apiImportArchive(config: ArchiveConfig)
// Response: ChatResponse2.archiveImported(archiveErrors: [ArchiveError])
```

Import replaces the current databases with the archive contents. The app must be restarted after import.

### Archive Errors

`ArchiveError` is an array returned with both export and import results, listing any non-fatal issues encountered (e.g., missing files, corrupt entries).

---

## 8. App Group Sharing

### Shared Access Model

The main app and NSE share database access through the iOS App Group container:

```
Main App ──┐
            ├── {App Group}/simplex_v1_chat.db
            ├── {App Group}/simplex_v1_agent.db
NSE ────────┘
```

### Coordination

- Both processes can initialize their own `chat_ctrl` instance pointing to the same database files
- SQLite WAL mode allows concurrent reads
- Write coordination uses `chat_close_store` / `chat_reopen_store` to manage database locks
- The main app suspends its chat controller when entering background, allowing NSE to access the database
- NSE is short-lived (~30 seconds per notification) and releases its lock quickly

### App State Communication

The `appStateGroupDefault` in `GroupDefaults` communicates app state between main app and NSE:
- `.active` -- main app is in foreground
- `.suspended` -- main app is in background
- `.stopped` -- main app is terminated

The NSE checks this flag to determine whether to process notifications (it avoids processing if the main app is active).

---

## Source Files

| File | Path |
|------|------|
| File utilities & constants | [`SimpleXChat/FileUtils.swift`](../SimpleXChat/FileUtils.swift) |
| Database management UI | [`Shared/Views/Database/DatabaseView.swift`](../Shared/Views/Database/DatabaseView.swift) |
| Encryption settings UI | [`Shared/Views/Database/DatabaseEncryptionView.swift`](../Shared/Views/Database/DatabaseEncryptionView.swift) |
| C FFI (migration, file ops) | `SimpleXChat/SimpleX.h` |
| Haskell store root | `../../src/Simplex/Chat/Store/` |
| Haskell migrations | `../../src/Simplex/Chat/Store/SQLite/Migrations/` |
