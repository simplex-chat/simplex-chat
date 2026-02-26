# Database & Storage

## Table of Contents

1. [Overview](#1-overview)
2. [Database Files & Paths](#2-database-files--paths)
3. [Haskell Store Modules](#3-haskell-store-modules)
4. [Migrations](#4-migrations)
5. [Database Encryption](#5-database-encryption)
6. [File Storage](#6-file-storage)
7. [Export & Import](#7-export--import)
8. [Source Files](#8-source-files)

---

## 1. Overview

SimpleX Chat uses **two SQLite databases** managed entirely by the Haskell core. Kotlin code **never reads or writes the databases directly** -- all data access goes through the JNI command/response protocol defined in [SimpleXAPI.kt](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt).

The two databases are:

| Database | Suffix | Contents |
|----------|--------|----------|
| Chat database | `_chat.db` | Users, contacts, groups, messages, files metadata, settings |
| Agent database | `_agent.db` | SMP/XFTP agent state: connections, queues, encryption keys, delivery tracking |

Both databases are created and migrated by the `chatMigrateInit` JNI function. The Kotlin layer handles:
- Providing the correct file path prefix (`dbAbsolutePrefixPath`)
- Providing the encryption key
- Interpreting migration results (`DBMigrationResult`)
- Exposing API functions that proxy to Haskell store operations

---

## 2. Database Files & Paths

### Expect Declarations

The common module declares platform-dependent paths as `expect` values in [Files.kt](../common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt):

```kotlin
expect val dataDir: File              // L18
expect val tmpDir: File               // L19
expect val filesDir: File             // L20
expect val appFilesDir: File          // L21
expect val wallpapersDir: File        // L22
expect val coreTmpDir: File           // L23
expect val dbAbsolutePrefixPath: String  // L24
expect val preferencesDir: File       // L25
expect val preferencesTmpDir: File    // L26

expect val chatDatabaseFileName: String  // L28
expect val agentDatabaseFileName: String // L29

expect val databaseExportDir: File    // L35
expect val remoteHostsDir: File       // L37
```

### Android Actual Values

From [Files.android.kt](../common/src/androidMain/kotlin/chat/simplex/common/platform/Files.android.kt):

| Variable | Value | Notes |
|----------|-------|-------|
| `dataDir` | `androidAppContext.dataDir` | `/data/data/<package>/` |
| `tmpDir` | `getDir("temp", MODE_PRIVATE)` | Private temp directory |
| `filesDir` | `dataDir/files` | Parent for all file storage |
| `appFilesDir` | `filesDir/app_files` | User-visible chat file attachments |
| `wallpapersDir` | `filesDir/assets/wallpapers` | Custom wallpaper images |
| `coreTmpDir` | `filesDir/temp_files` | Haskell core temp directory |
| `dbAbsolutePrefixPath` | `dataDir/files` | Prefix: core appends `_chat.db` / `_agent.db` |
| `chatDatabaseFileName` | `"files_chat.db"` | Full filename: `files_chat.db` |
| `agentDatabaseFileName` | `"files_agent.db"` | Full filename: `files_agent.db` |
| `databaseExportDir` | `androidAppContext.cacheDir` | Temp location for archive export |
| `remoteHostsDir` | `tmpDir/remote_hosts` | Remote host file staging |
| `preferencesDir` | `dataDir/shared_prefs` | Android SharedPreferences directory |

### Desktop Actual Values

From [Files.desktop.kt](../common/src/desktopMain/kotlin/chat/simplex/common/platform/Files.desktop.kt):

| Variable | Value | Notes |
|----------|-------|-------|
| `dataDir` | `desktopPlatform.dataPath` | XDG_DATA_HOME (Linux), AppData (Windows), Application Support (macOS) |
| `tmpDir` | `java.io.tmpdir/simplex` | System temp with `deleteOnExit` |
| `filesDir` | `dataDir/simplex_v1_files` | Flat file storage |
| `appFilesDir` | Same as `filesDir` | No subdirectory on desktop |
| `wallpapersDir` | `dataDir/simplex_v1_assets/wallpapers` | Custom wallpaper images |
| `coreTmpDir` | `dataDir/tmp` | Haskell core temp directory |
| `dbAbsolutePrefixPath` | `dataDir/simplex_v1` | Prefix: core appends `_chat.db` / `_agent.db` |
| `chatDatabaseFileName` | `"simplex_v1_chat.db"` | Full filename: `simplex_v1_chat.db` |
| `agentDatabaseFileName` | `"simplex_v1_agent.db"` | Full filename: `simplex_v1_agent.db` |
| `databaseExportDir` | Same as `tmpDir` | Temp location for archive export |
| `remoteHostsDir` | `dataDir/remote_hosts` | Remote host file staging |
| `preferencesDir` | `desktopPlatform.configPath` | Platform config directory |

### Resulting Database Paths

| Platform | Chat DB | Agent DB |
|----------|---------|----------|
| Android | `/data/data/<pkg>/files_chat.db` | `/data/data/<pkg>/files_agent.db` |
| Desktop (Linux) | `~/.local/share/simplex/simplex_v1_chat.db` | `~/.local/share/simplex/simplex_v1_agent.db` |
| Desktop (macOS) | `~/Library/Application Support/simplex/simplex_v1_chat.db` | ... |
| Desktop (Windows) | `%APPDATA%/simplex/simplex_v1_chat.db` | ... |

---

## 3. Haskell Store Modules

The Haskell core organizes database access into store modules. Kotlin code invokes these indirectly through `CC` commands. The store modules are:

| Module | Path | Responsibilities |
|--------|------|-----------------|
| `Messages.hs` | `src/Simplex/Chat/Store/Messages.hs` | Message CRUD, chat items, reactions, delivery statuses, TTL cleanup |
| `Groups.hs` | `src/Simplex/Chat/Store/Groups.hs` | Group profiles, membership, roles, invitations, group links |
| `Direct.hs` | `src/Simplex/Chat/Store/Direct.hs` | Contact management, direct connections, contact requests |
| `Files.hs` | `src/Simplex/Chat/Store/Files.hs` | File transfer metadata, XFTP state, standalone files |
| `Profiles.hs` | `src/Simplex/Chat/Store/Profiles.hs` | User profiles, display names, address book |
| `Connections.hs` | `src/Simplex/Chat/Store/Connections.hs` | SMP agent connections, pending connections, server switches |

All store operations execute within SQLite transactions managed by the Haskell core. The Kotlin layer has no direct knowledge of table schemas or SQL queries.

---

## 4. Migrations

### JNI Entry Point

Database migration is triggered by the `chatMigrateInit` external function ([Core.kt#L25](../common/src/commonMain/kotlin/chat/simplex/common/platform/Core.kt#L25)):

```kotlin
external fun chatMigrateInit(dbPath: String, dbKey: String, confirm: String): Array<Any>
```

**Parameters:**
- `dbPath` -- the `dbAbsolutePrefixPath` (core appends `_chat.db` and `_agent.db`)
- `dbKey` -- encryption passphrase (empty string = unencrypted)
- `confirm` -- migration confirmation mode: `"error"`, `"yesUp"`, or `"yesUpDown"`

**Returns:** `Array<Any>` where:
- `[0]` -- JSON string encoding a `DBMigrationResult`
- `[1]` -- `ChatCtrl` handle (Long) if migration succeeded

### Migration Flow in `initChatController`

The full initialization sequence is in [Core.kt#L62](../common/src/commonMain/kotlin/chat/simplex/common/platform/Core.kt#L62):

1. Obtain the DB encryption key from `DatabaseUtils.useDatabaseKey()`.
2. Determine the confirmation mode (default: `YesUp`; developer mode with confirm upgrades: `Error`).
3. Call `chatMigrateInit(dbAbsolutePrefixPath, dbKey, "error")` -- first attempt with `Error` to detect pending migrations.
4. Parse the result as `DBMigrationResult`.
5. If the result is `ErrorMigration` with an `Upgrade` error and confirmation allows it, re-run `chatMigrateInit` with the appropriate confirmation (`"yesUp"`).
6. If `OK`, store the `ChatCtrl` handle, set `chatDbEncrypted`, and proceed to start the chat.
7. If not `OK`, handle special case: if the `newDatabaseInitialized` preference is not set AND the database was only partially initialized (single DB file exists), remove both files and retry once.

<a id="DBMigrationResult"></a>

### DBMigrationResult

Defined in [DatabaseUtils.kt#L79](../common/src/commonMain/kotlin/chat/simplex/common/views/helpers/DatabaseUtils.kt#L79):

```kotlin
sealed class DBMigrationResult {
  object OK                                         // Migration succeeded
  object InvalidConfirmation                        // Invalid confirmation parameter
  data class ErrorNotADatabase(val dbFile: String)  // File exists but is not a valid database
  data class ErrorMigration(val dbFile: String,     // Migration error with details
                            val migrationError: MigrationError)
  data class ErrorSQL(val dbFile: String,           // SQL error during migration
                      val migrationSQLError: String)
  object ErrorKeychain                              // Keychain/keystore error
  data class Unknown(val json: String)              // Unparseable response
}
```

### MigrationError

```kotlin
sealed class MigrationError {
  class Upgrade(val upMigrations: List<UpMigration>)    // Pending forward migrations
  class Downgrade(val downMigrations: List<String>)     // Database is newer than app
  class Error(val mtrError: MTRError)                   // Conflict or missing migrations
}
```

### MigrationConfirmation

```kotlin
enum class MigrationConfirmation(val value: String) {
  YesUp("yesUp"),         // Auto-confirm forward migrations
  YesUpDown("yesUpDown"), // Auto-confirm both directions (not used in UI)
  Error("error")          // Report errors without running migrations
}
```

---

## 5. Database Encryption

### Encryption API

Two API functions manage database encryption, both in [SimpleXAPI.kt](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt):

| Function | Parameters | Description | Line |
|----------|-----------|-------------|------|
| `apiStorageEncryption` | `currentKey: String, newKey: String` | Change or set the database encryption passphrase | [L999](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L999) |
| `testStorageEncryption` | `key: String, ctrl: ChatCtrl?` | Test whether a given key can decrypt the database | [L1006](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1006) |

Both delegate to the Haskell core via `CC.ApiStorageEncryption(DBEncryptionConfig)` and `CC.TestStorageEncryption(key)` respectively.

<a id="DBEncryptionConfig"></a>

`DBEncryptionConfig` ([SimpleXAPI.kt#L4166](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L4166)):

```kotlin
class DBEncryptionConfig(val currentKey: String, val newKey: String)
```

### Passphrase Storage -- CryptorInterface

The `CryptorInterface` ([Cryptor.kt](../common/src/commonMain/kotlin/chat/simplex/common/platform/Cryptor.kt)) provides platform-specific key encryption for storing the DB passphrase at rest:

```kotlin
interface CryptorInterface {
  fun decryptData(data: ByteArray, iv: ByteArray, alias: String): String?
  fun encryptText(text: String, alias: String): Pair<ByteArray, ByteArray>
  fun deleteKey(alias: String)
}

expect val cryptor: CryptorInterface
```

### Android Implementation

[Cryptor.android.kt](../common/src/androidMain/kotlin/chat/simplex/common/platform/Cryptor.android.kt):

- Uses **Android KeyStore** (`"AndroidKeyStore"` provider)
- Algorithm: **AES/GCM/NoPadding** (128-bit authentication tag)
- Keys are hardware-backed when available
- On decryption failure with a random initial passphrase, throws to prevent overwriting
- Shows user alerts for keychain errors

```kotlin
internal class Cryptor: CryptorInterface {
  private var keyStore: KeyStore = KeyStore.getInstance("AndroidKeyStore").apply { load(null) }
  // AES-GCM encryption/decryption using AndroidKeyStore-managed keys
}
```

### Desktop Implementation

[Cryptor.desktop.kt](../common/src/desktopMain/kotlin/chat/simplex/common/platform/Cryptor.desktop.kt):

- **Placeholder/no-op implementation** -- data is returned as-is
- No actual encryption of the stored passphrase on desktop
- `decryptData` returns `String(data)` without decryption
- `encryptText` returns the raw bytes without encryption

```kotlin
actual val cryptor: CryptorInterface = object : CryptorInterface {
  override fun decryptData(data: ByteArray, iv: ByteArray, alias: String): String? = String(data)
  override fun encryptText(text: String, alias: String) = text.toByteArray() to text.toByteArray()
  override fun deleteKey(alias: String) {}
}
```

### Passphrase Management

`DatabaseUtils` ([DatabaseUtils.kt](../common/src/commonMain/kotlin/chat/simplex/common/views/helpers/DatabaseUtils.kt)) provides:

- `ksDatabasePassword` -- encrypted passphrase stored in platform preferences (SharedPreferences on Android, file-based on desktop)
- `useDatabaseKey()` -- retrieves the passphrase, decrypting it via `CryptorInterface`
- `randomDatabasePassword()` -- generates a 32-byte random passphrase (Base64-encoded) for initial database creation

The flow:
1. On first launch, `randomDatabasePassword()` generates a key.
2. `CryptorInterface.encryptText()` encrypts the key for storage.
3. The encrypted (data, IV) pair is saved to preferences via `ksDatabasePassword`.
4. On subsequent launches, `ksDatabasePassword.get()` retrieves the encrypted pair, and `CryptorInterface.decryptData()` recovers the plaintext key.
5. The key is passed to `chatMigrateInit` to open the encrypted SQLite databases.

---

## 6. File Storage

### Directory Layout

Declared in [Files.kt](../common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt) with platform-specific implementations:

| Directory | Variable | Android Path | Desktop Path | Purpose |
|-----------|----------|-------------|--------------|---------|
| App files | `appFilesDir` | `dataDir/files/app_files` | `dataDir/simplex_v1_files` | Chat file attachments (images, videos, documents) |
| Wallpapers | `wallpapersDir` | `dataDir/files/assets/wallpapers` | `dataDir/simplex_v1_assets/wallpapers` | Custom chat wallpaper images |
| Core temp | `coreTmpDir` | `dataDir/files/temp_files` | `dataDir/tmp` | Haskell core temporary files (in-progress transfers) |
| App temp | `tmpDir` | `getDir("temp", MODE_PRIVATE)` | `java.io.tmpdir/simplex` | Application-level temporary files |
| Remote hosts | `remoteHostsDir` | `tmpDir/remote_hosts` | `dataDir/remote_hosts` | Files staged for remote host sessions |
| DB export | `databaseExportDir` | `androidAppContext.cacheDir` | Same as `tmpDir` | Temporary storage for database archive ZIP |
| Preferences | `preferencesDir` | `dataDir/shared_prefs` | `desktopPlatform.configPath` | User preferences, theme YAML |
| Migration temp | `getMigrationTempFilesDirectory()` | `dataDir/migration_temp_files` | `dataDir/migration_temp_files` | Temporary files during database migration |

### File Path Resolution

Files referenced by chat items use `CryptoFile` (optional encryption metadata + relative path). Path resolution is handled by helper functions in [Files.kt](../common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt):

- `getAppFilePath(fileName)` ([L81](../common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt#L81)) -- resolves to `appFilesDir/fileName` for local, or `remoteHostsDir/<storePath>/simplex_v1_files/fileName` for remote hosts
- `getWallpaperFilePath(fileName)` ([L91](../common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt#L91)) -- resolves wallpaper paths similarly
- `getLoadedFilePath(file)` ([L105](../common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt#L105)) -- returns the full path if the file is downloaded and ready

### Local File Encryption

The `apiSetEncryptLocalFiles(enable)` command ([SimpleXAPI.kt#L967](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L967)) tells the Haskell core to encrypt files stored in `appFilesDir`. When enabled, files are written as `CryptoFile` with a random AES key and nonce. The JNI functions `chatEncryptFile` and `chatDecryptFile` ([Core.kt#L39-L40](../common/src/commonMain/kotlin/chat/simplex/common/platform/Core.kt#L39)) handle the actual crypto operations.

---

## 7. Export & Import

### API Functions

| Function | CC Command | CR Response | Line |
|----------|-----------|-------------|------|
| `apiExportArchive(config)` | `CC.ApiExportArchive(config)` | `CR.ArchiveExported(archiveErrors)` | [SimpleXAPI.kt#L981](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L981) |
| `apiImportArchive(config)` | `CC.ApiImportArchive(config)` | `CR.ArchiveImported(archiveErrors)` | [SimpleXAPI.kt#L987](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L987) |
| `apiDeleteStorage()` | `CC.ApiDeleteStorage()` | `CR.CmdOk` | [SimpleXAPI.kt#L993](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L993) |

### ArchiveConfig

Defined at [SimpleXAPI.kt#L4162](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L4162):

```kotlin
class ArchiveConfig(
  val archivePath: String,              // Full path to the ZIP archive
  val disableCompression: Boolean?,     // Skip compression for speed
  val parentTempDirectory: String?      // Temp directory for extraction
)
```

### Export Flow

1. UI constructs an `ArchiveConfig` with a path under `databaseExportDir`.
2. Calls `apiExportArchive(config)` which sends `CC.ApiExportArchive` to the Haskell core.
3. The core creates a ZIP containing both `_chat.db` and `_agent.db` (and optionally files).
4. Returns `CR.ArchiveExported` with a list of `ArchiveError` (non-fatal issues during export).
5. UI offers the archive file for sharing/saving.

### Import Flow

1. User selects an archive file.
2. UI copies it to a temp location and constructs an `ArchiveConfig`.
3. Calls `apiImportArchive(config)` which sends `CC.ApiImportArchive` to the Haskell core.
4. The core extracts and replaces both databases.
5. Returns `CR.ArchiveImported` with a list of `ArchiveError` (non-fatal issues during import).
6. UI triggers re-initialization via `initChatController`.

<a id="ArchiveError"></a>

### ArchiveError

Defined at [SimpleXAPI.kt#L7658](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L7658):

```kotlin
sealed class ArchiveError {
  class ArchiveErrorImport(val importError: String)                // General import error
  class ArchiveErrorFile(val file: String, val fileError: String)  // Per-file error
}
```

### Delete Storage

`apiDeleteStorage()` removes both database files entirely. This is used during account deletion or database reset operations. After calling this, `initChatController` must be called to create fresh databases.

---

## 8. Source Files

| File | Purpose | Path |
|------|---------|------|
| SimpleXAPI.kt | API functions: encryption, export/import, storage commands | `common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt` |
| Core.kt | JNI externals (`chatMigrateInit`, `chatEncryptFile`, etc.), `initChatController` | `common/src/commonMain/kotlin/chat/simplex/common/platform/Core.kt` |
| Files.kt | Platform-expect file/directory path declarations | `common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt` |
| Files.android.kt | Android actual paths (dataDir, appFilesDir, etc.) | `common/src/androidMain/kotlin/chat/simplex/common/platform/Files.android.kt` |
| Files.desktop.kt | Desktop actual paths (XDG/AppData, etc.) | `common/src/desktopMain/kotlin/chat/simplex/common/platform/Files.desktop.kt` |
| Cryptor.kt | Platform-expect encryption interface for passphrase storage | `common/src/commonMain/kotlin/chat/simplex/common/platform/Cryptor.kt` |
| Cryptor.android.kt | Android: AES-GCM via AndroidKeyStore | `common/src/androidMain/kotlin/chat/simplex/common/platform/Cryptor.android.kt` |
| Cryptor.desktop.kt | Desktop: placeholder (no-op) implementation | `common/src/desktopMain/kotlin/chat/simplex/common/platform/Cryptor.desktop.kt` |
| DatabaseUtils.kt | `DBMigrationResult`, `MigrationError`, `MigrationConfirmation`, passphrase helpers | `common/src/commonMain/kotlin/chat/simplex/common/views/helpers/DatabaseUtils.kt` |
| Messages.hs | Haskell store: message CRUD, reactions, delivery | `src/Simplex/Chat/Store/Messages.hs` |
| Groups.hs | Haskell store: groups, membership, roles | `src/Simplex/Chat/Store/Groups.hs` |
| Direct.hs | Haskell store: contacts, direct connections | `src/Simplex/Chat/Store/Direct.hs` |
| Files.hs | Haskell store: file transfer metadata | `src/Simplex/Chat/Store/Files.hs` |
| Profiles.hs | Haskell store: user profiles | `src/Simplex/Chat/Store/Profiles.hs` |
| Connections.hs | Haskell store: SMP agent connections | `src/Simplex/Chat/Store/Connections.hs` |

All Kotlin paths are relative to `apps/multiplatform/`. All Haskell paths are relative to the repository root.
