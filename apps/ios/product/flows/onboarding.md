# Onboarding Flow

> **Related spec:** [spec/architecture.md](../../spec/architecture.md) | [spec/database.md](../../spec/database.md)

## Overview

First-time setup and migration flows for SimpleX Chat iOS. Covers app initialization, profile creation, server operator selection, notification configuration, and database import/export for device migration. The app uses a Haskell runtime for its core chat engine, with SQLite databases shared between the main app and the Notification Service Extension (NSE).

## Prerequisites

- Fresh install of SimpleX Chat from the App Store, or
- Existing install with database archive for import/migration
- iOS 15+ with App Group entitlement configured

## Step-by-Step Processes

### 1. App Initialization Sequence

On every app launch, `SimpleXApp.init()` executes the following in order:

```
1. haskell_init()                              -- Start Haskell runtime system (GHC RTS)
2. UserDefaults.standard.register(defaults:)   -- Set default preferences (appDefaults)
3. setGroupDefaults()                          -- Configure app group shared defaults
4. registerGroupDefaults()                     -- Register group container defaults
5. setDbContainer()                            -- Configure database paths in app group container
6. BGManager.shared.register()                 -- Register background task handlers
7. NtfManager.shared.registerCategories()      -- Register notification action categories
```

Then in `ContentView.onAppear`:
- If no migration is in progress and authentication is set up, `initChatAndMigrate()` is called.
- This triggers `chatMigrateInit()` to initialize/migrate databases.
- Then `startChat()` is called to start the chat engine.

### 2. Fresh Install -- Onboarding Steps

Onboarding is managed by `OnboardingStage` enum and `OnboardingView`:

**Step 1: SimpleX Info** (`step1_SimpleXInfo`)
1. `SimpleXInfo` view is presented.
2. Explains SimpleX's architecture: no user identifiers, E2E encryption, decentralized servers.
3. User taps "Create your profile" to proceed.

**Step 2: Create Profile** (`step2_CreateProfile` -- now inline in step 1)
1. `CreateFirstProfile` view (embedded in the onboarding flow).
2. User enters display name (required). Full name is set to empty string.
3. Display name is validated via `mkValidName()` and `canCreateProfile()`.
4. On "Create":
   ```swift
   AppChatState.shared.set(.active)
   m.currentUser = try apiCreateActiveUser(profile)
   try startChat()
   ```
5. `apiCreateActiveUser(Profile(displayName:fullName:shortDescr:))` creates the user in the Haskell core.
6. `startChat()` initializes the chat engine.
7. Onboarding advances to `step3_ChooseServerOperators`.

**Step 3: Choose Server Operators** (`step3_ChooseServerOperators`)
1. `OnboardingConditionsView` is presented (simplified conditions acceptance).
2. User reviews and accepts server operator conditions.
3. This configures which SMP/XFTP server operators to use.
4. Advances to `step4_SetNotificationsMode`.

**Step 4: Set Notifications** (`step4_SetNotificationsMode`)
1. `SetNotificationsMode` view is presented.
2. Three options:
   - **Instant**: Requires Apple Push Notification service. Registers device token via `apiRegisterToken(token:notificationMode:)`.
   - **Periodic**: Uses iOS background app refresh. No push token needed.
   - **Off**: No notifications.
3. For instant mode: `apiRegisterToken` sends `ChatCommand.apiRegisterToken(token:notificationMode:)` and receives `ChatResponse2.ntfTokenStatus(status)`.
4. On completion: `onboardingStageDefault.set(.onboardingComplete)`.

**Onboarding Complete** (`onboardingComplete`)
1. `ChatListView` is shown.
2. Empty state displays "Add contact" prompt via `ChatHelp`.
3. If delivery receipts haven't been configured: `chatModel.setDeliveryReceipts = true` triggers a prompt.

### 3. startChat() -- Chat Engine Startup

Called after profile creation or on subsequent app launches:

```swift
func startChat(refreshInvitations: Bool = true, onboarding: Bool = false) throws {
    1. setNetworkConfig(getNetCfg())           -- Apply network configuration
    2. apiCheckChatRunning()                    -- Check if already running
    3. listUsers()                              -- Load all user profiles
    4. getUserChatData()                        -- Load chats, tags, address, TTL
    5. NtfManager.shared.setNtfBadgeCount(...)  -- Set badge count
    6. refreshCallInvitations()                 -- Check pending call invitations
    7. apiGetNtfToken()                         -- Get notification token status
    8. apiStartChat()                           -- Start the Haskell chat engine
    9. registerToken(token:)                    -- Register push token if available
    10. ChatReceiver.shared.start()             -- Start message receive loop
}
```

### 4. Database Setup

**Location:**
- App group container (shared with NSE): determined by `dbContainerGroupDefault`
- Path prefix: `simplex_v1` (`DB_FILE_PREFIX`)
- Chat database: `simplex_v1_chat.db` (messages, contacts, groups, settings)
- Agent database: `simplex_v1_agent.db` (SMP connections, encryption keys, queues)

**Initialization:**
- `chatMigrateInit(useKey:confirmMigrations:backgroundMode:)` in `SimpleXChat/API.swift`.
- Creates databases if they do not exist.
- Runs pending migrations with confirmation mode.
- Handles database encryption:
  - If keychain storage enabled: generates random DB key on first run (`randomDatabasePassword()`).
  - Stores key in keychain via `kcDatabasePassword`.
  - `initialRandomDBPassphraseGroupDefault` tracks whether using auto-generated key.

**Encryption:**
- Optional database encryption passphrase via `DatabaseEncryptionView`.
- `apiStorageEncryption(currentKey:newKey:)` changes encryption key.
- `testStorageEncryption(key:)` validates a key against the database.

### 5. Database Export (Source Device)

1. User navigates to Settings -> Database -> "Export database".
2. Chat must be stopped first for data consistency.
3. Calls `apiExportArchive(config: ArchiveConfig)`:
   ```swift
   func apiExportArchive(config: ArchiveConfig) async throws -> [ArchiveError]
   ```
4. Core creates a ZIP archive containing both databases and file attachments.
5. Returns any non-fatal `[ArchiveError]` (e.g., file access issues).
6. User transfers the archive to the new device via AirDrop, file share, etc.

### 6. Database Import (Destination Device)

1. On new device: during onboarding or Settings -> Database -> "Import database".
2. User selects the archive file.
3. Calls `apiImportArchive(config: ArchiveConfig)`:
   ```swift
   func apiImportArchive(config: ArchiveConfig) async throws -> [ArchiveError]
   ```
4. Core extracts the archive, replacing local databases.
5. Returns any non-fatal `[ArchiveError]`.
6. Chat engine is restarted with the imported data.
7. All contacts, groups, messages, and settings are restored.

### 7. In-App Device Migration

An alternative to manual export/import using direct device-to-device transfer.

**Source device** (`MigrateFromDevice` view):
1. User navigates to Settings -> Database -> "Migrate to another device".
2. App creates a temporary database and uploads archive via XFTP standalone file.
3. Generates a migration link containing the file URL and encryption key.
4. Displays QR code / share link for the destination device.

**Destination device** (`MigrateToDevice` view):
1. On new device: onboarding detects migration state or user selects "Migrate".
2. Scans/pastes the migration link.
3. `downloadStandaloneFile(user:url:file:ctrl:)` downloads the archive from XFTP.
4. `standaloneFileInfo(url:ctrl:)` validates the file metadata.
5. Archive is imported, databases are restored.
6. `chatInitTemporaryDatabase(url:key:confirmation:)` may be used for temporary DB operations during migration.
7. Chat engine starts with the migrated data.

If migration is interrupted:
- `chatModel.migrationState` preserves state across app restarts.
- On next launch, `ContentView.onAppear` detects pending migration and resumes.

### 8. Additional Profile Creation (Multi-Account)

1. From `UserPicker` (profile switcher) -> "Add profile".
2. `CreateProfile` view is presented (distinct from `CreateFirstProfile`).
3. User enters display name and optional bio (max 160 bytes JSON-encoded, `MAX_BIO_LENGTH_BYTES`).
4. `apiCreateActiveUser(profile)` creates additional user.
5. `listUsers()` and `getUserChatData()` refresh the model.
6. No onboarding steps -- goes directly to chat list.

## Data Structures

| Type | Location | Description |
|------|----------|-------------|
| `OnboardingStage` | `Shared/Views/Onboarding/OnboardingView.swift` | Enum: `step1_SimpleXInfo`, `step2_CreateProfile`, `step3_ChooseServerOperators`, `step4_SetNotificationsMode`, `onboardingComplete` |
| `Profile` | `SimpleXChat/ChatTypes.swift` | `displayName`, `fullName`, `image`, `shortDescr` |
| `User` | `SimpleXChat/ChatTypes.swift` | Full user model with profile, userId, and settings |
| `ArchiveConfig` | `SimpleXChat/APITypes.swift` | Configuration for database export/import |
| `DBMigrationResult` | `SimpleXChat/API.swift` | Result of database migration: `.ok`, `.errorNotADatabase`, `.errorKeychain`, etc. |
| `MigrationConfirmation` | `SimpleXChat/API.swift` | Migration confirmation mode: `.error`, `.yesUp`, `.yesUpDown` |
| `DeviceToken` | `SimpleXChat/ChatTypes.swift` | Apple push notification device token |
| `NtfTknStatus` | `SimpleXChat/ChatTypes.swift` | Notification token status: registered, active, expired, etc. |
| `NotificationsMode` | `SimpleXChat/ChatTypes.swift` | `.off`, `.periodic`, `.instant` |
| `MigrationFileLinkData` | Used in standalone file transfers for device migration |
| `AppChatState` | `SimpleXChat/` | Shared state: `.active`, `.stopped`, `.suspended` |

## Error Cases

| Error | Cause | Handling |
|-------|-------|----------|
| `DBMigrationResult.errorNotADatabase` | Wrong encryption key or corrupt DB | Show `DatabaseErrorView` with options |
| `DBMigrationResult.errorKeychain` | Keychain access failed | Show error, offer to re-enter passphrase |
| `DBMigrationResult.errorMigration` | Schema migration failure | Show error with migration details |
| `duplicateUserError` | Display name already in use | `UserProfileAlert.duplicateUserError` |
| `invalidDisplayNameError` | Invalid characters in display name | `UserProfileAlert.invalidDisplayNameError` |
| `createUserError` | Core failed to create user | Alert with error details |
| `invalidNameError(validName)` | Name needs normalization | Alert suggesting the valid name |
| Archive import errors | Missing files, version mismatch | Non-fatal `[ArchiveError]` displayed |
| Migration interrupted | Network failure, app killed | State preserved in `chatModel.migrationState`, resumed on next launch |

## Key Files

| File | Purpose |
|------|---------|
| `Shared/SimpleXApp.swift` | App entry point: `haskell_init`, defaults registration, DB container setup, BG tasks |
| `Shared/AppDelegate.swift` | Push notification registration, URL handling |
| `Shared/ContentView.swift` | Root view: authentication, onboarding routing, chat initialization |
| `Shared/Views/Onboarding/OnboardingView.swift` | Onboarding step router, `OnboardingStage` enum |
| `Shared/Views/Onboarding/SimpleXInfo.swift` | Step 1: Privacy architecture explanation |
| `Shared/Views/Onboarding/CreateProfile.swift` | Profile creation: `CreateProfile` (additional) and `CreateFirstProfile` (onboarding) |
| `Shared/Views/Onboarding/ChooseServerOperators.swift` | Step 3: Server operator conditions |
| `Shared/Views/Onboarding/SetNotificationsMode.swift` | Step 4: Notification mode selection |
| `Shared/Views/Onboarding/CreateSimpleXAddress.swift` | Optional address creation during onboarding |
| `Shared/Views/Onboarding/HowItWorks.swift` | Educational content about SimpleX protocol |
| `Shared/Views/Migration/MigrateFromDevice.swift` | Source device migration UI |
| `Shared/Views/Migration/MigrateToDevice.swift` | Destination device migration UI |
| `Shared/Views/Database/DatabaseView.swift` | Database management: export, import, encryption |
| `Shared/Views/Database/DatabaseEncryptionView.swift` | Database passphrase management |
| `Shared/Views/Database/DatabaseErrorView.swift` | Database error recovery UI |
| `Shared/Views/Database/MigrateToAppGroupView.swift` | Legacy migration from Documents to App Group container |
| `Shared/Model/SimpleXAPI.swift` | `startChat`, `apiCreateActiveUser`, `apiExportArchive`, `apiImportArchive`, `apiRegisterToken` |
| `SimpleXChat/API.swift` | `chatMigrateInit`, `chatInitTemporaryDatabase`, low-level DB initialization |
| `SimpleXChat/FileUtils.swift` | DB file paths, constants (`DB_FILE_PREFIX`, `CHAT_DB`, `AGENT_DB`) |
| `SimpleXChat/AppGroup.swift` | App group container configuration |
| `SimpleXChat/KeyChain.swift` | Keychain access for DB passphrase and app passwords |
| `Shared/Model/BGManager.swift` | Background task registration and scheduling |
| `Shared/Model/NtfManager.swift` | Notification management and badge counts |

## Related Specifications

- `apps/ios/product/README.md` -- Product overview: architecture and capabilities
- `apps/ios/product/flows/connection.md` -- After onboarding, user establishes first connections
- `apps/ios/product/flows/messaging.md` -- Messaging starts after profile creation
