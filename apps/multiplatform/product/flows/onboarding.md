# Onboarding Flow

> **Related spec:** [spec/client/navigation.md](../../spec/client/navigation.md) | [spec/architecture.md](../../spec/architecture.md)

## Overview

Onboarding is the first-run experience that initializes the Haskell chat core, creates the local database, sets up the user profile, configures server operators, and (on Android) selects the notification mode. The flow is tracked by the `OnboardingStage` enum persisted in `AppPreferences.onboardingStage`.

The initialization path differs slightly between Android and Desktop, but both converge on the common `chatMigrateInit` JNI call and shared `ChatController` logic.

## Prerequisites

- Fresh install or database reset.
- On Android: `SimplexApp.onCreate()` has been called.
- On Desktop: `main()` has been called.

---

## 1. Platform Initialization

### 1.1 Android: SimplexApp.onCreate()

1. `SimplexApp.onCreate()` is called by the Android framework.
2. `AppContextProvider.initialize(this)` sets the application context.
3. Phoenix process detection: if this is a restart process, return early.
4. A global error handler is registered.
5. `initHaskell(packageName)` loads the native `libapp-lib.so` and calls `initHS()` to initialize the Haskell runtime.
6. `initMultiplatform()` sets up:
   - `androidAppContext` reference.
   - `ntfManager` (notification manager bridge to Android `NtfManager`).
   - `platform` interface implementation with Android-specific callbacks for services, notifications, call management, and UI configuration.
7. `reconfigureBroadcastReceivers()` ensures notification-related receivers match saved preferences.
8. `runMigrations()` performs any pending app-level data migrations.
9. Temp directory is cleaned and recreated.
10. If a migration state exists (`chatModel.migrationState.value != null`), onboarding is forced to `Step1_SimpleXInfo`.
11. Otherwise, if authentication keys are available, `initChatControllerOnStart()` is called.

### 1.2 Desktop: Main.kt main()

1. `initHaskell()` loads native libraries:
   - On Linux/macOS: `libapp-lib.so` / `libapp-lib.dylib`.
   - On Windows: `libcrypto-3-x64.dll`, `libsimplex.dll`, `libapp-lib.dll` plus VLC libraries.
2. `initHS()` initializes the Haskell runtime.
3. `platform` interface is set with Desktop-specific callbacks (app update notice).
4. `runMigrations()` performs pending app-level data migrations.
5. `setupUpdateChecker()` configures the desktop update channel.
6. `initApp()` initializes common app state.
7. Temp directory is cleaned and recreated.
8. `showApp()` launches the Compose Desktop window, which renders the `AppView`.

---

## 2. Database Initialization (chatMigrateInit)

### 2.1 initChatController

1. `initChatController(useKey, confirmMigrations, startChat)` is called (from `Core.kt`).
2. If `ctrlInitInProgress` is already true, return (prevents double initialization).
3. The database key is resolved:
   - From `useKey` parameter if provided.
   - Otherwise from `DatabaseUtils.useDatabaseKey()` which reads from the keystore.
4. Migration confirmation mode is determined:
   - `MigrationConfirmation.YesUp` (auto-confirm forward migrations) by default.
   - `MigrationConfirmation.Error` if developer tools + confirm upgrades are enabled.
5. `chatMigrateInit(dbPath, dbKey, confirm)` is called via JNI. This:
   - Opens (or creates) the SQLite database at `dbAbsolutePrefixPath`.
   - Runs all pending schema migrations.
   - Returns a `ChatCtrl` handle (Long) and a `DBMigrationResult`.
6. On `DBMigrationResult.OK`:
   - The `ChatCtrl` is stored globally.
   - `ChatModel.chatDbStatus` is set.
   - App file paths are configured via `apiSetAppFilePaths`.
   - `apiGetActiveUser` checks for an existing user.
7. If an active user exists, `startChat(user)` is called.
8. If no user exists, `startChatWithoutUser()` is called and onboarding begins at `Step1_SimpleXInfo`.

### 2.2 Error Handling

- `DBMigrationResult.ErrorNotADatabase`: Wrong passphrase or corrupted DB. User is prompted.
- `DBMigrationResult.ErrorMigration`: Migration failed. Details shown to user.
- `DBMigrationResult.ErrorKeyNotSet`: Encryption key missing.
- `DBMigrationResult.InvalidConfirmation`: Migrations need manual confirmation (developer mode).
- On any error, `ChatModel.chatDbStatus` is set and the UI shows the appropriate database error screen.

---

## 3. Onboarding Stages

The onboarding flow is controlled by `OnboardingStage`, persisted in `AppPreferences.onboardingStage`:

```kotlin
enum class OnboardingStage {
  Step1_SimpleXInfo,
  Step2_CreateProfile,
  LinkAMobile,
  Step2_5_SetupDatabasePassphrase,
  Step3_ChooseServerOperators,
  Step3_CreateSimpleXAddress,
  Step4_SetNotificationsMode,
  OnboardingComplete
}
```

### 3.1 Step1_SimpleXInfo

1. The `SimpleXInfo` screen is shown.
2. Explains what SimpleX Chat is: privacy, no user identifiers, decentralized.
3. User taps "Create your profile" to proceed.
4. On Desktop, a "Link a Mobile" option is also available.

### 3.2 Step2_CreateProfile

1. The `CreateProfile` screen is shown.
2. User enters a display name (validated via `chatValidName` JNI) and optional full name.
3. On submit, `ChatController.apiCreateActiveUser(rh, profile)` is called:
   ```kotlin
   suspend fun apiCreateActiveUser(rh: Long?, p: Profile?, pastTimestamp: Boolean = false, ctrl: ChatCtrl? = null): User?
   ```
4. The core command `CC.CreateActiveUser(p, pastTimestamp)` creates the user in the database.
5. On success, `CR.ActiveUser` returns the new `User` object.
6. `ChatModel.currentUser` is set.
7. If the chat is not yet running, `startChat(user)` is called:
   - `apiSetNetworkConfig` configures network settings.
   - `apiStartChat` starts the message receiver.
   - `startReceiver()` begins polling for incoming messages.
8. Onboarding advances to `Step3_ChooseServerOperators`.

### 3.3 LinkAMobile (Desktop Only)

1. Available as an alternative to creating a profile on Desktop.
2. Shows a QR code for linking with a mobile device.
3. The desktop acts as a remote host controlled by the mobile app.

### 3.4 Step2_5_SetupDatabasePassphrase (Desktop Only)

1. On Desktop, after profile creation, the user is optionally prompted to set a database passphrase.
2. If skipped, a random passphrase is used (`desktopOnboardingRandomPassword` flag).
3. `ChatModel.desktopOnboardingRandomPassword` tracks this state.

### 3.5 Step3_ChooseServerOperators

1. The `ChooseServerOperators` screen is shown.
2. User selects which preset server operators to use for messaging and file transfer.
3. Server operator conditions may need to be accepted.
4. The selection is saved via the server configuration APIs.

### 3.6 Step3_CreateSimpleXAddress

1. User is prompted to create a SimpleX address for receiving contact requests.
2. This calls the address creation API.
3. Can be skipped.

### 3.7 Step4_SetNotificationsMode (Android Only)

1. The `SetNotificationsMode` screen is shown.
2. Three modes are available:
   - `NotificationsMode.SERVICE`: Persistent background service (instant notifications).
   - `NotificationsMode.PERIODIC`: Periodic background work (delayed notifications).
   - `NotificationsMode.OFF`: No background processing (manual check only).
3. On selection, `appPrefs.notificationsMode` is set.
4. On Desktop, this step is skipped entirely.

### 3.8 OnboardingComplete

1. `appPrefs.onboardingStage` is set to `OnboardingComplete`.
2. The chat list view (`ChatListView`) is shown.
3. On Android, `SimplexService.showBackgroundServiceNoticeIfNeeded()` may show additional setup prompts.
4. On Android with `NotificationsMode.SERVICE`, `SimplexService.start()` is called.

---

## 4. startChat Flow

After the user is created and onboarding progresses, `ChatController.startChat(user)` orchestrates the final setup:

1. `apiSetNetworkConfig(getNetCfg())` applies network configuration.
2. `apiCheckChatRunning()` checks if the core is already running.
3. `listUsers(null)` loads all user profiles into `ChatModel.users`.
4. If chat is not running:
   - `ChatModel.currentUser` is set.
   - `apiStartChat()` starts the core's message processing.
   - `startReceiver()` begins the message receive loop.
   - `setLocalDeviceName` sets the device name for remote access.
5. `apiGetChats` loads the chat list.
6. `chatModel.chatsContext.updateChats(chats)` populates the UI.
7. User address and chat item TTL are loaded.
8. `appPrefs.chatLastStart` is updated.
9. `ChatModel.chatRunning` is set to `true`.
10. `platform.androidChatInitializedAndStarted()` is called for Android-specific post-start tasks.

---

## Key Types Reference

| Type | Location | Purpose |
|------|----------|---------|
| `OnboardingStage` | `views/onboarding/OnboardingView.kt` | Enum tracking onboarding progress |
| `SimplexApp` | `android/.../SimplexApp.kt` | Android Application class, entry point |
| `Main.kt` | `desktop/.../Main.kt` | Desktop entry point |
| `ChatController` | `model/SimpleXAPI.kt` | Core API controller, manages chat lifecycle |
| `ChatModel` | `model/ChatModel.kt` | Global observable state |
| `DBMigrationResult` | `views/helpers/DatabaseUtils.kt` | Database migration outcome |
| `chatMigrateInit` | `platform/Core.kt` | JNI function: initialize DB and run migrations |
| `initChatController` | `platform/Core.kt` | High-level initialization orchestrator |
| `AppPreferences` | `model/SimpleXAPI.kt` | Persistent user preferences |
