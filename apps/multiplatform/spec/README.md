# SimpleX Chat -- Kotlin Multiplatform Specification

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Dependency Graph](#dependency-graph)
3. [Specification Documents](#specification-documents)
4. [Product Documents](#product-documents)
5. [Source Entry Points](#source-entry-points)

---

## Executive Summary

SimpleX Chat is a Kotlin Multiplatform application targeting **Android** and **Desktop** (JVM) platforms. The UI layer is built entirely with Jetpack Compose. The application communicates with a Haskell-based cryptographic core (`simplex-chat`) through a **JNI bridge** -- native functions declared in Kotlin and linked at runtime to a shared library (`libapp-lib`). Platform-specific behavior (notifications, file system paths, services, audio/video) is abstracted using the `expect`/`actual` pattern and a runtime-assignable `PlatformInterface` callback object.

The Gradle project is structured as three modules:

| Module | Purpose |
|---|---|
| `:common` | Shared Compose UI, models, platform abstractions (`commonMain`, `androidMain`, `desktopMain`) |
| `:android` | Android application entry point (`SimplexApp`, `MainActivity`) |
| `:desktop` | Desktop application entry point (`Main.kt`, `showApp()`) |

All meaningful application logic resides in `:common/commonMain`. Platform source sets (`androidMain`, `desktopMain`) provide `actual` implementations for `expect` declarations and host platform-specific integration code.

---

## Dependency Graph

```
App Entry Points
+-- Android: SimplexApp.onCreate -> initHaskell -> initMultiplatform -> initChatControllerOnStart
|            MainActivity.onCreate -> setContent { AppScreen() }
+-- Desktop: main() -> initHaskell -> runMigrations -> initApp -> showApp -> AppWindow -> AppScreen()
    |
    v
Common Module (commonMain)
+-- ChatModel (Compose state singleton) <-> ChatController/SimpleXAPI (JNI bridge) <-> Haskell Core (chat_ctrl)
+-- Views (Compose)
|   +-- App.kt: AppScreen -> MainScreen
|   +-- ChatListView -> ChatView -> ComposeView -> SendMsgView
|   +-- ChatItemView (message rendering: text, image, video, voice, file, call, events)
|   +-- Settings: SettingsView, UserProfileView, UserProfilesView
|   +-- Onboarding: OnboardingView, WhatsNewView, CreateFirstProfile
|   +-- Call: CallView, IncomingCallAlertView
|   +-- Database: DatabaseView, DatabaseEncryptionView, DatabaseErrorView
|   +-- Groups: GroupChatInfoView, AddGroupMembersView, GroupMemberInfoView
|   +-- Contacts: ContactListNavView
|   +-- Remote: ConnectDesktopView, ConnectMobileView
|   +-- Terminal: TerminalView
+-- Models
|   +-- ChatModel       -- global app state (Compose MutableState singleton)
|   +-- ChatsContext     -- per-context chat list state (primary + optional secondary)
|   +-- Chat             -- per-conversation state (chatInfo, chatItems, chatStats)
|   +-- ChatController   -- API command dispatch, event receiver, preferences
|   +-- AppPreferences   -- 150+ SharedPreferences keys
+-- Services
|   +-- NtfManager       -- abstract notification coordinator (Android/Desktop implementations)
|   +-- SimplexService   -- Android foreground service for background messaging
|   +-- ThemeManager     -- theme resolution (system/light/dark/simplex/black + per-user overrides)
|   +-- CallManager      -- WebRTC call lifecycle
+-- Platform (expect/actual)
    +-- Core.kt          -- JNI declarations (external fun), initChatController, chatInitTemporaryDatabase
    +-- AppCommon.kt     -- runMigrations, AppPlatform enum
    +-- Files.kt         -- dataDir, tmpDir, filesDir, dbAbsolutePrefixPath (expect)
    +-- Share.kt         -- shareText, shareFile, openFile (expect)
    +-- VideoPlayer.kt   -- VideoPlayerInterface, VideoPlayer (expect class)
    +-- RecAndPlay.kt    -- RecorderInterface, AudioPlayerInterface (expect)
    +-- UI.kt            -- showToast, hideKeyboard, getKeyboardState (expect)
    +-- Notifications.kt -- allowedToShowNotification (expect)
    +-- NtfManager.kt    -- abstract NtfManager class
    +-- Platform.kt      -- PlatformInterface (runtime callback object)
    +-- Cryptor.kt       -- CryptorInterface (expect)
    +-- Images.kt        -- bitmap utilities (expect)
    +-- SimplexService.kt-- getWakeLock (expect)
    +-- Log.kt, Modifier.kt, Back.kt, ScrollableColumn.kt, PlatformTextField.kt, Resources.kt
```

---

## Specification Documents

| Document | Path | Description |
|---|---|---|
| Architecture | [spec/architecture.md](architecture.md) | System layers, module structure, JNI bridge, app lifecycle, event streaming, platform abstraction |
| State Management | [spec/state.md](state.md) | ChatModel singleton, ChatsContext, Chat data class, AppPreferences, ActiveChatState |

---

## Product Documents

Product documents are not yet created for the Kotlin Multiplatform project. The equivalent iOS product documents (for reference structure) reside in `apps/ios/product/`:

| Category | iOS Equivalent Path | Topic |
|---|---|---|
| Overview | `apps/ios/product/README.md` | Product overview |
| Concepts | `apps/ios/product/concepts.md` | Core domain concepts |
| Glossary | `apps/ios/product/glossary.md` | Term definitions |
| Rules | `apps/ios/product/rules.md` | Product rules |
| Gaps | `apps/ios/product/gaps.md` | Known gaps |
| Flows | `apps/ios/product/flows/` | onboarding, messaging, connection, calling, file-transfer, group-lifecycle |
| Views | `apps/ios/product/views/` | chat-list, chat, settings, onboarding, call, new-chat, contact-info, group-info, user-profiles |

Planned directories for Kotlin Multiplatform product docs:

- `apps/multiplatform/product/flows/` -- user interaction flows
- `apps/multiplatform/product/views/` -- view-level product specs

---

## Source Entry Points

| Component | File | Key Symbol | Line |
|---|---|---|---|
| Android Application | [`SimplexApp.kt`](../android/src/main/java/chat/simplex/app/SimplexApp.kt#L41) | `class SimplexApp` | 41 |
| Android Activity | [`MainActivity.kt`](../android/src/main/java/chat/simplex/app/MainActivity.kt#L27) | `class MainActivity` | 27 |
| Desktop Entry | [`Main.kt`](../desktop/src/jvmMain/kotlin/chat/simplex/desktop/Main.kt#L21) | `fun main()` | 21 |
| Desktop App Window | [`DesktopApp.kt`](../common/src/desktopMain/kotlin/chat/simplex/common/DesktopApp.kt#L33) | `fun showApp()` | 33 |
| Desktop Init | [`AppCommon.desktop.kt`](../common/src/desktopMain/kotlin/chat/simplex/common/platform/AppCommon.desktop.kt#L21) | `fun initApp()` | 21 |
| Common App Screen | [`App.kt`](../common/src/commonMain/kotlin/chat/simplex/common/App.kt#L47) | `fun AppScreen()` | 46 |
| JNI Bridge | [`Core.kt`](../common/src/commonMain/kotlin/chat/simplex/common/platform/Core.kt#L18) | `external fun initHS()` | 17 |
| Chat Controller | [`SimpleXAPI.kt`](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L493) | `object ChatController` | 492 |
| Chat Model | [`ChatModel.kt`](../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L86) | `object ChatModel` | 85 |
| App Preferences | [`SimpleXAPI.kt`](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L94) | `class AppPreferences` | 93 |
| Platform Interface | [`Platform.kt`](../common/src/commonMain/kotlin/chat/simplex/common/platform/Platform.kt#L15) | `interface PlatformInterface` | 15 |
| Notification Manager | [`NtfManager.kt`](../common/src/commonMain/kotlin/chat/simplex/common/platform/NtfManager.kt#L19) | `abstract class NtfManager` | 18 |
| Theme Manager | [`ThemeManager.kt`](../common/src/commonMain/kotlin/chat/simplex/common/ui/theme/ThemeManager.kt#L18) | `object ThemeManager` | 18 |
| Android Haskell Init | [`AppCommon.android.kt`](../common/src/androidMain/kotlin/chat/simplex/common/platform/AppCommon.android.kt#L33) | `fun initHaskell()` | 33 |
| Common Migrations | [`AppCommon.kt`](../common/src/commonMain/kotlin/chat/simplex/common/platform/AppCommon.kt#L41) | `fun runMigrations()` | 41 |
| Android Service | [`SimplexService.kt`](../android/src/main/java/chat/simplex/app/SimplexService.kt#L41) | `class SimplexService` | 41 |
| Gradle Root | [`settings.gradle.kts`](../settings.gradle.kts#L22) | `include(":android", ":desktop", ":common")` | 22 |
| Common Build | [`build.gradle.kts`](../common/build.gradle.kts#L14) | `kotlin { androidTarget(); jvm("desktop") }` | 14 |
