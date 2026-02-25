# SimpleX Chat Android & Desktop -- Product Overview

> SimpleX Chat multiplatform product specification (Android + Desktop). Bidirectional code links: product docs reference source files, source files reference product docs.
>
> **Related spec:** [spec/README.md](../spec/README.md) | [spec/architecture.md](../spec/architecture.md)

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Vision](#vision)
3. [Target Users](#target-users)
4. [Capability Map](#capability-map)
5. [Navigation Map](#navigation-map)
6. [Related Specifications](#related-specifications)

## Executive Summary

SimpleX Chat is the first messaging platform with no user identifiers of any kind -- not even random numbers. It provides end-to-end encrypted messaging (with optional post-quantum cryptography), audio/video calls, file sharing, and group communication through a fully decentralized architecture where users control their own SMP relay servers.

The Android and Desktop apps share a single **Kotlin Multiplatform + Compose Multiplatform** codebase. Common UI and business logic lives in a shared `common/` module, while platform-specific behavior (notifications, audio, video playback, file system access, call management) is abstracted through the Kotlin `expect`/`actual` pattern and a runtime `PlatformInterface` delegate. The Haskell core library is loaded via **JNI** (`external fun` declarations in `Core.kt`), exposing the full SimpleX Chat API (message send/receive, encryption, migration, file handling) through native FFI.

Key platform differences:

- **Android** uses a 2-column layout (`AndroidScreen`): chat list slides to chat view. Background messaging is handled by `SimplexService` (foreground service) + `MessagesFetcherWorker` (WorkManager periodic fetch). Calls use a dedicated `CallService` + `CallActivity`.
- **Desktop** uses a 3-column layout (`DesktopScreen`): chat list (start) | chat view (center) | detail panel (`ModalManager.end`). It includes `AppUpdater` for in-app update checking, `StoreWindowState` for window geometry persistence, and VLC-based video playback. Calls use browser-based WebRTC rendered inline.

---

## Vision

SimpleX Chat is the first messaging platform that has no user identifiers -- not even random numbers. It uses double-ratchet end-to-end encryption with optional post-quantum cryptography. The system is fully decentralized with user-controlled SMP relay servers.

The protocol design ensures that no server or network observer can determine who communicates with whom. Each conversation uses separate unidirectional messaging queues on potentially different servers, and there is no shared identifier between the sender and receiver queues.

---

## Target Users

- **Privacy-conscious individuals** wanting secure messaging without phone-number or email-based identity
- **Groups and communities** needing encrypted group communication with role-based access control
- **Users avoiding identity linkage** who want to communicate without any persistent user identifier
- **Organizations** needing self-hosted messaging infrastructure with full control over relay servers
- **Desktop users** wanting a native desktop client with the same privacy guarantees as the mobile app

---

## Capability Map

All source paths below are relative to `apps/multiplatform/`. The common source root is `common/src/commonMain/kotlin/chat/simplex/common/`.

### 1. Messaging

Core message composition, delivery, and interaction features.

| Feature | Description | Key Source (Kotlin) |
|---------|-------------|---------------------|
| Text with markdown | Rich text formatting with SimpleX markdown syntax | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/ComposeView.kt` |
| Images | Compressed inline images with full-screen gallery | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/CIImageView.kt` |
| Video | Video message recording and playback | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/CIVideoView.kt` |
| Voice messages | Audio recording and playback (5min / 510KB limit) | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/CIVoiceView.kt` |
| File sharing | Files up to 1GB via XFTP protocol | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/CIFileView.kt` |
| Link previews | OpenGraph metadata extraction and display | `common/src/commonMain/kotlin/chat/simplex/common/views/helpers/LinkPreviews.kt` |
| Message reactions | Emoji reactions on sent/received messages | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/EmojiItemView.kt` |
| Message editing | Edit previously sent messages | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/ComposeView.kt` |
| Message deletion | Broadcast delete (for recipient) or internal-only delete | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/MarkedDeletedItemView.kt` |
| Timed messages | Self-destructing messages with configurable TTL | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/CIChatFeatureView.kt` |
| Quoted replies | Reply to specific messages with quote context | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/ContextItemView.kt` |
| Forwarding | Forward messages between chats | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/ComposeView.kt` |
| Search | Full-text search within conversations | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatView.kt` |
| Message reports | Report messages to group moderators | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/group/GroupReportsView.kt` |
| Send message bar | Composable message input with attachments, voice, send button | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/SendMsgView.kt` |

### 2. Contacts

Establishing, managing, and verifying contacts.

| Feature | Description | Key Source (Kotlin) |
|---------|-------------|---------------------|
| Add via SimpleX address | Connect using a SimpleX contact address | `common/src/commonMain/kotlin/chat/simplex/common/views/newchat/NewChatView.kt` |
| Add via QR code | Scan QR code to establish connection | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/ScanCodeView.kt` |
| Contact requests | Accept or reject incoming contact requests | `common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ContactRequestView.kt` |
| Local aliases | Set private display names for contacts | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatInfoView.kt` |
| Contact verification | Compare security codes out-of-band | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/VerifyCodeView.kt` |
| Blocking | Block contacts from sending messages | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatInfoView.kt` |
| Incognito mode | Per-contact random profile generation | `common/src/commonMain/kotlin/chat/simplex/common/views/usersettings/IncognitoView.kt` |
| Bot detection | Identify automated/bot contacts | `common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt` |
| Contact list | Dedicated contact browsing view | `common/src/commonMain/kotlin/chat/simplex/common/views/contacts/ContactListNavView.kt` |

### 3. Groups

Multi-party encrypted conversations with role-based management.

| Feature | Description | Key Source (Kotlin) |
|---------|-------------|---------------------|
| Create groups | Create new group with initial members | `common/src/commonMain/kotlin/chat/simplex/common/views/newchat/AddGroupView.kt` |
| Invite members | Invite by individual contact or link | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/group/AddGroupMembersView.kt` |
| Member roles | Owner, admin, moderator, member, observer | `common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt` |
| Member admission | Queue-based admission with review workflow | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/group/MemberAdmission.kt` |
| Group links | Shareable invite links for groups | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/group/GroupLinkView.kt` |
| Business chat mode | Structured business communication groups | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/group/GroupChatInfoView.kt` |
| Content moderation | Member reports and moderator actions | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/group/MemberSupportView.kt` |
| Group preferences | Configure group-level feature settings | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/group/GroupPreferences.kt` |
| Member direct contacts | Establish direct chats from group membership | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/group/GroupMemberInfoView.kt` |
| Group mentions | @-mention members in group messages | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/group/GroupMentions.kt` |
| Welcome message | Custom welcome message for new group members | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/group/WelcomeMessageView.kt` |
| Group profile | Edit group name, image, description | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/group/GroupProfileView.kt` |
| Member support chat | Scoped support threads between members and admins | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/group/MemberSupportChatView.kt` |

### 4. Calling

End-to-end encrypted audio and video communication.

| Feature | Description | Key Source (Kotlin) |
|---------|-------------|---------------------|
| E2E encrypted calls | Audio/video calls via WebRTC with E2E encryption | `common/src/commonMain/kotlin/chat/simplex/common/views/call/WebRTC.kt` |
| Call manager | Call state machine and lifecycle management | `common/src/commonMain/kotlin/chat/simplex/common/views/call/CallManager.kt` |
| Call history | Call events displayed as chat items | `common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/CICallItemView.kt` |
| Incoming call view | Dedicated UI for incoming call notifications | `common/src/commonMain/kotlin/chat/simplex/common/views/call/IncomingCallAlertView.kt` |
| Android CallService | Foreground service for active calls on Android | `android/src/main/java/chat/simplex/app/CallService.kt` |
| Android CallActivity | Dedicated Activity for call UI on Android | `android/src/main/java/chat/simplex/app/views/call/CallActivity.kt` |
| Desktop inline calls | Browser-based WebRTC rendered inline in desktop window | `common/src/commonMain/kotlin/chat/simplex/common/views/call/CallView.kt` |

### 5. Privacy & Security

Encryption, authentication, and privacy controls.

| Feature | Description | Key Source (Kotlin) |
|---------|-------------|---------------------|
| E2E encryption | Double-ratchet encryption for all messages | `common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt` |
| Post-quantum encryption | Optional PQ key exchange for direct chats | `common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt` |
| Local authentication | Biometric (fingerprint/face) or app passcode lock | `common/src/commonMain/kotlin/chat/simplex/common/views/localauth/LocalAuthView.kt` |
| Passcode entry | Custom numeric/alphanumeric passcode UI | `common/src/commonMain/kotlin/chat/simplex/common/views/localauth/PasscodeView.kt` |
| Hidden profiles | Password-protected profiles invisible in UI | `common/src/commonMain/kotlin/chat/simplex/common/views/usersettings/HiddenProfileView.kt` |
| Database encryption | AES encryption of local SQLite database | `common/src/commonMain/kotlin/chat/simplex/common/views/database/DatabaseEncryptionView.kt` |
| Screen privacy | Blur/hide app content when in app switcher | `common/src/commonMain/kotlin/chat/simplex/common/views/usersettings/PrivacySettings.kt` |
| Encrypted file storage | Local files encrypted at rest | `common/src/commonMain/kotlin/chat/simplex/common/model/CryptoFile.kt` |
| Delivery receipts control | Toggle delivery/read receipts per contact/group | `common/src/commonMain/kotlin/chat/simplex/common/views/usersettings/SetDeliveryReceiptsView.kt` |
| App lock | Automatic lock on background/timeout with configurable delay | `common/src/commonMain/kotlin/chat/simplex/common/AppLock.kt` |

### 6. User Management

Multiple profiles and identity management.

| Feature | Description | Key Source (Kotlin) |
|---------|-------------|---------------------|
| Multiple profiles | Multiple user profiles within one app | `common/src/commonMain/kotlin/chat/simplex/common/views/usersettings/UserProfilesView.kt` |
| Active user switching | Switch between profiles via user picker | `common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/UserPicker.kt` |
| Incognito contacts | Per-contact random identities | `common/src/commonMain/kotlin/chat/simplex/common/views/usersettings/IncognitoView.kt` |
| Profile sharing | Share profile via contact address link | `common/src/commonMain/kotlin/chat/simplex/common/views/usersettings/UserAddressView.kt` |
| User muting | Mute notifications for specific profiles | `common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/UserPicker.kt` |
| User profile editing | Edit display name and profile image | `common/src/commonMain/kotlin/chat/simplex/common/views/usersettings/UserProfileView.kt` |

### 7. Network

Server configuration, proxy support, and connectivity.

| Feature | Description | Key Source (Kotlin) |
|---------|-------------|---------------------|
| Custom SMP servers | Configure personal SMP relay servers | `common/src/commonMain/kotlin/chat/simplex/common/views/usersettings/networkAndServers/ProtocolServersView.kt` |
| Custom XFTP servers | Configure personal XFTP file servers | `common/src/commonMain/kotlin/chat/simplex/common/views/usersettings/networkAndServers/ProtocolServersView.kt` |
| Tor/onion support | Route traffic through Tor .onion addresses | `common/src/commonMain/kotlin/chat/simplex/common/views/usersettings/networkAndServers/AdvancedNetworkSettings.kt` |
| SOCKS5 proxy | Route connections through SOCKS5 proxy | `common/src/commonMain/kotlin/chat/simplex/common/views/usersettings/networkAndServers/AdvancedNetworkSettings.kt` |
| Custom ICE servers | Configure WebRTC ICE/TURN servers | `common/src/commonMain/kotlin/chat/simplex/common/views/usersettings/RTCServers.kt` |
| Network timeouts | Configure connection timeout parameters | `common/src/commonMain/kotlin/chat/simplex/common/views/usersettings/networkAndServers/AdvancedNetworkSettings.kt` |
| Server operators | Configure and manage SMP/XFTP server operators | `common/src/commonMain/kotlin/chat/simplex/common/views/usersettings/networkAndServers/OperatorView.kt` |
| Server status | View aggregate server connectivity status | `common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ServersSummaryView.kt` |
| Network & servers hub | Central network configuration entry point | `common/src/commonMain/kotlin/chat/simplex/common/views/usersettings/networkAndServers/NetworkAndServers.kt` |

### 8. Customization

Visual appearance and UI preferences.

| Feature | Description | Key Source (Kotlin) |
|---------|-------------|---------------------|
| Themes | Light, dark, SimpleX, black, and custom themes | `common/src/commonMain/kotlin/chat/simplex/common/ui/theme/ThemeManager.kt` |
| Wallpapers | Preset and custom chat wallpapers | `common/src/commonMain/kotlin/chat/simplex/common/views/helpers/ChatWallpaper.kt` |
| Chat bubble styling | Customize message bubble appearance | `common/src/commonMain/kotlin/chat/simplex/common/ui/theme/Theme.kt` |
| One-handed UI mode | Compact layout for single-hand use (Android) | `common/src/commonMain/kotlin/chat/simplex/common/views/usersettings/Appearance.kt` |
| Language selection | In-app language override | `common/src/commonMain/kotlin/chat/simplex/common/views/usersettings/Appearance.kt` |
| Theme mode editor | Interactive theme color and mode customization | `common/src/commonMain/kotlin/chat/simplex/common/views/helpers/ThemeModeEditor.kt` |

### 9. Data Management

Import, export, encryption, and storage management.

| Feature | Description | Key Source (Kotlin) |
|---------|-------------|---------------------|
| Export/import profiles | Full database export and import | `common/src/commonMain/kotlin/chat/simplex/common/views/database/DatabaseView.kt` |
| Database encryption | Encrypt/decrypt local database with passphrase | `common/src/commonMain/kotlin/chat/simplex/common/views/database/DatabaseEncryptionView.kt` |
| Local file encryption | Encrypt stored media and attachments | `common/src/commonMain/kotlin/chat/simplex/common/model/CryptoFile.kt` |
| Database error handling | Recovery UI for database migration failures | `common/src/commonMain/kotlin/chat/simplex/common/views/database/DatabaseErrorView.kt` |
| Device-to-device migration | Migrate full profile between devices | `common/src/commonMain/kotlin/chat/simplex/common/views/migration/MigrateFromDevice.kt` |
| Receive migration | Accept incoming device migration transfer | `common/src/commonMain/kotlin/chat/simplex/common/views/migration/MigrateToDevice.kt` |
| Database utilities | Key storage, password management, helper functions | `common/src/commonMain/kotlin/chat/simplex/common/views/helpers/DatabaseUtils.kt` |

### 10. Desktop Features

Desktop-specific functionality not present on Android.

| Feature | Description | Key Source (Kotlin) |
|---------|-------------|---------------------|
| 3-column layout | Start (chat list) / center (chat) / end (detail) panels | `common/src/commonMain/kotlin/chat/simplex/common/App.kt` (`DesktopScreen`) |
| ModalManager.end | Third-column detail panel for settings/info views | `common/src/commonMain/kotlin/chat/simplex/common/views/helpers/ModalView.kt` (`ModalManager`) |
| App update checker | In-app notification for available updates | `common/src/desktopMain/kotlin/chat/simplex/common/views/helpers/AppUpdater.kt` |
| Window state persistence | Save/restore window position and dimensions | `common/src/desktopMain/kotlin/chat/simplex/common/StoreWindowState.kt` |
| VLC video playback | Desktop video playback via VLC native libraries | `common/src/desktopMain/kotlin/chat/simplex/common/platform/VideoPlayer.desktop.kt` |
| Desktop app entry | Main function, Haskell init, VLC loading | `desktop/src/jvmMain/kotlin/chat/simplex/desktop/Main.kt` |
| Desktop notification manager | Platform-native desktop notifications | `common/src/desktopMain/kotlin/chat/simplex/common/platform/Notifications.desktop.kt` |
| Connect mobile device | Pair desktop with a mobile device for remote access | `common/src/commonMain/kotlin/chat/simplex/common/views/remote/ConnectMobileView.kt` |
| Desktop platform abstraction | Desktop-specific PlatformInterface implementation | `common/src/desktopMain/kotlin/chat/simplex/common/platform/AppCommon.desktop.kt` |
| Desktop app shell | Compose Desktop window, theming, lifecycle | `common/src/desktopMain/kotlin/chat/simplex/common/DesktopApp.kt` |

---

## Navigation Map

### Android Navigation (2-column slide)

```
Onboarding
  views/onboarding/SimpleXInfo.kt
    -> SimpleXInfo -> CreateFirstProfile -> SetupDatabasePassphrase
    -> ChooseServerOperators -> SetNotificationsMode
    -> ChatListView (home)

ChatListView (home)
  views/chatlist/ChatListView.kt
    -> ChatView .................. (tap conversation row, slides in)
    -> NewChatSheet .............. (+ FAB button)
    -> SettingsView .............. (gear icon)
    -> UserPicker ................ (avatar tap)
    -> TagListView ............... (tag filter bar)
    -> ServersSummaryView ........ (server status indicator)
    -> ShareListView ............. (share intent from external apps)
    -> ChatHelpView .............. (empty state help)

ChatView
  views/chat/ChatView.kt
    -> ChatInfoView .............. (contact name tap, direct chat)
    -> GroupChatInfoView ......... (group name tap, group chat)
    -> ActiveCallView ............ (call button, launches CallActivity)
    -> ComposeView ............... (message input area)
    -> ChatItemInfoView .......... (long press -> info)
    -> MemberSupportChatView ..... (member support thread)
    -> ScanCodeView .............. (scan QR)
    -> CommandsMenuView .......... (/ commands)

ChatInfoView
  views/chat/ChatInfoView.kt
    -> ContactPreferences ........ (preferences)
    -> VerifyCodeView ............ (verify security code)

GroupChatInfoView
  views/chat/group/GroupChatInfoView.kt
    -> GroupProfileView .......... (edit profile)
    -> AddGroupMembersView ....... (invite members)
    -> GroupLinkView ............. (manage group link)
    -> MemberAdmission ........... (admission settings)
    -> GroupPreferences .......... (group feature settings)
    -> GroupMemberInfoView ....... (tap member)
    -> WelcomeMessageView ........ (welcome message)
    -> GroupReportsView .......... (view reports)

NewChatSheet
  views/newchat/NewChatSheet.kt
    -> NewChatView ............... (QR scanner / paste link)
    -> AddGroupView .............. (create group)
    -> UserAddressView ........... (create SimpleX address)

SettingsView
  views/usersettings/SettingsView.kt
    -> AppearanceView ............ (themes, wallpapers, UI)
    -> NetworkAndServers ......... (SMP/XFTP/proxy config)
    -> PrivacySettings ........... (privacy toggles)
    -> NotificationsSettingsView . (notification mode)
    -> DatabaseView .............. (export/import/encrypt)
    -> CallSettings .............. (call preferences)
    -> VersionInfoView ........... (about/version)
    -> DeveloperView ............. (developer options)
    -> HelpView .................. (help & support)

UserPicker
  views/chatlist/UserPicker.kt
    -> UserProfilesView .......... (manage all profiles)
    -> UserAddressView ........... (SimpleX address)
    -> Preferences ............... (user preferences)
    -> SettingsView .............. (app settings)
    -> ConnectDesktopView ........ (pair with desktop)
```

### Desktop Navigation (3-column panels)

```
+---------------------------+----------------------------------+----------------------------+
|     START PANEL           |        CENTER PANEL              |       END PANEL            |
|  (DEFAULT_START_MODAL_    |   (flexible width, min           |  (DEFAULT_END_MODAL_       |
|   WIDTH)                  |    DEFAULT_MIN_CENTER_MODAL_     |   WIDTH)                   |
|                           |    WIDTH)                        |                            |
+---------------------------+----------------------------------+----------------------------+
|                           |                                  |                            |
|  ChatListView             |  ChatView                        |  ChatInfoView              |
|    - chat rows            |    - message list                |  GroupChatInfoView         |
|    - search               |    - ComposeView                 |  GroupMemberInfoView       |
|    - tag filters          |    - media viewer                |  ContactPreferences        |
|    - server status        |                                  |  GroupPreferences          |
|                           |  OR (when no chat selected):     |  GroupProfileView          |
|  UserPicker (overlay)     |    "No selected chat"            |  AddGroupMembersView       |
|    - profile switcher     |                                  |  MemberAdmission           |
|    - quick settings       |  OR (when modal open):           |  VerifyCodeView            |
|                           |    ModalManager.center content   |  SettingsView subtabs      |
|  ModalManager.start       |    (settings, new chat, etc.)    |                            |
|    - secondary modals     |                                  |  ModalManager.end          |
|                           |                                  |    - detail modals         |
+---------------------------+----------------------------------+----------------------------+

ModalManager Placement (Desktop):
  - ModalManager.start  -> left panel overlay (settings subviews)
  - ModalManager.center -> center panel (replaces chat, used when chatId is null)
  - ModalManager.end    -> right panel (detail/info views)
  - ModalManager.fullscreen -> full window overlay (onboarding, auth, call)

On Android, all ModalManager instances (start/center/end/fullscreen) collapse to a
single shared ModalManager that presents modals as full-screen overlays.

Desktop-only navigation targets:
  ConnectMobileView ......... (pair with mobile device)
  AppUpdater notice ......... (update available notification)
  Floating terminal ......... (developer console)
  ActiveCallView ............ (inline WebRTC call, not separate Activity)
```

---

## Platform Abstraction

The codebase uses two mechanisms for platform-specific behavior:

### 1. `expect`/`actual` Declarations

Kotlin Multiplatform `expect` declarations in `common/src/commonMain/kotlin/chat/simplex/common/platform/` with corresponding `actual` implementations in:
- `common/src/androidMain/kotlin/chat/simplex/common/platform/*.android.kt`
- `common/src/desktopMain/kotlin/chat/simplex/common/platform/*.desktop.kt`

Key `expect`/`actual` abstractions: `appPlatform`, `BackHandler`, `VideoPlayer`, `AudioPlayer`, `RecorderNative`, `NtfManager`, `showToast`, `getKeyboardState`, `PlatformTextField`, image processing, file sharing, and more.

### 2. Runtime `PlatformInterface`

Defined in `common/src/commonMain/kotlin/chat/simplex/common/platform/Platform.kt`, this interface provides platform-specific callbacks that cannot use `expect`/`actual` (because `android/` module code cannot be called from `common/androidMain/`). The `platform` variable is reassigned at app startup:
- **Android:** `SimplexApp` sets `platform` to an implementation with `CallService`, notification channels, orientation locking, status bar theming, and PiP support.
- **Desktop:** `Main.kt` sets `platform` to an implementation with `desktopShowAppUpdateNotice()`.

### 3. Haskell Core (JNI/FFI)

Native FFI bindings are declared in `common/src/commonMain/kotlin/chat/simplex/common/platform/Core.kt` as `external fun` declarations. These include: `chatMigrateInit`, `chatSendCmdRetry`, `chatRecvMsg`, `chatParseMarkdown`, `chatPasswordHash`, `chatWriteFile`, `chatReadFile`, `chatEncryptFile`, `chatDecryptFile`, and more. The native library (`libapp-lib`) is loaded at startup from platform-specific resource directories.

---

## Background Messaging (Android)

Android has no equivalent to iOS NSE (Notification Service Extension). Instead, it uses:

- **`SimplexService`** (`android/src/main/java/chat/simplex/app/SimplexService.kt`) -- A foreground service that keeps the Haskell core running to receive messages in real-time. Displays a persistent notification while active.
- **`MessagesFetcherWorker`** (`android/src/main/java/chat/simplex/app/MessagesFetcherWorker.kt`) -- A WorkManager-based periodic task that wakes the app at configurable intervals to fetch messages when the foreground service is not running (battery-optimized mode).
- **Notification modes:** Instant (foreground service always running), Periodic (WorkManager fetch every N minutes), Off.

---

## Related Specifications

- [concepts.md](concepts.md) -- Feature concept index with bidirectional code links
- [spec/README.md](../spec/README.md) -- Technical specification overview
- Haskell core: `../../src/Simplex/Chat/Controller.hs`, `../../src/Simplex/Chat/Types.hs`
- Kotlin model: `common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt`
- Kotlin API bridge: `common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt`
- Kotlin FFI: `common/src/commonMain/kotlin/chat/simplex/common/platform/Core.kt`
- Android entry: `android/src/main/java/chat/simplex/app/SimplexApp.kt`, `MainActivity.kt`
- Desktop entry: `desktop/src/jvmMain/kotlin/chat/simplex/desktop/Main.kt`
