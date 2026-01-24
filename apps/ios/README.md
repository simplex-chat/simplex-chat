# SimpleX Chat iOS app

This file provides guidance when working with code in this repository.

## iOS App Overview

The iOS app is a SwiftUI application that interfaces with the Haskell core library via FFI. It shares the SimpleXChat framework with two extensions: Notification Service Extension (NSE) for push notifications and Share Extension (SE) for sharing content from other apps.

## Build & Development

Open `SimpleX.xcodeproj` in Xcode. The project has five targets:
- **SimpleX (iOS)** - Main app (Bundle ID: `chat.simplex.app`)
- **SimpleXChat** - Framework containing FFI bridge and shared types
- **SimpleX NSE** - Notification Service Extension
- **SimpleX SE** - Share Extension
- **Tests iOS** - UI tests

Build and run via Xcode (Product > Build/Run). Tests run via Product > Test or:
```bash
xcodebuild test -scheme "SimpleX (iOS)" -destination 'platform=iOS Simulator,name=iPhone 15'
```

Deployment target: iOS 15.0+, Swift 5.0.

## Architecture

### Haskell Core Integration

The app calls the Haskell core library through C FFI defined in `SimpleXChat/SimpleX.h`:
- `chat_migrate_init_key()` - Initialize/migrate database
- `chat_send_cmd_retry()` - Send command to chat controller
- `chat_recv_msg_wait()` - Receive messages from controller

Swift wrappers in `SimpleXChat/API.swift`:
- `chatMigrateInit()` - Initialize chat controller
- `sendSimpleXCmd<R>()` - Send typed commands and parse responses
- `recvSimpleXMsg<R>()` - Receive typed messages

Haskell runtime initialization (`SimpleXChat/hs_init.c`) uses different memory configurations:
- Main app: 64MB heap
- NSE: 512KB heap (minimal footprint for background processing)
- SE: 1MB heap

Pre-compiled Haskell libraries are in `Libraries/{ios,mac,sim}/`.

### State Management

- **ChatModel** (`Shared/Model/ChatModel.swift`) - Main singleton `ObservableObject` for app-wide state (chat list, active chat, users)
- **ItemsModel** - Manages chat items within a selected chat (similar to Kotlin's ChatsContext)
- **AppTheme** - Theme management and customization

### App Structure

Entry point: `Shared/SimpleXApp.swift`

Key directories in `Shared/`:
- `Model/` - Data models and API layer (`ChatModel.swift`, `SimpleXAPI.swift`)
- `Views/` - SwiftUI views organized by feature:
  - `ChatList/` - Chat list and user picker
  - `Chat/` - Message display and composition
  - `Call/` - VoIP call UI
  - `UserSettings/` - App settings
  - `LocalAuth/` - Passcode and biometric authentication
  - `Database/` - Database initialization and migration

### Shared Data Between Targets

All three targets share data via App Group (`group.chat.simplex.app`):
- `SimpleXChat/AppGroup.swift` - GroupDefaults wrapper for typed shared preferences
- Keychain for sensitive data: `kcDatabasePassword`, `kcAppPassword`, `kcSelfDestructPassword`

### Key Types

Types are defined in `SimpleXChat/`:
- `ChatTypes.swift` - User, Chat, Message, Group types
- `APITypes.swift` - API request/response types

Commands follow `ChatCmdProtocol` (has `cmdString` property), sent as JSON through FFI.

## Localization

31 languages supported. Localization files in `SimpleX Localizations/`.

Workflow:
- `Product > Export Localizations` - Export XLIFF files
- `Product > Import Localizations` - Import updated translations

## Background Capabilities

Configured in Info.plist:
- Background modes: audio, fetch, remote-notification, voip
- URL scheme: `simplex://` for deep linking
- BGTaskScheduler: `chat.simplex.app.receive`
