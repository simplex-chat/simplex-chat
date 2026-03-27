# SimpleX Chat iOS -- Specification Overview

> Technical specification suite for the SimpleX Chat iOS application. Each document provides bidirectional links to product documentation and source code.

## Executive Summary

The SimpleX Chat iOS app is a native SwiftUI frontend that communicates with a Haskell core library via C FFI. All chat logic, encryption, protocol handling, and database operations happen in the Haskell core (`chat_ctrl`). The iOS layer handles UI rendering, system integration (CallKit, Push Notifications, Background Tasks), local preferences, and theming. The app shares its database with a Notification Service Extension (NSE) for decrypting push payloads while the main app is inactive.

## Dependency Graph

```
SimpleXApp (root entry point)
├── ChatModel (ObservableObject state) <-> SimpleXAPI (FFI bridge) <-> Haskell Core (chat_ctrl)
├── Views (SwiftUI)
│   ├── ChatListView -> ChatView -> ComposeView
│   ├── ChatItemView (renders individual messages)
│   ├── Settings, UserProfiles, Onboarding
│   └── ActiveCallView (WebRTC + CallKit)
├── Models
│   ├── ChatModel (global app state -- singleton)
│   ├── ItemsModel (per-chat message list state -- singleton + secondary instances)
│   ├── ChatTagsModel (tag filtering state)
│   └── Chat (per-conversation observable state)
├── Services
│   ├── NtfManager (push notification coordination)
│   ├── BGManager (background task scheduling)
│   ├── CallController (CallKit + VoIP push)
│   └── ThemeManager (theme resolution engine)
└── Extensions
    ├── SimpleX NSE (Notification Service Extension -- decrypts push payloads)
    └── SimpleX SE (Share Extension)
```

## Specification Documents

| Document | Description |
|----------|-------------|
| [Architecture](architecture.md) | System architecture, FFI bridge, app lifecycle, extension model |
| [Chat API Reference](api.md) | Complete ChatCommand, ChatResponse, ChatEvent, ChatError type reference |
| [State Management](state.md) | ChatModel, ItemsModel, Chat, ChatInfo, preference storage |
| [Database & Storage](database.md) | SQLite databases, encryption, file storage, export/import |
| [Chat View](client/chat-view.md) | Message rendering, chat item types, context menu actions |
| [Chat List](client/chat-list.md) | Conversation list, filtering, search, swipe actions |
| [Message Composition](client/compose.md) | Compose bar, attachments, reply/edit/forward modes, voice recording |
| [Navigation](client/navigation.md) | Navigation stack, deep linking, sheet presentation, call overlay |
| [Push Notifications](services/notifications.md) | NtfManager, NSE, notification modes, token lifecycle |
| [WebRTC Calling](services/calls.md) | CallController, WebRTCClient, CallKit, signaling via SMP |
| [File Transfer](services/files.md) | Inline/XFTP transfer, auto-receive, CryptoFile, file constants |
| [Theme Engine](services/theme.md) | ThemeManager, default themes, customization layers, wallpapers |
| [Impact Graph](impact.md) | Source file → product concept mapping, risk levels |

## Related Product Documentation

- [Product Overview](../product/README.md)
- [Concept Index](../product/concepts.md)
- [Business Rules](../product/rules.md)
- [Known Gaps](../product/gaps.md)
- [Glossary](../product/glossary.md)
- [Chat List View](../product/views/chat-list.md)
- [Chat View](../product/views/chat.md)

## Source Code Entry Points

| File | Role |
|------|------|
| `Shared/SimpleXApp.swift` | App entry point, Haskell init, lifecycle management |
| `Shared/AppDelegate.swift` | UIApplicationDelegate for push token registration |
| `Shared/ContentView.swift` | Root view -- authentication gate, call overlay, navigation |
| `Shared/Model/ChatModel.swift` | Primary observable state (ChatModel, ItemsModel, Chat) |
| `Shared/Model/SimpleXAPI.swift` | FFI bridge -- chatSendCmd, chatApiSendCmd, sendSimpleXCmd |
| `Shared/Model/AppAPITypes.swift` | ChatCommand, ChatResponse, ChatEvent enums (iOS app layer) |
| `SimpleXChat/APITypes.swift` | APIResult, ChatError, ChatCmdProtocol (shared framework) |
| `SimpleXChat/ChatTypes.swift` | User, ChatInfo, Contact, GroupInfo, ChatItem data types |
| `SimpleXChat/SimpleX.h` | C header for Haskell FFI functions |
