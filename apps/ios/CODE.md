# SimpleX Chat iOS — Code Navigation Guide

## Purpose

This document is the entry point for developers and LLM sessions working on the SimpleX Chat iOS app. It maps product features to specification documents to source code, enabling fast context extraction in any direction.

## Quick Start

1. **Feature → Code**: Start at [product/concepts.md](product/concepts.md) → find your concept → follow spec and source links.
2. **Code → Feature**: Use the [Document Map](#document-map) below → find your source file → follow spec and product links.
3. **Understanding a screen**: Go to [product/views/](product/views/) → read the relevant view spec.
4. **Understanding a flow**: Go to [product/flows/](product/flows/) → read the relevant flow spec.
5. **API reference**: See [spec/api.md](spec/api.md) for ChatCommand/ChatResponse/ChatEvent types.
6. **Architecture**: See [spec/architecture.md](spec/architecture.md) for system design.

## Specification Structure

```
apps/ios/
├── CODE.md                          ← You are here (navigation guide)
├── product/                         ← WHAT the app does (user-facing)
│   ├── README.md                    ← Vision, users, capability map
│   ├── concepts.md                  ← Central index: concept → spec → code
│   ├── glossary.md                  ← Domain terminology
│   ├── views/                       ← Per-screen specifications
│   │   ├── chat-list.md             ← Chat list / home screen
│   │   ├── chat.md                  ← Conversation screen
│   │   ├── new-chat.md              ← New chat / connection screen
│   │   ├── group-info.md            ← Group management screen
│   │   ├── contact-info.md          ← Contact details screen
│   │   ├── call.md                  ← Audio/video call screens
│   │   ├── settings.md              ← Settings & preferences
│   │   ├── user-profiles.md         ← Profile management
│   │   └── onboarding.md            ← First-time setup
│   └── flows/                       ← End-to-end user flows
│       ├── messaging.md             ← Send/receive/edit/delete messages
│       ├── connection.md            ← Connect with contacts
│       ├── group-lifecycle.md       ← Create/manage/delete groups
│       ├── calling.md               ← Audio/video call flow
│       ├── file-transfer.md         ← File/media sharing
│       └── onboarding.md            ← First-time setup flow
├── spec/                            ← HOW it's organized (technical)
│   ├── README.md                    ← Spec overview & dependency graph
│   ├── architecture.md              ← System architecture
│   ├── api.md                       ← ChatCommand/ChatResponse/ChatEvent API
│   ├── state.md                     ← State management (ChatModel, ItemsModel)
│   ├── database.md                  ← Database schema & storage
│   ├── client/                      ← Client-side module specs
│   │   ├── chat-view.md             ← Chat view module
│   │   ├── chat-list.md             ← Chat list module
│   │   ├── compose.md               ← Message composition module
│   │   └── navigation.md            ← Navigation architecture
│   └── services/                    ← Service module specs
│       ├── notifications.md         ← Push notification service
│       ├── calls.md                 ← WebRTC calling service
│       ├── files.md                 ← File transfer service
│       └── theme.md                 ← Theme engine
└── Shared/                          ← Source code (existing)
    ├── Views/                       ← SwiftUI views
    ├── Model/                       ← Data models & state
    └── Theme/                       ← Theming system
```

## Document Map

| Source File | Spec Document | Product Document |
|---|---|---|
| `Shared/ContentView.swift` | [spec/client/navigation.md](spec/client/navigation.md) | [product/views/chat-list.md](product/views/chat-list.md) |
| `Shared/Views/ChatList/ChatListView.swift` | [spec/client/chat-list.md](spec/client/chat-list.md) | [product/views/chat-list.md](product/views/chat-list.md) |
| `Shared/Views/Chat/ChatView.swift` | [spec/client/chat-view.md](spec/client/chat-view.md) | [product/views/chat.md](product/views/chat.md) |
| `Shared/Views/Chat/ComposeMessage/ComposeView.swift` | [spec/client/compose.md](spec/client/compose.md) | [product/views/chat.md](product/views/chat.md) |
| `Shared/Views/Chat/ChatItem/` | [spec/client/chat-view.md](spec/client/chat-view.md) | [product/views/chat.md](product/views/chat.md) |
| `Shared/Views/Chat/ChatInfoView.swift` | [spec/client/chat-view.md](spec/client/chat-view.md) | [product/views/contact-info.md](product/views/contact-info.md) |
| `Shared/Views/Chat/Group/GroupChatInfoView.swift` | [spec/client/chat-view.md](spec/client/chat-view.md) | [product/views/group-info.md](product/views/group-info.md) |
| `Shared/Views/Chat/Group/AddGroupMembersView.swift` | [spec/client/chat-view.md](spec/client/chat-view.md) | [product/views/group-info.md](product/views/group-info.md) |
| `Shared/Views/Chat/Group/GroupLinkView.swift` | [spec/client/chat-view.md](spec/client/chat-view.md) | [product/views/group-info.md](product/views/group-info.md) |
| `Shared/Views/NewChat/NewChatView.swift` | [spec/client/navigation.md](spec/client/navigation.md) | [product/views/new-chat.md](product/views/new-chat.md) |
| `Shared/Views/NewChat/QRCode.swift` | [spec/client/navigation.md](spec/client/navigation.md) | [product/views/new-chat.md](product/views/new-chat.md) |
| `Shared/Views/Call/ActiveCallView.swift` | [spec/services/calls.md](spec/services/calls.md) | [product/views/call.md](product/views/call.md) |
| `Shared/Views/Call/CallController.swift` | [spec/services/calls.md](spec/services/calls.md) | [product/flows/calling.md](product/flows/calling.md) |
| `Shared/Views/Call/WebRTCClient.swift` | [spec/services/calls.md](spec/services/calls.md) | [product/flows/calling.md](product/flows/calling.md) |
| `Shared/Views/UserSettings/SettingsView.swift` | [spec/client/navigation.md](spec/client/navigation.md) | [product/views/settings.md](product/views/settings.md) |
| `Shared/Views/UserSettings/AppearanceSettings.swift` | [spec/services/theme.md](spec/services/theme.md) | [product/views/settings.md](product/views/settings.md) |
| `Shared/Views/UserSettings/NetworkAndServers/` | [spec/architecture.md](spec/architecture.md) | [product/views/settings.md](product/views/settings.md) |
| `Shared/Views/UserSettings/UserProfilesView.swift` | [spec/client/navigation.md](spec/client/navigation.md) | [product/views/user-profiles.md](product/views/user-profiles.md) |
| `Shared/Views/Onboarding/` | [spec/client/navigation.md](spec/client/navigation.md) | [product/views/onboarding.md](product/views/onboarding.md) |
| `Shared/Views/LocalAuth/` | [spec/architecture.md](spec/architecture.md) | [product/views/settings.md](product/views/settings.md) |
| `Shared/Views/Database/` | [spec/database.md](spec/database.md) | [product/views/settings.md](product/views/settings.md) |
| `Shared/Views/Migration/` | [spec/database.md](spec/database.md) | [product/flows/onboarding.md](product/flows/onboarding.md) |
| `Shared/Model/ChatModel.swift` | [spec/state.md](spec/state.md) | [product/concepts.md](product/concepts.md) |
| `Shared/Model/SimpleXAPI.swift` | [spec/api.md](spec/api.md) | [product/concepts.md](product/concepts.md) |
| `Shared/Model/AppAPITypes.swift` | [spec/api.md](spec/api.md) | [product/concepts.md](product/concepts.md) |
| `Shared/Model/NtfManager.swift` | [spec/services/notifications.md](spec/services/notifications.md) | [product/flows/messaging.md](product/flows/messaging.md) |
| `Shared/Model/BGManager.swift` | [spec/services/notifications.md](spec/services/notifications.md) | [product/flows/messaging.md](product/flows/messaging.md) |
| `Shared/Theme/ThemeManager.swift` | [spec/services/theme.md](spec/services/theme.md) | [product/views/settings.md](product/views/settings.md) |
| `SimpleXChat/ChatTypes.swift` | [spec/api.md](spec/api.md) | [product/glossary.md](product/glossary.md) |
| `SimpleXChat/APITypes.swift` | [spec/api.md](spec/api.md) | [product/concepts.md](product/concepts.md) |
| `SimpleXChat/CallTypes.swift` | [spec/services/calls.md](spec/services/calls.md) | [product/flows/calling.md](product/flows/calling.md) |
| `SimpleXChat/FileUtils.swift` | [spec/services/files.md](spec/services/files.md) | [product/flows/file-transfer.md](product/flows/file-transfer.md) |
| `SimpleXChat/Notifications.swift` | [spec/services/notifications.md](spec/services/notifications.md) | [product/flows/messaging.md](product/flows/messaging.md) |
| `SimpleX NSE/NotificationService.swift` | [spec/services/notifications.md](spec/services/notifications.md) | [product/flows/messaging.md](product/flows/messaging.md) |
| `Shared/SimpleXApp.swift` | [spec/architecture.md](spec/architecture.md) | [product/flows/onboarding.md](product/flows/onboarding.md) |
| `Shared/AppDelegate.swift` | [spec/architecture.md](spec/architecture.md) | [product/flows/onboarding.md](product/flows/onboarding.md) |

### Haskell Core Sources

| Source File | Spec Document | Product Document |
|---|---|---|
| `src/Simplex/Chat/Controller.hs` | [spec/api.md](spec/api.md) | [product/concepts.md](product/concepts.md) |
| `src/Simplex/Chat/Types.hs` | [spec/api.md](spec/api.md) | [product/glossary.md](product/glossary.md) |
| `src/Simplex/Chat/Core.hs` | [spec/architecture.md](spec/architecture.md) | [product/concepts.md](product/concepts.md) |
| `src/Simplex/Chat/Protocol.hs` | [spec/architecture.md](spec/architecture.md) | [product/concepts.md](product/concepts.md) |
| `src/Simplex/Chat/Messages.hs` | [spec/api.md](spec/api.md) | [product/flows/messaging.md](product/flows/messaging.md) |
| `src/Simplex/Chat/Messages/CIContent.hs` | [spec/api.md](spec/api.md) | [product/flows/messaging.md](product/flows/messaging.md) |
| `src/Simplex/Chat/Call.hs` | [spec/services/calls.md](spec/services/calls.md) | [product/flows/calling.md](product/flows/calling.md) |
| `src/Simplex/Chat/Files.hs` | [spec/services/files.md](spec/services/files.md) | [product/flows/file-transfer.md](product/flows/file-transfer.md) |
| `src/Simplex/Chat/Store/Messages.hs` | [spec/database.md](spec/database.md) | [product/flows/messaging.md](product/flows/messaging.md) |
| `src/Simplex/Chat/Store/Groups.hs` | [spec/database.md](spec/database.md) | [product/flows/group-lifecycle.md](product/flows/group-lifecycle.md) |
| `src/Simplex/Chat/Store/Direct.hs` | [spec/database.md](spec/database.md) | [product/flows/connection.md](product/flows/connection.md) |
| `src/Simplex/Chat/Store/Files.hs` | [spec/database.md](spec/database.md) | [product/flows/file-transfer.md](product/flows/file-transfer.md) |
| `src/Simplex/Chat/Store/Profiles.hs` | [spec/database.md](spec/database.md) | [product/views/user-profiles.md](product/views/user-profiles.md) |

## Change Protocol

When modifying iOS source code:

1. **Identify scope** — Read [product/concepts.md](product/concepts.md) for affected concepts
2. **Load product context** — Read relevant `product/views/*.md` or `product/flows/*.md`
3. **Load spec context** — Follow product → spec links
4. **Load source context** — Follow spec → source links
5. **Implement** — Make code changes
6. **Update spec** — Update spec documents if architecture/API changed
7. **Update product** — Update product documents if user-facing behavior changed
