# SimpleX Chat iOS -- Concept Index

> SimpleX Chat iOS concept index. Maps every product concept to its documentation and source code with bidirectional links.
>
> **Related spec:** [spec/api.md](../spec/api.md) | [spec/state.md](../spec/state.md) | [spec/architecture.md](../spec/architecture.md)

## Table of Contents

1. [Feature Concepts](#section-1-feature-concepts)
2. [Entity Index](#section-2-entity-index)

## Executive Summary

This document provides a structured mapping between product-level concepts, their documentation, and their implementation in both the Swift iOS layer and the Haskell core library. All source paths are relative to `apps/ios/` for Swift and use `../../src/` prefix for Haskell files (relative to `apps/ios/`).

---

## Section 1: Feature Concepts

| # | Concept | Product Docs | Spec Docs | Source Files (Swift) | Source Files (Haskell) |
|---|---------|-------------|-----------|---------------------|----------------------|
| 1 | Chat List | [views/chat-list.md](views/chat-list.md), [views/onboarding.md](views/onboarding.md) | [spec/client/chat-list.md](../spec/client/chat-list.md) | `Shared/Views/ChatList/ChatListView.swift` | `Controller.hs` (`APIGetChats`) |
| 2 | Direct Chat | [views/chat.md](views/chat.md), [flows/messaging.md](flows/messaging.md) | [spec/client/chat-view.md](../spec/client/chat-view.md) | `Shared/Views/Chat/ChatView.swift`, `ChatInfoView.swift` | `Types.hs` (`Contact`), `Messages.hs` |
| 3 | Group Chat | [views/chat.md](views/chat.md), [views/group-info.md](views/group-info.md) | [spec/client/chat-view.md](../spec/client/chat-view.md) | `Shared/Views/Chat/ChatView.swift`, `Group/GroupChatInfoView.swift` | `Types.hs` (`GroupInfo`, `GroupMember`) |
| 4 | Message Composition | [views/chat.md](views/chat.md) | [spec/client/compose.md](../spec/client/compose.md) | `ComposeMessage/ComposeView.swift`, `SendMessageView.swift` | `Controller.hs` (`APISendMessages`) |
| 5 | Message Reactions | [views/chat.md](views/chat.md) | [spec/api.md](../spec/api.md) | `ChatItem/EmojiItemView.swift` | `Controller.hs` (`APIChatItemReaction`) |
| 6 | Message Editing | [views/chat.md](views/chat.md) | [spec/client/compose.md](../spec/client/compose.md) | `ComposeMessage/ComposeView.swift`, `ChatItemInfoView.swift` | `Controller.hs` (`APIUpdateChatItem`) |
| 7 | Message Deletion | [views/chat.md](views/chat.md) | [spec/api.md](../spec/api.md) | `ChatItem/MarkedDeletedItemView.swift`, `DeletedItemView.swift` | `Controller.hs` (`APIDeleteChatItem`) |
| 8 | Timed Messages | [views/chat.md](views/chat.md) | [spec/api.md](../spec/api.md) | `ChatItem/CIChatFeatureView.swift` | `Types/Preferences.hs` (`TimedMessagesPreference`) |
| 9 | Voice Messages | [views/chat.md](views/chat.md) | [spec/client/compose.md](../spec/client/compose.md) | `ChatItem/CIVoiceView.swift`, `ComposeVoiceView.swift` | `Protocol.hs` (`MCVoice`) |
| 10 | File Transfer | [flows/file-transfer.md](flows/file-transfer.md) | [spec/services/files.md](../spec/services/files.md) | `ChatItem/CIFileView.swift`, `SimpleXChat/FileUtils.swift` | `Files.hs`, `Store/Files.hs` |
| 11 | Link Previews | [views/chat.md](views/chat.md) | [spec/client/chat-view.md](../spec/client/chat-view.md) | `ChatItem/CILinkView.swift`, `ComposeLinkView.swift` | `Protocol.hs` (`MCLink`) |
| 12 | Contact Connection | [flows/connection.md](flows/connection.md), [views/new-chat.md](views/new-chat.md) | [spec/api.md](../spec/api.md) | `NewChat/NewChatView.swift`, `QRCode.swift` | `Controller.hs` (`APIConnect`, `APIAddContact`) |
| 13 | Contact Verification | [views/contact-info.md](views/contact-info.md) | [spec/api.md](../spec/api.md) | `Shared/Views/Chat/VerifyCodeView.swift` | `Controller.hs` (`APIVerifyContact`) |
| 14 | Group Management | [flows/group-lifecycle.md](flows/group-lifecycle.md) | [spec/api.md](../spec/api.md), [spec/database.md](../spec/database.md) | `NewChat/AddGroupView.swift`, `Group/GroupChatInfoView.swift` | `Controller.hs` (`APINewGroup`), `Store/Groups.hs` |
| 15 | Group Links | [views/group-info.md](views/group-info.md) | [spec/api.md](../spec/api.md) | `Group/GroupLinkView.swift` | `Controller.hs` (`APICreateGroupLink`) |
| 16 | Member Roles | [views/group-info.md](views/group-info.md) | [spec/api.md](../spec/api.md) | `SimpleXChat/ChatTypes.swift`, `Group/GroupMemberInfoView.swift` | `Types/Shared.hs` (`GroupMemberRole`) |
| 17 | Audio/Video Calls | [views/call.md](views/call.md), [flows/calling.md](flows/calling.md) | [spec/services/calls.md](../spec/services/calls.md) | `Call/ActiveCallView.swift`, `CallController.swift`, `WebRTCClient.swift` | `Call.hs` (`RcvCallInvitation`, `CallType`) |
| 18 | Push Notifications | [views/settings.md](views/settings.md) | [spec/services/notifications.md](../spec/services/notifications.md) | `Model/NtfManager.swift`, `SimpleX NSE/NotificationService.swift` | `Controller.hs` |
| 19 | User Profiles | [views/user-profiles.md](views/user-profiles.md) | [spec/state.md](../spec/state.md), [spec/client/navigation.md](../spec/client/navigation.md) | `UserSettings/UserProfilesView.swift`, `ChatList/UserPicker.swift` | `Types.hs` (`User`), `Store/Profiles.hs` |
| 20 | Incognito Mode | [views/contact-info.md](views/contact-info.md) | [spec/api.md](../spec/api.md) | `UserSettings/IncognitoHelp.swift` | `ProfileGenerator.hs`, `Types.hs` |
| 21 | Hidden Profiles | [views/user-profiles.md](views/user-profiles.md) | [spec/api.md](../spec/api.md) | `UserSettings/HiddenProfileView.swift` | `Controller.hs` (`APIHideUser`, `APIUnhideUser`) |
| 22 | Local Authentication | [views/settings.md](views/settings.md) | [spec/architecture.md](../spec/architecture.md) | `LocalAuth/LocalAuthView.swift`, `PasscodeView.swift` | N/A (iOS-only) |
| 23 | Database Encryption | [views/settings.md](views/settings.md) | [spec/database.md](../spec/database.md) | `Database/DatabaseEncryptionView.swift`, `DatabaseView.swift` | `Controller.hs` (`APIExportArchive`) |
| 24 | Theme System | [views/settings.md](views/settings.md) | [spec/services/theme.md](../spec/services/theme.md) | `Theme/ThemeManager.swift`, `SimpleXChat/Theme/ThemeTypes.swift` | `Types/UITheme.hs` |
| 25 | Network Configuration | [views/settings.md](views/settings.md) | [spec/architecture.md](../spec/architecture.md) | `NetworkAndServers/NetworkAndServers.swift`, `ProtocolServersView.swift` | `Controller.hs` (`APISetNetworkConfig`) |
| 26 | Device Migration | [flows/onboarding.md](flows/onboarding.md) | [spec/database.md](../spec/database.md) | `Migration/MigrateFromDevice.swift`, `MigrateToDevice.swift` | `Archive.hs` |
| 27 | Remote Desktop | [views/settings.md](views/settings.md) | [spec/architecture.md](../spec/architecture.md) | `RemoteAccess/ConnectDesktopView.swift` | `Remote.hs`, `Remote/Types.hs` |
| 28 | Chat Tags | [views/chat-list.md](views/chat-list.md) | [spec/state.md](../spec/state.md) | `ChatList/TagListView.swift`, `ChatListView.swift` | `Types.hs` (`ChatTag`), `Controller.hs` |
| 29 | User Address | [views/settings.md](views/settings.md) | [spec/api.md](../spec/api.md) | `UserSettings/UserAddressView.swift`, `Onboarding/AddressCreationCard.swift` | `Controller.hs` (`APICreateMyAddress`) |
| 30 | Member Support Chat | [views/group-info.md](views/group-info.md) | [spec/api.md](../spec/api.md) | `Group/MemberSupportView.swift`, `MemberAdmissionView.swift` | `Messages.hs` (`GroupChatScope`), `Controller.hs` |
| 31 | Channels (Relays) | [glossary.md](glossary.md), [views/chat.md](views/chat.md) | [spec/api.md](../spec/api.md), [spec/state.md](../spec/state.md), [spec/client/chat-view.md](../spec/client/chat-view.md) | `SimpleXChat/ChatTypes.swift` (`RelayStatus`, `GroupRelay`, `GroupMemberRole.relay`, `CIDirection.channelRcv`, `GroupInfo.chatIconName`), `Shared/Views/Chat/ChatView.swift` (channel message rendering), `Shared/Model/AppAPITypes.swift` (`GroupShortLinkInfo`, `UserChatRelay`), `Shared/Model/SimpleXAPI.swift` (`apiNewPublicGroup`) | `Controller.hs` (`APINewPublicGroup`) |

---

## Section 2: Entity Index

Core data entities, their storage, and the operations that manage their lifecycle.

| Entity | DB Table (Haskell) | Created By | Read By | Mutated By | Deleted By |
|--------|-------------------|------------|---------|------------|------------|
| **User** | `users` | `CreateActiveUser` in `Controller.hs` | `ListUsers`, `APISetActiveUser` in `Controller.hs` | `APISetActiveUser`, `APIHideUser`, `APIUnhideUser`, `APIMuteUser`, `APIUpdateProfile` in `Controller.hs` | `APIDeleteUser` in `Controller.hs`; `Store/Profiles.hs` |
| **Contact** | `contacts`, `contact_profiles` | `APIAddContact`, `APIConnect` in `Controller.hs` | `APIGetChat` in `Controller.hs`; `Store/Direct.hs` (getContact) | `APISetContactAlias`, `APISetConnectionAlias` in `Controller.hs`; `Store/Direct.hs` | `APIDeleteChat` in `Controller.hs`; `Store/Direct.hs` (deleteContact) |
| **GroupInfo** | `groups`, `group_profiles` | `APINewGroup` in `Controller.hs`; `Store/Groups.hs` (createNewGroup) | `APIGetChat`, `APIGroupInfo` in `Controller.hs`; `Store/Groups.hs` | `APIUpdateGroupProfile` in `Controller.hs`; `Store/Groups.hs` (updateGroupProfile) | `APIDeleteChat` in `Controller.hs`; `Store/Groups.hs` (deleteGroup) |
| **GroupMember** | `group_members`, `contact_profiles` | `APIAddMember`, `APIJoinGroup` in `Controller.hs`; `Store/Groups.hs` (createNewGroupMember) | `APIListMembers` in `Controller.hs`; `Store/Groups.hs` (getGroupMembers) | `APIMembersRole` in `Controller.hs`; `Store/Groups.hs` (updateGroupMemberRole) | `APIRemoveMembers` in `Controller.hs`; `Store/Groups.hs` (deleteGroupMember) |
| **ChatItem** | `chat_items`, `chat_item_versions` | `APISendMessages` in `Controller.hs`; `Store/Messages.hs` (createNewChatItem) | `APIGetChat`, `APIGetChatItems` in `Controller.hs`; `Store/Messages.hs` (getChatItems) | `APIUpdateChatItem`, `APIChatItemReaction` in `Controller.hs`; `Store/Messages.hs` (updateChatItem) | `APIDeleteChatItem` in `Controller.hs`; `Store/Messages.hs` (deleteChatItem) |
| **Connection** | `connections` | `createConnection` via SMP agent; `Store/Connections.hs` | `Store/Connections.hs` (getConnectionEntity) | `Store/Connections.hs` (updateConnectionStatus) | `Store/Connections.hs` (deleteConnection) |
| **FileTransfer** | `files`, `snd_files`, `rcv_files`, `xftp_file_descriptions` | `APISendMessages` (with file), `ReceiveFile` in `Controller.hs`; `Store/Files.hs` | `Store/Files.hs` (getFileTransfer) | `Store/Files.hs` (updateFileStatus, updateFileProgress) | `Store/Files.hs` (deleteFileTransfer) |
| **GroupLink** | `user_contact_links` | `APICreateGroupLink` in `Controller.hs`; `Store/Groups.hs` | `APIGetGroupLink` in `Controller.hs`; `Store/Groups.hs` | N/A (recreated on change) | `APIDeleteGroupLink` in `Controller.hs`; `Store/Groups.hs` |
| **ChatTag** | `chat_tags`, `chat_tags_chats` | `APICreateChatTag` in `Controller.hs` | `APIGetChats` in `Controller.hs` | `APIUpdateChatTag`, `APISetChatTags` in `Controller.hs` | `APIDeleteChatTag` in `Controller.hs` |
| **RcvCallInvitation** | In-memory (not persisted) | Received via `XCallInv` message in `Library/Subscriber.hs`; stored in `ChatModel.callInvitations` | `CallController.swift`, `IncomingCallView.swift` | Updated on call accept/reject in `CallManager.swift` | Removed on call end/reject; `Controller.hs` |

---

## Cross-References

- Product overview: [README.md](README.md)
- Glossary: [glossary.md](glossary.md)
- Haskell core controller: `../../src/Simplex/Chat/Controller.hs`
- Haskell core types: `../../src/Simplex/Chat/Types.hs`
- Haskell store layer: `../../src/Simplex/Chat/Store/` (Direct.hs, Groups.hs, Messages.hs, Files.hs, Profiles.hs, Connections.hs)
- Swift model: `Shared/Model/ChatModel.swift`
- Swift API types: `SimpleXChat/APITypes.swift`, `SimpleXChat/ChatTypes.swift`
- Swift API bridge: `SimpleXChat/API.swift`, `Shared/Model/SimpleXAPI.swift`
