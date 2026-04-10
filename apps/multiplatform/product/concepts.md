# SimpleX Chat Android & Desktop -- Concept Index

> SimpleX Chat multiplatform concept index. Maps every product concept to its documentation and source code with bidirectional links.
>
> **Related spec:** [spec/README.md](../spec/README.md) | [spec/architecture.md](../spec/architecture.md)

## Table of Contents

1. [Feature Concepts](#section-1-feature-concepts)
2. [Entity Index](#section-2-entity-index)

## Executive Summary

This document provides a structured mapping between product-level concepts, their documentation, and their implementation in both the Kotlin multiplatform layer and the Haskell core library. All Kotlin source paths are relative to `apps/multiplatform/`. Haskell paths use `../../src/` prefix (relative to `apps/multiplatform/`). The common source root abbreviation used below is `common/src/commonMain/kotlin/chat/simplex/common/`.

---

## Section 1: Feature Concepts

| # | Concept | Product Docs | Spec Docs | Source Files (Kotlin) | Source Files (Haskell) |
|---|---------|-------------|-----------|----------------------|----------------------|
| PC1 | Chat List | [README.md](README.md) (Navigation Map) | [spec/client/chat-list.md](../spec/client/chat-list.md) | `common/.../views/chatlist/ChatListView.kt`, `ChatListNavLinkView.kt`, `ChatPreviewView.kt` | `Controller.hs` (`APIGetChats`) |
| PC2 | Direct Chat | [README.md](README.md) (Messaging) | [spec/client/chat-view.md](../spec/client/chat-view.md) | `common/.../views/chat/ChatView.kt`, `ChatInfoView.kt` | `Types.hs` (`Contact`), `Messages.hs` |
| PC3 | Group Chat | [README.md](README.md) (Groups) | [spec/client/chat-view.md](../spec/client/chat-view.md) | `common/.../views/chat/ChatView.kt`, `group/GroupChatInfoView.kt` | `Types.hs` (`GroupInfo`, `GroupMember`) |
| PC4 | Message Composition | [README.md](README.md) (Messaging) | [spec/client/compose.md](../spec/client/compose.md) | `common/.../views/chat/ComposeView.kt`, `SendMsgView.kt`, `ComposeVoiceView.kt`, `ComposeImageView.kt`, `ComposeFileView.kt` | `Controller.hs` (`APISendMessages`) |
| PC5 | Message Reactions | [README.md](README.md) (Messaging) | [spec/api.md](../spec/api.md) | `common/.../views/chat/ChatItemView.kt` (ChatItemReactions composable) | `Controller.hs` (`APIChatItemReaction`) |
| PC6 | Message Editing | [README.md](README.md) (Messaging) | [spec/client/compose.md](../spec/client/compose.md) | `common/.../views/chat/ComposeView.kt`, `ChatItemInfoView.kt` | `Controller.hs` (`APIUpdateChatItem`) |
| PC7 | Message Deletion | [README.md](README.md) (Messaging) | [spec/api.md](../spec/api.md) | `common/.../views/chat/item/MarkedDeletedItemView.kt`, `DeletedItemView.kt` | `Controller.hs` (`APIDeleteChatItem`) |
| PC8 | Timed Messages | [README.md](README.md) (Messaging) | [spec/api.md](../spec/api.md) | `common/.../views/chat/item/CIChatFeatureView.kt` | `Types/Preferences.hs` (`TimedMessagesPreference`) |
| PC9 | Voice Messages | [README.md](README.md) (Messaging) | [spec/client/compose.md](../spec/client/compose.md) | `common/.../views/chat/item/CIVoiceView.kt`, `ComposeVoiceView.kt`, `platform/RecAndPlay.kt` | `Protocol.hs` (`MCVoice`) |
| PC10 | File Transfer | [README.md](README.md) (Messaging, Data Management) | [spec/services/files.md](../spec/services/files.md) | `common/.../views/chat/item/CIFileView.kt`, `platform/Files.kt` | `Files.hs`, `Store/Files.hs` |
| PC11 | Link Previews | [README.md](README.md) (Messaging) | [spec/client/chat-view.md](../spec/client/chat-view.md) | `common/.../views/helpers/LinkPreviews.kt` | `Protocol.hs` (`MCLink`) |
| PC12 | Contact Connection | [README.md](README.md) (Contacts) | [spec/api.md](../spec/api.md) | `common/.../views/newchat/NewChatView.kt`, `QRCode.kt`, `QRCodeScanner.kt`, `ConnectPlan.kt` | `Controller.hs` (`APIConnect`, `APIAddContact`) |
| PC13 | Contact Verification | [README.md](README.md) (Contacts) | [spec/api.md](../spec/api.md) | `common/.../views/chat/VerifyCodeView.kt` | `Controller.hs` (`APIVerifyContact`) |
| PC14 | Group Management | [README.md](README.md) (Groups) | [spec/api.md](../spec/api.md) | `common/.../views/newchat/AddGroupView.kt`, `group/GroupChatInfoView.kt`, `group/GroupProfileView.kt` | `Controller.hs` (`APINewGroup`), `Store/Groups.hs` |
| PC15 | Group Links | [README.md](README.md) (Groups) | [spec/api.md](../spec/api.md) | `common/.../views/chat/group/GroupLinkView.kt` | `Controller.hs` (`APICreateGroupLink`) |
| PC16 | Member Roles | [README.md](README.md) (Groups) | [spec/api.md](../spec/api.md) | `common/.../model/ChatModel.kt`, `group/GroupMemberInfoView.kt` | `Types/Shared.hs` (`GroupMemberRole`) |
| PC17 | Audio/Video Calls | [README.md](README.md) (Calling) | [spec/services/calls.md](../spec/services/calls.md) | `common/.../views/call/CallView.kt`, `CallManager.kt`, `WebRTC.kt`, `android/.../CallService.kt`, `android/.../views/call/CallActivity.kt` | `Call.hs` (`RcvCallInvitation`, `CallType`) |
| PC18 | Notifications | [README.md](README.md) (Background Messaging) | [spec/services/notifications.md](../spec/services/notifications.md) | `common/.../platform/NtfManager.kt`, `Notifications.kt`, `android/.../SimplexService.kt`, `android/.../MessagesFetcherWorker.kt`, `common/.../views/usersettings/NotificationsSettingsView.kt` | `Controller.hs` |
| PC19 | User Profiles | [README.md](README.md) (User Management) | [spec/state.md](../spec/state.md) | `common/.../views/usersettings/UserProfilesView.kt`, `UserProfileView.kt`, `views/chatlist/UserPicker.kt` | `Types.hs` (`User`), `Store/Profiles.hs` |
| PC20 | Incognito Mode | [README.md](README.md) (Contacts) | [spec/api.md](../spec/api.md) | `common/.../views/usersettings/IncognitoView.kt` | `ProfileGenerator.hs`, `Types.hs` |
| PC21 | Hidden Profiles | [README.md](README.md) (Privacy & Security) | [spec/api.md](../spec/api.md) | `common/.../views/usersettings/HiddenProfileView.kt` | `Controller.hs` (`APIHideUser`, `APIUnhideUser`) |
| PC22 | Local Authentication | [README.md](README.md) (Privacy & Security) | [spec/architecture.md](../spec/architecture.md) | `common/.../views/localauth/LocalAuthView.kt`, `PasscodeView.kt`, `SetAppPasscodeView.kt`, `PasswordEntry.kt`, `AppLock.kt` | N/A (client-only) |
| PC23 | Database Encryption | [README.md](README.md) (Data Management) | [spec/database.md](../spec/database.md) | `common/.../views/database/DatabaseEncryptionView.kt`, `DatabaseView.kt`, `views/helpers/DatabaseUtils.kt` | `Controller.hs` (`APIExportArchive`) |
| PC24 | Theme System | [README.md](README.md) (Customization) | [spec/services/theme.md](../spec/services/theme.md) | `common/.../ui/theme/ThemeManager.kt`, `Theme.kt`, `Color.kt`, `Type.kt`, `Shape.kt` | `Types/UITheme.hs` |
| PC25 | Network Configuration | [README.md](README.md) (Network) | [spec/architecture.md](../spec/architecture.md) | `common/.../views/usersettings/networkAndServers/NetworkAndServers.kt`, `ProtocolServersView.kt`, `AdvancedNetworkSettings.kt`, `OperatorView.kt` | `Controller.hs` (`APISetNetworkConfig`) |
| PC26 | Device Migration | [README.md](README.md) (Data Management) | [spec/database.md](../spec/database.md) | `common/.../views/migration/MigrateFromDevice.kt`, `MigrateToDevice.kt` | `Archive.hs` |
| PC27 | Remote Desktop | [README.md](README.md) (Desktop Features) | [spec/architecture.md](../spec/architecture.md) | `common/.../views/remote/ConnectDesktopView.kt`, `ConnectMobileView.kt` | `Remote.hs`, `Remote/Types.hs` |
| PC28 | Chat Tags | [README.md](README.md) (Navigation Map) | [spec/state.md](../spec/state.md) | `common/.../views/chatlist/TagListView.kt`, `ChatListView.kt` | `Types.hs` (`ChatTag`), `Controller.hs` |
| PC29 | User Address | [README.md](README.md) (Contacts, User Management) | [spec/api.md](../spec/api.md) | `common/.../views/usersettings/UserAddressView.kt`, `UserAddressLearnMore.kt` | `Controller.hs` (`APICreateMyAddress`) |
| PC30 | Member Support Chat | [README.md](README.md) (Groups) | [spec/api.md](../spec/api.md) | `common/.../views/chat/group/MemberSupportView.kt`, `MemberSupportChatView.kt`, `MemberAdmission.kt` | `Messages.hs` (`GroupChatScope`), `Controller.hs` |

**Legend for abbreviated paths:**
- `common/.../` expands to `common/src/commonMain/kotlin/chat/simplex/common/`
- `android/.../` expands to `android/src/main/java/chat/simplex/app/`
- Haskell files are in `../../src/Simplex/Chat/` (relative to `apps/multiplatform/`)

---

## Section 2: Entity Index

Core data entities, their storage, and the operations that manage their lifecycle.

| Entity | DB Table (Haskell) | Created By | Read By | Mutated By | Deleted By |
|--------|-------------------|------------|---------|------------|------------|
| **User** | `users` | `CreateActiveUser` in `Controller.hs` | `ListUsers`, `APISetActiveUser` in `Controller.hs` | `APISetActiveUser`, `APIHideUser`, `APIUnhideUser`, `APIMuteUser`, `APIUpdateProfile` in `Controller.hs` | `APIDeleteUser` in `Controller.hs`; `Store/Profiles.hs` |
| **Contact** | `contacts`, `contact_profiles` | `APIAddContact`, `APIConnect` in `Controller.hs` | `APIGetChat` in `Controller.hs`; `Store/Direct.hs` (`getContact`) | `APISetContactAlias`, `APISetConnectionAlias` in `Controller.hs`; `Store/Direct.hs` | `APIDeleteChat` in `Controller.hs`; `Store/Direct.hs` (`deleteContact`) |
| **GroupInfo** | `groups`, `group_profiles` | `APINewGroup` in `Controller.hs`; `Store/Groups.hs` (`createNewGroup`) | `APIGetChat`, `APIGroupInfo` in `Controller.hs`; `Store/Groups.hs` | `APIUpdateGroupProfile` in `Controller.hs`; `Store/Groups.hs` (`updateGroupProfile`) | `APIDeleteChat` in `Controller.hs`; `Store/Groups.hs` (`deleteGroup`) |
| **GroupMember** | `group_members`, `contact_profiles` | `APIAddMember`, `APIJoinGroup` in `Controller.hs`; `Store/Groups.hs` (`createNewGroupMember`) | `APIListMembers` in `Controller.hs`; `Store/Groups.hs` (`getGroupMembers`) | `APIMembersRole` in `Controller.hs`; `Store/Groups.hs` (`updateGroupMemberRole`) | `APIRemoveMembers` in `Controller.hs`; `Store/Groups.hs` (`deleteGroupMember`) |
| **ChatItem** | `chat_items`, `chat_item_versions` | `APISendMessages` in `Controller.hs`; `Store/Messages.hs` (`createNewChatItem`) | `APIGetChat`, `APIGetChatItems` in `Controller.hs`; `Store/Messages.hs` (`getChatItems`) | `APIUpdateChatItem`, `APIChatItemReaction` in `Controller.hs`; `Store/Messages.hs` (`updateChatItem`) | `APIDeleteChatItem` in `Controller.hs`; `Store/Messages.hs` (`deleteChatItem`) |
| **Connection** | `connections` | `createConnection` via SMP agent; `Store/Connections.hs` | `Store/Connections.hs` (`getConnectionEntity`) | `Store/Connections.hs` (`updateConnectionStatus`) | `Store/Connections.hs` (`deleteConnection`) |
| **FileTransfer** | `files`, `snd_files`, `rcv_files`, `xftp_file_descriptions` | `APISendMessages` (with file), `ReceiveFile` in `Controller.hs`; `Store/Files.hs` | `Store/Files.hs` (`getFileTransfer`) | `Store/Files.hs` (`updateFileStatus`, `updateFileProgress`) | `Store/Files.hs` (`deleteFileTransfer`) |
| **GroupLink** | `user_contact_links` | `APICreateGroupLink` in `Controller.hs`; `Store/Groups.hs` | `APIGetGroupLink` in `Controller.hs`; `Store/Groups.hs` | N/A (recreated on change) | `APIDeleteGroupLink` in `Controller.hs`; `Store/Groups.hs` |
| **ChatTag** | `chat_tags`, `chat_tags_chats` | `APICreateChatTag` in `Controller.hs` | `APIGetChats` in `Controller.hs` | `APIUpdateChatTag`, `APISetChatTags` in `Controller.hs` | `APIDeleteChatTag` in `Controller.hs` |
| **RcvCallInvitation** | In-memory (not persisted) | Received via `XCallInv` message in `Library/Subscriber.hs`; stored in `ChatModel.activeCallInvitation` | `CallManager.kt`, `IncomingCallAlertView.kt` | Updated on call accept/reject in `CallManager.kt` | Removed on call end/reject; `Controller.hs` |

---

## Platform-Specific Source Index

Key files that exist only on one platform, grouped by concern.

### Android-Only

| File | Purpose |
|------|---------|
| `android/src/main/java/chat/simplex/app/SimplexApp.kt` | Application subclass, PlatformInterface setup, Haskell init |
| `android/src/main/java/chat/simplex/app/MainActivity.kt` | Main Activity, deep link handling, lifecycle |
| `android/src/main/java/chat/simplex/app/SimplexService.kt` | Foreground service for persistent messaging |
| `android/src/main/java/chat/simplex/app/MessagesFetcherWorker.kt` | WorkManager periodic message fetch |
| `android/src/main/java/chat/simplex/app/CallService.kt` | Foreground service for active calls |
| `android/src/main/java/chat/simplex/app/views/call/CallActivity.kt` | Dedicated Activity for call UI |
| `android/src/main/java/chat/simplex/app/model/NtfManager.android.kt` | Android notification channels and manager |
| `common/src/androidMain/kotlin/chat/simplex/common/platform/*.android.kt` | All `actual` implementations for Android |

### Desktop-Only

| File | Purpose |
|------|---------|
| `desktop/src/jvmMain/kotlin/chat/simplex/desktop/Main.kt` | JVM entry point, Haskell/VLC init, PlatformInterface setup |
| `common/src/desktopMain/kotlin/chat/simplex/common/DesktopApp.kt` | Compose Desktop window creation and lifecycle |
| `common/src/desktopMain/kotlin/chat/simplex/common/StoreWindowState.kt` | Window position/size persistence |
| `common/src/desktopMain/kotlin/chat/simplex/common/views/helpers/AppUpdater.kt` | In-app update checker |
| `common/src/desktopMain/kotlin/chat/simplex/common/platform/Videos.desktop.kt` | VLC-based video detection |
| `common/src/desktopMain/kotlin/chat/simplex/common/platform/VideoPlayer.desktop.kt` | VLC video player implementation |
| `common/src/desktopMain/kotlin/chat/simplex/common/platform/Platform.desktop.kt` | Desktop platform detection (Linux/macOS/Windows) |
| `common/src/desktopMain/kotlin/chat/simplex/common/platform/*.desktop.kt` | All `actual` implementations for Desktop |

---

## Cross-References

- Product overview: [README.md](README.md)
- Haskell core controller: `../../src/Simplex/Chat/Controller.hs`
- Haskell core types: `../../src/Simplex/Chat/Types.hs`
- Haskell store layer: `../../src/Simplex/Chat/Store/` (`Direct.hs`, `Groups.hs`, `Messages.hs`, `Files.hs`, `Profiles.hs`, `Connections.hs`)
- Kotlin model: `common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt`
- Kotlin API bridge: `common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt`
- Kotlin FFI layer: `common/src/commonMain/kotlin/chat/simplex/common/platform/Core.kt`
- Platform abstraction: `common/src/commonMain/kotlin/chat/simplex/common/platform/Platform.kt` (`PlatformInterface`)
