# SimpleX Chat iOS -- Impact Graph

> Source file → product concept mapping. Use this to identify which product documents must be updated when a source file changes.
>
> Derived from [CODE.md](../CODE.md) Document Map and [product/concepts.md](../product/concepts.md).

---

## Product Concept Legend

| ID | Concept |
|----|---------|
| PC1 | Chat List |
| PC2 | Direct Chat |
| PC3 | Group Chat |
| PC4 | Message Composition |
| PC5 | Message Reactions |
| PC6 | Message Editing |
| PC7 | Message Deletion |
| PC8 | Timed Messages |
| PC9 | Voice Messages |
| PC10 | File Transfer |
| PC11 | Link Previews |
| PC12 | Contact Connection |
| PC13 | Contact Verification |
| PC14 | Group Management |
| PC15 | Group Links |
| PC16 | Member Roles |
| PC17 | Audio/Video Calls |
| PC18 | Push Notifications |
| PC19 | User Profiles |
| PC20 | Incognito Mode |
| PC21 | Hidden Profiles |
| PC22 | Local Authentication |
| PC23 | Database Encryption |
| PC24 | Theme System |
| PC25 | Network Configuration |
| PC26 | Device Migration |
| PC27 | Remote Desktop |
| PC28 | Chat Tags |
| PC29 | User Address |
| PC30 | Member Support Chat |
| PC31 | Channels (Relays) |

---

## 1. Swift Source Impact

| Source File | Product Concepts Affected | Risk Level | Notes |
|-------------|--------------------------|------------|-------|
| Shared/ContentView.swift | PC1, PC2, PC3 | High | Root navigation — affects all chat access |
| Shared/SimpleXApp.swift | PC1 through PC31 | High | App entry point — initialization affects everything |
| Shared/AppDelegate.swift | PC18 | Medium | Push notification registration |
| Shared/Views/ChatList/ChatListView.swift | PC1, PC28 | High | Main screen rendering and filtering |
| Shared/Views/Chat/ChatView.swift | PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC11, PC31 | High | Core conversation UI — most messaging features, channel message rendering |
| Shared/Views/Chat/ComposeMessage/ComposeView.swift | PC4, PC6, PC9, PC11 | High | Message composition — send path for all messages |
| Shared/Views/Chat/ChatItem/ | PC2, PC3, PC5, PC7, PC8, PC9, PC10, PC11 | Medium | Individual message rendering components |
| Shared/Views/Chat/ChatInfoView.swift | PC2, PC13, PC20 | Medium | Contact details and verification |
| Shared/Views/Chat/Group/GroupChatInfoView.swift | PC3, PC14, PC15, PC16, PC30 | High | Group management hub |
| Shared/Views/Chat/Group/AddGroupMembersView.swift | PC14, PC16 | Medium | Member invitation flow |
| Shared/Views/Chat/Group/GroupLinkView.swift | PC15 | Low | Group link creation/sharing |
| Shared/Views/Chat/Group/GroupMemberInfoView.swift | PC3, PC14, PC16, PC30 | Medium | Member details and role management |
| Shared/Views/NewChat/NewChatView.swift | PC12, PC31 | High | New connection creation — onramp for all contacts and channels |
| Shared/Views/NewChat/QRCode.swift | PC12 | Low | QR code display/scanning utility |
| Shared/Views/Call/ActiveCallView.swift | PC17 | Medium | Call UI rendering |
| Shared/Views/Call/CallController.swift | PC17 | High | CallKit integration — call lifecycle |
| Shared/Views/Call/WebRTCClient.swift | PC17 | High | WebRTC session management |
| Shared/Views/UserSettings/SettingsView.swift | PC18, PC22, PC23, PC24, PC25, PC29 | Medium | Settings navigation hub |
| Shared/Views/UserSettings/AppearanceSettings.swift | PC24 | Low | Theme customization UI |
| Shared/Views/UserSettings/NetworkAndServers/ | PC25, PC31 | High | Server configuration — affects connectivity and relay validation |
| Shared/Views/UserSettings/UserProfilesView.swift | PC19, PC21 | Medium | Profile management |
| Shared/Views/Onboarding/ | PC1 | Medium | First-time setup — affects initial state |
| Shared/Views/LocalAuth/ | PC22 | Medium | App lock functionality |
| Shared/Views/Database/ | PC23, PC26 | High | Database encryption and export |
| Shared/Views/Migration/ | PC26 | High | Device migration — data portability |
| Shared/Model/ChatModel.swift | PC1 through PC31 | High | Central state — all features depend on it |
| Shared/Model/SimpleXAPI.swift | PC1 through PC31 | High | FFI bridge — all commands flow through here |
| Shared/Model/AppAPITypes.swift | PC1 through PC31 | High | Command/response types — all API communication |
| Shared/Model/NtfManager.swift | PC18 | High | Notification delivery |
| Shared/Model/BGManager.swift | PC18 | Medium | Background fetch scheduling |
| Shared/Theme/ThemeManager.swift | PC24 | Medium | Theme resolution engine |
| SimpleXChat/ChatTypes.swift | PC1 through PC31 | High | Core data types — all features use them |
| SimpleXChat/APITypes.swift | PC1 through PC31 | High | API result types and error handling |
| SimpleXChat/CallTypes.swift | PC17 | Medium | Call-specific data types |
| SimpleXChat/FileUtils.swift | PC10, PC23, PC26 | Medium | File paths and encryption utilities |
| SimpleXChat/Notifications.swift | PC18 | Medium | Notification type definitions |
| SimpleX NSE/NotificationService.swift | PC18 | High | Push notification decryption and display |
| Shared/Views/Chat/ChatItemsMerger.swift | PC2, PC3, PC31 | Low | Chat item merge categories — added channelRcv hash |
| SimpleX SE/ShareAPI.swift | PC4, PC31 | Medium | Share extension API — sendAsGroup support |

---

## 2. Haskell Core Impact

| Source File | Product Concepts Affected | Risk Level | Notes |
|-------------|--------------------------|------------|-------|
| src/Simplex/Chat/Controller.hs | PC1 through PC31 | High | Command processor — all API commands |
| src/Simplex/Chat/Types.hs | PC1 through PC31 | High | Core data types shared across all features |
| src/Simplex/Chat/Core.hs | PC1 through PC31 | High | Chat engine lifecycle |
| src/Simplex/Chat/Protocol.hs | PC2, PC3, PC4, PC5, PC6, PC7 | High | Chat-level message protocol (x-events) |
| src/Simplex/Chat/Messages.hs | PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9 | High | Message types and content |
| src/Simplex/Chat/Messages/CIContent.hs | PC4, PC5, PC6, PC7, PC8, PC9, PC11 | Medium | Chat item content variants |
| src/Simplex/Chat/Call.hs | PC17 | Medium | Call signaling types |
| src/Simplex/Chat/Files.hs | PC10 | Medium | File transfer orchestration |
| src/Simplex/Chat/Store/Messages.hs | PC4, PC5, PC6, PC7, PC8 | High | Message persistence |
| src/Simplex/Chat/Store/Groups.hs | PC3, PC14, PC15, PC16, PC30 | High | Group persistence |
| src/Simplex/Chat/Store/Direct.hs | PC2, PC12, PC13 | High | Contact persistence |
| src/Simplex/Chat/Store/Files.hs | PC10 | Medium | File transfer persistence |
| src/Simplex/Chat/Store/Profiles.hs | PC19, PC21 | Medium | User profile persistence |
| src/Simplex/Chat/Store/Connections.hs | PC2, PC12 | High | Connection persistence and entity resolution |
| src/Simplex/Chat/Archive.hs | PC26 | Medium | Database export/import for migration |
| src/Simplex/Chat/ProfileGenerator.hs | PC20 | Low | Random profile generation for incognito |
| src/Simplex/Chat/Remote.hs | PC27 | Medium | Remote desktop protocol handler |
| src/Simplex/Chat/Remote/Types.hs | PC27 | Low | Remote desktop data types |
| src/Simplex/Chat/Types/UITheme.hs | PC24 | Low | Theme data types for UI customization |
| src/Simplex/Chat/Types/Preferences.hs | PC2, PC3, PC8 | Medium | Chat feature preferences (timed messages, etc.) |
| src/Simplex/Chat/Types/Shared.hs | PC3, PC16 | Medium | Shared types including GroupMemberRole |
