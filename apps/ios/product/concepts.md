# SimpleX Chat iOS -- Concept Index

> SimpleX Chat iOS concept index. Maps every product concept to its documentation and source code with bidirectional links.

## Table of Contents

1. [Feature Concepts](#section-1-feature-concepts)
2. [Entity Index](#section-2-entity-index)

## Executive Summary

This document provides a structured mapping between product-level concepts, their documentation, and their implementation in both the Swift iOS layer and the Haskell core library. All source paths are relative to `apps/ios/` for Swift and use `../../src/` prefix for Haskell files (relative to `apps/ios/`).

---

## Section 1: Feature Concepts

| # | Concept | Description | Product Docs | Source Files (Swift) | Source Files (Haskell) |
|---|---------|-------------|-------------|---------------------|----------------------|
| 1 | Chat List | Main screen showing all conversations, filtered by tags, with unread counts and preview text | [README.md#messaging](README.md) | `Shared/Views/ChatList/ChatListView.swift`, `Shared/Views/ChatList/ChatPreviewView.swift`, `Shared/Views/ChatList/ChatListNavLink.swift` | `../../src/Simplex/Chat/Controller.hs` (APIGetChats), `../../src/Simplex/Chat/Messages.hs` (AChatItem) |
| 2 | Direct Chat | One-to-one encrypted conversation between two contacts using pairwise double-ratchet sessions | [README.md#messaging](README.md) | `Shared/Views/Chat/ChatView.swift`, `Shared/Views/Chat/ChatInfoView.swift` | `../../src/Simplex/Chat/Types.hs` (Contact), `../../src/Simplex/Chat/Messages.hs` (ChatInfo: DirectChat) |
| 3 | Group Chat | Multi-party encrypted conversation where each member pair has independent E2E sessions | [README.md#groups](README.md) | `Shared/Views/Chat/ChatView.swift`, `Shared/Views/Chat/Group/GroupChatInfoView.swift` | `../../src/Simplex/Chat/Types.hs` (GroupInfo, GroupMember), `../../src/Simplex/Chat/Messages.hs` (ChatInfo: GroupChat) |
| 4 | Message Composition | Creating and sending messages with text, attachments, quotes, and editing context | [README.md#messaging](README.md) | `Shared/Views/Chat/ComposeMessage/ComposeView.swift`, `Shared/Views/Chat/ComposeMessage/SendMessageView.swift`, `Shared/Views/Chat/ComposeMessage/NativeTextEditor.swift` | `../../src/Simplex/Chat/Controller.hs` (APISendMessages), `../../src/Simplex/Chat/Messages/CIContent.hs` |
| 5 | Message Reactions | Emoji reactions attached to individual messages, displayed as reaction summaries | [README.md#messaging](README.md) | `Shared/Views/Chat/ChatItem/EmojiItemView.swift`, `Shared/Views/Chat/Emoji.swift` | `../../src/Simplex/Chat/Controller.hs` (APIChatItemReaction), `../../src/Simplex/Chat/Messages.hs` |
| 6 | Message Editing | Edit previously sent messages; recipients see edit indicator and version history | [README.md#messaging](README.md) | `Shared/Views/Chat/ComposeMessage/ComposeView.swift`, `Shared/Views/Chat/ChatItemInfoView.swift` | `../../src/Simplex/Chat/Controller.hs` (APIUpdateChatItem), `../../src/Simplex/Chat/Messages/CIContent.hs` |
| 7 | Message Deletion | Delete messages locally or broadcast deletion to recipient; moderation deletion in groups | [README.md#messaging](README.md) | `Shared/Views/Chat/ChatItem/MarkedDeletedItemView.swift`, `Shared/Views/Chat/ChatItem/DeletedItemView.swift` | `../../src/Simplex/Chat/Controller.hs` (APIDeleteChatItem), `../../src/Simplex/Chat/Messages/CIContent.hs` (CIDeleteMode) |
| 8 | Timed Messages | Self-destructing messages with configurable time-to-live; auto-deleted after TTL expiry | [README.md#messaging](README.md) | `Shared/Views/Chat/ChatItem/CIChatFeatureView.swift`, `Shared/Views/Helpers/CustomTimePicker.swift` | `../../src/Simplex/Chat/Types/Preferences.hs` (TimedMessagesPreference), `../../src/Simplex/Chat/Messages.hs` |
| 9 | Voice Messages | Audio recording (up to 5 min / 510KB) with waveform visualization and inline playback | [README.md#messaging](README.md) | `Shared/Views/Chat/ChatItem/CIVoiceView.swift`, `Shared/Views/Chat/ChatItem/FramedCIVoiceView.swift`, `Shared/Views/Chat/ComposeMessage/ComposeVoiceView.swift`, `Shared/Model/AudioRecPlay.swift` | `../../src/Simplex/Chat/Messages/CIContent.hs` (CISndMsgContent, MCVoice) |
| 10 | File Transfer | Share files (up to 1GB) and images/videos via XFTP protocol with progress tracking | [README.md#messaging](README.md) | `Shared/Views/Chat/ChatItem/CIFileView.swift`, `Shared/Views/Chat/ComposeMessage/ComposeFileView.swift`, `SimpleXChat/FileUtils.swift` | `../../src/Simplex/Chat/Types.hs` (FileTransfer, FileTransferMeta), `../../src/Simplex/Chat/Files.hs`, `../../src/Simplex/Chat/Store/Files.hs` |
| 11 | Link Previews | Automatic OpenGraph metadata extraction for URLs, displayed as rich preview cards | [README.md#messaging](README.md) | `Shared/Views/Chat/ChatItem/CILinkView.swift`, `Shared/Views/Chat/ComposeMessage/ComposeLinkView.swift` | `../../src/Simplex/Chat/Messages/CIContent.hs` (MCLink) |
| 12 | Contact Connection | Establishing new contacts via SimpleX address links, QR codes, or pasted invitation URIs | [README.md#contacts](README.md) | `Shared/Views/NewChat/NewChatView.swift`, `Shared/Views/Chat/ScanCodeView.swift`, `Shared/Views/NewChat/QRCode.swift`, `Shared/Views/ChatList/ContactConnectionView.swift` | `../../src/Simplex/Chat/Controller.hs` (APIConnect, APIAddContact), `../../src/Simplex/Chat/Store/Direct.hs` |
| 13 | Contact Verification | Out-of-band security code comparison to verify E2E encryption integrity with a contact | [README.md#privacy--security](README.md) | `Shared/Views/Chat/VerifyCodeView.swift` | `../../src/Simplex/Chat/Controller.hs` (APIVerifyContact), `../../src/Simplex/Chat/Types.hs` |
| 14 | Group Management | Create, configure, and delete groups; manage members, roles, and group profile | [README.md#groups](README.md) | `Shared/Views/NewChat/AddGroupView.swift`, `Shared/Views/Chat/Group/GroupChatInfoView.swift`, `Shared/Views/Chat/Group/GroupProfileView.swift` | `../../src/Simplex/Chat/Controller.hs` (APINewGroup, APIUpdateGroupProfile), `../../src/Simplex/Chat/Store/Groups.hs` |
| 15 | Group Links | Shareable invitation links that allow new members to join a group directly | [README.md#groups](README.md) | `Shared/Views/Chat/Group/GroupLinkView.swift` | `../../src/Simplex/Chat/Controller.hs` (APICreateGroupLink, APIDeleteGroupLink), `../../src/Simplex/Chat/Types.hs` (GroupLink) |
| 16 | Member Roles | Hierarchical role system: owner > admin > moderator > member > observer | [README.md#groups](README.md) | `SimpleXChat/ChatTypes.swift`, `Shared/Views/Chat/Group/GroupMemberInfoView.swift` | `../../src/Simplex/Chat/Types/Shared.hs` (GroupMemberRole: GROwner, GRAdmin, GRModerator, GRMember, GRObserver) |
| 17 | Audio/Video Calls | WebRTC-based E2E encrypted audio and video calls with CallKit system integration | [README.md#calling](README.md) | `Shared/Views/Call/ActiveCallView.swift`, `Shared/Views/Call/WebRTCClient.swift`, `Shared/Views/Call/CallController.swift`, `Shared/Views/Call/CallManager.swift` | `../../src/Simplex/Chat/Call.hs` (RcvCallInvitation, CallType) |
| 18 | Push Notifications | Configurable notification delivery: instant (via notification server), periodic (background fetch), or off | [README.md#customization](README.md) | `Shared/Views/UserSettings/NotificationsView.swift`, `Shared/Model/NtfManager.swift`, `SimpleX NSE/NotificationService.swift` | `../../src/Simplex/Chat/Controller.hs` |
| 19 | User Profiles | Multiple independent user profiles within a single app installation | [README.md#user-management](README.md) | `Shared/Views/UserSettings/UserProfilesView.swift`, `Shared/Views/UserSettings/UserProfile.swift`, `Shared/Views/ChatList/UserPicker.swift` | `../../src/Simplex/Chat/Types.hs` (User), `../../src/Simplex/Chat/Store/Profiles.hs` |
| 20 | Incognito Mode | Per-contact random profile generation so each contact sees a different identity | [README.md#contacts](README.md) | `Shared/Views/UserSettings/IncognitoHelp.swift`, `Shared/Views/Chat/ComposeMessage/ContextProfilePickerView.swift` | `../../src/Simplex/Chat/ProfileGenerator.hs`, `../../src/Simplex/Chat/Types.hs` (incognitoProfile) |
| 21 | Hidden Profiles | Password-protected user profiles invisible in the profile list until unlocked | [README.md#privacy--security](README.md) | `Shared/Views/UserSettings/HiddenProfileView.swift` | `../../src/Simplex/Chat/Types.hs` (User: viewPwdHash), `../../src/Simplex/Chat/Controller.hs` (APIHideUser, APIUnhideUser) |
| 22 | Local Authentication | App lock via Face ID, Touch ID, or custom numeric/alphanumeric passcode | [README.md#privacy--security](README.md) | `Shared/Views/LocalAuth/LocalAuthView.swift`, `Shared/Views/LocalAuth/PasscodeView.swift`, `Shared/Views/LocalAuth/SetAppPasscodeView.swift`, `Shared/Views/Helpers/LocalAuthenticationUtils.swift` | N/A (iOS-only feature) |
| 23 | Database Encryption | AES-256 encryption of the local SQLite database with user-supplied passphrase | [README.md#data-management](README.md) | `Shared/Views/Database/DatabaseEncryptionView.swift`, `Shared/Views/Database/DatabaseView.swift` | `../../src/Simplex/Chat/Controller.hs` (APIExportArchive, APIImportArchive) |
| 24 | Theme System | Visual customization with built-in themes (light/dark/simplex/black) and custom user themes | [README.md#customization](README.md) | `Shared/Theme/Theme.swift`, `Shared/Theme/ThemeManager.swift`, `SimpleXChat/Theme/ThemeTypes.swift`, `SimpleXChat/Theme/Color.swift`, `Shared/Views/Helpers/ThemeModeEditor.swift` | `../../src/Simplex/Chat/Types/UITheme.hs` |
| 25 | Network Configuration | Custom SMP/XFTP server lists, SOCKS5 proxy, Tor .onion routing, and timeout tuning | [README.md#network](README.md) | `Shared/Views/UserSettings/NetworkAndServers/NetworkAndServers.swift`, `Shared/Views/UserSettings/NetworkAndServers/ProtocolServersView.swift`, `Shared/Views/UserSettings/NetworkAndServers/AdvancedNetworkSettings.swift` | `../../src/Simplex/Chat/Controller.hs` (APISetNetworkConfig), `../../src/Simplex/Chat/Options.hs` |
| 26 | Device Migration | Export full profile from one device and import on another, including all chats, contacts, and files | [README.md#data-management](README.md) | `Shared/Views/Migration/MigrateFromDevice.swift`, `Shared/Views/Migration/MigrateToDevice.swift` | `../../src/Simplex/Chat/Archive.hs` |
| 27 | Remote Desktop | Control the mobile app from a paired desktop client via encrypted reverse HTTP transport | [README.md#desktop-integration](README.md) | `Shared/Views/RemoteAccess/ConnectDesktopView.swift` | `../../src/Simplex/Chat/Remote.hs`, `../../src/Simplex/Chat/Remote/Types.hs`, `../../src/Simplex/Chat/Remote/Protocol.hs`, `../../src/Simplex/Chat/Remote/Transport.hs` |
| 28 | Chat Tags | User-defined tags for organizing and filtering conversations in the chat list | [README.md#messaging](README.md) | `Shared/Views/ChatList/TagListView.swift`, `Shared/Views/ChatList/ChatListView.swift` | `../../src/Simplex/Chat/Types.hs` (ChatTag), `../../src/Simplex/Chat/Controller.hs` (ChatTagData, APISetChatTags) |
| 29 | User Address | Long-lived shareable SimpleX contact address that accepts multiple connection requests with optional auto-accept | [README.md#contacts](README.md) | `Shared/Views/UserSettings/UserAddressView.swift`, `Shared/Views/UserSettings/UserAddressLearnMore.swift`, `Shared/Views/Onboarding/AddressCreationCard.swift` | `../../src/Simplex/Chat/Controller.hs` (APICreateMyAddress, APIDeleteMyAddress, APIAddressAutoAccept) |
| 30 | Member Support Chat | Private admin-to-member chat threads for admission review, moderation, and member support within groups | [README.md#groups](README.md) | `Shared/Views/Chat/Group/MemberSupportView.swift`, `Shared/Views/Chat/Group/MemberSupportChatToolbar.swift`, `Shared/Views/Chat/Group/MemberAdmissionView.swift`, `Shared/Views/Chat/Group/SecondaryChatView.swift` | `../../src/Simplex/Chat/Types.hs` (GroupChatScope), `../../src/Simplex/Chat/Controller.hs` |

---

## Section 2: Entity Index

Core data entities, their storage, and the operations that manage their lifecycle.

| Entity | DB Table (Haskell) | Created By | Read By | Mutated By | Deleted By |
|--------|-------------------|------------|---------|------------|------------|
| **User** | `users` | `CreateActiveUser` in `Controller.hs` | `APIListUsers`, `APISetActiveUser` in `Controller.hs` | `APISetActiveUser`, `APIHideUser`, `APIUnhideUser`, `APIMuteUser`, `APIUpdateProfile` in `Controller.hs` | `APIDeleteUser` in `Controller.hs`; `Store/Profiles.hs` |
| **Contact** | `contacts`, `contact_profiles` | `APIAddContact`, `APIConnect` in `Controller.hs` | `APIGetChat` in `Controller.hs`; `Store/Direct.hs` (getContact) | `APISetContactAlias`, `APISetConnectionAlias` in `Controller.hs`; `Store/Direct.hs` | `APIDeleteChat` in `Controller.hs`; `Store/Direct.hs` (deleteContact) |
| **GroupInfo** | `groups`, `group_profiles` | `APINewGroup` in `Controller.hs`; `Store/Groups.hs` (createNewGroup) | `APIGetChat`, `APIGetGroupInfo` in `Controller.hs`; `Store/Groups.hs` | `APIUpdateGroupProfile` in `Controller.hs`; `Store/Groups.hs` (updateGroupProfile) | `APIDeleteChat` in `Controller.hs`; `Store/Groups.hs` (deleteGroup) |
| **GroupMember** | `group_members`, `contact_profiles` | `APIAddMember`, `APIJoinGroup` in `Controller.hs`; `Store/Groups.hs` (createNewGroupMember) | `APIListMembers` in `Controller.hs`; `Store/Groups.hs` (getGroupMembers) | `APIMembersRole` in `Controller.hs`; `Store/Groups.hs` (updateGroupMemberRole) | `APIRemoveMembers` in `Controller.hs`; `Store/Groups.hs` (deleteGroupMember) |
| **ChatItem** | `chat_items`, `chat_item_versions` | `APISendMessages` in `Controller.hs`; `Store/Messages.hs` (createNewChatItem) | `APIGetChat`, `APIGetChatItems` in `Controller.hs`; `Store/Messages.hs` (getChatItems) | `APIUpdateChatItem`, `APIChatItemReaction` in `Controller.hs`; `Store/Messages.hs` (updateChatItem) | `APIDeleteChatItem` in `Controller.hs`; `Store/Messages.hs` (deleteChatItem) |
| **Connection** | `connections` | `createConnection` via SMP agent; `Store/Connections.hs` | `Store/Connections.hs` (getConnectionEntity) | `Store/Connections.hs` (updateConnectionStatus) | `Store/Connections.hs` (deleteConnection) |
| **FileTransfer** | `files`, `snd_files`, `rcv_files`, `xftp_file_descriptions` | `APISendMessages` (with file), `APIReceiveFile` in `Controller.hs`; `Store/Files.hs` | `Store/Files.hs` (getFileTransfer) | `Store/Files.hs` (updateFileStatus, updateFileProgress) | `Store/Files.hs` (deleteFileTransfer) |
| **GroupLink** | `user_contact_links` | `APICreateGroupLink` in `Controller.hs`; `Store/Groups.hs` | `APIGetGroupLink` in `Controller.hs`; `Store/Groups.hs` | N/A (recreated on change) | `APIDeleteGroupLink` in `Controller.hs`; `Store/Groups.hs` |
| **ChatTag** | `chat_tags`, `chat_tags_chats` | `APICreateChatTag` in `Controller.hs` | `APIGetChats` in `Controller.hs` | `APIUpdateChatTag`, `APISetChatTags` in `Controller.hs` | `APIDeleteChatTag` in `Controller.hs` |
| **RcvCallInvitation** | In-memory (not persisted) | Received via `XCallInv` message in `Controller.hs`; stored in `ChatModel.callInvitations` | `CallController.swift`, `IncomingCallView.swift` | Updated on call accept/reject in `CallManager.swift` | Removed on call end/reject; `Controller.hs` |

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
