# SimpleX Chat iOS -- Product Overview

> Codewatch product specification. Bidirectional code links: product docs reference source files, source files reference product docs.

## Table of Contents

1. [Vision](#vision)
2. [Target Users](#target-users)
3. [Capability Map](#capability-map)
4. [Navigation Map](#navigation-map)
5. [Related Specifications](#related-specifications)

## Executive Summary

SimpleX Chat is the first messaging platform with no user identifiers of any kind -- not even random numbers. It provides end-to-end encrypted messaging (with optional post-quantum cryptography), audio/video calls, file sharing, and group communication through a fully decentralized architecture where users control their own SMP relay servers. The iOS app is a native SwiftUI application backed by a Haskell core library.

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

---

## Capability Map

### 1. Messaging

Core message composition, delivery, and interaction features.

| Feature | Description | Key Source (Swift) |
|---------|-------------|--------------------|
| Text with markdown | Rich text formatting with SimpleX markdown syntax | `Shared/Views/Chat/ComposeMessage/ComposeView.swift` |
| Images | Compressed inline images (up to 255KB) | `Shared/Views/Chat/ChatItem/CIImageView.swift` |
| Video | Video message recording and playback | `Shared/Views/Chat/ChatItem/CIVideoView.swift` |
| Voice messages | Audio recording and playback (5min / 510KB limit) | `Shared/Views/Chat/ChatItem/CIVoiceView.swift` |
| File sharing | Files up to 1GB via XFTP protocol | `Shared/Views/Chat/ChatItem/CIFileView.swift` |
| Link previews | OpenGraph metadata extraction and display | `Shared/Views/Chat/ChatItem/CILinkView.swift` |
| Message reactions | Emoji reactions on sent/received messages | `Shared/Views/Chat/ChatItem/EmojiItemView.swift` |
| Message editing | Edit previously sent messages | `Shared/Views/Chat/ComposeMessage/ComposeView.swift` |
| Message deletion | Broadcast delete (for recipient) or internal-only delete | `Shared/Views/Chat/ChatItem/MarkedDeletedItemView.swift` |
| Timed messages | Self-destructing messages with configurable TTL | `Shared/Views/Chat/ChatItem/CIChatFeatureView.swift` |
| Quoted replies | Reply to specific messages with quote context | `Shared/Views/Chat/ComposeMessage/ContextItemView.swift` |
| Forwarding | Forward messages between chats | `Shared/Views/Chat/ChatItemForwardingView.swift` |
| Search | Full-text search within conversations | `Shared/Views/Chat/ChatView.swift` |
| Message reports | Report messages to group moderators | `Shared/Views/Chat/ChatView.swift` |

### 2. Contacts

Establishing, managing, and verifying contacts.

| Feature | Description | Key Source (Swift) |
|---------|-------------|--------------------|
| Add via SimpleX address | Connect using a SimpleX contact address | `Shared/Views/NewChat/NewChatView.swift` |
| Add via QR code | Scan QR code to establish connection | `Shared/Views/Chat/ScanCodeView.swift` |
| Contact requests | Accept or reject incoming contact requests | `Shared/Views/ChatList/ContactRequestView.swift` |
| Local aliases | Set private display names for contacts | `Shared/Views/Chat/ChatInfoView.swift` |
| Contact verification | Compare security codes out-of-band | `Shared/Views/Chat/VerifyCodeView.swift` |
| Blocking | Block contacts from sending messages | `Shared/Views/Chat/ChatInfoView.swift` |
| Incognito mode | Per-contact random profile generation | `Shared/Views/UserSettings/IncognitoHelp.swift` |
| Bot detection | Identify automated/bot contacts | `SimpleXChat/ChatTypes.swift` |

### 3. Groups

Multi-party encrypted conversations with role-based management.

| Feature | Description | Key Source (Swift) |
|---------|-------------|--------------------|
| Create groups | Create new group with initial members | `Shared/Views/NewChat/AddGroupView.swift` |
| Invite members | Invite by individual contact or link | `Shared/Views/Chat/Group/AddGroupMembersView.swift` |
| Member roles | Owner, admin, moderator, member, observer | `SimpleXChat/ChatTypes.swift` |
| Member admission | Queue-based admission with review workflow | `Shared/Views/Chat/Group/MemberAdmissionView.swift` |
| Group links | Shareable invite links for groups | `Shared/Views/Chat/Group/GroupLinkView.swift` |
| Business chat mode | Structured business communication groups | `Shared/Views/Chat/Group/GroupChatInfoView.swift` |
| Content moderation | Member reports and moderator actions | `Shared/Views/Chat/Group/MemberSupportView.swift` |
| Group preferences | Configure group-level feature settings | `Shared/Views/Chat/Group/GroupPreferencesView.swift` |
| Member direct contacts | Establish direct chats from group membership | `Shared/Views/Chat/Group/GroupMemberInfoView.swift` |

### 4. Calling

End-to-end encrypted audio and video communication.

| Feature | Description | Key Source (Swift) |
|---------|-------------|--------------------|
| E2E encrypted calls | Audio/video calls via WebRTC with E2E encryption | `Shared/Views/Call/WebRTCClient.swift` |
| CallKit integration | Native iOS system call UI (ring, answer, decline) | `Shared/Views/Call/CallController.swift` |
| Audio device switching | Switch between speaker, earpiece, Bluetooth | `Shared/Views/Call/CallAudioDeviceManager.swift` |
| Call history | Call events displayed as chat items | `Shared/Views/Chat/ChatItem/CICallItemView.swift` |
| Incoming call view | Dedicated UI for incoming call notifications | `Shared/Views/Call/IncomingCallView.swift` |

### 5. Privacy & Security

Encryption, authentication, and privacy controls.

| Feature | Description | Key Source (Swift) |
|---------|-------------|--------------------|
| E2E encryption | Double-ratchet encryption for all messages | `SimpleXChat/API.swift` |
| Post-quantum encryption | Optional PQ key exchange for direct chats | `SimpleXChat/ChatTypes.swift` |
| Local authentication | Face ID, Touch ID, or app passcode lock | `Shared/Views/LocalAuth/LocalAuthView.swift` |
| Hidden profiles | Password-protected profiles invisible in UI | `Shared/Views/UserSettings/HiddenProfileView.swift` |
| Database encryption | AES encryption of local SQLite database | `Shared/Views/Database/DatabaseEncryptionView.swift` |
| Screen privacy | Blur app content when in app switcher | `Shared/Views/UserSettings/PrivacySettings.swift` |
| Encrypted file storage | Local files encrypted at rest | `SimpleXChat/CryptoFile.swift` |
| Delivery receipts control | Toggle delivery/read receipts per contact/group | `Shared/Views/UserSettings/SetDeliveryReceiptsView.swift` |

### 6. User Management

Multiple profiles and identity management.

| Feature | Description | Key Source (Swift) |
|---------|-------------|--------------------|
| Multiple profiles | Multiple user profiles within one app | `Shared/Views/UserSettings/UserProfilesView.swift` |
| Active user switching | Switch between profiles via user picker | `Shared/Views/ChatList/UserPicker.swift` |
| Incognito contacts | Per-contact random identities | `Shared/Views/UserSettings/IncognitoHelp.swift` |
| Profile sharing | Share profile via contact address link | `Shared/Views/UserSettings/UserAddressView.swift` |
| User muting | Mute notifications for specific profiles | `Shared/Views/ChatList/UserPicker.swift` |

### 7. Network

Server configuration, proxy support, and connectivity.

| Feature | Description | Key Source (Swift) |
|---------|-------------|--------------------|
| Custom SMP servers | Configure personal SMP relay servers | `Shared/Views/UserSettings/NetworkAndServers/ProtocolServersView.swift` |
| Custom XFTP servers | Configure personal XFTP file servers | `Shared/Views/UserSettings/NetworkAndServers/ProtocolServersView.swift` |
| Tor/onion support | Route traffic through Tor .onion addresses | `Shared/Views/UserSettings/NetworkAndServers/AdvancedNetworkSettings.swift` |
| SOCKS5 proxy | Route connections through SOCKS5 proxy | `Shared/Views/UserSettings/NetworkAndServers/AdvancedNetworkSettings.swift` |
| Custom ICE servers | Configure WebRTC ICE/TURN servers | `Shared/Views/UserSettings/RTCServers.swift` |
| Network timeouts | Configure connection timeout parameters | `Shared/Views/UserSettings/NetworkAndServers/AdvancedNetworkSettings.swift` |

### 8. Customization

Visual appearance and UI preferences.

| Feature | Description | Key Source (Swift) |
|---------|-------------|--------------------|
| Themes | Light, dark, SimpleX, black, and custom themes | `Shared/Theme/ThemeManager.swift` |
| Wallpapers | Preset and custom chat wallpapers | `Shared/Views/Helpers/ChatWallpaper.swift` |
| Chat bubble styling | Customize message bubble appearance | `SimpleXChat/Theme/ThemeTypes.swift` |
| One-handed UI mode | Compact layout for single-hand use | `Shared/Views/ChatList/OneHandUICard.swift` |
| Language selection | In-app language override | `Shared/Views/UserSettings/AppearanceSettings.swift` |

### 9. Data Management

Import, export, encryption, and storage management.

| Feature | Description | Key Source (Swift) |
|---------|-------------|--------------------|
| Export/import profiles | Full database export and import | `Shared/Views/Database/DatabaseView.swift` |
| Database encryption | Encrypt/decrypt local database with passphrase | `Shared/Views/Database/DatabaseEncryptionView.swift` |
| Local file encryption | Encrypt stored media and attachments | `SimpleXChat/CryptoFile.swift` |
| Storage breakdown | View storage usage by category | `Shared/Views/UserSettings/StorageView.swift` |
| Device-to-device migration | Migrate full profile between iOS devices | `Shared/Views/Migration/MigrateFromDevice.swift` |

### 10. Desktop Integration

Remote control of the mobile app from a desktop client.

| Feature | Description | Key Source (Swift) |
|---------|-------------|--------------------|
| Remote control pairing | Pair with desktop app via QR code | `Shared/Views/RemoteAccess/ConnectDesktopView.swift` |
| Session management | Manage active desktop control sessions | `Shared/Views/RemoteAccess/ConnectDesktopView.swift` |

---

## Navigation Map

```
Onboarding
  OnboardingView.swift
    -> SimpleXInfo -> CreateProfile -> ChooseServerOperators -> SetNotificationsMode -> CreateSimpleXAddress
    -> ChatListView (home)

ChatListView (home)
  Shared/Views/ChatList/ChatListView.swift
    -> ChatView .................. (tap conversation row)
    -> NewChatMenuButton ......... (+ button)
    -> SettingsView .............. (gear icon)
    -> UserPicker ................ (avatar tap)
    -> TagListView ............... (tag filter bar)
    -> ServersSummaryView ........ (server status)

ChatView
  Shared/Views/Chat/ChatView.swift
    -> ChatInfoView .............. (contact name tap, direct chat)
    -> GroupChatInfoView ......... (group name tap, group chat)
    -> ActiveCallView ............ (call button)
    -> ComposeView ............... (message input area)
    -> ChatItemInfoView .......... (long press -> info)
    -> ChatItemForwardingView .... (long press -> forward)
    -> SecondaryChatView ......... (member support thread)

ChatInfoView
  Shared/Views/Chat/ChatInfoView.swift
    -> ContactPreferencesView .... (preferences)
    -> VerifyCodeView ............ (verify security code)

GroupChatInfoView
  Shared/Views/Chat/Group/GroupChatInfoView.swift
    -> GroupProfileView .......... (edit profile)
    -> AddGroupMembersView ....... (invite members)
    -> GroupLinkView ............. (manage group link)
    -> MemberAdmissionView ....... (admission settings)
    -> GroupPreferencesView ...... (group feature settings)
    -> GroupMemberInfoView ....... (tap member)
    -> GroupWelcomeView .......... (welcome message)

NewChatMenuButton
  Shared/Views/NewChat/NewChatMenuButton.swift
    -> NewChatView ............... (QR scanner / paste link)
    -> AddGroupView .............. (create group)
    -> UserAddressView ........... (create SimpleX address)

SettingsView
  Shared/Views/UserSettings/SettingsView.swift
    -> AppearanceSettings ........ (themes, wallpapers, UI)
    -> NetworkAndServers ......... (SMP/XFTP/proxy config)
    -> PrivacySettings ........... (privacy toggles)
    -> NotificationsView ......... (push notification mode)
    -> DatabaseView .............. (export/import/encrypt)
    -> CallSettings .............. (call preferences)
    -> StorageView ............... (storage usage)
    -> VersionView ............... (about/version)
    -> DeveloperView ............. (developer options)

UserPicker
  Shared/Views/ChatList/UserPicker.swift
    -> UserProfilesView .......... (manage all profiles)
    -> UserAddressView ........... (SimpleX address)
    -> PreferencesView ........... (user preferences)
    -> SettingsView .............. (app settings)
    -> ConnectDesktopView ........ (pair with desktop)
```

---

## Related Specifications

- [concepts.md](concepts.md) -- Feature concept index with bidirectional code links
- [glossary.md](glossary.md) -- Domain term glossary
- [spec/README.md](../spec/README.md) -- Technical specification overview
- [spec/architecture.md](../spec/architecture.md) -- Architecture specification
- Haskell core: `../../src/Simplex/Chat/Controller.hs`, `../../src/Simplex/Chat/Types.hs`
- Swift model: `Shared/Model/ChatModel.swift`, `SimpleXChat/ChatTypes.swift`
- Swift API bridge: `SimpleXChat/API.swift`, `Shared/Model/SimpleXAPI.swift`
