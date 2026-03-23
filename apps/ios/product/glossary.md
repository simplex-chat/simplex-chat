# SimpleX Chat iOS -- Glossary

> SimpleX Chat iOS domain glossary. Defines all domain terms used in SimpleX Chat with links to relevant specifications and source code.
>
> **Related spec:** [spec/api.md](../spec/api.md) | [spec/architecture.md](../spec/architecture.md)

## Table of Contents

1. [Protocols & Cryptography](#protocols--cryptography)
2. [Core Data Types](#core-data-types)
3. [Commands & Events](#commands--events)
4. [Connection & Identity](#connection--identity)
5. [Messaging Features](#messaging-features)
6. [Calling & Media](#calling--media)
7. [Notifications & Background](#notifications--background)
8. [Application Architecture](#application-architecture)
9. [Configuration & Preferences](#configuration--preferences)

---

## Protocols & Cryptography

### SMP (Simplex Messaging Protocol)
The core messaging protocol used for asynchronous message delivery through relay servers. Each conversation uses separate unidirectional queues, and sender and receiver queues have no shared identifier. Defined in the [simplexmq](https://github.com/simplex-chat/simplexmq) library. *See: protocol spec `simplexmq/protocol/simplex-messaging.md`, implementation `simplexmq/src/Simplex/Messaging/Protocol.hs`*

### SMP Server
A relay server that stores and forwards encrypted messages between parties. Users can configure custom SMP servers or use defaults. Servers cannot see message contents or correlate senders with receivers. *See: `Shared/Views/UserSettings/NetworkAndServers/ProtocolServersView.swift`*

### XFTP (eXtended File Transfer Protocol)
A protocol for transferring large files (up to 1GB) through relay servers. Files are encrypted, split into chunks, and uploaded to XFTP servers. Recipients download and reassemble chunks independently. Defined in the [simplexmq](https://github.com/simplex-chat/simplexmq) library. *See: protocol spec `simplexmq/protocol/xftp.md`, implementation `simplexmq/src/Simplex/FileTransfer/Protocol.hs`; chat-level integration `../../src/Simplex/Chat/Files.hs`*

### XFTP Server
A relay server that stores encrypted file chunks for asynchronous file transfer. Like SMP servers, users can configure custom XFTP servers. *See: `Shared/Views/UserSettings/NetworkAndServers/ProtocolServersView.swift`*

### SMP Agent
The lower-level agent library (in [simplexmq](https://github.com/simplex-chat/simplexmq)) that manages SMP connections, queue creation/rotation, duplex connection establishment, message delivery, and the double-ratchet encryption protocol. The chat application layer communicates with the agent via its functional API. *See: protocol spec `simplexmq/protocol/agent-protocol.md`, implementation `simplexmq/src/Simplex/Messaging/Agent.hs`; chat-level integration `../../src/Simplex/Chat/Controller.hs`*

### Double Ratchet
The key agreement protocol used for E2E encryption. Provides forward secrecy and break-in recovery by deriving new encryption keys for each message. Based on the Signal protocol's double-ratchet algorithm, augmented with post-quantum KEM (PQDR). Implemented in the [simplexmq](https://github.com/simplex-chat/simplexmq) library. *See: protocol spec `simplexmq/protocol/pqdr.md`, implementation `simplexmq/src/Simplex/Messaging/Crypto/Ratchet.hs`*

### Post-Quantum Encryption
Optional quantum-resistant key exchange (PQ) available for direct chats. Uses a hybrid scheme combining classical X25519 with Streamlined NTRU-Prime 761 (sntrup761) KEM. The hybrid secret is SHA3-256(DH_secret || KEM_shared_secret). Implemented in the [simplexmq](https://github.com/simplex-chat/simplexmq) library. *See: protocol spec `simplexmq/protocol/pqdr.md`, implementation `simplexmq/src/Simplex/Messaging/Crypto/SNTRUP761.hs`; Swift types `SimpleXChat/ChatTypes.swift` (PQEncryption, PQSupport)*

### E2E Encryption
End-to-end encryption ensuring that only the communicating parties can read message contents. Neither SMP relay servers nor any network observer can decrypt messages. All SimpleX Chat messages are E2E encrypted by default using the double-ratchet protocol. *See: `simplexmq/src/Simplex/Messaging/Crypto/Ratchet.hs` (ratchet implementation), `simplexmq/src/Simplex/Messaging/Agent/Protocol.hs` (E2E message envelopes)*

### Forward Secrecy
A property of the double-ratchet protocol ensuring that compromise of current encryption keys does not compromise past session keys. Each message uses a derived key that is deleted after use. *See: `simplexmq/protocol/pqdr.md`, `simplexmq/src/Simplex/Messaging/Crypto/Ratchet.hs`*

### Chat Protocol (x-events)
The chat-level protocol defining message envelopes and content types exchanged between chat participants. Includes x-events (XMsgNew, XMsgUpdate, XMsgDel, XCallInv, XFileCancel, XGrpMemNew, etc.), MsgContent (text, image, video, voice, file, link), and message encoding (Binary/JSON). This is distinct from the lower-level SMP transport protocol. *See: `../../src/Simplex/Chat/Protocol.hs`*

### Security Code
A hash of the shared encryption session displayed as a numeric code and QR code. Contacts can compare security codes out-of-band to verify they have an uncompromised E2E session. *See: `Shared/Views/Chat/VerifyCodeView.swift`, `../../src/Simplex/Chat/Controller.hs` (APIVerifyContact)*

---

## Core Data Types

### ChatItem
The fundamental unit of content in a conversation. Represents a single message, event, call record, or system notification within a chat. Each ChatItem has direction (sent/received), content, metadata, and optional quoted context. *See: `../../src/Simplex/Chat/Messages.hs` (data ChatItem), `SimpleXChat/ChatTypes.swift`*

### ChatInfo
A type-safe wrapper identifying a conversation and its metadata. Variants: DirectChat (1:1 with Contact), GroupChat (with GroupInfo), LocalChat (note folder), ContactRequest, ContactConnection. *See: `../../src/Simplex/Chat/Messages.hs` (data ChatInfo), `SimpleXChat/ChatTypes.swift`*

### CIContent
The content payload of a ChatItem. Differentiates sent vs. received content types: message content (text/image/file/voice/link), deletion markers, call records, group events, and feature preference changes. *See: `../../src/Simplex/Chat/Messages/CIContent.hs` (data CIContent)*

### User
A local user profile within the app. Each user has an independent set of contacts, groups, and connections. Multiple users can exist in one app installation. Fields include userId, profile, display name, and optional view password hash for hidden profiles. *See: `../../src/Simplex/Chat/Types.hs` (data User), `Shared/Model/ChatModel.swift`*

### Contact
A remote party with whom the user has an established E2E encrypted connection. Stores the contact's profile, local alias, connection status, feature preferences, and UI settings. *See: `../../src/Simplex/Chat/Types.hs` (data Contact), `SimpleXChat/ChatTypes.swift`*

### GroupInfo
Metadata for a group conversation including group profile, member count, preferences, and membership status. Contains the user's own membership record as a GroupMember. *See: `../../src/Simplex/Chat/Types.hs` (data GroupInfo)*

### GroupMember
A participant in a group conversation. Each member has a role, status, profile, and optionally a direct connection. The user's own membership is also represented as a GroupMember within GroupInfo. *See: `../../src/Simplex/Chat/Types.hs` (data GroupMember)*

### Connection
A low-level SMP agent connection between two parties. Each connection has a status (new, joined, ready, deleted), an agent connection ID, and is associated with a specific contact or group member. *See: `../../src/Simplex/Chat/Types.hs` (data Connection)*

### ConnStatus
The lifecycle state of a Connection: ConnNew (created, awaiting join), ConnJoined (joined, handshake in progress), ConnReady (fully established), ConnDeleted (terminated). *See: `../../src/Simplex/Chat/Types.hs` (data ConnStatus)*

### ContactStatus
The status of a contact record: CSActive (normal), CSDeleted (deleted by contact), CSDeletedByUser (deleted by user). *See: `../../src/Simplex/Chat/Types.hs` (data ContactStatus)*

### GroupMemberRole
Hierarchical role assigned to a group member. From most to least privileged: GROwner, GRAdmin, GRModerator, GRMember, GRObserver. Roles determine permissions for sending messages, managing members, and moderating content. *See: `../../src/Simplex/Chat/Types/Shared.hs` (data GroupMemberRole)*

### GroupMemberStatus
The lifecycle state of a group member: GSMemRejected, GSMemRemoved, GSMemLeft, GSMemGroupDeleted, GSMemUnknown, GSMemInvited, GSMemIntroduced, GSMemIntroInvited, GSMemAccepted, GSMemAnnounced, GSMemConnected, GSMemComplete, GSMemCreator, GSMemPendingReview, GSMemPendingApproval. *See: `../../src/Simplex/Chat/Types.hs` (data GroupMemberStatus)*

### FileTransfer
Represents an in-progress or completed file transfer. Variants: FTSnd (sending, with metadata and per-recipient transfer records) and FTRcv (receiving). Tracks protocol (SMP inline or XFTP), progress, and encryption parameters. *See: `../../src/Simplex/Chat/Types.hs` (data FileTransfer)*

### ChatTag
A user-defined label for organizing conversations in the chat list. Each tag has a text label and optional emoji. Chats can have multiple tags, and the chat list can be filtered by tag. *See: `../../src/Simplex/Chat/Types.hs` (data ChatTag), `Shared/Views/ChatList/TagListView.swift`*

---

## Commands & Events

### ChatCommand
A sum type representing all commands the UI can send to the chat controller. Examples: APISendMessages, APIGetChat, APIConnect, APINewGroup, APIDeleteChatItem. Commands are serialized and dispatched through the FFI bridge. *See: `../../src/Simplex/Chat/Controller.hs` (data ChatCommand)*

### ChatResponse
A sum type representing synchronous responses from the chat controller to the UI after processing a ChatCommand. Examples: CRActiveUser, CRNewChatItems, CRChatItemUpdated. *See: `../../src/Simplex/Chat/Controller.hs` (data ChatResponse)*

### ChatEvent
A sum type representing asynchronous events pushed from the chat controller to the UI. These are unsolicited notifications about state changes: incoming messages, connection status changes, call invitations, etc. *See: `../../src/Simplex/Chat/Controller.hs` (data ChatEvent)*

### ChatError
Error types returned by the chat controller. Variants: ChatError (application-level), ChatErrorAgent (SMP agent errors), ChatErrorStore (database errors), ChatErrorRemoteHost (remote desktop errors). *See: `../../src/Simplex/Chat/Controller.hs` (data ChatError)*

---

## Connection & Identity

### SimpleX Address
A long-lived contact address that others can use to send connection requests. Unlike one-time invitation links, an address can be reused by multiple contacts. The user can accept or reject each incoming request. *See: `Shared/Views/UserSettings/UserAddressView.swift`, `../../src/Simplex/Chat/Controller.hs` (APICreateMyAddress)*

### Contact Link
A one-time or reusable URI that initiates a contact connection. When scanned or opened, it triggers the SMP handshake to establish an E2E encrypted channel between two parties. *See: `Shared/Views/NewChat/NewChatView.swift`*

### Group Link
A shareable URI that allows new members to join a group. The link connects to the group host, who then introduces the new member to existing members. Configurable with a default member role. *See: `Shared/Views/Chat/Group/GroupLinkView.swift`, `../../src/Simplex/Chat/Types.hs` (data GroupLink)*

### Short Link
A compact version of SimpleX contact or group links, using a shorter URI format for easier sharing. Contains encoded connection parameters with reduced character length. *See: `../../src/Simplex/Chat/Controller.hs`*

### Incognito Mode
A privacy feature that generates a random profile (display name and avatar) for each new contact connection. The real user profile is never shared with incognito contacts. Can be toggled per-connection at invitation time. *See: `Shared/Views/UserSettings/IncognitoHelp.swift`, `../../src/Simplex/Chat/ProfileGenerator.hs`*

### Hidden Profile
A user profile protected by a separate password. Hidden profiles do not appear in the user picker or profile list. To access a hidden profile, the user enters its password in the search field of the user picker. *See: `Shared/Views/UserSettings/HiddenProfileView.swift`, `../../src/Simplex/Chat/Controller.hs` (APIHideUser)*

---

## Messaging Features

### Delivery Receipt
A confirmation that a message was successfully delivered to the recipient's device. Displayed as a double-check indicator on sent messages. Can be enabled or disabled per contact or globally. *See: `Shared/Views/UserSettings/SetDeliveryReceiptsView.swift`, `../../src/Simplex/Chat/Controller.hs`*

### Read Receipt
An indicator that a recipient has viewed a received message. Currently not implemented as a separate feature; delivery receipts serve as the primary delivery confirmation. *See: `Shared/Views/UserSettings/PrivacySettings.swift`*

### Timed Message
A message with a configurable time-to-live (TTL). After the TTL expires, the message is automatically deleted from both sender and recipient devices. The TTL is set as a chat feature preference. Also referred to as a disappearing message. *See: `../../src/Simplex/Chat/Types/Preferences.hs` (TimedMessagesPreference)*

### Disappearing Message
Synonym for Timed Message. A message that self-destructs after a configured duration. The timer starts when the message is read by the recipient. *See: `../../src/Simplex/Chat/Types/Preferences.hs` (TimedMessagesPreference)*

### Message Integrity
Verification that messages are received in order and without gaps. The system detects skipped messages and decryption failures, displaying integrity error indicators in the chat. *See: `Shared/Views/Chat/ChatItem/IntegrityErrorItemView.swift`, `../../src/Simplex/Chat/Messages/CIContent.hs`*

### Decryption Error
An error occurring when a received message cannot be decrypted, typically due to ratchet synchronization issues. The UI displays a specific error view with recovery options. *See: `Shared/Views/Chat/ChatItem/CIRcvDecryptionError.swift`, `../../src/Simplex/Chat/Messages/CIContent.hs`*

---

## Calling & Media

### CallKit
Apple's framework for integrating VoIP calls with the native iOS call UI. SimpleX Chat uses CallKit to display incoming calls on the lock screen, support call answering from the system UI, and manage audio sessions. *See: `Shared/Views/Call/CallController.swift`, `Shared/Views/Call/CallManager.swift`*

### WebRTC
The real-time communication framework used for audio/video calls. SimpleX Chat wraps WebRTC in an E2E encrypted layer, with signaling performed through the existing SMP message channel rather than a central server. *See: `Shared/Views/Call/WebRTC.swift`, `Shared/Views/Call/WebRTCClient.swift`*

### ICE Server
An Interactive Connectivity Establishment server used by WebRTC to discover network paths between call participants. SimpleX Chat supports configuring custom ICE servers. *See: `Shared/Views/UserSettings/RTCServers.swift`, `SimpleXChat/CallTypes.swift`*

### TURN Server
A Traversal Using Relays around NAT server that relays WebRTC media when direct peer-to-peer connection is not possible. A specific type of ICE server. SimpleX Chat allows configuring custom TURN servers for call relay. *See: `Shared/Views/UserSettings/RTCServers.swift`*

### RcvCallInvitation
An in-memory data structure representing an incoming call invitation. Contains the calling contact, call type (audio/video), encryption keys, and shared key for the WebRTC session. Not persisted to database. *See: `../../src/Simplex/Chat/Call.hs` (data RcvCallInvitation)*

---

## Notifications & Background

### Notification Service Extension (NSE)
An iOS app extension that processes incoming push notifications while the main app is not running. The NSE starts a temporary chat controller, decrypts the incoming message, and displays a notification with the message preview. *See: `SimpleX NSE/NotificationService.swift`, `SimpleX NSE/NSEAPITypes.swift`*

### Background Task
An iOS background execution context used for periodic message fetching when instant notifications are not enabled. Managed by BGManager to check for new messages at system-determined intervals. *See: `Shared/Model/BGManager.swift`*

---

## Application Architecture

### chat_ctrl
The opaque C pointer to the Haskell chat controller, obtained via FFI initialization. All chat operations are dispatched through this controller handle. The main app and NSE maintain separate chat_ctrl instances. *See: `SimpleXChat/API.swift` (chatController, getChatCtrl)*

### ComposeState
A Swift struct holding the current state of the message composition area. Tracks the message text, parsed markdown, preview, attached media, editing context, quote context, and voice recording state. *See: `Shared/Views/Chat/ComposeMessage/ComposeView.swift` (struct ComposeState)*

### ChatModel
The central observable model object for the iOS app. Holds all reactive state: current user, chat list, active chat, call state, app preferences, and navigation state. Published properties drive SwiftUI view updates. *See: `Shared/Model/ChatModel.swift` (class ChatModel)*

### ItemsModel
An observable model managing the list of ChatItems displayed in a conversation view. Handles item loading, pagination, merging of new items, and secondary chat filtering. *See: `Shared/Model/ChatModel.swift` (class ItemsModel)*

### AppTheme
An observable object encapsulating the current visual theme: name, base theme, color overrides, app-specific colors, and wallpaper configuration. Shared as an environment object across the SwiftUI view hierarchy. *See: `Shared/Theme/Theme.swift` (class AppTheme)*

---

## Configuration & Preferences

### FeaturePreference
A type class (Haskell) / protocol pattern representing a user's preference for a specific chat feature (e.g., timed messages, voice messages, calls). Each preference has an allow/enable setting and optional parameters. Feature preferences are negotiated between contacts. *See: `../../src/Simplex/Chat/Types/Preferences.hs` (class FeatureI, type FeaturePreference)*

### ChatSettings
Per-chat configuration including notification mode (all/mentions/off), send receipts toggle, favorite flag, and tag assignments. Stored per contact and per group. *See: `../../src/Simplex/Chat/Types.hs` (data ChatSettings)*

### UserDefaults / GroupDefaults
iOS persistent key-value storage for app preferences. GroupDefaults (UserDefaults with the app group suite name) is shared between the main app and the NSE extension. Stores settings like notification mode, appearance preferences, and runtime flags. *See: `SimpleXChat/AppGroup.swift` (groupDefaults)*

---

## Cross-References

- Product overview: [README.md](README.md)
- Concept index: [concepts.md](concepts.md)
- Haskell core types: `../../src/Simplex/Chat/Types.hs`
- Haskell controller: `../../src/Simplex/Chat/Controller.hs`
- Haskell chat protocol (x-events): `../../src/Simplex/Chat/Protocol.hs`
- Haskell messages: `../../src/Simplex/Chat/Messages.hs`
- Swift model: `Shared/Model/ChatModel.swift`
- Swift API types: `SimpleXChat/APITypes.swift`, `SimpleXChat/ChatTypes.swift`
- simplexmq library (SMP, XFTP, Agent, encryption): [github.com/simplex-chat/simplexmq](https://github.com/simplex-chat/simplexmq)
