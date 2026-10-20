# SimpleX Chat iOS -- Chat API Reference

> Complete specification of the ChatCommand, ChatResponse, ChatEvent, and ChatError types that form the API between the Swift UI layer and the Haskell core.
>
> Related specs: [Architecture](architecture.md) | [State Management](state.md) | [README](README.md)
> Related product: [Concept Index](../product/concepts.md)

**Source:** [`AppAPITypes.swift`](../Shared/Model/AppAPITypes.swift) | [`SimpleXAPI.swift`](../Shared/Model/SimpleXAPI.swift) | [`APITypes.swift`](../SimpleXChat/APITypes.swift) | [`API.swift`](../SimpleXChat/API.swift)

---

## Table of Contents

1. [Overview](#1-overview)
2. [Command Categories (ChatCommand)](#2-command-categories)
3. [Response Types (ChatResponse)](#3-response-types)
4. [Event Types (ChatEvent)](#4-event-types)
5. [Error Types (ChatError)](#5-error-types)
6. [FFI Bridge Functions](#6-ffi-bridge-functions)
7. [Result Type (APIResult)](#7-result-type)
8. [Directory Search and Connection Plans](#8-directory-search-and-connection-plans)

---

## 1. Overview

The iOS app communicates with the Haskell core exclusively through a command/response protocol:

1. Swift constructs a `ChatCommand` enum value
2. The command's `cmdString` property serializes it to a text command
3. The FFI bridge sends the string to Haskell via `chat_send_cmd_retry`
4. Haskell returns a JSON response, decoded as `APIResult<ChatResponse>`
5. Async events arrive separately via `chat_recv_msg_wait`, decoded as `ChatEvent`

**Source files**:
- [`Shared/Model/AppAPITypes.swift`](../Shared/Model/AppAPITypes.swift) -- `ChatCommand` ([L15](../Shared/Model/AppAPITypes.swift#L15)), `ChatResponse0` ([L729](../Shared/Model/AppAPITypes.swift#L729)), `ChatResponse1` ([L857](../Shared/Model/AppAPITypes.swift#L857)), `ChatResponse2` ([L1003](../Shared/Model/AppAPITypes.swift#L1003)), `ChatEvent` ([L1179](../Shared/Model/AppAPITypes.swift#L1179)) enums
- [`SimpleXChat/APITypes.swift`](../SimpleXChat/APITypes.swift) -- `APIResult<R>` ([L27](../SimpleXChat/APITypes.swift#L27)), `ChatAPIResult` ([L65](../SimpleXChat/APITypes.swift#L65)), `ChatError` ([L765](../SimpleXChat/APITypes.swift#L765))
- [`Shared/Model/SimpleXAPI.swift`](../Shared/Model/SimpleXAPI.swift) -- FFI bridge functions (`chatSendCmd` [L121](../Shared/Model/SimpleXAPI.swift#L121), `chatRecvMsg` [L241](../Shared/Model/SimpleXAPI.swift#L241))
- [`SimpleXChat/API.swift`](../SimpleXChat/API.swift) -- Low-level FFI (`sendSimpleXCmd` [L115](../SimpleXChat/API.swift#L115), `recvSimpleXMsg` [L137](../SimpleXChat/API.swift#L137))
- `SimpleXChat/ChatTypes.swift` -- Data types used in commands/responses (User, Contact, GroupInfo, ChatItem, etc.)
- `../../src/Simplex/Chat/Controller.hs` -- Haskell controller (function `chat_send_cmd_retry`, `chat_recv_msg_wait`)

---

## 2. Command Categories

The `ChatCommand` enum ([`AppAPITypes.swift` L15](../Shared/Model/AppAPITypes.swift#L15)) contains all commands the iOS app can send to the Haskell core. Commands are organized below by functional area.

### 2.1 User Management

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `showActiveUser` | -- | Get current active user | [L16](../Shared/Model/AppAPITypes.swift#L16) |
| `createActiveUser` | `profile: Profile?, pastTimestamp: Bool` | Create new user profile | [L17](../Shared/Model/AppAPITypes.swift#L17) |
| `listUsers` | -- | List all user profiles | [L18](../Shared/Model/AppAPITypes.swift#L18) |
| `apiSetActiveUser` | `userId: Int64, viewPwd: String?` | Switch active user | [L19](../Shared/Model/AppAPITypes.swift#L19) |
| `apiHideUser` | `userId: Int64, viewPwd: String` | Hide user behind password | [L25](../Shared/Model/AppAPITypes.swift#L25) |
| `apiUnhideUser` | `userId: Int64, viewPwd: String` | Unhide hidden user | [L26](../Shared/Model/AppAPITypes.swift#L26) |
| `apiMuteUser` | `userId: Int64` | Mute notifications for user | [L27](../Shared/Model/AppAPITypes.swift#L27) |
| `apiUnmuteUser` | `userId: Int64` | Unmute notifications for user | [L28](../Shared/Model/AppAPITypes.swift#L28) |
| `apiDeleteUser` | `userId: Int64, delSMPQueues: Bool, viewPwd: String?` | Delete user profile | [L29](../Shared/Model/AppAPITypes.swift#L29) |
| `apiUpdateProfile` | `userId: Int64, profile: Profile` | Update user display name/image | [L149](../Shared/Model/AppAPITypes.swift#L149) |
| `setAllContactReceipts` | `enable: Bool` | Set delivery receipts for all contacts | [L20](../Shared/Model/AppAPITypes.swift#L20) |
| `apiSetUserContactReceipts` | `userId: Int64, userMsgReceiptSettings` | Per-user contact receipt settings | [L21](../Shared/Model/AppAPITypes.swift#L21) |
| `apiSetUserGroupReceipts` | `userId: Int64, userMsgReceiptSettings` | Per-user group receipt settings | [L22](../Shared/Model/AppAPITypes.swift#L22) |
| `apiSetUserAutoAcceptMemberContacts` | `userId: Int64, enable: Bool` | Auto-accept group member contacts | [L23](../Shared/Model/AppAPITypes.swift#L23) |

### 2.2 Chat Lifecycle Control

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `startChat` | `mainApp: Bool, enableSndFiles: Bool` | Start chat engine | [L30](../Shared/Model/AppAPITypes.swift#L30) |
| `checkChatRunning` | -- | Check if chat is running | [L31](../Shared/Model/AppAPITypes.swift#L31) |
| `apiStopChat` | -- | Stop chat engine | [L32](../Shared/Model/AppAPITypes.swift#L32) |
| `apiActivateChat` | `restoreChat: Bool` | Resume from background | [L33](../Shared/Model/AppAPITypes.swift#L33) |
| `apiSuspendChat` | `timeoutMicroseconds: Int` | Suspend for background | [L34](../Shared/Model/AppAPITypes.swift#L34) |
| `apiSetAppFilePaths` | `filesFolder, tempFolder, assetsFolder` | Set file storage paths | [L35](../Shared/Model/AppAPITypes.swift#L35) |
| `apiSetEncryptLocalFiles` | `enable: Bool` | Toggle local file encryption | [L36](../Shared/Model/AppAPITypes.swift#L36) |

### 2.3 Chat & Message Operations

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiGetChats` | `userId: Int64` | Get all chat previews for user | [L45](../Shared/Model/AppAPITypes.swift#L45) |
| `apiGetChat` | `chatId, scope, contentTag, pagination, search` | Get messages for a chat | [L46](../Shared/Model/AppAPITypes.swift#L46) |
| `apiGetChatContentTypes` | `chatId, scope` | Get content type counts for a chat | [L47](../Shared/Model/AppAPITypes.swift#L47) |
| `apiGetChatItemInfo` | `type, id, scope, itemId` | Get detailed info for a message | [L48](../Shared/Model/AppAPITypes.swift#L48) |
| `apiSendMessages` | `type, id, scope, sendAsGroup, live, ttl, composedMessages` | Send one or more messages; `sendAsGroup` sends as channel owner | [L49](../Shared/Model/AppAPITypes.swift#L49) |
| `apiCreateChatItems` | `noteFolderId, composedMessages` | Create items in notes folder | [L55](../Shared/Model/AppAPITypes.swift#L55) |
| `apiUpdateChatItem` | `type, id, scope, itemId, updatedMessage, live` | Edit a sent message | [L57](../Shared/Model/AppAPITypes.swift#L57) |
| `apiDeleteChatItem` | `type, id, scope, itemIds, mode` | Delete messages | [L58](../Shared/Model/AppAPITypes.swift#L58) |
| `apiDeleteMemberChatItem` | `groupId, itemIds` | Moderate group messages | [L59](../Shared/Model/AppAPITypes.swift#L59) |
| `apiChatItemReaction` | `type, id, scope, itemId, add, reaction` | Add/remove emoji reaction | [L62](../Shared/Model/AppAPITypes.swift#L62) |
| `apiGetReactionMembers` | `userId, groupId, itemId, reaction` | Get who reacted | [L63](../Shared/Model/AppAPITypes.swift#L63) |
| `apiPlanForwardChatItems` | `fromChatType, fromChatId, fromScope, itemIds` | Plan message forwarding | [L64](../Shared/Model/AppAPITypes.swift#L64) |
| `apiForwardChatItems` | `toChatType, toChatId, toScope, sendAsGroup, from..., itemIds, ttl` | Forward messages; `sendAsGroup` forwards as channel owner | [L65](../Shared/Model/AppAPITypes.swift#L65) |
| `apiReportMessage` | `groupId, chatItemId, reportReason, reportText` | Report group message | [L56](../Shared/Model/AppAPITypes.swift#L56) |
| `apiChatRead` | `type, id, scope` | Mark entire chat as read | [L177](../Shared/Model/AppAPITypes.swift#L177) |
| `apiChatItemsRead` | `type, id, scope, itemIds` | Mark specific items as read | [L178](../Shared/Model/AppAPITypes.swift#L178) |
| `apiChatUnread` | `type, id, unreadChat` | Toggle unread badge | [L179](../Shared/Model/AppAPITypes.swift#L179) |

### 2.4 Contact Management

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiAddContact` | `userId, incognito` | Create invitation link | [L133](../Shared/Model/AppAPITypes.swift#L133) |
| `apiConnect` | `userId, incognito, connLink` | Connect via link | [L144](../Shared/Model/AppAPITypes.swift#L144) |
| `apiConnectPlan` | `userId, connLink` | Plan connection (preview) | [L136](../Shared/Model/AppAPITypes.swift#L136) |
| `apiPrepareContact` | `userId, connLink, contactShortLinkData` | Prepare contact from link | [L138](../Shared/Model/AppAPITypes.swift#L138) |
| `apiPrepareGroup` | `userId, connLink, directLink, groupShortLinkData` | Prepare group from link; `directLink` (required, no default) indicates whether link is a direct (non-relay) group link | [L139](../Shared/Model/AppAPITypes.swift#L139) |
| `apiConnectPreparedContact` | `contactId, incognito, msg` | Connect prepared contact | [L142](../Shared/Model/AppAPITypes.swift#L142) |
| `apiConnectPreparedGroup` | `groupId, incognito, msg` | Connect to a prepared group/channel; returns `(GroupInfo, [RelayConnectionResult])?` | [L143](../Shared/Model/AppAPITypes.swift#L143) |
| `apiConnectContactViaAddress` | `userId, incognito, contactId` | Connect via address | [L145](../Shared/Model/AppAPITypes.swift#L145) |
| `apiAcceptContact` | `incognito, contactReqId` | Accept contact request | [L165](../Shared/Model/AppAPITypes.swift#L165) |
| `apiRejectContact` | `contactReqId` | Reject contact request | [L166](../Shared/Model/AppAPITypes.swift#L166) |
| `apiDeleteChat` | `type, id, chatDeleteMode` | Delete conversation | [L146](../Shared/Model/AppAPITypes.swift#L146) |
| `apiClearChat` | `type, id` | Clear conversation history | [L147](../Shared/Model/AppAPITypes.swift#L147) |
| `apiListContacts` | `userId` | List all contacts | [L148](../Shared/Model/AppAPITypes.swift#L148) |
| `apiSetContactPrefs` | `contactId, preferences` | Set contact preferences | [L150](../Shared/Model/AppAPITypes.swift#L150) |
| `apiSetContactAlias` | `contactId, localAlias` | Set local alias | [L151](../Shared/Model/AppAPITypes.swift#L151) |
| `apiSetConnectionAlias` | `connId, localAlias` | Set pending connection alias | [L153](../Shared/Model/AppAPITypes.swift#L153) |
| `apiContactInfo` | `contactId` | Get contact info + connection stats | [L119](../Shared/Model/AppAPITypes.swift#L119) |
| `apiSetConnectionIncognito` | `connId, incognito` | Toggle incognito on pending connection | [L134](../Shared/Model/AppAPITypes.swift#L134) |

### 2.5 Group Management

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiNewGroup` | `userId, incognito, groupProfile` | Create new group | [L75](../Shared/Model/AppAPITypes.swift#L75) |
| `apiNewPublicGroup` | `userId, incognito, relayIds, groupProfile` | Create new public group (channel) with chat relays | [L76](../Shared/Model/AppAPITypes.swift#L76) |
| `apiGetGroupRelays` | `groupId` | Get group relay list with status (owner only) | [L77](../Shared/Model/AppAPITypes.swift#L77) |
| `apiAddMember` | `groupId, contactId, memberRole` | Invite contact to group | [L79](../Shared/Model/AppAPITypes.swift#L79) |
| `apiJoinGroup` | `groupId` | Accept group invitation | [L80](../Shared/Model/AppAPITypes.swift#L80) |
| `apiAcceptMember` | `groupId, groupMemberId, memberRole` | Accept member (knocking) | [L81](../Shared/Model/AppAPITypes.swift#L81) |
| `apiRemoveMembers` | `groupId, memberIds, withMessages` | Remove members | [L85](../Shared/Model/AppAPITypes.swift#L85) |
| `apiLeaveGroup` | `groupId` | Leave group | [L86](../Shared/Model/AppAPITypes.swift#L86) |
| `apiListMembers` | `groupId` | List group members | [L87](../Shared/Model/AppAPITypes.swift#L87) |
| `apiUpdateGroupProfile` | `groupId, groupProfile` | Update group name/image/description | [L88](../Shared/Model/AppAPITypes.swift#L88) |
| `apiMembersRole` | `groupId, memberIds, memberRole` | Change member roles | [L83](../Shared/Model/AppAPITypes.swift#L83) |
| `apiBlockMembersForAll` | `groupId, memberIds, blocked` | Block members for all | [L84](../Shared/Model/AppAPITypes.swift#L84) |
| `apiCreateGroupLink` | `groupId, memberRole` | Create shareable group link | [L90](../Shared/Model/AppAPITypes.swift#L90) |
| `apiGroupLinkMemberRole` | `groupId, memberRole` | Change group link default role | [L91](../Shared/Model/AppAPITypes.swift#L91) |
| `apiDeleteGroupLink` | `groupId` | Delete group link | [L92](../Shared/Model/AppAPITypes.swift#L92) |
| `apiGetGroupLink` | `groupId` | Get existing group link | [L93](../Shared/Model/AppAPITypes.swift#L93) |
| `apiAddGroupShortLink` | `groupId` | Add short link to group | [L94](../Shared/Model/AppAPITypes.swift#L94) |
| `apiCreateMemberContact` | `groupId, groupMemberId` | Create direct contact from group member | [L95](../Shared/Model/AppAPITypes.swift#L95) |
| `apiSendMemberContactInvitation` | `contactId, msg` | Send contact invitation to member | [L96](../Shared/Model/AppAPITypes.swift#L96) |
| `apiGroupMemberInfo` | `groupId, groupMemberId` | Get member info + connection stats | [L120](../Shared/Model/AppAPITypes.swift#L120) |
| `apiDeleteMemberSupportChat` | `groupId, groupMemberId` | Delete member support chat | [L82](../Shared/Model/AppAPITypes.swift#L82) |
| `apiSetMemberSettings` | `groupId, groupMemberId, memberSettings` | Set per-member settings | [L117](../Shared/Model/AppAPITypes.swift#L117) |
| `apiSetGroupAlias` | `groupId, localAlias` | Set local group alias | [L152](../Shared/Model/AppAPITypes.swift#L152) |

### 2.6 Chat Tags

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiGetChatTags` | `userId` | Get all user tags | [L44](../Shared/Model/AppAPITypes.swift#L44) |
| `apiCreateChatTag` | `tag: ChatTagData` | Create a new tag | [L50](../Shared/Model/AppAPITypes.swift#L50) |
| `apiSetChatTags` | `type, id, tagIds` | Assign tags to a chat | [L51](../Shared/Model/AppAPITypes.swift#L51) |
| `apiDeleteChatTag` | `tagId` | Delete a tag | [L52](../Shared/Model/AppAPITypes.swift#L52) |
| `apiUpdateChatTag` | `tagId, tagData` | Update tag name/emoji | [L53](../Shared/Model/AppAPITypes.swift#L53) |
| `apiReorderChatTags` | `tagIds` | Reorder tags | [L54](../Shared/Model/AppAPITypes.swift#L54) |

### 2.7 File Operations

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `receiveFile` | `fileId, userApprovedRelays, encrypted, inline` | Accept and download file | [L180](../Shared/Model/AppAPITypes.swift#L180) |
| `setFileToReceive` | `fileId, userApprovedRelays, encrypted` | Mark file for auto-receive | [L181](../Shared/Model/AppAPITypes.swift#L181) |
| `cancelFile` | `fileId` | Cancel file transfer | [L182](../Shared/Model/AppAPITypes.swift#L182) |
| `apiUploadStandaloneFile` | `userId, file: CryptoFile` | Upload file to XFTP (no chat) | [L192](../Shared/Model/AppAPITypes.swift#L192) |
| `apiDownloadStandaloneFile` | `userId, url, file: CryptoFile` | Download from XFTP URL | [L193](../Shared/Model/AppAPITypes.swift#L193) |
| `apiStandaloneFileInfo` | `url` | Get file metadata from XFTP URL | [L194](../Shared/Model/AppAPITypes.swift#L194) |

### 2.8 WebRTC Call Operations

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiSendCallInvitation` | `contact, callType` | Initiate call | [L168](../Shared/Model/AppAPITypes.swift#L168) |
| `apiRejectCall` | `contact` | Reject incoming call | [L169](../Shared/Model/AppAPITypes.swift#L169) |
| `apiSendCallOffer` | `contact, callOffer: WebRTCCallOffer` | Send SDP offer | [L170](../Shared/Model/AppAPITypes.swift#L170) |
| `apiSendCallAnswer` | `contact, answer: WebRTCSession` | Send SDP answer | [L171](../Shared/Model/AppAPITypes.swift#L171) |
| `apiSendCallExtraInfo` | `contact, extraInfo: WebRTCExtraInfo` | Send ICE candidates or ICE restart offer/answer | [L172](../Shared/Model/AppAPITypes.swift#L172) |
| `apiEndCall` | `contact` | End active call | [L173](../Shared/Model/AppAPITypes.swift#L173) |
| `apiGetCallInvitations` | -- | Get pending call invitations | [L174](../Shared/Model/AppAPITypes.swift#L174) |
| `apiCallStatus` | `contact, callStatus` | Report call status change | [L175](../Shared/Model/AppAPITypes.swift#L175) |

### 2.9 Push Notifications

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiGetNtfToken` | -- | Get current notification token | [L68](../Shared/Model/AppAPITypes.swift#L68) |
| `apiRegisterToken` | `token, notificationMode` | Register device token with server | [L69](../Shared/Model/AppAPITypes.swift#L69) |
| `apiVerifyToken` | `token, nonce, code` | Verify token registration | [L70](../Shared/Model/AppAPITypes.swift#L70) |
| `apiCheckToken` | `token` | Check token status | [L71](../Shared/Model/AppAPITypes.swift#L71) |
| `apiDeleteToken` | `token` | Unregister token | [L72](../Shared/Model/AppAPITypes.swift#L72) |
| `apiGetNtfConns` | `nonce, encNtfInfo` | Get notification connections (NSE) | [L73](../Shared/Model/AppAPITypes.swift#L73) |
| `apiGetConnNtfMessages` | `connMsgReqs` | Get notification messages (NSE) | [L74](../Shared/Model/AppAPITypes.swift#L74) |

### 2.10 Settings & Configuration

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiSaveSettings` | `settings: AppSettings` | Save app settings to core | [L42](../Shared/Model/AppAPITypes.swift#L42) |
| `apiGetSettings` | `settings: AppSettings` | Get settings from core | [L43](../Shared/Model/AppAPITypes.swift#L43) |
| `apiSetChatSettings` | `type, id, chatSettings` | Per-chat notification settings | [L116](../Shared/Model/AppAPITypes.swift#L116) |
| `apiSetChatItemTTL` | `userId, seconds` | Set global message TTL | [L108](../Shared/Model/AppAPITypes.swift#L108) |
| `apiGetChatItemTTL` | `userId` | Get global message TTL | [L109](../Shared/Model/AppAPITypes.swift#L109) |
| `apiSetChatTTL` | `userId, type, id, seconds` | Per-chat message TTL | [L110](../Shared/Model/AppAPITypes.swift#L110) |
| `apiSetNetworkConfig` | `networkConfig: NetCfg` | Set network configuration | [L111](../Shared/Model/AppAPITypes.swift#L111) |
| `apiGetNetworkConfig` | -- | Get network configuration | [L112](../Shared/Model/AppAPITypes.swift#L112) |
| `apiSetNetworkInfo` | `networkInfo: UserNetworkInfo` | Set network type/status | [L113](../Shared/Model/AppAPITypes.swift#L113) |
| `reconnectAllServers` | -- | Force reconnect all servers | [L114](../Shared/Model/AppAPITypes.swift#L114) |
| `reconnectServer` | `userId, smpServer` | Reconnect specific server | [L115](../Shared/Model/AppAPITypes.swift#L115) |

### 2.11 Database & Storage

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiStorageEncryption` | `config: DBEncryptionConfig` | Set/change database encryption | [L40](../Shared/Model/AppAPITypes.swift#L40) |
| `testStorageEncryption` | `key: String` | Test encryption key | [L41](../Shared/Model/AppAPITypes.swift#L41) |
| `apiExportArchive` | `config: ArchiveConfig` | Export database archive | [L37](../Shared/Model/AppAPITypes.swift#L37) |
| `apiImportArchive` | `config: ArchiveConfig` | Import database archive | [L38](../Shared/Model/AppAPITypes.swift#L38) |
| `apiDeleteStorage` | -- | Delete all storage | [L39](../Shared/Model/AppAPITypes.swift#L39) |

### 2.12 Server Operations

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiGetServerOperators` | -- | Get server operators | [L100](../Shared/Model/AppAPITypes.swift#L100) |
| `apiSetServerOperators` | `operators` | Set server operators | [L101](../Shared/Model/AppAPITypes.swift#L101) |
| `apiGetUserServers` | `userId` | Get user's configured servers | [L102](../Shared/Model/AppAPITypes.swift#L102) |
| `apiSetUserServers` | `userId, userServers` | Set user's servers | [L103](../Shared/Model/AppAPITypes.swift#L103) |
| `apiValidateServers` | `userId, userServers` | Validate server configuration; returns errors and warnings | [L104](../Shared/Model/AppAPITypes.swift#L104) |
| `apiGetUsageConditions` | -- | Get usage conditions | [L105](../Shared/Model/AppAPITypes.swift#L105) |
| `apiAcceptConditions` | `conditionsId, operatorIds` | Accept usage conditions | [L107](../Shared/Model/AppAPITypes.swift#L107) |
| `apiTestProtoServer` | `userId, server` | Test server connectivity | [L98](../Shared/Model/AppAPITypes.swift#L98) |

### 2.13 Theme & UI

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiSetUserUIThemes` | `userId, themes: ThemeModeOverrides?` | Set per-user theme | [L154](../Shared/Model/AppAPITypes.swift#L154) |
| `apiSetChatUIThemes` | `chatId, themes: ThemeModeOverrides?` | Set per-chat theme | [L155](../Shared/Model/AppAPITypes.swift#L155) |

### 2.14 Remote Desktop

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `setLocalDeviceName` | `displayName` | Set device name for pairing | [L184](../Shared/Model/AppAPITypes.swift#L184) |
| `connectRemoteCtrl` | `xrcpInvitation` | Connect to desktop via QR code | [L185](../Shared/Model/AppAPITypes.swift#L185) |
| `findKnownRemoteCtrl` | -- | Find previously paired desktops | [L186](../Shared/Model/AppAPITypes.swift#L186) |
| `confirmRemoteCtrl` | `remoteCtrlId` | Confirm known remote controller | [L187](../Shared/Model/AppAPITypes.swift#L187) |
| `verifyRemoteCtrlSession` | `sessionCode` | Verify session code | [L188](../Shared/Model/AppAPITypes.swift#L188) |
| `listRemoteCtrls` | -- | List known remote controllers | [L189](../Shared/Model/AppAPITypes.swift#L189) |
| `stopRemoteCtrl` | -- | Stop remote session | [L190](../Shared/Model/AppAPITypes.swift#L190) |
| `deleteRemoteCtrl` | `remoteCtrlId` | Delete known controller | [L191](../Shared/Model/AppAPITypes.swift#L191) |

### 2.15 Diagnostics

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `showVersion` | -- | Get core version info | [L201](../Shared/Model/AppAPITypes.swift#L201) |
| `getAgentSubsTotal` | `userId` | Get total SMP subscriptions | [L202](../Shared/Model/AppAPITypes.swift#L202) |
| `getAgentServersSummary` | `userId` | Get server summary stats | [L203](../Shared/Model/AppAPITypes.swift#L203) |
| `resetAgentServersStats` | -- | Reset server statistics | [L204](../Shared/Model/AppAPITypes.swift#L204) |

### 2.16 Address Management

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiCreateMyAddress` | `userId` | Create SimpleX address | [L156](../Shared/Model/AppAPITypes.swift#L156) |
| `apiDeleteMyAddress` | `userId` | Delete SimpleX address | [L157](../Shared/Model/AppAPITypes.swift#L157) |
| `apiShowMyAddress` | `userId` | Show current address | [L158](../Shared/Model/AppAPITypes.swift#L158) |
| `apiAddMyAddressShortLink` | `userId` | Add short link to address | [L159](../Shared/Model/AppAPITypes.swift#L159) |
| `apiSetProfileAddress` | `userId, on: Bool` | Toggle address in profile | [L160](../Shared/Model/AppAPITypes.swift#L160) |
| `apiSetAddressSettings` | `userId, addressSettings` | Configure address settings | [L161](../Shared/Model/AppAPITypes.swift#L161) |

### 2.17 Connection Security

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiGetContactCode` | `contactId` | Get verification code | [L129](../Shared/Model/AppAPITypes.swift#L129) |
| `apiGetGroupMemberCode` | `groupId, groupMemberId` | Get member verification code | [L130](../Shared/Model/AppAPITypes.swift#L130) |
| `apiVerifyContact` | `contactId, connectionCode` | Verify contact identity | [L131](../Shared/Model/AppAPITypes.swift#L131) |
| `apiVerifyGroupMember` | `groupId, groupMemberId, connectionCode` | Verify group member identity | [L132](../Shared/Model/AppAPITypes.swift#L132) |
| `apiSwitchContact` | `contactId` | Switch contact connection (key rotation) | [L123](../Shared/Model/AppAPITypes.swift#L123) |
| `apiSwitchGroupMember` | `groupId, groupMemberId` | Switch group member connection | [L124](../Shared/Model/AppAPITypes.swift#L124) |
| `apiAbortSwitchContact` | `contactId` | Abort contact switch | [L125](../Shared/Model/AppAPITypes.swift#L125) |
| `apiAbortSwitchGroupMember` | `groupId, groupMemberId` | Abort member switch | [L126](../Shared/Model/AppAPITypes.swift#L126) |
| `apiSyncContactRatchet` | `contactId, force` | Sync double-ratchet state | [L127](../Shared/Model/AppAPITypes.swift#L127) |
| `apiSyncGroupMemberRatchet` | `groupId, groupMemberId, force` | Sync member ratchet | [L128](../Shared/Model/AppAPITypes.swift#L128) |

### 2.18 Service Requests

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiSendServiceRequest` | `userId, target, timeoutSec: Double?, requestJSON` | Send a JSON request to a service address (e.g. SimpleX Directory) and wait up to the timeout for its response (`/_service_request`); returns `serviceResponse` | [L137](../Shared/Model/AppAPITypes.swift#L137) |

---

## 3. Response Types

Responses are split across three enums due to Swift enum size limitations:

### ChatResponse0

Synchronous query responses ([`AppAPITypes.swift` L729](../Shared/Model/AppAPITypes.swift#L729)):

| Response | Key Fields | Description | Source |
|----------|-----------|-------------|--------|
| `activeUser` | `user: User` | Current active user | [L730](../Shared/Model/AppAPITypes.swift#L730) |
| `usersList` | `users: [UserInfo]` | All user profiles | [L731](../Shared/Model/AppAPITypes.swift#L731) |
| `chatStarted` | -- | Chat engine started | [L732](../Shared/Model/AppAPITypes.swift#L732) |
| `chatRunning` | -- | Chat is already running | [L733](../Shared/Model/AppAPITypes.swift#L733) |
| `chatStopped` | -- | Chat engine stopped | [L734](../Shared/Model/AppAPITypes.swift#L734) |
| `apiChats` | `user, chats: [ChatData]` | All chat previews | [L735](../Shared/Model/AppAPITypes.swift#L735) |
| `apiChat` | `user, chat: ChatData, navInfo` | Single chat with messages | [L736](../Shared/Model/AppAPITypes.swift#L736) |
| `chatTags` | `user, userTags: [ChatTag]` | User's chat tags | [L738](../Shared/Model/AppAPITypes.swift#L738) |
| `chatItemInfo` | `user, chatItem, chatItemInfo` | Message detail info | [L739](../Shared/Model/AppAPITypes.swift#L739) |
| `serverTestResult` | `user, testServer, testFailure` | Server test result | [L740](../Shared/Model/AppAPITypes.swift#L740) |
| `userServersValidation` | `user, serverErrors: [UserServersError], serverWarnings: [UserServersWarning]` | Server validation result with errors and warnings | [L744](../Shared/Model/AppAPITypes.swift#L744) |
| `networkConfig` | `networkConfig: NetCfg` | Current network config | [L747](../Shared/Model/AppAPITypes.swift#L747) |
| `contactInfo` | `user, contact, connectionStats, customUserProfile` | Contact details | [L748](../Shared/Model/AppAPITypes.swift#L748) |
| `groupMemberInfo` | `user, groupInfo, member, connectionStats` | Member details | [L750](../Shared/Model/AppAPITypes.swift#L750) |
| `connectionVerified` | `verified, expectedCode` | Verification result | [L760](../Shared/Model/AppAPITypes.swift#L760) |
| `tagsUpdated` | `user, userTags, chatTags` | Tags changed | [L761](../Shared/Model/AppAPITypes.swift#L761) |

### ChatResponse1

Contact, message, and profile responses ([`AppAPITypes.swift` L857](../Shared/Model/AppAPITypes.swift#L857)):

| Response | Key Fields | Description | Source |
|----------|-----------|-------------|--------|
| `invitation` | `user, connLinkInvitation, connection` | Created invitation link | [L858](../Shared/Model/AppAPITypes.swift#L858) |
| `connectionPlan` | `user, connLink, connectionPlan` | Connection plan preview | [L861](../Shared/Model/AppAPITypes.swift#L861) |
| `serviceResponse` | `user, responseData: JSONValue` | A service's response to `apiSendServiceRequest`, left undecoded for the caller | [L862](../Shared/Model/AppAPITypes.swift#L862) |
| `newPreparedChat` | `user, chat: ChatData` | Prepared contact/group | [L863](../Shared/Model/AppAPITypes.swift#L863) |
| `startedConnectionToGroup` | `user, groupInfo, relayResults: [RelayConnectionResult]` | Group/channel join initiated; relay results indicate per-relay connection success/failure | [L869](../Shared/Model/AppAPITypes.swift#L869) |
| `contactDeleted` | `user, contact` | Contact deleted | [L872](../Shared/Model/AppAPITypes.swift#L872) |
| `newChatItems` | `user, chatItems: [AChatItem]` | New messages sent/received | [L890](../Shared/Model/AppAPITypes.swift#L890) |
| `chatItemUpdated` | `user, chatItem: AChatItem` | Message edited | [L894](../Shared/Model/AppAPITypes.swift#L894) |
| `chatItemReaction` | `user, added, reaction` | Reaction change | [L896](../Shared/Model/AppAPITypes.swift#L896) |
| `chatItemsDeleted` | `user, chatItemDeletions, byUser` | Messages deleted | [L898](../Shared/Model/AppAPITypes.swift#L898) |
| `contactsList` | `user, contacts: [Contact]` | All contacts list | [L899](../Shared/Model/AppAPITypes.swift#L899) |
| `userProfileUpdated` | `user, fromProfile, toProfile` | Profile changed | [L878](../Shared/Model/AppAPITypes.swift#L878) |
| `userContactLinkCreated` | `user, connLinkContact` | Address created | [L886](../Shared/Model/AppAPITypes.swift#L886) |
| `forwardPlan` | `user, chatItemIds, forwardConfirmation` | Forward plan result | [L893](../Shared/Model/AppAPITypes.swift#L893) |
| `groupChatItemsDeleted` | `user, groupInfo, chatItemIDs, byUser, member_` | Group items deleted | [L892](../Shared/Model/AppAPITypes.swift#L892) |

### ChatResponse2

Group, file, call, notification, and misc responses ([`AppAPITypes.swift` L1003](../Shared/Model/AppAPITypes.swift#L1003)):

| Response | Key Fields | Description | Source |
|----------|-----------|-------------|--------|
| `groupCreated` | `user, groupInfo` | New group created | [L1005](../Shared/Model/AppAPITypes.swift#L1005) |
| `publicGroupCreated` | `user, groupInfo, groupLink, groupRelays: [GroupRelay]` | New public group (channel) created with relay info | [L1006](../Shared/Model/AppAPITypes.swift#L1006) |
| `groupRelays` | `user, groupInfo, groupRelays: [GroupRelay]` | Group relay list (owner API response) | [L1008](../Shared/Model/AppAPITypes.swift#L1008) |
| `sentGroupInvitation` | `user, groupInfo, contact, member` | Group invitation sent | [L1011](../Shared/Model/AppAPITypes.swift#L1011) |
| `groupMembers` | `user, group: Group` | Group member list | [L1015](../Shared/Model/AppAPITypes.swift#L1015) |
| `membersRoleUser` | `user, groupInfo, members, toRole` | Role changed | [L1019](../Shared/Model/AppAPITypes.swift#L1019) |
| `groupUpdated` | `user, toGroup: GroupInfo` | Group profile updated | [L1021](../Shared/Model/AppAPITypes.swift#L1021) |
| `groupLinkCreated` | `user, groupInfo, groupLink` | Group link created | [L1024](../Shared/Model/AppAPITypes.swift#L1024) |
| `rcvFileAccepted` | `user, chatItem` | File download started | [L1031](../Shared/Model/AppAPITypes.swift#L1031) |
| `callInvitations` | `callInvitations: [RcvCallInvitation]` | Pending calls | [L1040](../Shared/Model/AppAPITypes.swift#L1040) |
| `ntfToken` | `token, status, ntfMode, ntfServer` | Notification token info | [L1043](../Shared/Model/AppAPITypes.swift#L1043) |
| `versionInfo` | `versionInfo, chatMigrations, agentMigrations` | Core version | [L1051](../Shared/Model/AppAPITypes.swift#L1051) |
| `cmdOk` | `user_` | Generic success | [L1052](../Shared/Model/AppAPITypes.swift#L1052) |
| `archiveExported` | `archiveErrors: [ArchiveError]` | Export result | [L1056](../Shared/Model/AppAPITypes.swift#L1056) |
| `archiveImported` | `archiveErrors: [ArchiveError]` | Import result | [L1057](../Shared/Model/AppAPITypes.swift#L1057) |
| `appSettings` | `appSettings: AppSettings` | Retrieved settings | [L1058](../Shared/Model/AppAPITypes.swift#L1058) |

---

## 4. Event Types

The `ChatEvent` enum ([`AppAPITypes.swift` L1179](../Shared/Model/AppAPITypes.swift#L1179)) represents async events from the Haskell core. These arrive via `chat_recv_msg_wait` polling, not as responses to commands.

Event processing entry point: [`processReceivedMsg`](../Shared/Model/SimpleXAPI.swift#L2555) in `SimpleXAPI.swift`.

### Connection Events

| Event | Key Fields | Description | Source |
|-------|-----------|-------------|--------|
| `contactConnected` | `user, contact, userCustomProfile` | Contact connection established | [L1186](../Shared/Model/AppAPITypes.swift#L1186) |
| `contactConnecting` | `user, contact` | Contact connecting in progress | [L1187](../Shared/Model/AppAPITypes.swift#L1187) |
| `contactSndReady` | `user, contact` | Ready to send to contact | [L1188](../Shared/Model/AppAPITypes.swift#L1188) |
| `contactDeletedByContact` | `user, contact` | Contact deleted by other party | [L1185](../Shared/Model/AppAPITypes.swift#L1185) |
| `contactUpdated` | `user, toContact` | Contact profile updated | [L1190](../Shared/Model/AppAPITypes.swift#L1190) |
| `receivedContactRequest` | `user, contactRequest, chat_` | Incoming contact request | [L1189](../Shared/Model/AppAPITypes.swift#L1189) |
| `subscriptionStatus` | `subscriptionStatus, connections` | Connection subscription change | [L1192](../Shared/Model/AppAPITypes.swift#L1192) |

### Message Events

| Event | Key Fields | Description | Source |
|-------|-----------|-------------|--------|
| `newChatItems` | `user, chatItems: [AChatItem]` | New messages received | [L1194](../Shared/Model/AppAPITypes.swift#L1194) |
| `chatItemUpdated` | `user, chatItem: AChatItem` | Message edited remotely | [L1196](../Shared/Model/AppAPITypes.swift#L1196) |
| `chatItemReaction` | `user, added, reaction: ACIReaction` | Reaction added/removed | [L1197](../Shared/Model/AppAPITypes.swift#L1197) |
| `chatItemsDeleted` | `user, chatItemDeletions, byUser` | Messages deleted | [L1198](../Shared/Model/AppAPITypes.swift#L1198) |
| `chatItemsStatusesUpdated` | `user, chatItems: [AChatItem]` | Delivery status changed | [L1195](../Shared/Model/AppAPITypes.swift#L1195) |
| `groupChatItemsDeleted` | `user, groupInfo, chatItemIDs, byUser, member_` | Group items deleted | [L1200](../Shared/Model/AppAPITypes.swift#L1200) |
| `chatInfoUpdated` | `user, chatInfo` | Chat metadata changed | [L1193](../Shared/Model/AppAPITypes.swift#L1193) |

### Group Events

| Event | Key Fields | Description | Source |
|-------|-----------|-------------|--------|
| `receivedGroupInvitation` | `user, groupInfo, contact, memberRole` | Group invitation received | [L1201](../Shared/Model/AppAPITypes.swift#L1201) |
| `userAcceptedGroupSent` | `user, groupInfo, hostContact` | Joined group | [L1203](../Shared/Model/AppAPITypes.swift#L1203) |
| `groupLinkConnecting` | `user, groupInfo, hostMember` | Connecting via group link | [L1204](../Shared/Model/AppAPITypes.swift#L1204) |
| `joinedGroupMemberConnecting` | `user, groupInfo, hostMember, member` | Member joining | [L1206](../Shared/Model/AppAPITypes.swift#L1206) |
| `memberRole` | `user, groupInfo, byMember, member, fromRole, toRole` | Role changed | [L1208](../Shared/Model/AppAPITypes.swift#L1208) |
| `memberBlockedForAll` | `user, groupInfo, byMember, member, blocked` | Member blocked | [L1209](../Shared/Model/AppAPITypes.swift#L1209) |
| `deletedMemberUser` | `user, groupInfo, member, withMessages` | Current user removed | [L1210](../Shared/Model/AppAPITypes.swift#L1210) |
| `deletedMember` | `user, groupInfo, byMember, deletedMember` | Member removed | [L1211](../Shared/Model/AppAPITypes.swift#L1211) |
| `leftMember` | `user, groupInfo, member` | Member left | [L1212](../Shared/Model/AppAPITypes.swift#L1212) |
| `groupDeleted` | `user, groupInfo, member` | Group deleted | [L1213](../Shared/Model/AppAPITypes.swift#L1213) |
| `userJoinedGroup` | `user, groupInfo, hostMember` | Successfully joined; `hostMember` is upserted into group members | [L1214](../Shared/Model/AppAPITypes.swift#L1214) |
| `joinedGroupMember` | `user, groupInfo, member` | New member joined | [L1215](../Shared/Model/AppAPITypes.swift#L1215) |
| `connectedToGroupMember` | `user, groupInfo, member, memberContact` | E2E session established with member | [L1216](../Shared/Model/AppAPITypes.swift#L1216) |
| `groupUpdated` | `user, toGroup: GroupInfo` | Group profile changed | [L1217](../Shared/Model/AppAPITypes.swift#L1217) |
| `groupLinkRelaysUpdated` | `user, groupInfo, groupLink, groupRelays: [GroupRelay]` | Channel relay configuration changed | [L1107](../Shared/Model/AppAPITypes.swift#L1107) |
| `groupMemberUpdated` | `user, groupInfo, fromMember, toMember` | Member info updated | [L1191](../Shared/Model/AppAPITypes.swift#L1191) |
| `groupRelayUpdated` | `user, groupInfo, member, groupRelay` | Owner-side: a relay's `relayStatus` and/or the member's status changed. Fires on `XGrpRelayReject` with `relayStatus = .rsRejected` and `member.memberStatus = .memRejected` — final until cleared by the relay operator's `/group allow <groupId>` (no event emitted to the owner for that clear). | Controller.hs (`CEvtGroupRelayUpdated`) |

### File Transfer Events

| Event | Key Fields | Description | Source |
|-------|-----------|-------------|--------|
| `rcvFileStart` | `user, chatItem` | Download started | [L1224](../Shared/Model/AppAPITypes.swift#L1224) |
| `rcvFileProgressXFTP` | `user, chatItem_, receivedSize, totalSize` | Download progress | [L1225](../Shared/Model/AppAPITypes.swift#L1225) |
| `rcvFileComplete` | `user, chatItem` | Download complete | [L1226](../Shared/Model/AppAPITypes.swift#L1226) |
| `rcvFileSndCancelled` | `user, chatItem, rcvFileTransfer` | Sender cancelled | [L1228](../Shared/Model/AppAPITypes.swift#L1228) |
| `rcvFileError` | `user, chatItem_, agentError, rcvFileTransfer` | Download error | [L1229](../Shared/Model/AppAPITypes.swift#L1229) |
| `sndFileStart` | `user, chatItem, sndFileTransfer` | Upload started | [L1232](../Shared/Model/AppAPITypes.swift#L1232) |
| `sndFileComplete` | `user, chatItem, sndFileTransfer` | Upload complete (inline) | [L1233](../Shared/Model/AppAPITypes.swift#L1233) |
| `sndFileProgressXFTP` | `user, chatItem_, fileTransferMeta, sentSize, totalSize` | Upload progress | [L1235](../Shared/Model/AppAPITypes.swift#L1235) |
| `sndFileCompleteXFTP` | `user, chatItem, fileTransferMeta` | XFTP upload complete | [L1237](../Shared/Model/AppAPITypes.swift#L1237) |
| `sndFileError` | `user, chatItem_, fileTransferMeta, errorMessage` | Upload error | [L1239](../Shared/Model/AppAPITypes.swift#L1239) |

### Call Events

| Event | Key Fields | Description | Source |
|-------|-----------|-------------|--------|
| `callInvitation` | `callInvitation: RcvCallInvitation` | Incoming call | [L1242](../Shared/Model/AppAPITypes.swift#L1242) |
| `callOffer` | `user, contact, callType, offer, sharedKey, askConfirmation` | SDP offer received | [L1243](../Shared/Model/AppAPITypes.swift#L1243) |
| `callAnswer` | `user, contact, answer` | SDP answer received | [L1244](../Shared/Model/AppAPITypes.swift#L1244) |
| `callExtraInfo` | `user, contact, extraInfo` | ICE candidates received | [L1245](../Shared/Model/AppAPITypes.swift#L1245) |
| `callEnded` | `user, contact` | Call ended by remote | [L1246](../Shared/Model/AppAPITypes.swift#L1246) |

### Connection Security Events

| Event | Key Fields | Description | Source |
|-------|-----------|-------------|--------|
| `contactSwitch` | `user, contact, switchProgress` | Key rotation progress | [L1181](../Shared/Model/AppAPITypes.swift#L1181) |
| `groupMemberSwitch` | `user, groupInfo, member, switchProgress` | Member key rotation | [L1182](../Shared/Model/AppAPITypes.swift#L1182) |
| `contactRatchetSync` | `user, contact, ratchetSyncProgress` | Ratchet sync progress | [L1183](../Shared/Model/AppAPITypes.swift#L1183) |
| `groupMemberRatchetSync` | `user, groupInfo, member, ratchetSyncProgress` | Member ratchet sync | [L1184](../Shared/Model/AppAPITypes.swift#L1184) |

### System Events

| Event | Key Fields | Description | Source |
|-------|-----------|-------------|--------|
| `chatSuspended` | -- | Core suspended | [L1180](../Shared/Model/AppAPITypes.swift#L1180) |

---

## 5. Error Types

Defined in [`SimpleXChat/APITypes.swift` L765](../SimpleXChat/APITypes.swift#L765):

```swift
public enum ChatError: Decodable, Hashable, Error {
    case error(errorType: ChatErrorType)
    case errorAgent(agentError: AgentErrorType)
    case errorStore(storeError: StoreError)
    case errorDatabase(databaseError: DatabaseError)
    case errorRemoteCtrl(remoteCtrlError: RemoteCtrlError)
    case invalidJSON(json: Data?)
    case unexpectedResult(type: String)
}
```

### Error Categories

| Category | Enum | Description | Source |
|----------|------|-------------|--------|
| Chat logic | `ChatErrorType` | Business logic errors (e.g., invalid state, permission denied, `chatRelayExists`) | [`APITypes.swift` L788](../SimpleXChat/APITypes.swift#L788) |
| SMP Agent | `AgentErrorType` | Protocol/network errors from the SMP agent layer | [`APITypes.swift` L1046](../SimpleXChat/APITypes.swift#L1046) |
| Database store | `StoreError` | SQLite query/constraint errors (includes relay-related: `relayUserNotFound`, `duplicateMemberId`, `userChatRelayNotFound`, `groupRelayNotFound`, `groupRelayNotFoundByMemberId`) | [`APITypes.swift` L964](../SimpleXChat/APITypes.swift#L964) |
| Database engine | `DatabaseError` | DB open/migration/encryption errors | [`APITypes.swift` L1033](../SimpleXChat/APITypes.swift#L1033) |
| Remote control | `RemoteCtrlError` | Remote desktop session errors | [`APITypes.swift` L1234](../SimpleXChat/APITypes.swift#L1234) |
| Parse failure | `invalidJSON` | Failed to decode response JSON | [`APITypes.swift` L765](../SimpleXChat/APITypes.swift#L765) |
| Unexpected | `unexpectedResult` | Response type does not match expected | [`APITypes.swift` L765](../SimpleXChat/APITypes.swift#L765) |

---

## 6. FFI Bridge Functions

Defined in [`Shared/Model/SimpleXAPI.swift`](../Shared/Model/SimpleXAPI.swift):

### Synchronous (blocking current thread)

```swift
// Throws on error, returns typed result
func chatSendCmdSync<R: ChatAPIResult>(           // SimpleXAPI.swift L93
    _ cmd: ChatCommand,
    bgTask: Bool = true,
    bgDelay: Double? = nil,
    ctrl: chat_ctrl? = nil,
    log: Bool = true
) throws -> R

// Returns APIResult (caller handles error)
func chatApiSendCmdSync<R: ChatAPIResult>(         // SimpleXAPI.swift L99
    _ cmd: ChatCommand,
    bgTask: Bool = true,
    bgDelay: Double? = nil,
    ctrl: chat_ctrl? = nil,
    retryNum: Int32 = 0,
    log: Bool = true
) -> APIResult<R>
```

### Asynchronous (Swift concurrency)

```swift
// Throws on error, returns typed result
func chatSendCmd<R: ChatAPIResult>(                // SimpleXAPI.swift L121
    _ cmd: ChatCommand,
    bgTask: Bool = true,
    bgDelay: Double? = nil,
    ctrl: chat_ctrl? = nil,
    log: Bool = true
) async throws -> R

// Returns APIResult with optional retry on network errors
func chatApiSendCmdWithRetry<R: ChatAPIResult>(    // SimpleXAPI.swift L127
    _ cmd: ChatCommand,
    bgTask: Bool = true,
    bgDelay: Double? = nil,
    inProgress: BoxedValue<Bool>? = nil,
    retryNum: Int32 = 0,
    log: Bool = true
) async -> APIResult<R>?
```

### Low-Level FFI

```swift
// Direct C FFI call -- serializes cmd.cmdString, calls chat_send_cmd_retry, decodes response
public func sendSimpleXCmd<R: ChatAPIResult>(      // API.swift L115
    _ cmd: ChatCmdProtocol,
    _ ctrl: chat_ctrl?,
    retryNum: Int32 = 0
) -> APIResult<R>
```

### Event Receiver

```swift
// Polls for async events from the Haskell core
func chatRecvMsg(                                  // SimpleXAPI.swift L241
    _ ctrl: chat_ctrl? = nil
) async -> APIResult<ChatEvent>?

// Processes a received event and updates app state
func processReceivedMsg(                           // SimpleXAPI.swift L2555
    _ res: ChatEvent
) async
```

---

## 7. Result Type

Defined in [`SimpleXChat/APITypes.swift` L27](../SimpleXChat/APITypes.swift#L27):

```swift
public enum APIResult<R>: Decodable where R: Decodable, R: ChatAPIResult {
    case result(R)           // Successful response
    case error(ChatError)    // Error response from core
    case invalid(type: String, json: Data)  // Undecodable response

    public var responseType: String { ... }
    public var unexpected: ChatError { ... }
}

public protocol ChatAPIResult: Decodable {         // APITypes.swift L65
    var responseType: String { get }
    var details: String { get }
    static func fallbackResult(_ type: String, _ json: NSDictionary) -> Self?
}
```

The `decodeAPIResult<R>` function ([`APITypes.swift` L86](../SimpleXChat/APITypes.swift#L86)) handles JSON decoding with fallback logic:
1. Try standard `JSONDecoder.decode(APIResult<R>.self, from: data)`
2. If that fails, try manual JSON parsing via `JSONSerialization`
3. Check for `"error"` key -- return `.error`
4. Check for `"result"` key -- try `R.fallbackResult` or return `.invalid`
5. Last resort: return `.invalid(type: "invalid", json: ...)`

---

## 8. Directory Search and Connection Plans

Wrappers in [`Shared/Model/SimpleXAPI.swift`](../Shared/Model/SimpleXAPI.swift) used by the online search (see [Chat List, Online Search](client/chat-list.md#11-online-search)):

| Function | Description | Source |
|----------|-------------|--------|
| `apiSearchDirectory(_ text:, cursor:)` | Sends `apiSendServiceRequest` to `DIRECTORY_SERVICE_LINK` with `DIRECTORY_SEARCH_TIMEOUT_SEC` and the search request; returns `parseDirectorySearchResponse(responseData)`, or nil on any error (logged). Blocks for up to the timeout | [L1044](../Shared/Model/SimpleXAPI.swift#L1044) |
| `apiConnectPlan(connLink:, resolveMode:, linkOwnerSig:, inProgress:, onFailure:)` | Plans a connection; returns `ConnectionPlanResult?`. `notResolvedLocally` (a `.never` lookup that matched nothing) returns nil silently; any other failure is handled by `onFailure` | [L1064](../Shared/Model/SimpleXAPI.swift#L1064) |
| `ConnectPlanFailure` | `.alert` (default, every existing caller): `apiConnectResponseAlert`; `.log`: debug log only, used by the online search so name resolution and per-entry lookups never raise alerts | [L1060](../Shared/Model/SimpleXAPI.swift#L1060) |

A service response is carried as [`JSONValue`](../SimpleXChat/APITypes.swift#L722) (any JSON), so the framework does not depend on a service's own types; callers decode it with [`decodeJSONValue`](../SimpleXChat/APITypes.swift#L760).

---

## Source Files

| File | Path |
|------|------|
| ChatCommand enum | [`Shared/Model/AppAPITypes.swift` L15](../Shared/Model/AppAPITypes.swift#L15) |
| ChatResponse0/1/2 enums | [`Shared/Model/AppAPITypes.swift` L729, L857, L1003](../Shared/Model/AppAPITypes.swift#L729) |
| ChatEvent enum | [`Shared/Model/AppAPITypes.swift` L1179](../Shared/Model/AppAPITypes.swift#L1179) |
| APIResult, ChatError | [`SimpleXChat/APITypes.swift` L27, L765](../SimpleXChat/APITypes.swift#L27) |
| FFI bridge functions | [`Shared/Model/SimpleXAPI.swift`](../Shared/Model/SimpleXAPI.swift) |
| Service response JSON | [`SimpleXChat/APITypes.swift` L722](../SimpleXChat/APITypes.swift#L722) |
| Low-level FFI | [`SimpleXChat/API.swift`](../SimpleXChat/API.swift) |
| Data types | `SimpleXChat/ChatTypes.swift` |
| C header | `SimpleXChat/SimpleX.h` |
| Haskell controller | `../../src/Simplex/Chat/Controller.hs` |
